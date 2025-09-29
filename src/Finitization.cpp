#include "Finitization.h"
using namespace std;

#ifndef RESTRICT
#if defined(__GNUC__) || defined(__clang__)
#define RESTRICT __restrict__
#else
#define RESTRICT
#endif
#endif


Finitization::Finitization(int n): m_finitizationOrder(n), m_dprobs{nullptr}, m_finish{false}, m_unif_double_distribution{0.0, 1.0}, m_unif_int_distribution(0,n) {
    // Memory allocation for alias method
    const int K = m_finitizationOrder + 1;
    m_alias = new int[K];
    m_prob = new double[K];
    m_values = new int[K];
    m_ntsfFirstTime = true;
    for(int i = 0; i <= n; i++)
        m_values[i] = i;
    //m_generator.seed(time(0)); // Seed RNG

    // optional small-K PMF cache (allocated lazily in setProbs)
    m_pmf_small = nullptr;
    m_smallK    = false;
}

Finitization::~Finitization() {
    delete[] m_alias;
    delete[] m_prob;
    delete[] m_values;
    delete[] m_dprobs;
}

/*
void Finitization::setProbs(double* p) {
    int i, a, g;
    int tmp = m_finitizationOrder + 1;
    int *S = new int[tmp];
    int *L = new int[tmp];
    double *P = new double[tmp];

    double sum=0.0;
    for (i = 0; i < tmp; ++i ) {
        sum += p[i];
    }
    for (i = 0; i < tmp; ++i )
        P[i] = p[i] * (tmp) / sum;

    int nS = 0, nL = 0;
    for (i = m_finitizationOrder; i>=0; --i ) {
        P[i] < 1 ? (S[nS++] = i) : (L[nL++] = i);
    }
    while ( nS && nL ) {
        a = S[--nS];
        g = L[--nL];
        m_prob[a] = P[a];
        m_alias[a] = g;
        P[g] += P[a] - 1;
        P[g] < 1 ? (S[nS++] = g) : (L[nL++] = g) ;
    }

    while ( nL )
        m_prob[ L[--nL] ] = 1;

    while ( nS )
        m_prob[ S[--nS] ] = 1;

    delete[] S;
    delete[] L;
    delete[] P;

}
*/

void Finitization::setProbs(double* p) {
    const int K = m_finitizationOrder + 1;
    if (K <= 0) stop("Internal: K <= 0 in setProbs.");

    // 1) Normalize p -> prob
    double sum = 0.0;
    for (int i = 0; i < K; ++i) {
        if (p[i] < 0.0) stop("Negative probability at index %d.", i);
        sum += p[i];
    }
    if (sum <= 0.0) stop("Sum of probabilities is zero in setProbs().");
    const double inv_sum = 1.0 / sum;

    // Optional: cache normalized PMF for tiny-K fast path (K ≤ 8)
    if (K <= 8) {
        if (!m_pmf_small) m_pmf_small = new double[K];
        for (int i = 0; i < K; ++i) m_pmf_small[i] = p[i] * inv_sum;
        m_smallK = true;
    } else {
        // free if previously allocated for smaller K (optional hygiene)
        if (m_pmf_small) { delete[] m_pmf_small; m_pmf_small = nullptr; }
        m_smallK = false;
    }

    // 2) Scale by K (Vose/Walker) and partition into small/large
    //    We’ll re-use a temporary array P (length K).
    double* P = new double[K];
    for (int i = 0; i < K; ++i) P[i] = (p[i] * inv_sum) * (double)K;

    // Simple stack-based small/large lists
    int* S = new int[K];
    int* L = new int[K];
    int nS = 0, nL = 0;

    // Fill S/L (reverse order or forward doesn’t matter)
    for (int i = K - 1; i >= 0; --i) {
        (P[i] < 1.0) ? (S[nS++] = i) : (L[nL++] = i);
    }

    // 3) Build alias table
    while (nS && nL) {
        const int a = S[--nS];   // small
        const int g = L[--nL];   // large

        m_prob[a]  = P[a];       // cutoff in [0,1]
        m_alias[a] = g;          // alias target

        P[g] = (P[g] + P[a]) - 1.0;
        (P[g] < 1.0) ? (S[nS++] = g) : (L[nL++] = g);
    }

    // 4) Leftovers have cutoff 1 and alias to themselves (robustness)
    while (nL) { const int i = L[--nL]; m_prob[i] = 1.0; m_alias[i] = i; }
    while (nS) { const int i = S[--nS]; m_prob[i] = 1.0; m_alias[i] = i; }

    delete[] P;
    delete[] S;
    delete[] L;
}

/*
IntegerVector Finitization::rvalues(int no) {
    IntegerVector result(no);
    int* r = result.begin();
    double r2;
    int ii;
    for( int i = 0; i < no; ++i) {
        ii = m_values[m_unif_int_distribution(m_generator)];
        r2 = m_unif_double_distribution(m_generator);
        r[i] = (r2 < m_prob[ii]) ? ii : m_alias[ii];
    }
    return result;
}
*/
// Fast alias sampling: single RNG boundary, no allocations, tight loop.

/*
IntegerVector Finitization::rvalues(int no) {
    if (no < 0) stop("'no' must be nonnegative.");

    const int K = this->m_finitizationOrder+1;
    const double* RESTRICT cutoff = this->m_prob;   // acceptance cutoff
    const int*    RESTRICT alias  = this->m_alias;  // alias target
    const double  Kd = static_cast<double>(K);

    IntegerVector out(no);
    int* RESTRICT outp = out.begin();

    GetRNGstate();

    // Unroll factor
    constexpr int UNROLL = 8;
    const int n8 = (no / UNROLL) * UNROLL;

    int t = 0;
    for (; t < n8; t += UNROLL) {
        double ip;

        // 1
        double uK0 = unif_rand() * Kd;
        double f0  = modf(uK0, &ip);
        uint32_t j0 = (uint32_t)ip;
        outp[t+0] = (f0 < cutoff[j0]) ? (int)j0 : alias[j0];

        // 2
        double uK1 = unif_rand() * Kd;
        double f1  = modf(uK1, &ip);
        uint32_t j1 = (uint32_t)ip;
        outp[t+1] = (f1 < cutoff[j1]) ? (int)j1 : alias[j1];

        // 3
        double uK2 = unif_rand() * Kd;
        double f2  = modf(uK2, &ip);
        uint32_t j2 = (uint32_t)ip;
        outp[t+2] = (f2 < cutoff[j2]) ? (int)j2 : alias[j2];

        // 4
        double uK3 = unif_rand() * Kd;
        double f3  = modf(uK3, &ip);
        uint32_t j3 = (uint32_t)ip;
        outp[t+3] = (f3 < cutoff[j3]) ? (int)j3 : alias[j3];

        // 5
        double uK4 = unif_rand() * Kd;
        double f4  = modf(uK4, &ip);
        uint32_t j4 = (uint32_t)ip;
        outp[t+4] = (f4 < cutoff[j4]) ? (int)j4 : alias[j4];

        // 6
        double uK5 = unif_rand() * Kd;
        double f5  = modf(uK5, &ip);
        uint32_t j5 = (uint32_t)ip;
        outp[t+5] = (f5 < cutoff[j5]) ? (int)j5 : alias[j5];

        // 7
        double uK6 = unif_rand() * Kd;
        double f6  = modf(uK6, &ip);
        uint32_t j6 = (uint32_t)ip;
        outp[t+6] = (f6 < cutoff[j6]) ? (int)j6 : alias[j6];

        // 8
        double uK7 = unif_rand() * Kd;
        double f7  = modf(uK7, &ip);
        uint32_t j7 = (uint32_t)ip;
        outp[t+7] = (f7 < cutoff[j7]) ? (int)j7 : alias[j7];
    }

    // Remainder
    for (; t < no; ++t) {
        double ip;
        double uK = unif_rand() * Kd;        // one RNG call per draw
        double f  = modf(uK, &ip);           // split into index + fraction
        uint32_t j = (uint32_t)ip;           // j in [0..K-1]
        outp[t] = (f < cutoff[j]) ? (int)j : alias[j];
    }

    PutRNGstate();
    return out;
}
*/

IntegerVector Finitization::rvalues(int no) {
    if (no < 0) stop("'no' must be nonnegative.");

    const int K  = m_finitizationOrder + 1;
    const double* RESTRICT cutoff = m_prob;   // alias cutoff in [0,1]
    const int*    RESTRICT alias  = m_alias;  // alias indices 0..K-1

    IntegerVector out(no);
    int* RESTRICT p = out.begin();

    GetRNGstate();

    // ---- Tiny-K (K ≤ 8): unrolled CDF ladder (very fast for small n) ----
    if (K <= 8 && m_smallK && m_pmf_small != nullptr) {
        // build CDF once
        double cdf8[8];
        double acc = 0.0;
        for (int i = 0; i < K; ++i) { acc += m_pmf_small[i]; cdf8[i] = acc; }
        cdf8[K - 1] = 1.0; // exact 1

        constexpr int UN = 16;
        const int nU = (no / UN) * UN;
        int t = 0;

        // unrolled ladder for K up to 4 (most common finitization orders).
        // For K>4 && ≤8, the while-ladder fallback is still fast; but we’ll keep
        // a branch-minimal version here for K up to 4.
        if (K == 4) {
            for (; t < nU; t += UN, p += UN) {
                const double u0 = unif_rand();  p[0]  = (u0 < cdf8[0]) ? 0 : (u0 < cdf8[1]) ? 1 : (u0 < cdf8[2]) ? 2 : 3;
                const double u1 = unif_rand();  p[1]  = (u1 < cdf8[0]) ? 0 : (u1 < cdf8[1]) ? 1 : (u1 < cdf8[2]) ? 2 : 3;
                const double u2 = unif_rand();  p[2]  = (u2 < cdf8[0]) ? 0 : (u2 < cdf8[1]) ? 1 : (u2 < cdf8[2]) ? 2 : 3;
                const double u3 = unif_rand();  p[3]  = (u3 < cdf8[0]) ? 0 : (u3 < cdf8[1]) ? 1 : (u3 < cdf8[2]) ? 2 : 3;
                const double u4 = unif_rand();  p[4]  = (u4 < cdf8[0]) ? 0 : (u4 < cdf8[1]) ? 1 : (u4 < cdf8[2]) ? 2 : 3;
                const double u5 = unif_rand();  p[5]  = (u5 < cdf8[0]) ? 0 : (u5 < cdf8[1]) ? 1 : (u5 < cdf8[2]) ? 2 : 3;
                const double u6 = unif_rand();  p[6]  = (u6 < cdf8[0]) ? 0 : (u6 < cdf8[1]) ? 1 : (u6 < cdf8[2]) ? 2 : 3;
                const double u7 = unif_rand();  p[7]  = (u7 < cdf8[0]) ? 0 : (u7 < cdf8[1]) ? 1 : (u7 < cdf8[2]) ? 2 : 3;
                const double u8 = unif_rand();  p[8]  = (u8 < cdf8[0]) ? 0 : (u8 < cdf8[1]) ? 1 : (u8 < cdf8[2]) ? 2 : 3;
                const double u9 = unif_rand();  p[9]  = (u9 < cdf8[0]) ? 0 : (u9 < cdf8[1]) ? 1 : (u9 < cdf8[2]) ? 2 : 3;
                const double uA = unif_rand();  p[10] = (uA < cdf8[0]) ? 0 : (uA < cdf8[1]) ? 1 : (uA < cdf8[2]) ? 2 : 3;
                const double uB = unif_rand();  p[11] = (uB < cdf8[0]) ? 0 : (uB < cdf8[1]) ? 1 : (uB < cdf8[2]) ? 2 : 3;
                const double uC = unif_rand();  p[12] = (uC < cdf8[0]) ? 0 : (uC < cdf8[1]) ? 1 : (uC < cdf8[2]) ? 2 : 3;
                const double uD = unif_rand();  p[13] = (uD < cdf8[0]) ? 0 : (uD < cdf8[1]) ? 1 : (uD < cdf8[2]) ? 2 : 3;
                const double uE = unif_rand();  p[14] = (uE < cdf8[0]) ? 0 : (uE < cdf8[1]) ? 1 : (uE < cdf8[2]) ? 2 : 3;
                const double uF = unif_rand();  p[15] = (uF < cdf8[0]) ? 0 : (uF < cdf8[1]) ? 1 : (uF < cdf8[2]) ? 2 : 3;
            }
            for (; t < no; ++t, ++p) {
                const double u = unif_rand();
                *p = (u < cdf8[0]) ? 0 : (u < cdf8[1]) ? 1 : (u < cdf8[2]) ? 2 : 3;
            }
            PutRNGstate();
            return out;
        } else {
            // Generic tiny-K ladder (K 2..8)
            const int nU2 = (no / 8) * 8;
            int s = 0;
            for (; s < nU2; s += 8, p += 8) {
                for (int r = 0; r < 8; ++r) {
                    const double u = unif_rand();
                    int x = 0;
                    while (x < K - 1 && u > cdf8[x]) ++x;
                    p[r] = x;
                }
            }
            for (; s < no; ++s, ++p) {
                const double u = unif_rand();
                int x = 0;
                while (x < K - 1 && u > cdf8[x]) ++x;
                *p = x;
            }
            PutRNGstate();
            return out;
        }
    }

    // ---- General path: Vose alias, 1 uniform per draw, unrolled ×16 ----
    const double Kd = (double)K;
    constexpr int UN = 16;
    const int nU = (no / UN) * UN;

    int t = 0;
    for (; t < nU; t += UN, p += UN) {
        // 1
        double uK0 = unif_rand() * Kd; const uint32_t j0 = (uint32_t)uK0; const double f0 = uK0 - (double)j0;
        p[0] = (f0 < cutoff[j0]) ? (int)j0 : alias[j0];
        // 2
        double uK1 = unif_rand() * Kd; const uint32_t j1 = (uint32_t)uK1; const double f1 = uK1 - (double)j1;
        p[1] = (f1 < cutoff[j1]) ? (int)j1 : alias[j1];
        // 3
        double uK2 = unif_rand() * Kd; const uint32_t j2 = (uint32_t)uK2; const double f2 = uK2 - (double)j2;
        p[2] = (f2 < cutoff[j2]) ? (int)j2 : alias[j2];
        // 4
        double uK3 = unif_rand() * Kd; const uint32_t j3 = (uint32_t)uK3; const double f3 = uK3 - (double)j3;
        p[3] = (f3 < cutoff[j3]) ? (int)j3 : alias[j3];
        // 5
        double uK4 = unif_rand() * Kd; const uint32_t j4 = (uint32_t)uK4; const double f4 = uK4 - (double)j4;
        p[4] = (f4 < cutoff[j4]) ? (int)j4 : alias[j4];
        // 6
        double uK5 = unif_rand() * Kd; const uint32_t j5 = (uint32_t)uK5; const double f5 = uK5 - (double)j5;
        p[5] = (f5 < cutoff[j5]) ? (int)j5 : alias[j5];
        // 7
        double uK6 = unif_rand() * Kd; const uint32_t j6 = (uint32_t)uK6; const double f6 = uK6 - (double)j6;
        p[6] = (f6 < cutoff[j6]) ? (int)j6 : alias[j6];
        // 8
        double uK7 = unif_rand() * Kd; const uint32_t j7 = (uint32_t)uK7; const double f7 = uK7 - (double)j7;
        p[7] = (f7 < cutoff[j7]) ? (int)j7 : alias[j7];
        // 9
        double uK8 = unif_rand() * Kd; const uint32_t j8 = (uint32_t)uK8; const double f8 = uK8 - (double)j8;
        p[8] = (f8 < cutoff[j8]) ? (int)j8 : alias[j8];
        // 10
        double uK9 = unif_rand() * Kd; const uint32_t j9 = (uint32_t)uK9; const double f9 = uK9 - (double)j9;
        p[9] = (f9 < cutoff[j9]) ? (int)j9 : alias[j9];
        // 11
        double uK10 = unif_rand() * Kd; const uint32_t j10 = (uint32_t)uK10; const double f10 = uK10 - (double)j10;
        p[10] = (f10 < cutoff[j10]) ? (int)j10 : alias[j10];
        // 12
        double uK11 = unif_rand() * Kd; const uint32_t j11 = (uint32_t)uK11; const double f11 = uK11 - (double)j11;
        p[11] = (f11 < cutoff[j11]) ? (int)j11 : alias[j11];
        // 13
        double uK12 = unif_rand() * Kd; const uint32_t j12 = (uint32_t)uK12; const double f12 = uK12 - (double)j12;
        p[12] = (f12 < cutoff[j12]) ? (int)j12 : alias[j12];
        // 14
        double uK13 = unif_rand() * Kd; const uint32_t j13 = (uint32_t)uK13; const double f13 = uK13 - (double)j13;
        p[13] = (f13 < cutoff[j13]) ? (int)j13 : alias[j13];
        // 15
        double uK14 = unif_rand() * Kd; const uint32_t j14 = (uint32_t)uK14; const double f14 = uK14 - (double)j14;
        p[14] = (f14 < cutoff[j14]) ? (int)j14 : alias[j14];
        // 16
        double uK15 = unif_rand() * Kd; const uint32_t j15 = (uint32_t)uK15; const double f15 = uK15 - (double)j15;
        p[15] = (f15 < cutoff[j15]) ? (int)j15 : alias[j15];
    }

    // remainder
    for (int r = nU; r < no; ++r, ++p) {
        const double uK = unif_rand() * (double)K;
        const uint32_t j = (uint32_t)uK;
        const double   f = uK - (double)j;
        *p = (f < cutoff[j]) ? (int)j : alias[j];
    }

    PutRNGstate();
    return out;
}

ex Finitization::ntsf( ex pnb) {

    if(m_ntsfFirstTime) {
        m_ntsfSymb = series_to_poly(pnb.series(m_x == 0, m_finitizationOrder+1));
        m_ntsfFirstTime = false;
    }
    return m_ntsfSymb;
}

ex Finitization::pdf(ex ntsf, int x_val) {
    ex optheta = -m_paramSymb;
    ex pdf;
    if( x_val > 1 && m_cache.find(x_val-1) != m_cache.end()) {
        pdf = m_cache[x_val-1].diff(m_x, 1);
        m_cache.insert({x_val, pdf});
    }
    else {
        pdf = ntsf.diff(m_x, x_val);
        m_cache.insert({x_val, pdf});
    }

    pdf = pdf.subs(m_x == optheta) * pow(m_paramSymb, x_val) / factorial(x_val);
    return pdf;

}

ex Finitization::fin_pdfSymb(int x_val) {
    ex pdf_ = pdf(ntsf(ntsd_base(m_x, m_paramSymb)), x_val);
    return pdf_;
}


#include <cfloat>   // DBL_EPSILON
#include <cmath>    // std::fabs
#include <ginac/ginac.h>
// Map the expression tree, turning numerics into doubles,
// and zeroing those strictly below machine epsilon.
GiNaC::ex map_double_and_zero_eps(const GiNaC::ex& e) {
    using namespace GiNaC;
    if (is_a<numeric>(e)) {
        const double d = ex_to<numeric>(e).to_double();
        if (std::fabs(d) < DBL_EPSILON)
            return numeric(DBL_EPSILON);      // exact zero
        return numeric(d);          // floating numeric from a double
    }
    return e.map(map_double_and_zero_eps);
}

string Finitization::pdfToString(int val, bool tolatex) {
    stringstream result;
    ex pdf_ = fin_pdfSymb(val);

    pdf_ = map_double_and_zero_eps(pdf_); // then ensure atoms are made from doubles

    if(tolatex)
        result << latex;
    result << pdf_;
    return result.str();
}


double Finitization::fin_pdf(int val) {
    if(m_finish && val <= m_finitizationOrder)
        return m_dprobs[val];
    else {
        ex pdf_ = fin_pdfSymb(val);
        double tmp = GiNaC::ex_to<GiNaC::numeric>(evalf(pdf_.subs(m_paramSymb == m_theta))).to_double();
        const double eps   = std::numeric_limits<double>::epsilon();
        const double atmp  = std::abs(tmp);
        const double scale = std::max(1.0, atmp);
        const double tol   = 64.0 * eps * scale + 1e-300; // denormal floor
        double x = (atmp <= tol) ? 0.0 : tmp;  // zero tiny magnitudes
        x = std::max(x, 0.0);
        return x;
    }
}

