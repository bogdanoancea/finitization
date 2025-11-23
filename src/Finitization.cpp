#include "Finitization.h"
using namespace std;

#ifndef RESTRICT
#if defined(__GNUC__) || defined(__clang__)
#define RESTRICT __restrict__
#else
#define RESTRICT
#endif
#endif


Finitization::Finitization(int n): m_finitizationOrder(n), m_dprobs{nullptr}, m_finish{false} {
    // Memory allocation for alias method
    const int K = m_finitizationOrder + 1;
    m_alias = new int[K];
    m_prob = new double[K];
    m_values = new int[K];
    m_ntsfFirstTime = true;
    for(int i = 0; i <= n; i++)
        m_values[i] = i;

    // optional small-K PMF cache (allocated lazily in setProbs)
    m_pmf_small = nullptr;
    m_smallK    = false;
}

Finitization::~Finitization() {
    delete[] m_alias;
    delete[] m_prob;
    delete[] m_values;
    delete[] m_dprobs;
    if (m_pmf_small) {
        delete[] m_pmf_small;
        m_pmf_small = nullptr;
    }
}

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

    // --- small-K PMF cache for macOS CDF ladder (K <= 4) ---
    if (K <= 4) {
        if (!m_pmf_small) {
            m_pmf_small = new double[K];
        }
        for (int i = 0; i < K; ++i) {
            m_pmf_small[i] = p[i] * inv_sum;  // normalized pmf
        }
        m_smallK = true;
    } else {
        if (m_pmf_small) {
            delete[] m_pmf_small;
            m_pmf_small = nullptr;
        }
        m_smallK = false;
    }

    // 2) Scale by K (Vose/Walker) and partition into small/large
    double* P = new double[K];
    for (int i = 0; i < K; ++i)
        P[i] = (p[i] * inv_sum) * static_cast<double>(K);

    int* S = new int[K];
    int* L = new int[K];
    int nS = 0, nL = 0;

    for (int i = K - 1; i >= 0; --i) {
        (P[i] < 1.0) ? (S[nS++] = i) : (L[nL++] = i);
    }

    // 3) Build alias table
    while (nS && nL) {
        const int a = S[--nS];
        const int g = L[--nL];

        m_prob[a]  = P[a];
        m_alias[a] = g;

        P[g] = (P[g] + P[a]) - 1.0;
        (P[g] < 1.0) ? (S[nS++] = g) : (L[nL++] = g);
    }

    // 4) Leftovers
    while (nL) {
        const int i = L[--nL];
        m_prob[i]  = 1.0;
        m_alias[i] = i;
    }
    while (nS) {
        const int i = S[--nS];
        m_prob[i]  = 1.0;
        m_alias[i] = i;
    }

    delete[] P;
    delete[] S;
    delete[] L;
}
IntegerVector Finitization::rvalues(int no) {
    if (no < 0) {
        stop("'no' must be nonnegative.");
    }

    const int K  = m_finitizationOrder + 1;
    const double* RESTRICT cutoff = m_prob;   // alias cutoff in [0,1]
    const int*    RESTRICT alias  = m_alias;  // alias indices 0..K-1

    IntegerVector out(no);
    int* RESTRICT p = out.begin();

    GetRNGstate();

    // ===== ARM64-specific tiny-K path: CDF ladder for K <= 4 =====
#if defined(__aarch64__) || defined(_M_ARM64)
    if (K <= 4 && m_smallK && m_pmf_small != nullptr) {
        // build small CDF once
        double cdf4[4];
        double acc = 0.0;
        for (int i = 0; i < K; ++i) {
            acc += m_pmf_small[i];
            cdf4[i] = acc;
        }
        cdf4[K - 1] = 1.0; // enforce exact 1.0 on last support point

        for (int i = 0; i < no; ++i, ++p) {
            const double u = unif_rand();
            int x = 0;

            if (u <= cdf4[0]) {
                x = 0;
            } else if (K > 1 && u <= cdf4[1]) {
                x = 1;
            } else if (K > 2 && u <= cdf4[2]) {
                x = 2;
            } else {
                x = K - 1;  // 3 if K == 4, or last support point if K < 4
            }

            *p = x;
        }

        PutRNGstate();
        return out;
    }
#endif

    // ===== General alias path for all other cases / platforms =====
    const double Kd = static_cast<double>(K);
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
        const double uK = unif_rand() * Kd;
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

