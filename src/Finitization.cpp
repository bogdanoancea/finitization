#include "Finitization.h"
using namespace std;

#ifndef RESTRICT
#if defined(__GNUC__) || defined(__clang__)
#define RESTRICT __restrict__
#else
#define RESTRICT
#endif
#endif

static const int K_LADDER_MAX = 8;

#if defined(_MSC_VER)
#define FORCEINLINE __forceinline
#else
#define FORCEINLINE __attribute__((always_inline)) inline
#endif

template<int K>
FORCEINLINE int sample_cdf_ladder(double u, const double* cdf, const int* idx) {
    int ix = 0;
    if  (K > 1) ix += (u > cdf[0]);
    if  (K > 2) ix += (u > cdf[1]);
    if  (K > 3) ix += (u > cdf[2]);
    if  (K > 4) ix += (u > cdf[3]);
    if  (K > 5) ix += (u > cdf[4]);
    if  (K > 6) ix += (u > cdf[5]);
    if  (K > 7) ix += (u > cdf[6]);
    // For K<=8, ix is in [0..K-1]
    return idx[ix];
}

FORCEINLINE int sample_cdf_ladder_runtime(int K, double u, const double* cdf, const int* idx) {
    switch (K) {
    case 1: return idx[0];
    case 2: return sample_cdf_ladder<2>(u, cdf, idx);
    case 3: return sample_cdf_ladder<3>(u, cdf, idx);
    case 4: return sample_cdf_ladder<4>(u, cdf, idx);
    case 5: return sample_cdf_ladder<5>(u, cdf, idx);
    case 6: return sample_cdf_ladder<6>(u, cdf, idx);
    case 7: return sample_cdf_ladder<7>(u, cdf, idx);
    case 8: return sample_cdf_ladder<8>(u, cdf, idx);
    default: {
        // fallback: loop / binary search / alias for larger K
        int ix = K - 1;
        for (int t = 0; t < K - 1; ++t) { if (u <= cdf[t]) { ix = t; break; } }
        return idx[ix];
    }
    }
}


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

    // --- small-K PMF cache for macOS CDF ladder (K <= K_LADDER_MAX) ---
    if (K <= K_LADDER_MAX) {
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
    const double* RESTRICT cutoff = m_prob;
    const int*    RESTRICT alias  = m_alias;

    IntegerVector out(no);
    int* RESTRICT p = out.begin();

    GetRNGstate();

#if defined(__APPLE__)
    // ===== APPLE: small-K path with probability-ordered ladder =====
    if (K <= K_LADDER_MAX && m_pmf_small != nullptr) {
        // Copy pmf and indices into small local arrays
        alignas(64) double prob[8];
        alignas(64) int    idx[8];
        alignas(64) double cdf_ladder[8];

        for (int i = 0; i < K; ++i) {
            prob[i] = m_pmf_small[i];  // normalized pmf for state i
            idx[i]  = i;               // original support value
        }

        // Sort by decreasing probability (tiny insertion sort, K<=4)
        for (int i = 1; i < K; ++i) {
            double key_p = prob[i];
            int    key_i = idx[i];
            int j = i - 1;
            while (j >= 0 && prob[j] < key_p) {
                prob[j + 1] = prob[j];
                idx[j + 1]  = idx[j];
                --j;
            }
            prob[j + 1] = key_p;
            idx[j + 1]  = key_i;
        }

        // Build CDF in sorted order
        double acc = 0.0;
        for (int i = 0; i < K; ++i) {
            acc += prob[i];
            cdf_ladder[i] = acc;
        }
        cdf_ladder[K - 1] = 1.0; // enforce exact 1

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC unroll 16
#endif
        for (int i = 0; i < no; ++i) {
            p[i] = sample_cdf_ladder_runtime(K, unif_rand(), cdf_ladder, idx);
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

        double   uK[UN];
        uint32_t j[UN];
        float    f[UN];

        // 1) Generate uniforms scaled by K
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC unroll 16
#endif
        for (int i = 0; i < UN; ++i) {
            uK[i] = unif_rand() * Kd;
        }

        // 2) Split into integer bucket j and fractional part f
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC unroll 16
#endif
        for (int i = 0; i < UN; ++i) {
            const double uki = uK[i];
            const uint32_t ji = (uint32_t)uki;         // floor for uki>=0
            j[i] = ji;
            f[i] = (float)(uki - (double)ji);
        }

        // 3) Alias decision + write output
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC unroll 16
#endif
        for (int i = 0; i < UN; ++i) {
            const uint32_t jj = j[i];
            // cutoff is double in your class; compare in float is fine, but keep cutoff as double
            p[i] = ((double)f[i] < cutoff[jj]) ? (int)jj : alias[jj];
        }
    }

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

