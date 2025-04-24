#ifndef DISTRIBUTIONTYPE_H_
#define DISTRIBUTIONTYPE_H_

/**
 * @class DistributionType
 * @brief A simple utility class that defines constants for identifying distribution types.
 *
 * This class provides integer constants used throughout the finitization framework
 * to differentiate between supported distributions. These identifiers are used for
 * switch-case logic, function dispatching, and internal configuration.
 */
class DistributionType {

public:
    // Identifier for the Poisson distribution
    static const int POISSON = 0;

    // Identifier for the Binomial distribution
    static const int BINOMIAL = 1;

    // Identifier for the Negative Binomial distribution
    static const int NEGATIVEBINOMIAL = 2;

    // Identifier for the Logarithmic distribution
    static const int LOGARITHMIC = 3;
};

#endif /* DISTRIBUTIONTYPE_H_ */
