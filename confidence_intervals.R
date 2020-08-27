#!/usr/bin/env Rscript 

#install.packages("nortest")
library(nortest)

is_normal_distribution = function(data) {
    sw_test_results = shapiro.test(data)
    sw_p_value = sw_test_results[2]
    ad_test_results = ad.test(data)
    ad_p_value = ad_test_results[2]
    alpha_level = 0.05
    if (sw_p_value > alpha_level || ad_p_value > alpha_level) {
        print("TRUE")
        TRUE
    } else {
        print("NOT NORMAL")
        FALSE
    }
}

plot_distribution = function(data, label, distribution_is_normal) {
    jpeg(file = sprintf("%s_distribution.jpg", label))
    main_heading = sprintf("Histogram of %s", label)
    sub_heading = sprintf("Normal distribution: %s", distribution_is_normal)
    hist(data,
        xlab=label,
        main=main_heading,
        sub=sub_heading)
    dev.off()
}

bimodal_distribution = c(runif(100,1,30),
    runif(200,1,90),
    runif(100,60,90))

is_normal = is_normal_distribution(bimodal_distribution)
plot_distribution(bimodal_distribution, "bimodal", is_normal)

samples_target = 100
sample_size = 30
sample_statistics = c()

for (i in (1:samples_target)) {
    random_sample = sample(bimodal_distribution, size=sample_size)
    sample_statistic = mean(random_sample) #Also called a point estimate
    sample_statistics = c(sample_statistics, sample_statistic)
}

is_normal = is_normal_distribution(sample_statistics)
plot_distribution(sample_statistics, "sample_means", is_normal)

# Regardless of the population distribution, the distribution of sample statistics (eg a list of mean for all random samples) will be a normal distribution, given large enough sample sizes
# Is a sample representative of the population? We can measure that with confidence intervals.
# With a larger confidence interval, we can make the claim that more of the sample's observations are going to represent the population faithfully.
# A smaller confidence interval will take in data closest to the mean of a normal distribution, while a larger confidence interval will include the mean but also more of the areas towards the extremes.
# Because a normal distribution trails off towards the extremes, then the size of samples is inversely proportional to the standard error

# confidence intervals

# critical values | confidence intervals
# 1.645           | 90%
# 1.96            | 95%
# 2.576           | 99%

a_sample = sample(bimodal_distribution, size=sample_size)

# When you know the SD of the population
standard_error = sd(bimodal_distribution) / sqrt(length(bimodal_distribution))
point_estimate = mean(bimodal_distribution)
critical_value = 1.96
print("mean of population (using population mean as point estimate):")
print(sprintf("95pc confidence for +/- %#.3f of %f", (critical_value * standard_error), point_estimate))
confidence_interval_lower = point_estimate - (critical_value * standard_error)
confidence_interval_upper = point_estimate + (critical_value * standard_error)
print(sprintf("%f < population mean < %f", confidence_interval_lower, confidence_interval_upper))

# Need to have some tests for the below... even manual ones

# When you know the SD of the sample but not the population 
standard_error = sd(a_sample) / sqrt(length(a_sample))
point_estimate = mean(a_sample)
critical_value = max( qt(c(0.025, 0.975), df=length(a_sample)-1) )
print("confidence interval (using mean):")
print(sprintf("95pc confidence for +/- %#.3f of %f", (critical_value * standard_error), point_estimate))
confidence_interval_lower = point_estimate - (critical_value * standard_error)
confidence_interval_upper = point_estimate + (critical_value * standard_error)
print(sprintf("%f < population mean < %f", confidence_interval_lower, confidence_interval_upper))
