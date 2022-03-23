
packages = c('nortest', 'tidyr', 'jsonlite', 'car')
for(package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

get_linear_intersection = function(y_int_1, gradient_1, y_int_2, gradient_2, x_int_2=NULL) {
    # The second line may have an x_int and then its gradient and y_int are disregarded
    # This is expected to work for linear functions only. Quadratics etc are not gauranteed to have an x intercept
    if(!is.null(x_int_2)) {
        point_y_1 = gradient_1 * x_int_2 + y_int_1
        point = c(x_int_2, point_y_1)
        return (point)
    }
    common_x = (y_int_2 - y_int_1) / (gradient_1 - gradient_2)
    point_y_1 = gradient_1 * common_x + y_int_1
    point_y_2 = gradient_2 * common_x + y_int_2
    if (round(point_y_1, 10) == round(point_y_2, 10)) {
        point = c(common_x, point_y_1)
        return (point)
    } else {
        print("No intersection") 
    }
}

feature_scale = function(original_data) {
    return ((original_data-min(original_data))/(max(original_data)-min(original_data)))
}

plot_correlation = function(dependent, independent, dep_label, ind_label, cor_method="pearson", save_to_file=FALSE) {
    correlation_summary = cor.test(dependent, independent, method=cor_method)
    title = sprintf("Correlation coefficient: %f", correlation_summary$estimate)
    if (save_to_file) {
        jpeg(file = sprintf("%s_correlation.jpg", ind_label))
    }    
    plot(independent,
        dependent,
        abline( lm(dependent~independent)),
        xlab=ind_label,
        ylab=dep_label,
        main=title)
    if (save_to_file) {
        dev.off()
    }
}

is_normal_distribution = function(data, alpha_level = 0.05) {
    # These tests use H_0 that population is normal, so p-value < alpha means you reject and say the population is not normal
    sw_test_results = shapiro.test(data)
    sw_p_value = sw_test_results[2]
    ad_test_results = ad.test(data)
    ad_p_value = ad_test_results[2]
    if (sw_p_value > alpha_level && ad_p_value > alpha_level) {
        print("TRUE")
        TRUE
    } else {
        print("NOT NORMAL")
        FALSE
    }
}

plot_distribution = function(data, label, distribution_is_normal, save_to_file=FALSE) {
    if (save_to_file) {
        jpeg(file = sprintf("%s_distribution.jpg", label))
    }    
    main_heading = sprintf("Histogram of %s", label)
    sub_heading = sprintf("Normal distribution: %s", distribution_is_normal)
    hist(data,
        xlab=label,
        main=main_heading,
        sub=sub_heading)
    if (save_to_file) {
        dev.off()
    }
}

summarise = function(data, row_header) {
    header = sprintf("%s, mean, SD, median, IQR", row_header)
    message(header)
    number_of_columns = ncol(data)
    if (is.null(number_of_columns) || number_of_columns == 1) {
        summary_row = sprintf("%s, %#.2f, %#.2f, %#.2f, %#.2f", ".", mean(data), sd(data), median(data), IQR(data))
        message(summary_row)
    } else {
        for (i in (1:number_of_columns)) {
            column = data[,i]
            summary_row = sprintf("%s, %#.2f, %#.2f, %#.2f, %#.2f", names(data)[i], mean(column), sd(column), median(column), IQR(column))
            message(summary_row)
        }
    }
}

plot_multiline = function(series_1, label_1, series_2, label_2, colour_1="red", colour_2="green", filename="multiline.jpg", save_to_file=FALSE) {
    series_1_numbers = tidyr::extract_numeric(series_1)
    series_2_numbers = tidyr::extract_numeric(series_2)
    y_upper_limit = max(c(series_1_numbers, series_2_numbers))
    if (save_to_file) {    
        jpeg(file = filename)
    }
    plot(series_1_numbers, ylim=c(0,y_upper_limit), type="l", col=colour_1, ylab="Y")
    lines(series_2_numbers, col=colour_2)
    legend("topleft", legend=c(label_1, label_2),
        col=c(colour_1, colour_2), lty=1:1, cex=0.8)
    if (save_to_file) { 
        dev.off()
    }
}

all_are_numeric = function(series) {
    for(elem in series) {
        if(! is.numeric(elem) || is.na(elem)) {
            return(FALSE)
        }
    }
    return (TRUE)
}

get_moving_averages = function(series, moving_average_bin_size, output_offset=0) {
    length_of_series = length(series)
    moving_averages = c()
    elements_before_average = floor(moving_average_bin_size / 2) + output_offset #Make this a separate function, write tests
    elements_after_average = ceiling(moving_average_bin_size / 2) - 1 - output_offset
    moving_averages_end_at = length_of_series - elements_after_average
    number_of_moving_averages = moving_averages_end_at - elements_before_average
    first_index_for_average = 1
    for (i in seq(1, number_of_moving_averages)) {
        subset_to_find_average = series[first_index_for_average:(first_index_for_average + moving_average_bin_size - 1)]
        if(all_are_numeric(subset_to_find_average)) {
            moving_averages = c(moving_averages, mean(subset_to_find_average))
        } else {
            moving_averages = c(moving_averages, NA)
        }
        first_index_for_average = first_index_for_average + 1
    }
    if(elements_before_average > 0) {
        moving_averages = c(rep(NA, elements_before_average), moving_averages)
    }
    if(elements_after_average > 0) {
        moving_averages = c(moving_averages, rep(NA, elements_after_average))
    }
    return (moving_averages)
}

check_weights = function(operands, weights) {
  operands_count = length(operands)
  weights_count = length(weights)
  if(typeof(weights) != "double") {
    stop("Weights should be a vector of doubles") 
  }
  if(weights_count != operands_count) {
    stop(sprintf("There are %s weights and %s operands", weights_count, operands_count)) 
  }
}

geometric_mean = function(operands, weights=NULL) {
  if(typeof(operands)!="double") {
    stop("Operands should be a vector of doubles")
  }
  if(!is.null(weights)) {
    check_weights(operands, weights)
    return (prod(operands ^ weights) ^ (1/sum(weights)))
  }
  return (prod(operands) ^ (1/length(operands)))
}

power_mean = function(operands, power, weights=NULL) {
  # p =-1 harmonic mean
  # p = 0 geometric mean
  # p = 1 arithmetic mean
  if(power == 0) {
    #Cannot have anything to the power of zero, as it always returns 1. The power mean would work if the p was approaching zero, as the mean would approach the geometric mean
    geometric_mean(operands, weights)
  } else {
    if(typeof(operands)!="double") {
      stop("Operands should be a vector of doubles")
    }
    if(!is.null(weights)) {
      check_weights(operands, weights)
      #operands = operands * weights
      return ((sum(weights*operands^power)/sum(weights))^(1/power))
    }
    return ((sum(operands^power) / length(operands))^(1/power))
  }
}

get_linear_model = function(independent, dependent) {
    df = data.frame(independent)
    df$dependent = dependent 
    linear_model = lm(dependent~independent, data=df)
    return (linear_model)
}

