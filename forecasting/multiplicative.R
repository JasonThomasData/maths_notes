#!/usr/bin/env Rscript

source("../data_science_helpers/R_helpers.R")

get_means_for_quarters = function(series) {
    quarter_1_total = 0
    quarter_2_total = 0
    quarter_3_total = 0
    quarter_4_total = 0
    quarter_1_count = 0
    quarter_2_count = 0
    quarter_3_count = 0
    quarter_4_count = 0
    for (i in seq_along(series)) {
        if(i %% 4 == 0) {
            quarter_4 = series[i]
            if(!is.na(quarter_4) & is.numeric(quarter_4)) {
                quarter_4_total = quarter_4_total + quarter_4
                quarter_4_count = quarter_4_count + 1
            }
            quarter_3 = series[i-1]
            if(!is.na(quarter_3) & is.numeric(quarter_3)) {
                quarter_3_total = quarter_3_total + quarter_3
                quarter_3_count = quarter_3_count + 1
            }
            quarter_2 = series[i-2]
            if(!is.na(quarter_2) & is.numeric(quarter_2)) {
                quarter_2_total = quarter_2_total + quarter_2
                quarter_2_count = quarter_2_count + 1
            }
            quarter_1 = series[i-3]
            if(!is.na(quarter_1) & is.numeric(quarter_1)) {
                quarter_1_total = quarter_1_total + quarter_1
                quarter_1_count = quarter_1_count + 1
            }
        }
    }
    return (list("qtr_1"=quarter_1_total/quarter_1_count,
        "qtr_2"=quarter_2_total/quarter_2_count,
        "qtr_3"=quarter_3_total/quarter_3_count,
        "qtr_4"=quarter_4_total/quarter_4_count))
}

get_trend = function(series) {
    moving_average_yearly = get_moving_averages(series, 4)
    centred_moving_average = get_moving_averages(moving_average_yearly, 2, output_offset=-1) #since the moving average is not odd numbers
    return (centred_moving_average) # This is just the trend
}

get_seasonal_variation = function(series_without_trend, series_including_forecast_length) {
    means_for_quarters = get_means_for_quarters(series_without_trend)
    print(series_including_forecast_length)
}

# Given some cyclical timeseries data, make some forecasts
timeseries = read.csv("cyclical_data.csv", header = TRUE)
# So, clearly the data is years 1-4, and we need to make forecasts about Year 5
trend = get_trend(timeseries[ ,"Sales"])
series_without_trend = timeseries[ ,"Sales"] / trend
seasonal_variation = get_seasonal_variation(series_without_trend, length(timeseries[ ,"T"]))