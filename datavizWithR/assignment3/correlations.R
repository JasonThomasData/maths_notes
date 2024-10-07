asthma <- read.csv("asthma.csv")
diabetes <- read.csv("diabetes.csv")
heart_stroke_vascular <- read.csv("heart_stroke_vascular.csv")
three_or_more_chronic <- read.csv("three_or_more_chronic.csv")
SA2_SEIFA_2021 <- read.csv("SA2_SEIFA_2021.csv")

#Remove duplicates
diabetes <- diabetes[, -2]
heart_stroke_vascular <- heart_stroke_vascular[, -2]
three_or_more_chronic <- three_or_more_chronic[, -2]
SA2_SEIFA_2021 <- SA2_SEIFA_2021[, -2]

names(SA2_SEIFA_2021)[1] <- "SA2.Code"

data <- merge(asthma, diabetes, "SA2.Code")
data <- merge(data, heart_stroke_vascular, "SA2.Code")
data <- merge(data, three_or_more_chronic, "SA2.Code")
data <- merge(data, SA2_SEIFA_2021, "SA2.Code")
#data <- data[!is.na(data),]
# NAs are not plotted by default

plot(data$ISRAD, data$Three.or.more.chronic.....aged.60.years.and.over)

variable_labeller <- function(value){
  variable_names <- list(
    "Heart.stroke.vascular.....All.ages" = "Three or more chronic diseases (%), all ages",
    "Heart.stroke.vascular.....60.years.and.over" = "Three or more chronic diseases (%), aged 60 years and older",
    "Heart.stroke.vascular.....70.years.and.over" = "Three or more chronic diseases (%), aged 70 years and older",
    "Three.or.more.chronic.....All.ages" = "Three or more chronic diseases (%), all ages",
    "Three.or.more.chronic.....60.years.and.over" = "Three or more chronic diseases (%), aged 60 years and older",
    "Three.or.more.chronic.....70.years.and.over" = "Three or more chronic diseases (%), aged 70 years and older"
  )
  return(variable_names[value])
}

head(data)

# Need a process to select the column from Seifa and Health data, and then 
# remove rows that contain unsatisfactory data. Then show the number of points.

plot(data$ISRAD, data$Asthma.....All.ages)