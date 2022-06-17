library(dplyr)
library(tsibble)
library(readr)
library(lubridate)

bikets <- read_csv(
  "SeoulBikeData.csv",
  col_names = c(
    "Date",
    "BikeCount",
    "Hour",
    "Temperature",
    "Humidity",
    "WindSpeed",
    "Visibility",
    "Dewpoint",
    "SolarRadiation",
    "Rainfall",
    "Snowfall",
    "Seasons",
    "Holiday",
    "FunctionalDay"
  ),
  skip = 1,
  col_types = cols(
    "Hour" = col_time(format = "%H"),
    Seasons = "f",
    Holiday = "f",
    FunctionalDay = "f"
  )
) %>%
  mutate(
    Hour = parse_date_time(
      paste(Date, Hour),
      orders = c("dmy HMS", "dmY HMS"),
      tz = "Asia/Seoul"
    ),
    .before = everything(),
    Date = NULL,
    BikeCount = ifelse(FunctionalDay == "Yes", BikeCount, NA),
    FunctionalDay = NULL,
    Holiday = forcats::fct_relevel(Holiday, "No Holiday")
  ) %>%
  as_tsibble(index = Hour)

## Insert any other data prep/cleaning steps here






## Create training and test folds

ndays <- nrow(bikets)/24
days_for_cv <- 50

bikes_train <- stretch_tsibble(bikets,
                               .step = 24,
                               .init = nrow(bikets)-(days_for_cv*24)-3,
                               .id = "fold") %>%
  filter(fold <= 50)

bikes_test <- new_data(bikes_train, n = 27) %>%
  inner_join(bikets, by = "Hour")
