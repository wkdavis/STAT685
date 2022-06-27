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

### tidyverts way

bikets2 <- bikets %>%
  mutate(rowid = row_number())

bikets_cv <- filter_index(bikets2, . ~ "2018-10-11 23:00:00 KST")


bikes_stretched <- stretch_tsibble(slice_head(bikets_cv, n = -24),
                                   .step = 24,
                                   .init = nrow(bikets_cv)*.8,
                                   .id = "fold")

bikes_stretched %>%
  filter(fold == max(fold)) %>%
  select(fold, rowid) %>%
  tail()

### caret way

bikets_cv <- filter_index(bikets, . ~ "2018-10-11 23:00:00 KST")


bikeslice <- createTimeSlices(bikets_cv$Hour,
                              initialWindow = nrow(bikets_cv)*.8,
                              horizon = 24,
                              skip = 23,
                              fixedWindow = FALSE)
