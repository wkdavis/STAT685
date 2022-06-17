bikets <- readr::read_csv("SeoulBikeData.csv",
                        col_names = c("Date",
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
                                      "FunctionalDay"),
                        skip = 1,
                        col_types = cols("Hour" = col_time(format = "%H"),
                                         Seasons = "f",
                                         Holiday = "f",
                                         FunctionalDay = "f")) %>%
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

bikets

holidays <- bikets %>%
  filter(Holiday == "Holiday") %>%
  mutate(ds = as.Date(Hour)) %>%
  distinct(ds) %>%
  mutate(holiday = "holiday")
