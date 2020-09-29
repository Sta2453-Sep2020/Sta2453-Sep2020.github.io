
library(tidyverse)
library(rvest)
library(tidyverse)

# read in the data
bike <- read_csv('Bikeshare Ridership (2017 Q1).csv')

# print a table of the first 6 observations
DT::datatable(head(bike), options = list(scrollX = T))

# number of observations and variables
cat("number of rows:", nrow(bike), "Number of columns:", ncol(bike), "\n")

# or we could print both at once
dim(bike)

# a look at the data types
glimpse(bike)

# is the trip_id unique 
n_distinct(bike$trip_id) == nrow(bike)

# dates are character variables

# what is the format?

time_splits <- strsplit(bike$trip_start_time, "/")

# the first looks like day
table(as.numeric(sapply(time_splits, "[[", 1)))

# the second looks like month
table(as.numeric(sapply(time_splits, "[[", 2)))

# split the last section by the space
sub_time_split <- strsplit(sapply(time_splits, "[[", 3), " ")

# the third looks like year
table(as.numeric(sapply(sub_time_split, "[[", 1)))

# split the remaining section by :

sub_time_split2 <- strsplit(sapply(sub_time_split, "[[", 2), ":")

# the fourth looks like hour
table(as.numeric(sapply(sub_time_split2, "[[", 1)))

# the fifth looks like minute
table(as.numeric(sapply(sub_time_split2, "[[", 2)))

# so the format is day/month/year hour:minute (we can use lubridate)

bike <- bike %>% 
  mutate(trip_start_ts = lubridate::dmy_hm(trip_start_time),
         trip_stop_ts = lubridate::dmy_hm(trip_stop_time))

# this doesnt look right
bike %>% 
  count(hour = lubridate::hour(trip_start_ts)) %>% 
  ggplot(aes(hour, n)) +
  geom_bar(stat = "identity") +
  xlab("hour of trip start") +
  ylab("count") +
  ggtitle("Distribution of trip start time hour") +
  scale_x_continuous(breaks =1:23)

# let's look at a description of the data
'https://ckan0.cf.opendata.inter.prod-toronto.ca/en/dataset/bike-share-toronto/resource/497fe3ba-723a-43b8-bba8-98d3f673a7cb'

# convert timestamps to easter
bike <- bike %>% 
  mutate(trip_start_ts = with_tz(trip_start_ts, tzone = "EST"),
         trip_stop_ts = with_tz(trip_stop_ts, tzone = "EST"))

# that's what we expect!
bike %>% 
  count(hour = lubridate::hour(trip_start_ts)) %>% 
  ggplot(aes(hour, n)) +
  geom_bar(stat = "identity") +
  xlab("hour of trip start") +
  ylab("count") +
  ggtitle("Distribution of trip start time hour") +
  scale_x_continuous(breaks =1:23)


# day of month

bike %>% 
  count(day = lubridate::day(trip_start_ts)) %>% 
  ggplot(aes(day, n)) +
  geom_bar(stat = "identity") +
  xlab("day of month of trip start") +
  ylab("count") +
  ggtitle("Distribution of trip start time",
          subtitle = "day of month") +
  scale_x_continuous(breaks =1:31)


# Month plot

bike %>% 
  count(month = as_factor(lubridate::month(trip_start_ts))) %>% 
  ggplot(aes(month, n)) +
  geom_bar(stat = "identity") +
  xlab("month of trip start") +
  ylab("count") +
  ggtitle("Distribution of trip start time",
          subtitle = "by month") 

# avg number of trips by day of week

bike %>% 
  count(date = as_date(trip_start_ts)) %>% 
  mutate(day_of_week = wday(date, label = T)) %>% 
  ggplot(aes(day_of_week, n)) +
  geom_boxplot() +
  xlab("Day of week") +
  ylab("Average trips by day of week") +
  ggtitle("Distribution of trips",
          subtitle = "by day of week") 


## Create some additional features

bike <- bike %>% 
  mutate_at(vars("trip_start_ts"), .funs = c(start_month = lubridate::month,
                                             start_day_of_week = lubridate::wday,
                                             start_day = lubridate::day,
                                             start_hr = lubridate::hour,
                                             start_min = lubridate::minute)) %>% 
  mutate(date = as_date(trip_start_ts))

## Remove late 2016 values
bike <- bike %>% filter(year(date) == 2017)
### Back to the weather

clean_var_names <- function(x) {
  
  clean_x <- tolower(x) %>% 
    str_replace_all(., "/|\'|°", "") %>% 
    str_replace_all(., " ", "_")
  
  return(clean_x)
}




scrape_weather <- function(station = "51459", 
                           year = 2017, 
                           month = 3) {
  
  query_url <- glue::glue('http://climate.weather.gc.ca/climate_data/daily_data_e.html?StationID={station}&timeframe=2&Day={day}&Year={year}&Month={month}#')

  weather_data <- query_url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    as_tibble()
  
  
  names(weather_data) <- clean_var_names(names(weather_data))
  
  weather_data <- weather_data %>% 
    mutate(date = ymd(paste(year, month, day, '-'))) %>% 
    select(date, mean_temp_definitionc, total_precip_definitionmm) %>% 
    filter(!is.na(date))
  
  weather_data <- weather_data %>% 
    mutate(total_precip = as.numeric(ifelse(total_precip_definitionmm == "LegendTT", 
                                            "0.01",total_precip_definitionmm)),
           total_precip = ifelse(is.na(total_precip), 0, total_precip),
           mean_temp = as.numeric(mean_temp_definitionc)) %>% 
             select(date, mean_temp, total_precip)
  
  return(weather_data)
}



# get weather for jan-march -----------------------------------------------
weather <- list()
months <- 1:3

for(i in months) {
  weather[[i]] <- scrape_weather(month = i)
}

weather <- do.call(rbind, weather)


# get a plot of the weather -----------------------------------------------
weather %>% 
  ggplot(aes(date, mean_temp)) +
  geom_line()



# join bike and weather data ----------------------------------------------

bike <- bike %>% 
  left_join(weather, by = 'date')


