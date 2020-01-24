library(tidyverse)
library(lubridate)

# Read in data
accidents <- read_csv("2017-present.csv") %>%
  mutate(Date = as.Date(Date))
weather <- read_csv("Lincoln_Airport_Weather.csv", skip = 1, na = "M") %>%
  mutate_at(vars(Precipitation:SnowDepth), function(x) str_replace(x, "T", "0.0001") %>% as.numeric())

n_accidents <- accidents %>% group_by(Date) %>% count() %>%
  left_join(select(weather, Date, MinTemperature, Precipitation:SnowDepth) %>%
              filter(MinTemperature < 35 & Precipitation > 0))

ggplot(n_accidents, aes(x = Date, y = n)) +
  geom_rect(aes(xmin = Date, xmax = Date + days(1),
                ymin = -Inf, ymax = Inf, fill = SnowDepth > 0.0001, alpha = SnowDepth)) +
  geom_line() +
  scale_fill_manual("Snow Accumulation", values = c("FALSE" = "transparent", "TRUE" = "blue")) +
  scale_alpha_continuous(range = c(0, .5)) +
  theme_bw()

ggplot(n_accidents, aes(x = Date, y = n)) +
  geom_rect(aes(xmin = Date, xmax = Date + days(1),
                ymin = -Inf, ymax = Inf, fill = Precipitation), color = NA,
            data = filter(n_accidents, Precipitation > 0 & MinTemperature < 35)) +

  geom_rug(aes(Date), data = filter(n_accidents, SnowDepth > 0.0001), inherit.aes = F) +
  geom_line() +
  scale_fill_gradient("Precipitation (in)\nwhen Minimum \nTemperature <35 F  ",
                      low = "#FFFFFF", high =  "skyblue", na.value = "#FFFFFF",
                      trans = "log", breaks = c(0.0001, .01, .1, 1),
                      labels = c("trace", .01, .1, 1)) +
  scale_alpha_continuous(range = c(0, .5)) +
  scale_y_continuous("Daily Number of Reported Accidents", breaks = seq(0, 100, by = 10)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  theme_bw() +
  annotate("text", x = ymd("2017-06-01"), y = -Inf, label = "Day w/ Snow on the Ground", vjust = -1) +
  ggtitle("Reported Traffic Accidents in Lincoln, NE") +
  theme(legend.position = c(.85, 1), legend.justification = c(1, 1.02), legend.direction = "horizontal")

# ggplot(n_accidents, aes(x = Date, y = n)) +
#   geom_rect(aes(xmin = Date, xmax = Date + days(1),
#                 ymin = -Inf, ymax = Inf, fill = Snowfall)) +
#   geom_line() +
#   scale_fill_gradient("Snowfall (in)", low = "#FFFFFF", high =  "blue") +
#   theme_bw()
#
# ggplot(weather, aes(x = Date, y = Snowfall)) + geom_line()
