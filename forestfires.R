# Guided Project: Analyzing Forest Fire Data
# https://app.dataquest.io/m/277/guided-project%3A-analyzing-forest-fire-data/2/the-importance-of-forest-fire-data

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)


forestfires <- read_csv("data/forestfires.csv")
#View(forestfires)

# change month and day variables to factors
forestfires <- forestfires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

forestfires <- forestfires %>%
  mutate(day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))

# create summary variables
fires_by_month <- forestfires %>%
  group_by (month) %>%
  summarize (num = n())

fires_by_day <- forestfires %>%
  group_by (day) %>%
  summarize (num = n())


# create plots
ggplot(data = fires_by_month) +
  aes(x = month, y = num) +
  geom_bar(stat="identity") +
  labs(title = "Number of Forest Fires by Month")

ggplot(data = fires_by_day) +
  aes(x = day, y = num) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Forest Fires by Day")

# create variables for box plots
y_var <- names(forestfires[5:12])
x_var <- names(forestfires["month"])
x_var <- names(forestfires["day"])


#practice boxplot
ggplot(data = forestfires) +
  aes(x = month, y = FFMC) +
  geom_boxplot()


# by month boxplot function
create_boxplot_month <- function (x,y) {
  ggplot (data = forestfires) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}

# by day boxplot function
create_boxplot_day <- function (x,y) {
  ggplot (data = forestfires) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}

boxplot_compare_month <- map2(x_var, y_var, create_boxplot_month)
boxplot_compare_day <- map2(x_var, y_var, create_boxplot_day)
