# Guided Project: Analyzing Forest Fire Data
# https://app.dataquest.io/m/277/guided-project%3A-analyzing-forest-fire-data/2/the-importance-of-forest-fire-data

library(readr)
library(dplyr)
library(ggplot2)

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






