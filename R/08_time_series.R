library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)

covid <-
  read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

p1 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New cases")
p2 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_deaths)) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New deaths")
par(mfrow = c(1, 2))
p1 / p2
par(mfrow = c(1, 1))

covid$new_confirmed[covid$new_confirmed < 0] <- 0

p1 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New cases")
p2 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_deaths)) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New deaths")
par(mfrow = c(1, 2))
p1 / p2
par(mfrow = c(1, 1))

# Rolling mean ####
covid$roll_meanC <-
  zoo::rollmean(covid$new_confirmed, 14, fill = NA)
covid$roll_meanD <- zoo::rollmean(covid$new_deaths, 14, fill = NA)

p1 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  geom_line(aes(x = date, y = roll_meanC), color = "red", size = 1.2) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New cases")
p2 <- ggplot(covid) +
  geom_line(aes(x = date, y = new_deaths)) +
  geom_line(aes(x = date, y = roll_meanD), color = "red", size = 1.2) +
  scale_x_date(breaks = "3 months", date_labels = "%m/%y") +
  theme_minimal() +
  labs(x = "Date", y = "New deaths")
par(mfrow = c(2, 1))
p1 / p2
par(mfrow = c(1, 1))
