library(tidyverse)
library(ggplot2)

df <- read.csv("shootings.csv")
summary(df)

ggplot(df, aes(age)) +
  geom_boxplot()

ggplot(df, aes(age, fill = race)) +
  geom_histogram() +
  labs(x="Age", y="", fill="", col="")
 
ggplot(df, aes(manner_of_death)) +
  geom_bar(width=.5, color="black", fill="steelblue")

ggplot(df, aes(state)) +
  geom_bar()

ggplot(df, aes(gender)) +
  geom_bar(width = .5)

ggplot(df, aes(race, fill = gender)) +
  geom_bar(width = .7) +
  labs(x = "", y = "", fill="") +
  ggtitle("Deaths by race in USA", subtitle = "2015") +
  scale_color_manual(values = c("blue", "red", "yellow"))

ggplot(df, aes(race, fill = gender)) +
  geom_bar(width = .7, position = position_dodge()) +
  labs(x = "", y = "", fill="") +
  ggtitle("Deaths by race in USA", subtitle = "2015") +
  scale_color_manual(values = c("blue", "red", "yellow")) +
  theme(legend.position = "bottom")

ggplot(df, aes(body_camera)) +
  geom_bar()

ggplot(df, aes(race, fill=body_camera)) +
  geom_bar()

ggplot(df, aes(signs_of_mental_illness)) +
  geom_bar()

ggplot(df, aes(age, fill = race)) +
  geom_histogram()

ggplot(df, aes(race, state))
