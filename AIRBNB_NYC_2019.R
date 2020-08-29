library(tidyverse)

df <- read_csv("AIRBNB_NYC_2019.csv")

summary(df)

ggplot(df, aes(neighbourhood_group))+
  geom_bar() +
  ggtitle("Regiões de NYC com maior oferta no AirBNB", subtitle="Base de dados de 2019")+
  labs(x="", y="")

ggplot(df, aes(neighbourhood_group, fill=room_type))+
  geom_bar() +
  ggtitle("Regiões de NYC com maior oferta no AirBNB", subtitle="Base de dados de 2019")+
  labs(x="", y="", fill="")

ggplot(df, aes(neighbourhood_group, fill=room_type))+
  geom_bar(width=.5) +
  ggtitle("Regiões de NYC com maior oferta no AirBNB", subtitle="Base de dados de 2019")+
  labs(x="", y="", fill="") +
  scale_x_discrete(limits = c("Manhattan"))

ggplot(df, aes(availability_365, calculated_host_listings_count)) +
  geom_point() +
  facet_wrap(~ neighbourhood_group)
