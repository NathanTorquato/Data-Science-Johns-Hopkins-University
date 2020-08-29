df <- read.table("homicidios.csv", sep=";", header=TRUE)

library(ggplot2)

ggplot(df, aes(periodo, fill=nome))+
  geom_bar(aes(weight=valor)) +
  ggtitle("Homicídios no Brasil por UF", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")


ggplot(df, aes(periodo, fill=regiao))+
  geom_bar(aes(weight=valor)) +
  ggtitle("Homicídios no Brasil por Região", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")

ggplot(df, aes(periodo))+
  geom_bar(aes(weight=valor))+
  facet_wrap(~ regiao) +
  ggtitle("Homicídios no Brasil por Região", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="")

df2 <- read.table("homicidios-genero-raca.csv", sep=";", header=TRUE)

#wrong way
ggplot(df2, aes(periodo, valor, fill=genero))+
  geom_bar(stat="sum") +
  ggtitle("Homicídios no Brasil por gênero", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="")

#right way
ggplot(df2, aes(periodo, fill=genero))+
  geom_bar(aes(weight=valor))+
  ggtitle("Homicídios no Brasil por gênero", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="")

#wrong way
ggplot(df2, aes(periodo, valor, fill=raca))+
  geom_bar(stat="sum") +
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")

#right way
ggplot(df2, aes(periodo, fill=raca))+
  geom_bar(aes(weight=valor))+
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")

#wrong way
ggplot(df2, aes(periodo, valor, fill=raca))+
  geom_bar(stat="sum") +
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="")+
  facet_wrap(~regiao)

#right way
ggplot(df2, aes(periodo, fill=raca))+
  geom_bar(aes(weight=valor)) +
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")+
  facet_wrap(~regiao)

#para fazer um procv: left_join

min(df2$periodo)
max(df2$periodo)

ggplot(subset(df2, df2$periodo == '2000'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2000)", subtitle="Fonte: Atlas da Violência 2020")

ggplot(subset(df2, df2$periodo == '2017'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2017)", subtitle="Fonte: Atlas da Violência 2020")




