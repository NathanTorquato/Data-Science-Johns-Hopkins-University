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

sum(subset(df, df$periodo == '2017')$valor)

anos <- c()
homicidios <- c()
for(periodo in df$periodo){
  anos <- c(anos, periodo)
  homicidios <- c(homicidios, sum(df$valor[df$periodo == periodo]))
}
anos <- unique(anos)
homicidios <- unique(homicidios)
total_homicidios <- data.frame(anos, homicidios)
total_homicidios$homicidiosRounded <- round(homicidios/1000, 1)

ggplot(total_homicidios, aes(anos, homicidiosRounded, label=sprintf("%0.1f", round(homicidiosRounded, digits = 1))))+
  geom_point()+
  geom_line()+
  geom_text(aes(y=homicidiosRounded, vjust=-1))+
  ylim(c(0,70))
  

ggplot(subset(total_homicidios, total_homicidios$anos >= '1990'), aes(anos, homicidiosRounded))+
  geom_point()+
  geom_line()+
  ylim(c(0,70))

ggplot(subset(total_homicidios, total_homicidios$anos >= '2000'), aes(anos, homicidiosRounded, label=sprintf("%0.1f", round(homicidiosRounded, digits = 1))))+
  geom_point()+
  geom_line()+
  geom_text(aes(y=homicidiosRounded), vjust=-.5)+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil desde 2000 (em milhares de pessoas)", subtitle = "Fonte: Atlas da Violência 2020")+
  ylim(c(0,70))
  
ggplot(subset(total_homicidios, total_homicidios$anos >= '2000'), aes(anos, label=sprintf("%0.1f", round(homicidiosRounded, digits = 1))))+
  geom_bar(aes(weight=homicidiosRounded), fill="#00BFC4")+
  geom_text(aes(y=homicidiosRounded), vjust=-.5, col="#404040", size=4)+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil desde 2000 (em milhares de pessoas)", subtitle = "Fonte: Atlas da Violência 2020")+
  ylim(c(0,70))

ggplot(subset(total_homicidios, total_homicidios$anos >= '2010'), aes(anos, homicidiosRounded, label=sprintf("%0.1f", round(homicidiosRounded, digits = 1))))+
  geom_point()+
  geom_line()+
  geom_text(aes(y=homicidiosRounded), vjust=-.5)+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil na década de 2010 (em milhares de pessoas)", subtitle = "Fonte: Atlas da Violência 2020")+
  ylim(c(0,70))

ggplot(subset(total_homicidios, total_homicidios$anos >= '2010'), aes(anos, label=sprintf("%0.1f", round(homicidiosRounded, digits = 1))))+
  geom_bar(aes(weight=homicidiosRounded), fill="#00BFC4", width = .8)+
  geom_text(aes(y=homicidiosRounded), vjust=-.5, col="#404040", size=4, )+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil na década de 2010 (em milhares de pessoas)", subtitle = "Fonte: Atlas da Violência 2020")+
  ylim(c(0,70))

#Como fazer gráfico com barras e linhas simultaneamente?

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
  labs(x="", y="", fill="")+
  scale_fill_discrete(breaks = c("homem", "mulher"), labels = c("Homem", "Mulher"))

#wrong way
ggplot(df2, aes(periodo, valor, fill=raca))+
  geom_bar(stat="sum") +
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")

#right way
ggplot(df2, aes(periodo, fill=raca))+
  geom_bar(aes(weight=valor))+
  ggtitle("Homicídios no Brasil por raça", subtitle = "Fonte: Atlas da Violência 2020") +
  labs(x="", y="", fill="")+
  scale_fill_discrete(breaks = c("nao negro", "negro"), labels=c("Não negro", "Negro"))

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
  facet_wrap(~regiao)+
  scale_fill_discrete(breaks=c("nao negro", "negro"), labels=c("Não negro", "Negro"))

#para fazer um procv: left_join

min(df2$periodo)
max(df2$periodo)

ggplot(subset(df2, df2$periodo == '2000'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2000)", subtitle="Fonte: Atlas da Violência 2020")+
  ylim(c(0,15000))

ggplot(subset(df2, df2$periodo == '2017'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2017)", subtitle="Fonte: Atlas da Violência 2020")

ggplot(subset(df2, df2$periodo == '2017'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2017)", subtitle="Fonte: Atlas da Violência 2020")+
  ylim(c(0,15000))

library(patchwork)

a <- ggplot(subset(df2, df2$periodo == '2000'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2000)", subtitle="Fonte: Atlas da Violência 2020")+
  ylim(c(0,15000))

b <- ggplot(subset(df2, df2$periodo == '2017'), aes(nome))+
  geom_bar(aes(weight=valor))+
  labs(x="", y="")+
  ggtitle("Homicídios no Brasil por Estado (2017)", subtitle="Fonte: Atlas da Violência 2020")+
  ylim(c(0,15000))

a+b

#
df2000 <- subset(df2, df2$periodo == '2000')
df2017 <- subset(df2, df2$periodo == '2017')

#Os únicos estados onde a violência diminuiu entre 2000 e 2017 foram SP, RJ e DF
ggplot(mapping = aes(nome, weight=valor))+
  geom_bar(data = df2000, fill='steelblue', position='stack')+
  geom_bar(data = df2017, width=.5, position=position_dodge())

df3 <- rbind(df2000, df2017)

df3$periodo[df3$periodo == 'Ano 2000'] <- '2000 '
df3$periodo[df3$periodo == 'Ano 2017'] <- '2017 '

#Só para aprender a separar, mas não é isso que eu quero
ggplot(df3, aes(nome))+
  geom_bar(aes(weight=valor, fill=regiao))

#Isso era o que eu queria
ggplot(df3, aes(nome))+
  geom_bar(aes(weight=valor, fill=periodo), position='dodge')+
  labs(x="", y="", fill="")+
  ggtitle("Evolução do número de homicídios no Brasil entre 2000 e 2017", subtitle = "Fonte: Atlas da Violência 2020")+
  ylim(c(0,15000))

#Fazer um for para pegar UF por UF e criar uma tabela?
sum(df3$valor[df3$periodo == '2017' & df3$nome == 'SP'])/sum(df3$valor[df3$periodo == '2000' & df3$nome == 'SP'])
sum(df3$valor[df3$periodo == '2017' & df3$nome == 'RJ'])/sum(df3$valor[df3$periodo == '2000' & df3$nome == 'RJ'])
sum(df3$valor[df3$periodo == '2017' & df3$nome == 'DF'])/sum(df3$valor[df3$periodo == '2000' & df3$nome == 'DF'])
sum(df3$valor[df3$periodo == '2017' & df3$nome == 'BA'])/sum(df3$valor[df3$periodo == '2000' & df3$nome == 'BA'])

#Feito
UF <- c()
variacao <- c()
for(nome in df3$nome){
  UF <- c(UF, nome)
  variacao <- c(variacao,(sum(df3$valor[df3$periodo == '2017' & df3$nome == nome]) - sum(df3$valor[df3$periodo == '2000' & df3$nome == nome]))/sum(df3$valor[df3$periodo == '2000' & df3$nome == nome]))
}

UF <- unique(UF)
variacao <- unique(variacao)
variacoes <- data.frame(UF, variacao)
variacoes

#Plotando as variações por Estado
ggplot(variacoes, aes(UF))+
  geom_bar(aes(weight=variacao))+
  labs(x="", y="")

#Problema dos gráficos anteriores: consideram somente o total de assassinatos
#A população cresceu entre 2000 e 2017, impactando o significado do número de homicídios