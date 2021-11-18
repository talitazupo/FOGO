## ================== ZUPO et al.  ====================
# ================= artigo FIRE BEHAVIOR ====================
# Modelos de temperatura max ~ tratamento (time since last fire)
# sem mixed effects. março 2021


## como a variação dos random effects é quase zero, nao precisa
# fazer mixed models.
#além disso, só tem um source of random effect (plots), thus
#we don't need mixed effects models. ?

library(lme4)
library(dplyr)
library(ggplot2)

temp<-read.csv(file = "Data/Temperaturas.csv")
str(temp)
temp$time_last_fire<-as.factor(temp$time_last_fire)

#para temperaturas underground:
temp_below2<-temp%>%
  filter(alt_sensor=="aunderground")%>%
  filter(temp_max<=110)

ggplot(temp_below, aes(x=time_last_fire, y=temp_max)) + geom_point(shape=1)

#salvar figura (pra alessandra):
ggsave("Figs/dados_tempmax_underground.png", dpi = 300)

glm_b<-glm(temp_max ~ time_last_fire,
           data=temp_below, family = Gamma(link = "log"))

glm_b2b<-glm(temp_max ~ time_last_fire,
           data=temp_below2, family = Gamma(link = "identity"))
summary(glm_b)
summary(glm_b2b)
plot(resid(glm_b))
plot(resid(glm_b2))
#parece que tanto faz o link used...aí usar identity é melhor pra interpretar
#pq o outro ta na escala log

#para temperaturas on soil surface

temp_soil<-temp%>%
  filter(alt_sensor=="soil")

ggplot(temp_soil, aes(x=time_last_fire, y=temp_max)) + geom_point(shape=1)

#salvar figura (pra alessandra):
ggsave("Figs/dados_tempmax_soilsurface.png", dpi = 300)

glm_s<-glm(temp_max ~ time_last_fire,
                 data=temp_soil, family = Gamma(link = "identity"))
summary(glm_s)

e4 <- predictorEffect("time_last_fire", glm_s)
plot(e4)

#for aboveground
temp_above<-temp%>%
  filter(alt_sensor=="zaboveground")

ggplot(temp_above, aes(x=time_last_fire, y=temp_max)) + geom_point(shape=1)

glm_a<-glm(temp_max ~ time_last_fire,
           data=temp_above, family = Gamma(link = "identity"))
summary(glm_a)


#post-hoc
library(multcomp)
below_posthoc<-glht(glm_b2b, mcp(time_last_fire="Tukey"))
summary(below_posthoc)

soil_posthoc<-glht(glm_s, mcp(time_last_fire="Tukey"))
summary(soil_posthoc)

above_posthoc<-glht(glm_a, mcp(time_last_fire="Tukey"))
summary(above_posthoc)

temp1 <- ddply(temp, c("time_last_fire", "alt_sensor"), summarise,
               N    = length(temp_max),
               mean = mean(temp_max),
               sd   = sd(temp_max),
               se   = sd / sqrt(N)
)
