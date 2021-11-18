library(lme4)
library(dplyr)
library(ggplot2)


#tempo de residencia

tr<-read.csv(file = "Data/Tempo_residencia.csv")
class(tr$alt_sensor)
class(tr$temperature_threshold)

tr$alt_sensor<-as.factor(tr$alt_sensor)
tr$Treatment<-as.factor(tr$Treatment)
tr$temperature_threshold<-as.factor(tr$temperature_threshold)

tr60_b<-tr%>%
  filter(alt_sensor=="underground" & temperature_threshold=="above_60")

tr60_s<-tr%>%
  filter(alt_sensor=="soil" & temperature_threshold=="above_60")


tr60_a<-tr%>%
  filter(alt_sensor=="aboveground" & temperature_threshold=="above_60")

tr100_b<-tr%>%
  filter(alt_sensor=="underground" & temperature_threshold=="above_100")

tr100_s<-tr%>%
  filter(alt_sensor=="soil" & temperature_threshold=="above_100")

tr100_a<-tr%>%
  filter(alt_sensor=="aboveground" & temperature_threshold=="above_100")

# figura
ggplot(tr60_b, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)
ggplot(tr100_b, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)


ggplot(tr60_s, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)
ggplot(tr100_s, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)

ggplot(tr60_a, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)
ggplot(tr100_a, aes(x=Treatment, y=tempo_sec)) + geom_point(shape=1)

#glm#
#poisson distribution
#count data = how many seconds over 60 or over 100

#post-hoc
library(multcomp)

#aboveground:
glm_a1<-glm(tempo_sec ~ Treatment,
            data=tr60_a, family = "poisson")
summary(glm_a1)


glm_a2<-glm(tempo_sec ~ Treatment,
            data=tr100_a, family = "poisson")
summary(glm_a2)

#posthoc
a1_posthoc<-glht(glm_a1, mcp(Treatment="Tukey"))
summary(a1_posthoc)

a2_posthoc<-glht(glm_a2, mcp(Treatment="Tukey"))
summary(a2_posthoc)

#on soil:
glm_s1<-glm(tempo_sec ~ Treatment,
           data=tr60_s, family = "poisson")
summary(glm_s1)

glm_s2<-glm(tempo_sec ~ Treatment,
            data=tr100_s, family = "poisson")
summary(glm_s2)

#post-hoc
s1_posthoc<-glht(glm_s1, mcp(Treatment="Tukey"))
summary(s1_posthoc)

s2_posthoc<-glht(glm_s2, mcp(Treatment="Tukey"))
summary(s2_posthoc)

#belowground:
#zero inflated aqui... check:
100*sum(tr60_b$tempo_sec == 0)/nrow(tr60_b)

#54% are zeroes, pretty high!
library(lattice)
library(MASS)
require(pscl) # alternatively can use package ZIM for zero-inflated models
library(lmtest)

z_b1<-zeroinfl(tempo_sec ~ Treatment| Treatment,
            data=tr60_b, dist = 'poisson')
summary(z_b1)

z_bb1<-zeroinfl(tempo_sec ~ Treatment| Treatment,
               data=tr60_b, dist = 'negbin')
summary(z_bb1)

lrtest(z_b1, z_bb1)#negative binomial model é melhor que o poisson.

## Check for over/underdispersion in the model
E2 <- resid(z_bb1, type = "pearson")
N  <- nrow(tr60_b)
p  <- length(coef(z_bb1))
sum(E2^2) / (N - p)





