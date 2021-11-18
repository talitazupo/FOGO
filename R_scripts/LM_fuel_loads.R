## LM - biomassa nos diferentes tratamentos (time since last fire)
## Talita ZUpo - Março 2021

library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(broom)

#os dados de biomassa tem distribuiçao normal.

biomassa<-read.csv(file = "Data/biomassa.csv", sep = ";", dec = ".", header = T)

biomass <- biomassa%>%
  select (Plot, Subplot, Treatment, tratamento, fuel_type, fuel)

#só pra ver como varia por plot:
ggplot(biomass, aes(x = fuel, y=Plot)) + geom_point(shape=1)+ facet_wrap( tratamento~fuel_type)

str(biomass)
class(biomass$fuel)
class(biomass$tratamento)
biomass$tratamento<- as.factor(biomass$tratamento)
biomass$plot<- as.factor(biomass$plot)

#separando os tipos de fuel loads
total<-biomass%>%
  filter(fuel_type=="total_fuel")

dead<-biomass%>%
  filter(fuel_type=="dead")

live<-biomass%>%
  filter(fuel_type=="live")

#outras figuras (além dos boxplots) - talvez mais facil de
#entender os intercepts e tal

# para total biomass
ggplot(total, aes(x=Treatment, y=fuel)) + geom_point(shape=1)

# para live biomass
ggplot(live, aes(x=Treatment, y=fuel)) + geom_point(shape=1)

# para dead biomass
ggplot(dead, aes(x=tratamento, y=fuel)) + geom_point(shape=1)


## como a variação dos random effects é quase zero, nao precisa
# fazer mixed models. singular models
#além disso, só tem um source of random effect (plots), thus
#we don't need mixed effects models.

#### aí seria um LM ... ####
lm_dead<-lm(fuel~tratamento, data= dead)
summary(lm_dead)
head(resid(lm_dead))
plot(resid(lm_dead))

#com anova...
res.aov <- aov(fuel ~ tratamento, data = dead)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

#post-hoc
library(multcomp)
dead_posthoc<-glht(lm_dead, mcp(tratamento="Tukey"))
summary(dead_posthoc)

lm_total<-lm(fuel~tratamento, data= total)
summary(lm_total)
plot(resid(lm_total))

tot_posthoc<-glht(lm_total, mcp(tratamento="Tukey"))
summary(tot_posthoc)

lm_live<-lm(fuel~tratamento, data= live)
summary(lm_live)
plot(resid(lm_live))
l_posthoc<-glht(lm_live, mcp(tratamento="Tukey"))
summary(l_posthoc)

#nao é glm.
#glm_dead <-glm(fuel~tratamento, family = Gamma(link = "identity"), data=dead)
#summary(glm_dead)


## ========= e se usarmos treat (continuous variable, nao categorical.
# aí faz um modelo só...).
## NOt really, mas foi bom pra ver qualé..##

lm1<-lm(fuel~treat+fuel_type, data= biomass)
summary(lm1)



lm1 %>%
  augment(newdata = data.frame(fuel_type = "dead",
                               treat= 0:2,
                               fuel = mean(biomass$fuel))) -> fit.w

lm1 %>%
  augment(newdata = data.frame(fuel_type = "total_fuel",
                               treat= 0:2,
                               fuel = mean(biomass$fuel))) -> fit.t

lm1 %>%
  augment(newdata = data.frame(fuel_type = "live",
                               treat= 0:2,
                               fuel = mean(biomass$fuel))) -> fit.l
fit.df <- bind_rows(fit.w, fit.t, fit.l)
#ou
fit.df <- full_join(fit.w, fit.t)
fit.df2 <-full_join(fit.df, fit.l)%>%
  mutate(se.fit = x$se.fit)
x<-predict.lm(lm1,newdata=fit.df2, type="response", se.fit=T)

fit.df2 <- fit.df2 %>%
  mutate(up = .fitted + 1.96 * se.fit,
         low = .fitted - 1.96 * se.fit)

ggplot(fit.df2, aes(treat, .fitted, color = fuel_type)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = fuel_type), alpha = .5)
