# MODEL: What affects soil heating (belowground temp)?
## Talita Zupo - Março 2021

library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(broom)

#os dados de biomassa tem distribuiçao normal. de temperatura = gamma.

biomassa<-read.csv(file = "Data/biomassa.csv", sep = ";", dec = ".", header = T)
soil_temp<-read.csv(file = "Data/soil_temp.csv", sep = ";", dec = ".", header = T)
fire_dur<-read.csv(file = "Data/fire_duration.csv", sep = ";", dec = ".", header = T)

#Primeiro vamos criar a dataframe certa pra rodar o modelo.
bio_1ano_dead<-filter(biomassa, fuel_type=="dead" & tratamento == "1_yr")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))

bio_2ano_dead<- filter(biomassa, fuel_type=="dead" & tratamento == "2_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))

bio_4ano_dead<- filter(biomassa, fuel_type=="dead" & tratamento == "4_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))

Mean_dead<-bind_rows(bio_1ano_dead, bio_2ano_dead, bio_4ano_dead)
#media (e sd) do dead_fuel para cada parcela

#para fazer a media da temperatura no solo:
soil_surf <- soil_temp %>%
  select(Plot, surface)%>%
  group_by(Plot)%>%
  summarise (mean_surf = mean(surface))

  df1<-soil_temp%>%
        left_join(soil_surf, by = c("Plot" = "Plot")) %>%
    left_join(Mean_dead, by = c("Plot" = "Plot"))

#agora calculando media do fire_duration por parcela
fd <- fire_dur %>%
  select(Plot, fire_duration)%>%
  group_by(Plot)%>%
  summarise (mean_fd = mean(fire_duration), sd_fd = sd(fire_duration))

#joining fire duration na dataframe
df<- df1%>%
  left_join(fd, by = c("Plot" = "Plot"))

#writing dataframe
write.table(df, "./data/soil_model_df.csv", row.names=F, sep=";", dec=",")


#### agora testando modelo ####
sm<-read.csv2(file = "Data/soil_model_df.csv")

#primeiro é só dead fuel e fire duration:

str(sm)
sm$Plot <- as.factor(sm$Plot)

#usando glm
glm1<-glm(soil_heating~mean_dead + mean_fd + mean_surf, data= sm,
            family = Gamma(link = "identity"))
summary(glm1)

glm2<-glm(soil_heating~mean_dead*mean_fd + mean_surf, data= sm,
            family = Gamma(link = "identity"))
summary(glm2)

glm2b<-glm(soil_heating~mean_dead + mean_surf, data=sm,
           family= Gamma(link = "identity"))

glm3<-glm(soil_heating~mean_dead + mean_fd*mean_surf, data= sm,
          family = Gamma(link = "identity"))
summary(glm3)

glm3b<-glm(soil_heating~mean_dead + mean_fd, data=sm,
           family= Gamma(link = "identity"))

glm4<-glm(soil_heating~mean_dead*mean_fd*mean_surf, data= sm,
          family = Gamma(link = "identity"))
summary(glm4)

glm5<-glm(soil_heating~mean_dead, data= sm,
          family = Gamma(link = "identity"))

glm6<-glm(soil_heating~mean_fd, data= sm,
          family = Gamma(link = "identity"))

glm7<-glm(soil_heating~mean_surf, data= sm,
          family = Gamma(link = "identity"))

glm8<-glm(soil_heating~1, data= sm,
          family = Gamma(link = "identity"))
summary(glm8)

library(bbmle)
?bbmle
AICctab(glm1, glm2b, glm3b, glm5, glm6, glm7, glm8, weights=T)

## plot ####

## ==== TO PLOT PREDICTIONS

library(effects)

e1 <- predictorEffect("mean_dead", glm1, rescale.axis=F)
jpeg("Figs/pred1.jpg")
plot(e1, xlab="Dead fuel (kg/m2)", ylab = "Belowground temperature (oC)", main="")
dev.off()

eb<-predictorEffect("mean_surf", glm2b, rescale.axis=F)
jpeg("Figs/pred3b.jpg")
plot(eb, xlab="Dead fuel (kg/m2)", ylab = "Belowground temperature (oC)", main="")
dev.off()

e2 <- predictorEffect("mean_fd", glm1, rescale.axis=F)
jpeg("Figs/pred2.jpg")
plot(e2, xlab="Fire duration (s)", ylab = "Belowground temperature (oC)", main="")
dev.off()

e3 <- predictorEffect("mean_surf", glm1, rescale.axis=F)
jpeg("Figs/pred3.jpg")
plot(e3, xlab="Soil surface temperature (oC)", ylab = "Belowground temperature (oC)", main="")
dev.off()



# pode tentar depois com esse pacote que fica mais facil de usar ggplot.
# https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html

## ==== ##
# outras tentativas. ignore:
effect_plot(glm1, pred = soil_heating, interval = TRUE, y.label = "Belowground temp")

range(df$mean_dead)
xdead <- glm1$mean_dead
str(ysoil)
ysoil<- predict(glm1, interval = "prediction", type="response")

plot(df$mean_dead, df$soil_heating, pch = 16, xlab = "Dead biomass", ylab = "Soil heating")
plot(df$soil_heating, ysoil)

ggplot(df, aes(x=mean_dead, y=soil_heating)) + geom_point(shape=1)
