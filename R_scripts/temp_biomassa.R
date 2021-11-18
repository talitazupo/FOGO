## Temp ~ Biomassa ####
# scatterplot #

#packages
library(dplyr)
library(ggplot2)
library(gridExtra)

#files
biomassa<-read.csv(file = "Data/biomassa.csv", sep = ";", dec = ".", header = T)


# ====================== para BIOMASSA MORTA ============================ ####
# =========================================================================

#primeiro organizar os dados:

bio_1ano_dead<-filter(biomassa, fuel_type=="dead" & tratamento == "1_yr")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))%>%
  mutate(below_mean=c("40.5", "43.7", "51.7", "39.8"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("68", "49", "63", "64"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("245.3", "314", "327", "303.5"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("307", "368", "366", "388"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("296.7", "289.7", "294.3", "298.8"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("327", "373", "396", "420"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("1 ano", "1 ano", "1 ano", "1 ano"))

bio_2ano_dead<- filter(biomassa, fuel_type=="dead" & tratamento == "2_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))%>%
  mutate(below_mean=c("51.5", "56.3", "58.3", "46.5"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("65", "82", "66", "63"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("333.3", "362.5", "525.3", "339"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("477", "431", "581", "460"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("327", "172.8", "368.7", "444.3"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("390", "234", "496", "496"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("2 anos", "2 anos", "2 anos", "2 anos"))

bio_4ano_dead<- filter(biomassa, fuel_type=="dead" & tratamento == "4_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_dead = mean(fuel), sd_f = sd(fuel))%>%
  mutate(below_mean=c("57.8", "57", "114", "86"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("80", "63", "154", "120"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("344", "431.3", "389", "424.7"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("538", "667", "487", "485"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("457", "480", "541.7", "414"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("610", "585", "589", "687"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("4 anos", "4 anos", "4 anos", "4 anos"))

biom_dead <- bind_rows(bio_1ano_dead, bio_2ano_dead, bio_4ano_dead)

# figuras_Dead_Biomass
# separado por time since last fire
 ggplot(biom_dead, aes(x=mean_dead, y=below_mean,  color = treatment)) + geom_point()+
  scale_y_continuous(limits = c(0, 150),breaks=0:150*50) +
  geom_smooth(method=lm,   # Add linear regression lines
              se=F)
#todos os historicos juntos e below_mean para BIOMASSA MORTA
f1 <-ggplot(biom_dead, aes(x=mean_dead, y=below_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 150),breaks=0:150*50) +
  xlab(" ") +
  ylab("Mean max temp BG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confidence region

#agora below absoluta
f2 <-ggplot(biom_dead, aes(x=mean_dead, y=below_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 150),breaks=0:150*50) +
  xlab(" ") +
  ylab("Ab_max temp BG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confidence region

#agora soil surface mean
f3 <-ggplot(biom_dead, aes(x=mean_dead, y=soil_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 600),breaks=0:600*200) +
  xlab(" ") +
  ylab("Mean max temp soil") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora soil surface absoluta
f4 <-ggplot(biom_dead, aes(x=mean_dead, y=soil_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 600),breaks=0:600*200) +
  xlab(" ") +
  ylab("Ab_max temp soil") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora aboveground mean
f5 <-ggplot(biom_dead, aes(x=mean_dead, y=above_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 600),breaks=0:600*200) +
  xlab("Dead biomass") +
  ylab("Mean max temp AG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora aboveground ab
f6 <-ggplot(biom_dead, aes(x=mean_dead, y=above_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 600),breaks=0:600*200) +
  xlab("Dead biomass") +
  ylab("Ab_max temp AG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

png("Figs/figura01.png", res = 300, width = 2400, height = 1200)
grid.arrange(f1, f2, f3, f4,f5, f6, ncol=3)
dev.off()

png("Figs/figura01b.png", res = 300, width = 1200, height = 1800)
grid.arrange(f1, f2, f3, f4,f5, f6, ncol=2)
dev.off()

png("Figs/figura01c.png", res = 300, width = 1400, height = 2000)
grid.arrange(f1, f2, f3, f4,f5, f6, ncol=2)
dev.off()

### =========================== outras coisas =============================####
# ===  Same, but with different colors and add regression lines  ==========

ggplot(dat, aes(x=xvar, y=yvar, color=cond)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region

# Extend the regression lines beyond the domain of the data
ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


## =====  Set shape by cond ============= ##

# Same, but with different shapes
ggplot(bio_1ano_dead, aes(x=mean_fuel, y=below_mean)) + geom_point() +
  scale_shape_manual(values=c(1,2))  # Use a hollow circle and triangle


## ================== AGORA para biomassa total =======================####
# =========================================================================

#organizar os dados:

bio_1ano_total<-filter(biomassa, fuel_type=="total_fuel" & tratamento == "1_yr")%>%
  group_by(Plot)%>%
  summarise (mean_total = mean(fuel), sd_t = sd(fuel))%>%
  mutate(below_mean=c("40.5", "43.7", "51.7", "39.8"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("68", "49", "63", "64"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("245.3", "314", "327", "303.5"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("307", "368", "366", "388"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("296.7", "289.7", "294.3", "298.8"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("327", "373", "396", "420"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("1 ano", "1 ano", "1 ano", "1 ano"))

bio_2ano_total<- filter(biomassa, fuel_type=="total_fuel" & tratamento == "2_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_total = mean(fuel), sd_t = sd(fuel))%>%
  mutate(below_mean=c("51.5", "56.3", "58.3", "46.5"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("65", "82", "66", "63"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("333.3", "362.5", "525.3", "339"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("477", "431", "581", "460"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("327", "172.8", "368.7", "444.3"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("390", "234", "496", "496"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("2 anos", "2 anos", "2 anos", "2 anos"))

bio_4ano_total<- filter(biomassa, fuel_type=="total_fuel" & tratamento == "4_yrs")%>%
  group_by(Plot)%>%
  summarise (mean_total = mean(fuel), sd_t = sd(fuel))%>%
  mutate(below_mean=c("57.8", "57", "114", "86"))%>%
  mutate(below_mean = as.numeric(below_mean))%>%
  mutate(below_ab=c("80", "63", "154", "120"))%>%
  mutate(below_ab = as.numeric(below_ab))%>%
  mutate(soil_mean =c("344", "431.3", "389", "424.7"))%>%
  mutate(soil_mean = as.numeric(soil_mean))%>%
  mutate(soil_ab=c("538", "667", "487", "485"))%>%
  mutate(soil_ab = as.numeric(soil_ab))%>%
  mutate(above_mean=c("457", "480", "541.7", "414"))%>%
  mutate(above_mean = as.numeric(above_mean))%>%
  mutate(above_ab=c("610", "585", "589", "687"))%>%
  mutate(above_ab = as.numeric(above_ab))%>%
  mutate(treatment =c("4 anos", "4 anos", "4 anos", "4 anos"))

biom_total <- bind_rows(bio_1ano_total, bio_2ano_total, bio_4ano_total)

## ========= agora pra fazer a segunda figura ========= ##

f10 <-ggplot(biom_total, aes(x=mean_total, y=below_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 200),breaks=0:200*50) +
  xlab(" ") +
  ylab("Mean max temp BG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confidence region

#agora below absoluta
f11 <-ggplot(biom_total, aes(x=mean_total, y=below_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 200),breaks=0:200*50) +
  xlab(" ") +
  ylab("Ab_max temp BG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confidence region

#agora soil surface mean
f12 <-ggplot(biom_total, aes(x=mean_total, y=soil_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*200) +
  xlab(" ") +
  ylab("Mean max temp soil") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora soil surface absoluta
f13 <-ggplot(biom_total, aes(x=mean_total, y=soil_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*200) +
  xlab(" ") +
  ylab("Ab_max temp soil") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora aboveground mean
f14 <-ggplot(biom_total, aes(x=mean_total, y=above_mean)) + geom_point()+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*200) +
  xlab("Total biomass") +
  ylab("Mean max temp AG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden

#agora aboveground ab
f15 <-ggplot(biom_total, aes(x=mean_total, y=above_ab)) + geom_point()+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*200) +
  xlab("Total biomass") +
  ylab("Ab_max temp AG") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=T) # True to add confiden



png("Figs/figura02.png", res = 300, width = 1400, height = 2000)
grid.arrange(f10, f11, f12, f13,f14, f15, ncol=2)
dev.off()
