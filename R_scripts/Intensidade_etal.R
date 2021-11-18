### =============== boxplot BIOMASSA - com ggplot =========== ##
## ==== Paper fire behavior - Zupo etal. - Agosto 2020 ======##

#packages
library(dplyr)
library(ggplot2)

inten<-read.csv(file = "Data/Intensity_FireCampina15.csv", sep = ";", dec = ".", header = T)
alt <-read.csv(file = "Data/altura.csv", sep = ";", dec = ".", header = T)
str(inten)
inten$Treatment<-as.factor(inten$Treatment)

inte<-inten%>%
  group_by(Treatment)%>%
  summarise (mean_inten = mean(Intensity), sd_I = sd(Intensity))%>%
  mutate(se = sd_I/sqrt(4))

library(plyr)
adata <- ddply(inten, "Treatment", summarise,
               N    = length(Intensity),
               mean = mean(Intensity),
               sd   = sd(Intensity),
               se   = sd / sqrt(N)
)

rate<-inten%>%
  group_by(Treatment)%>%
summarise (mean_rs = mean(Rate_spread), sd_rs = sd(Rate_spread))%>%
  mutate(se = sd_rs/sqrt(4))
bdata <- ddply(inten, "Treatment", summarise,
               N    = length(Rate_spread),
               mean = mean(Rate_spread),
               sd   = sd(Rate_spread),
               se   = sd / sqrt(N)
)

fire_dur<-read.csv(file = "Data/fire_duration.csv", sep = ";", dec = ".", header = T)
str(fire_dur)
fire_dur$Treatment<-as.factor(fire_dur$Treatment)

fd2 <- fire_dur %>%
    group_by(Treatment)%>%
  summarise (mean_fd = mean(fire_duration), sd_fd = sd(fire_duration))


cdata <- ddply(fire_dur, "Treatment", summarise,
               N    = length(fire_duration),
               mean = mean(fire_duration),
               sd   = sd(fire_duration),
               se   = sd / sqrt(N)
)


#GLMs
library(lme4)

glm1<-glm(Intensity ~ Treatment,
           data=inten, family = Gamma(link = "identity"))
summary(glm1)

#post-hoc
library(multcomp)
inten_posthoc<-glht(glm1, mcp(Treatment="Tukey"))
summary(inten_posthoc)

#para rate of spread
glm2<-glm(Rate_spread ~ Treatment,
          data=inten, family = Gamma(link = "identity"))
summary(glm2)

#post-hoc
rs_posthoc<-glht(glm2, mcp(Treatment="Tukey"))
summary(rs_posthoc)

#para fire duration
glm3<-glm(fire_duration ~ Treatment,
          data=fire_dur, family = Gamma(link = "identity"))
summary(glm3)

#post-hoc
fd_posthoc<-glht(glm3, mcp(Treatment="Tukey"))
summary(fd_posthoc)


#residence time
tr <- read.csv(file = "Data/Tempo_residencia.csv")
str(tr)
tr$Treatment <-as.factor(tr$Treatment)

#RESIDENCE TIME

#over 100
tr1 <- tr%>%
  filter(temperature_threshold=="above_100")


trdata <- ddply(tr1, c("Treatment", "alt_sensor"), summarise,
                N    = length(tempo_sec),
                mean = mean(tempo_sec),
                sd   = sd(tempo_sec),
                se   = sd / sqrt(N)
)

#over 60
tr2 <- tr%>%
  filter(temperature_threshold=="above_60")

#over 60
trdata2 <- ddply(tr2, c("Treatment", "alt_sensor"), summarise,
                 N    = length(tempo_sec),
                 mean = mean(tempo_sec),
                 sd   = sd(tempo_sec),
                 se   = sd / sqrt(N)
)


#for below
ts<-tr2%>%
  filter(alt_sensor=="underground")

#residence time over 100
trs<-tr1%>%
  filter(alt_sensor=="soil")

trb2<-tr1%>%
  filter(alt_sensor=="underground")



## water content (or fuel moisture) ####

#files
water<-read.csv(file = "Data/water_content.csv")
water$Treatment<- as.factor(water$Treatment)

library(plyr)
water_data <- ddply(water, c("Treatment"), summarise,
                    N    = length(Water_content),
                    mean = mean(Water_content),
                    sd   = sd(Water_content),
                    se   = sd / sqrt(N)
)

ggplot(water, aes(x=Treatment, y= Water_content, group = Treatment)) +
  geom_boxplot()

glm15<- glm(Water_content~Treatment, data=water,
            family = Gamma(link = "identity"))
summary(glm15)
wc_posthoc<-glht(glm15, mcp(Treatment="Tukey"))
summary(wc_posthoc)

#altura da chama
str(alt)
alt$Treatment <- as.factor(alt$Treatment)

#para normal
qqp(alt$altura_m, "norm")
# lnorm means lognormal
qqp(alt$altura_m, "lnorm")

gamma<- fitdistr(alt$altura_m, "gamma")
qqp(alt$altura_m, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])


ddata <- ddply(alt, "Treatment", summarise,
               N    = length(altura_m),
               mean = mean(altura_m),
               sd   = sd(altura_m),
               se   = sd / sqrt(N)
)

lm_alt <-lm(altura_m ~Treatment, data = alt)
summary(lm_alt)

#post-hoc
alt_posthoc<-glht(lm_alt, mcp(Treatment="Tukey"))
summary(alt_posthoc)

## fuel moisture
fm <- read.csv(file = "Data/fuel_moisture.csv", sep = ";", dec = ".", header = T)
str(fm)
fm$Treatment <-as.factor(fm$Treatment)

fmdata <- ddply(fm, "Treatment", summarise,
               N    = length(fuel_moisture),
               mean = mean(fuel_moisture),
               sd   = sd(fuel_moisture),
               se   = sd / sqrt(N)
)
