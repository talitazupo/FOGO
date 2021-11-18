## boxplots  - basic R - de temperatura dos sensores ##

temp<-read.csv(file = "Data/temp.csv", sep=";", dec=".", header = T)
str(temp)

#### com ggplot ####
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)


temp<-read.csv(file = "Data/Temperaturas.csv")

ditch_the_axes <- theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
)

#para mudar facet labels
variable_names <- list(
  "aunderground" = "Belowground (-1 cm)" ,
  "soil" = "Soil surface (0 cm)", "zaboveground" = "Aboveground (50 cm)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

f1<-ggplot(temp, aes(x=time_last_fire, y= temp_max, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  ylab("Maximum temperature") +
  scale_x_discrete(name="",limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("FI 1","FI 2", "FI 4" ))+
     theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( ~ alt_sensor, labeller=variable_labeller)

#salvar figura:
ggsave("Figs/boxplot_temp2.png", dpi = 300)
#or
png("figs/boxplot_temp2.png", res = 300, width = 1800, height = 1200)
ggarrange(f1)
dev.off()


##=====##
#ou com cor: kinda
  ggplot(temp, aes(x=time_last_fire, y= temp_max, group = time_last_fire)) +
  geom_boxplot()+
  scale_fill_manual(values=c("#E7298A", "#E6AB02", "#2171B5"), labels = c("Belowground", "Soil surface", "Aboveground"))+
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  xlab("") +
  ylab("Maximum temperature (oC)") +
  theme_classic() +
  theme(legend.position="bottom", legend.text = element_text(size =9), legend.key.size = unit(0.35, "cm")) +
  facet_grid( ~ alt_sensor)


f0<- f0+labs(fill ="")

#medias e desvios####

library(plyr)
str(temp)
temp$time_last_fire <-as.factor(temp$time_last_fire)
t_data <- ddply(temp, c("time_last_fire", "alt_sensor"), summarise,
                N    = length(temp_max),
                mean = mean(temp_max),
                sd   = sd(temp_max),
                se   = sd / sqrt(N)
)

#png("figs/figura6_cor2.png", res = 300, width = 1700, height = 1100)
#ggarrange(f0,
 #         common.legend = TRUE, legend = "bottom")
#dev.off()

#### Fire duration ##

FD<-read.csv(file = "Data/fire_duration.csv", sep = ";", dec = ".", header = T)

#resumindo os dados se quiser fazer grafico de barras
#fd <- FD%>%
 # group_by(time_last_fire)%>%
 # summarise (mean = mean(fire_duration), sd = sd(fire_duration))

ggplot(FD, aes(x=time_last_fire, y=fire_duration, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("Fire duration (s)") +
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))

#salvar figura:
ggsave("Figs/boxplot_fire_duration.png", dpi = 300)

#tempo de residencia

tr<-read.csv(file = "Data/Tempo_residencia.csv", sep = ";", dec = ".", header = T)
tr2<-tr%>%
  filter(alt_sensor=="soil")


ggplot(tr2, aes(x=time_last_fire, y= tempo_sec, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("Residence time (sec)") +
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( ~ temperature_threshold)

#### INTENSIDADE ##

intensidade<-read.csv(file = "Data/Intensity_FireCampina15.csv", sep = ";", dec = ".", header = T)


ggplot(intensidade, aes(x=Treatment, y= Intensity, group = Treatment)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("Intensity") +
  scale_y_continuous(limits = c(500, 5000),breaks=0:5000*1000) +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))





