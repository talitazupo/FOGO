
tempo<-read.csv(file = "Data/Tempo_residencia.csv", sep=";", dec=".", header = T)
str(tempo)

### com ggplot
library(dplyr)
library(ggplot2)
library(gridExtra)

ditch_the_axes <- theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
)

over60<-tempo%>%
  filter(temperature_threshold=="above_60")
over100<-tempo%>%
  filter(temperature_threshold=="above_100")

ggplot(tempo, aes(x=time_last_fire, y= tempo_sec, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("residence time (s)") +
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( temperature_threshold~ alt_sensor, labeller=variable_labeller)

#para mudar facet labels
variable_names <- list(
  "underground" = "belowground (-1 cm)" ,
  "soil" = "soil surface (0 cm)", "aboveground" = "aboveground (50 cm)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

ggplot(over60, aes(x=time_last_fire, y= tempo_sec, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("residence time (s)") +
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( ~ alt_sensor, labeller=variable_labeller)

ggplot(over100, aes(x=time_last_fire, y= tempo_sec, group = time_last_fire)) +
  geom_boxplot()+
  ditch_the_axes +
  xlab("Time since last fire") +
  ylab("residence time (s)") +
  scale_x_discrete(limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("1 year","2 years", "4 years" ))+
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( ~ alt_sensor, labeller=variable_labeller)
