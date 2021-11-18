### =============== boxplot BIOMASSA - com ggplot =========== ##
## ==== Paper fire behavior - Zupo etal. - Agosto 2020 ======##

#packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#files
biomassa<-read.csv(file = "Data/biomassa.csv", sep = ";", dec = ".", header = T)

ditch_the_axes <- theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
)

variable_names <- list(
  "atotal_fuel" = "Total fuel load" ,
  "bdead" = "Dead fuel load", "live" = "Live fuel load"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

f1<-ggplot(biomassa, aes(x=tratamento, y= fuel, group = tratamento)) +
  geom_boxplot()+
  scale_x_discrete(name="", limits=c("1_yr","2_yrs", "4_yrs" ),
                   labels=c("FI 1","FI 2", "FI 4" ))+
   ylab("Fuel (kg/m2)") +
  ditch_the_axes +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8))+
  facet_grid( ~ fuel_type2, labeller=variable_labeller)

#salvar figura:
ggsave("Figs/boxplot_biomassa.png", dpi = 300)
#or
png("figs/boxplot_biomassa2.png", res = 300, width = 1800, height = 1200)
ggarrange(f1)
dev.off()

#medias e desvios

library(plyr)
biomass$tratamento<- as.factor(biomass$tratamento)
biomass <- biomassa%>%
  select (Plot, Subplot, Treatment, tratamento, fuel_type, fuel)
f_data <- ddply(biomass, c("tratamento", "fuel_type"), summarise,
               N    = length(fuel),
               mean = mean(fuel),
               sd   = sd(fuel),
               se   = sd / sqrt(N)
)



