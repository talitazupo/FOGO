# ============ graphs com mean and std  error ===========
#loading packages

library(dplyr)
library(ggplot2)
library(gridExtra)
library(textclean)

#reading files
temp<-read.csv(file = "Data/Temperaturas.csv", sep = ",", dec = ";", header = T)
tempo<-read.csv(file = "Data/Tempo_residencia.csv", sep = ";", dec = ".", header = T)
tempo2<-read.csv(file = "Data/tempo2.csv", sep = ";", dec = ".", header = T)

#### trabalhando com a TEMPERATURA - resumindo dados e fazer figura ####
temp_one <- filter(temp,time_last_fire == "1_yr")%>%
  group_by(alt_sensor)%>%
  summarise (mean = mean(temp_max), sd = sd(temp_max))%>%
  mutate(treatment="1_year")%>%
  mutate(sensor=c("caboveground", "bsoil", "aaunderground"))

temp_two<- filter(temp,time_last_fire =="2_yrs")%>%
  group_by(alt_sensor)%>%
  summarise (mean = mean(temp_max), sd = sd(temp_max))%>%
  mutate(treatment="2_years")%>%
  mutate(sensor=c("caboveground", "bsoil", "aaunderground"))

temp_four<- filter(temp,time_last_fire =="4_yrs")%>%
  group_by(alt_sensor)%>%
  summarise (mean = mean(temp_max), sd = sd(temp_max))%>%
  mutate(treatment="4_years")%>%
  mutate(sensor=c("caboveground", "bsoil", "aaunderground"))

temp_all <- bind_rows(temp_one, temp_two, temp_four)

temp_all_below <-temp_all%>%
  filter(alt_sensor =="aunderground")

temp_below <- temp%>%
  filter(alt_sensor =="aunderground")

temp_soil <- temp%>%
  filter(alt_sensor =="soil")

temp_all_soil <-temp_all%>%
  filter(alt_sensor =="soil")

#figuras
f1 <- ggplot(data = temp_all, aes(x=sensor, y=mean, fill=treatment, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.5))+
  scale_fill_manual(values=c("#CCCCCC", "#999999","#333333", "#CCCCCC","#999999", "#333333","#CCCCCC","#999999", "#333333"))+
  xlab("") +
  ylab("Maximum Temperature (oC)")+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*100) +
  scale_x_discrete(breaks=c("aaunderground", "bsoil", "caboveground"),
                   labels=c("underground", "soil", "aboveground"))+
  theme_classic()+
  theme(legend.position="bottom", legend.text = element_text(size =9), legend.key.size = unit(0.35, "cm"))

f1<- f1+labs(fill ="")

#salvar figura:
ggsave("Figs/Temperatura_1.png", dpi = 300)

#### trabalhando TEMPO de RESIDENCIA - resumindo dados e fazer figura ####
#acima de 60oC e acima de 100oC
#acima de 60
tempo60 <- filter(tempo2, limiar =="above_60")
tempo1 <- filter(tempo60,Trat =="1_ano")
tempo2 <- filter(tempo60,Trat =="2_anos")
tempo4 <- filter(tempo60,Trat =="4_anos")

tempo1b <- tempo1%>%
  group_by(sensor)%>%
  summarise (mean = mean(tempo_sec), sd = sd(tempo_sec))%>%
  mutate(treatment="1_year")%>%
  mutate(limiar ="above_60")

tempo2b <- tempo2%>%
  group_by(sensor)%>%
  summarise (mean = mean(tempo_sec), sd = sd(tempo_sec))%>%
  mutate(treatment="2_year")%>%
  mutate(limiar ="above_60")

tempo4b <- tempo4%>%
  group_by(sensor)%>%
  summarise (mean = mean(tempo_sec), sd = sd(tempo_sec))%>%
  mutate(treatment="4_year")%>%
  mutate(limiar ="above_60")

tempos_60 <- bind_rows(tempo1b, tempo2b, tempo4b)


ggplot(data = tempos_60, aes(x=sensor, y=mean, fill=treatment, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.5))+
  scale_fill_manual(values=c("#CCCCCC", "#999999","#333333", "#CCCCCC","#999999", "#333333","#CCCCCC","#999999", "#333333"))+
  xlab("") +
  ylab("Maximum Temperature (oC)")+
  scale_y_continuous(limits = c(0, 700),breaks=0:700*100) +
  scale_x_discrete(breaks=c("aaunderground", "bsoil", "caboveground"),
                   labels=c("underground", "soil", "aboveground"))+
  theme_classic()+
  theme(legend.position="bottom", legend.text = element_text(size =9), legend.key.size = unit(0.35, "cm"))

ggplot() +
  geom_bar(data=tempos_60, aes(y = mean, x = sensor, fill = treatment), stat="identity",
           position='stack') +
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'), labels = c("Decreased", "Stimulated", "Unchanged"))+
  scale_x_discrete(limits=c("100-1","100-3", "200 -1" ),
                   labels=c("100°C-1 min","100°C-3 min", "200°C-1 min" ))+
  xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
  theme(legend.position="bottom", legend.text = element_text(size =9), legend.key.size = unit(0.35, "cm")) +
  facet_grid( ~ x)



