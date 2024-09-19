
###########################################
## Circumscription ABM, Valley of Oaxaca ##
##      Williams and Mesoudi, 2024       ##
###########################################

####    Experiment 1    ####


library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(reshape2)
library(data.table)
library(zoo)



safe_colorblind_palette <- c("#6699CC", "#DDCC77", "#117733") 
                            

########################################################################################################
### plot average maximum hierarchy of model against archaeological estimates of settlement hierarchy ###


#25 iterations
EXPAa<-read.csv("Model_A_EXPA1_a.txt", header = TRUE) # probability.attack = 0.1, village.range = 1
EXPAb<-read.csv("Model_A_EXPA2_b.txt", header = TRUE) # probability.attack = 1, village.range = 1
EXPAc<-read.csv("Model_A_EXPA3_c.txt", header = TRUE) # probability.attack = 0.1, village.range = 10
EXPAd<-read.csv("Model_A_EXPA4_d.txt", header = TRUE) # probability.attack = 1, village.range = 10
EXPAe<-read.csv("Model_A_EXPA5_a.txt", header = TRUE) # probability.attack = 0.1, village.range = 50 patches
EXPAf<-read.csv("Model_A_EXPA6_a.txt", header = TRUE) # probability.attack = 1, village.range = 50 patches
#repeat another 25 iterations
EXPAg<-read.csv("Model_A_EXPA1_b.txt", header = TRUE)
EXPAh<-read.csv("Model_A_EXPA2_bb.txt", header = TRUE)
EXPAi<-read.csv("Model_A_EXPA3_b.txt", header = TRUE)
EXPAj<-read.csv("Model_A_EXPA4_b.txt", header = TRUE)
EXPAk<-read.csv("Model_A_EXPA5_b.txt", header = TRUE) 
EXPAl<-read.csv("Model_A_EXPA6_b.txt", header = TRUE)
#repeat another 50 iterations
EXPAm<-read.csv("Model_A_EXPA1_c.txt", header = TRUE)
EXPAn<-read.csv("Model_A_EXPA2_c.txt", header = TRUE)
EXPAo<-read.csv("Model_A_EXPA3_cc.txt", header = TRUE)
EXPAp<-read.csv("Model_A_EXPA4_c.txt", header = TRUE)
EXPAq<-read.csv("Model_A_EXPA5_c.txt", header = TRUE) 
EXPAr<-read.csv("Model_A_EXPA6_c.txt", header = TRUE)


EXPA<-rbind(EXPAa, EXPAb, EXPAc, EXPAd, EXPAe, EXPAf,
            EXPAg, EXPAh, EXPAi, EXPAj, EXPAk, EXPAl,
            EXPAm, EXPAn, EXPAo, EXPAp, EXPAq, EXPAr) #100 iterations total




#to make each time step 10 years
EXPA<- mutate(EXPA, year = step * 10)

#now need to make the years relate to the chronology
EXPA<- mutate(EXPA, time = year - 1400)

#also make a comparable hierarchy level
# EXPA<- mutate(EXPA, hierarchy = average.polity.hierarchy)
 EXPA<- mutate(EXPA, hierarchy = average.hierarchy)

#to filter out unnecessary info
EXPA<-EXPA %>% 
  select(hierarchy, time, moving.distance, probability.attack) %>% 
  filter(hierarchy>0) %>% 
  filter(time<200)


# Calculate percentiles for each step and geography
EXPA_percentiles <- EXPA %>%
  group_by(time, moving.distance, probability.attack) %>%
  summarise(
    percent25 = quantile(hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(hierarchy, probs = 0.75, na.rm = TRUE) )


## minimum estimated hierarchy in archaeology

OAXmin<-read.csv("Oaxaca_settle.hier_dataMIN.csv", header = TRUE)

OAXmin<-OAXmin %>% 
  filter(phase <= 8)

OAXmin1<-OAXmin %>% 
  mutate(time = c(-1400, -1150, -850, -700, -500, -300, -100, 200 )) 

OAXmin2<-OAXmin %>% 
  mutate(time = c(-1400, -1150, -850, -700, -500, -300, -100, 200 )) 

ARCHAEOLOGYmin<-rbind(OAXmin1, OAXmin2)

#also make a comparable level of hierarchy column
ARCHAEOLOGYmin<-mutate(ARCHAEOLOGYmin, archaeology.hierarchy = settle.hier)
ARCHAEOLOGYmin$settle.hier<-NULL


#make a chronology column that can relate to the model chronology column
#OAX<-mutate(OAX, chronology = time )

time<-seq(from= -1400, to = 200, by = 10)
timesteps<-as.data.frame(time)

ARCHAEOLOGYmin<-merge(timesteps, ARCHAEOLOGYmin, all.x = TRUE, by = "time")

ARCHAEOLOGYmin$phase<-na.locf(ARCHAEOLOGYmin$phase, na.rm=T)
ARCHAEOLOGYmin$archaeology.hierarchy<-na.locf(ARCHAEOLOGYmin$archaeology.hierarchy, na.rm=T)


time<-seq(from= -1400, to = 200, by = 10)
timesteps<-as.data.frame(time)

ARCHAEOLOGYmin<-merge(timesteps, ARCHAEOLOGYmin, all.x = TRUE, by = "time")

ARCHAEOLOGYmin<-mutate(ARCHAEOLOGYmin, hierarchy = archaeology.hierarchy)
ARCHAEOLOGYmin$archaeology.hierarchy<-NULL


ARCHAEOLOGYmin<-ARCHAEOLOGYmin %>% 
  filter(hierarchy>0) %>% 
  filter(hierarchy<9999)



## maximum estimated hierarchy in archaeology

OAXmax<-read.csv("Oaxaca_settle.hier_dataMAX.csv", header = TRUE)

OAXmax<-OAXmax %>% 
  filter(phase <= 8)

OAXmax1<-OAXmax %>% 
  mutate(time = c(-1400, -1150, -850, -700, -500, -300, -100, 200 )) 

OAXmax2<-OAXmax %>% 
  mutate(time = c(-1400, -1150, -850, -700, -500, -300, -100, 200 )) 

ARCHAEOLOGYmax<-rbind(OAXmax1, OAXmax2)

#also make a comparable level of hierarchy column
ARCHAEOLOGYmax<-mutate(ARCHAEOLOGYmax, archaeology.hierarchy = settle.hier)
ARCHAEOLOGYmax$settle.hier<-NULL



#make a chronology column that can relate to the model chronology column
#OAX<-mutate(OAX, chronology = time )

time<-seq(from= -1400, to = 200, by = 10)
timesteps<-as.data.frame(time)

ARCHAEOLOGYmax<-merge(timesteps, ARCHAEOLOGYmax, all.x = TRUE, by = "time")

ARCHAEOLOGYmax$phase<-na.locf(ARCHAEOLOGYmax$phase, na.rm=T)
ARCHAEOLOGYmax$archaeology.hierarchy<-na.locf(ARCHAEOLOGYmax$archaeology.hierarchy, na.rm=T)


time<-seq(from= -1400, to = 200, by = 10)
timesteps<-as.data.frame(time)

ARCHAEOLOGYmax<-merge(timesteps, ARCHAEOLOGYmax, all.x = TRUE, by = "time")

ARCHAEOLOGYmax<-mutate(ARCHAEOLOGYmax, hierarchy = archaeology.hierarchy)
ARCHAEOLOGYmax$archaeology.hierarchy<-NULL

ARCHAEOLOGYmax<-ARCHAEOLOGYmax %>% 
  filter(hierarchy>0) %>% 
  filter(hierarchy<9999)



#to relabel the facet grid rows

EXPA$probability.attack<-factor(EXPA$probability.attack,
                                    levels=c(0.1, 1),
                                    labels=c(" Polities likely to attack \n once every 100 years ",
                                             " Polities likely to attack \n once every 10 years "))

EXPA_percentiles$probability.attack<-factor(EXPA_percentiles$probability.attack,
                                      levels=c(0.1, 1),
                                      labels=c(" Polities likely to attack \n once every 100 years ",
                                               " Polities likely to attack \n once every 10 years "))




###to plot data
# all data

ggplot(EXPA, aes(x=time, y=hierarchy, color=as.factor(moving.distance)))+
  theme_bw()+
  facet_grid(probability.attack~.)+
  geom_errorbar(inherit.aes = FALSE, data = EXPA_percentiles,
                aes(x=time, ymin = percent25, ymax = percent75, color = as.factor(moving.distance)),
                width = 0, linewidth=2, alpha = 0.1)+
  geom_line(data = EXPA_percentiles,
            aes(x = time, y = percent25, color = as.factor(moving.distance)),
            linetype = "dashed")+
  geom_line(data = EXPA_percentiles,
            aes(x = time, y = percent75, color = as.factor(moving.distance)),
            linetype = "dashed")+
  # geom_point(alpha = 0.04)+
  geom_line(data = EXPA_percentiles, 
            aes(x = time, y = percent50, color = as.factor(moving.distance)), 
            linetype = "solid", linewidth = 1) +
  geom_line(inherit.aes = FALSE, data = ARCHAEOLOGYmin, aes(x=time, y=hierarchy), 
            na.rm=TRUE,
            linewidth=3, lineend = "round", color="#333333", alpha=0.6)+
  geom_line(inherit.aes = FALSE, data = ARCHAEOLOGYmax, aes(x=time, y=hierarchy), 
            na.rm=TRUE,
            linewidth=3, lineend = "round", color="#666666", alpha=0.6)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text.x  = element_text(size=25),
        axis.text.y =element_text(size=25),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=25))+
  scale_x_continuous(breaks=seq(-1400, 200, 200))+
  scale_y_continuous(breaks=seq(0, 8, 2))+ 

  xlab("Years (1400 BCE to 200 CE)")+
  ylab("Average village settlement hierarchy level")+
  scale_color_manual(
    values = safe_colorblind_palette,
                     name="",
                     breaks=c("1","10", "50"),
                     labels=c("Village range = 1.2km    ","Village range = 11.4km    ", "Village range = 57.2 km"))
  # ggtitle("Comparing the average level of hierarchy of settlements \n in the archaeological record with average village hierarchy ABM output")





###to plot data
# population size

EXPApop<-rbind(EXPAa, EXPAb, EXPAc, EXPAd, EXPAe, EXPAf,
            EXPAg, EXPAh, EXPAi, EXPAj, EXPAk, EXPAl,
            EXPAm, EXPAn, EXPAo, EXPAp, EXPAq, EXPAr) #100 iterations total




#to make each time step 10 years
EXPApop<- mutate(EXPApop, year = step * 10)

#now need to make the years relate to the chronology
EXPApop<- mutate(EXPApop, time = year - 1400)

#also make a comparable hierarchy level
# EXPA<- mutate(EXPA, hierarchy = average.polity.hierarchy)
EXPApop<- mutate(EXPApop, population = count.villages)

#to filter out unnecessary info
EXPApop<-EXPApop %>% 
  select(population, time, moving.distance, probability.attack) %>% 
  filter(time<200)


# Calculate percentiles for each step and geography
EXPApop_percentiles <- EXPApop %>%
  group_by(time, moving.distance, probability.attack) %>%
  summarise(
    percent25 = quantile(population, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(population, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(population, probs = 0.75, na.rm = TRUE) )

#to relabel the facet grid rows

EXPApop$probability.attack<-factor(EXPApop$probability.attack,
                                levels=c(0.1, 1),
                                labels=c(" Polities likely to attack \n once every 100 years ",
                                         " Polities likely to attack \n once every 10 years "))

EXPApop_percentiles$probability.attack<-factor(EXPApop_percentiles$probability.attack,
                                            levels=c(0.1, 1),
                                            labels=c(" Polities likely to attack \n once every 100 years ",
                                                     " Polities likely to attack \n once every 10 years "))




ggplot(EXPApop, aes(x=time, y=population, color=as.factor(moving.distance)))+
  theme_bw()+
  facet_grid(probability.attack~.)+
  geom_errorbar(inherit.aes = FALSE, data = EXPApop_percentiles,
                aes(x=time, ymin = percent25, ymax = percent75, color = as.factor(moving.distance)),
                width = 0, linewidth=2, alpha = 0.1)+
  geom_line(data = EXPApop_percentiles,
            aes(x = time, y = percent25, color = as.factor(moving.distance)),
            linetype = "dashed")+
  geom_line(data = EXPApop_percentiles,
            aes(x = time, y = percent75, color = as.factor(moving.distance)),
            linetype = "dashed")+
  # geom_point(alpha = 0.04)+
  geom_line(data = EXPApop_percentiles, 
            aes(x = time, y = percent50, color = as.factor(moving.distance)), 
            linetype = "solid", linewidth = 1) +
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text.x  = element_text(size=25),
        axis.text.y =element_text(size=12),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=25))+
  scale_x_continuous(breaks=seq(-1400, 200, 200))+
  xlab("Years (1400 BCE to 200 CE)")+
  ylab("Population size (number of villages)")+
  scale_color_manual(
    values = safe_colorblind_palette,
    name="",
    breaks=c("1","10", "50"),
    labels=c("Village range = 1.2km    ","Village range = 11.4km    ", "Village range = 57.2 km"))
# ggtitle("Comparing the average level of hierarchy of settlements \n in the archaeological record with average village hierarchy ABM output")

