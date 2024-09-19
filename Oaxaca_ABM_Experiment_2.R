
###########################################
## Circumscription ABM, Valley of Oaxaca ##
##      Williams and Mesoudi, 2024       ##
###########################################

####    Experiment 2    ####



library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(reshape2)
library(data.table)
library(zoo)


################### TEST 1 ###################

#### ARCHAEOLOICAL STARTING POINTS ####

#5 iterations
EXPAa<-read.csv("Model_B_EXPA1_a.txt", header = TRUE) # probability.attack = 0.1, village.range = 1
EXPAb<-read.csv("Model_B_EXPA2_b.txt", header = TRUE) # probability.attack = 1, village.range = 1
EXPAc<-read.csv("Model_B_EXPA3_c.txt", header = TRUE) # probability.attack = 0.1, village.range = 10
EXPAd<-read.csv("Model_B_EXPA4_d.txt", header = TRUE) # probability.attack = 1, village.range = 10

#20 iterations
EXPAe<-read.csv("Model_B_EXPA1_e.txt", header = TRUE) # probability.attack = 0.1, village.range = 1 patches
EXPAf<-read.csv("Model_B_EXPA2_f.txt", header = TRUE)
EXPAg<-read.csv("Model_B_EXPA3_g.txt", header = TRUE)
EXPAh<-read.csv("Model_B_EXPA4_h.txt", header = TRUE)

#5 iterations 
EXPAi<-read.csv("Model_B_EXPA1_i.txt", header = TRUE) # probability.attack = 0.1, village.range = 1 patches
EXPAj<-read.csv("Model_B_EXPA2_j.txt", header = TRUE)
EXPAk<-read.csv("Model_B_EXPA3_k.txt", header = TRUE)
EXPAl<-read.csv("Model_B_EXPA4_l.txt", header = TRUE)

#20 iterations
EXPAm<-read.csv("Model_B_EXPA1_m.txt", header = TRUE) # probability.attack = 0.1, village.range = 1 patches
EXPAn<-read.csv("Model_B_EXPA2_n.txt", header = TRUE)
EXPAo<-read.csv("Model_B_EXPA3_o.txt", header = TRUE)
EXPAp<-read.csv("Model_B_EXPA4_p.txt", header = TRUE)

#combine for 50 iterations 
EXPA<-rbind(EXPAa, EXPAb, EXPAc, EXPAd, EXPAe, EXPAf, EXPAg, EXPAh,
            EXPAi, EXPAj, EXPAk, EXPAl, EXPAm, EXPAn, EXPAo, EXPAp) 


# new columns for each subvalley and the hierarchy present there
EXPA<-  gather(EXPA, valley, valley.hierarchy, "etla.hierarchy", "tlac.hierarchy", "ocot.hierarchy")

#to filter out unnecessary info
EXPA<-EXPA %>% 
  select(valley.hierarchy, step, moving.distance, probability.attack, valley) %>% 
  filter(valley.hierarchy>0) 



EXPA$probability.attack<-factor(EXPA$probability.attack,
                                levels=c(0.1, 1),
                                labels=c(" Polities likely to attack \n once every 100 years ",
                                         " Polities likely to attack \n once every 10 years "))

EXPA$moving.distance<-factor(EXPA$moving.distance,
                             levels=c(1, 10),
                             labels=c("Small village range (1.2 km)",
                                      "Medium village range (11.4km)"))

EXPA<-EXPA %>% 
  filter(step>0) 

# Calculate percentiles for each step and geography
EXPA_percentiles <- EXPA %>%
  group_by(step, moving.distance, probability.attack, valley) %>%
  summarise(
    percent25 = quantile(valley.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(valley.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(valley.hierarchy, probs = 0.75, na.rm = TRUE) )


ggplot(EXPA, aes(x=step, y=valley.hierarchy, color=as.factor(valley)))+
  theme_bw()+
  facet_grid(probability.attack~moving.distance)+
  geom_errorbar(inherit.aes = FALSE, data = EXPA_percentiles,
                aes(x=step, ymin = percent25, ymax = percent75, color=as.factor(valley)),
                width=0, linewidth=1.25, alpha = 0.1)+
  geom_line(data = EXPA_percentiles,
            aes(x = step, y = percent25, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPA_percentiles,
            aes(x = step, y = percent75, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPA_percentiles, 
            aes(x = step, y = percent50, color = as.factor(valley)), 
            linetype = "solid", linewidth = 1) +
  # geom_point(alpha = 0.01)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=30),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30))+
  scale_y_continuous(breaks=seq(0, 10, 2))+ 
  xlab("Time step")+
  ylab("Average hierarchy within each valley")+
  scale_color_manual(values=c("#999933", "#CC6677","#88CCEE"),
                     name="Valley:  ",
                     labels=c("Etla/ Central  ","Ocotlán/ Zimatlán  ", "Tlacolula"))+
  ggtitle("Test 1: Archaeological village starting locations")





################### TEST 2 ###################
#### RANDOM on Class I and ClassII STARTING POINTS ####

#25 iterations
EXPRa<-read.csv("Model_B_EXPR1_a.txt", header = TRUE) # probability.attack = 0.1, village.range = 1
EXPRb<-read.csv("Model_B_EXPR2_b.txt", header = TRUE) # probability.attack = 1, village.range = 1
EXPRc<-read.csv("Model_B_EXPR3_c.txt", header = TRUE) # probability.attack = 0.1, village.range = 10
EXPRd<-read.csv("Model_B_EXPR4_d.txt", header = TRUE) # probability.attack = 1, village.range = 10

#25 iterations
EXPRe<-read.csv("Model_A_EXPR1_a.txt", header = TRUE)
EXPRf<-read.csv("Model_A_EXPR2_b.txt", header = TRUE)
EXPRg<-read.csv("Model_A_EXPR3_c.txt", header = TRUE)
EXPRh<-read.csv("Model_A_EXPR4_d.txt", header = TRUE)

EXPRz<-rbind(EXPRe, EXPRf, EXPRg, EXPRh)
EXPRz<-EXPRz %>% 
  select(-max.hierarchy) %>% 
  select(-average.polity.hierarchy)

EXPR<-rbind(EXPRa, EXPRb, EXPRc, EXPRd, EXPRz)




EXPR<-  gather(EXPR, valley, valley.hierarchy, "etla.hierarchy", "tlac.hierarchy", "ocot.hierarchy")

#to filter out unnecessary info
EXPR<-EXPR %>% 
  select(valley.hierarchy, step, moving.distance, probability.attack, valley) %>% 
  filter(valley.hierarchy>0) 




EXPR$probability.attack<-factor(EXPR$probability.attack,
                                levels=c(0.1, 1),
                                labels=c(" Polities likely to attack \n once every 100 years ",
                                         " Polities likely to attack \n once every 10 years "))

EXPR$moving.distance<-factor(EXPR$moving.distance,
                             levels=c(1, 10),
                             labels=c("Small village range (1.2 km)",
                                      "Medium village range (11.4km)"))

EXPR<-EXPR %>% 
  filter(step>0) 

# Calculate percentiles for each step and geography
EXPR_percentiles <- EXPR %>%
  group_by(step, moving.distance, probability.attack, valley) %>%
  summarise(
    percent25 = quantile(valley.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(valley.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(valley.hierarchy, probs = 0.75, na.rm = TRUE) )


ggplot(EXPR, aes(x=step, y=valley.hierarchy, color=as.factor(valley)))+
  theme_bw()+
  facet_grid(probability.attack~moving.distance)+
  geom_errorbar(inherit.aes = FALSE, data = EXPR_percentiles,
                aes(x=step, ymin = percent25, ymax = percent75, color=as.factor(valley)),
                width=0, linewidth=1.25, alpha = 0.1)+
  geom_line(data = EXPR_percentiles,
            aes(x = step, y = percent25, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPR_percentiles,
            aes(x = step, y = percent75, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPR_percentiles, 
            aes(x = step, y = percent50, color = as.factor(valley)), 
            linetype = "solid", linewidth = 1) +
  # geom_point(alpha = 0.01)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=30),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30))+
  scale_y_continuous(breaks=seq(0, 10, 2))+ 
  xlab("Time step")+
  ylab("Average hierarchy within each valley")+
  scale_color_manual(values=c("#999933", "#CC6677","#88CCEE"),
                     name="Valley:  ",
                     labels=c("Etla/ Central  ","Ocotlán/ Zimatlán  ", "Tlacolula"))+
  ggtitle("Test 2: Class I and Class II land starting points")







################### TEST 3 ###################
#### RANDOM on Class I and ClassII and Class IIIa STARTING POINTS ####

#25 iterations
EXPRRa<-read.csv("Model_B_EXPRR1_a.txt", header = TRUE) # probability.attack = 0.1, village.range = 1
EXPRRb<-read.csv("Model_B_EXPRR2_b.txt", header = TRUE) # probability.attack = 1, village.range = 1
EXPRRc<-read.csv("Model_B_EXPRR3_c.txt", header = TRUE) # probability.attack = 0.1, village.range = 10
EXPRRd<-read.csv("Model_B_EXPRR4_d.txt", header = TRUE) # probability.attack = 1, village.range = 10

#25 iterations
EXPRRe<-read.csv("Model_A_EXPRR1_a.txt", header = TRUE)
EXPRRf<-read.csv("Model_A_EXPRR2_b.txt", header = TRUE)
EXPRRg<-read.csv("Model_A_EXPRR3_c.txt", header = TRUE)
EXPRRh<-read.csv("Model_A_EXPRR4_d.txt", header = TRUE)



EXPRRz<-rbind(EXPRRe, EXPRRf, EXPRRg, EXPRRh)
EXPRRz<-EXPRRz %>% 
  select(-max.hierarchy) %>% 
  select(-average.polity.hierarchy)

EXPRR<-rbind(EXPRRa, EXPRRb, EXPRRc, EXPRRd, EXPRRz)


EXPRR<-  gather(EXPRR, valley, valley.hierarchy, "etla.hierarchy", "tlac.hierarchy", "ocot.hierarchy")

#to filter out unnecessary info
EXPRR<-EXPRR %>% 
  select(valley.hierarchy, step, moving.distance, probability.attack, valley) %>% 
  filter(valley.hierarchy>0) 


EXPRR$probability.attack<-factor(EXPRR$probability.attack,
                                 levels=c(0.1, 1),
                                 labels=c(" Polities likely to attack \n once every 100 years ",
                                          " Polities likely to attack \n once every 10 years "))

EXPRR$moving.distance<-factor(EXPRR$moving.distance,
                              levels=c(1, 10),
                              labels=c("Small village range (1.2 km)",
                                       "Medium village range (11.4km)"))

EXPRR<-EXPRR %>% 
  filter(step>0) 


# Calculate percentiles for each step and geography
EXPRR_percentiles <- EXPRR %>%
  group_by(step, moving.distance, probability.attack, valley) %>%
  summarise(
    percent25 = quantile(valley.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(valley.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(valley.hierarchy, probs = 0.75, na.rm = TRUE) )


ggplot(EXPRR, aes(x=step, y=valley.hierarchy, color=as.factor(valley)))+
  theme_bw()+
  facet_grid(probability.attack~moving.distance)+
  geom_errorbar(inherit.aes = FALSE, data = EXPRR_percentiles,
                aes(x=step, ymin = percent25, ymax = percent75, color=as.factor(valley)),
                width=0, linewidth=1.25, alpha = 0.1)+
  geom_line(data = EXPRR_percentiles,
            aes(x = step, y = percent25, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPRR_percentiles,
            aes(x = step, y = percent75, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = EXPRR_percentiles, 
            aes(x = step, y = percent50, color = as.factor(valley)), 
            linetype = "solid", linewidth = 1) +
  # geom_point(alpha = 0.01)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=30),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30))+
  scale_y_continuous(breaks=seq(0, 10, 2))+ 
  xlab("Time step")+
  ylab("Average hierarchy within each valley")+
  scale_color_manual(values=c("#999933", "#CC6677","#88CCEE"),
                     name="Valley:  ",
                     labels=c("Etla/ Central  ","Ocotlán/ Zimatlán  ", "Tlacolula"))+
  ggtitle("Test 3: Class I, Class II, and Class IIIa land starting points")






#############################################################

#### village.range = 1, to compare conditions side-by-side ####

EXPA.a <- EXPA %>% 
  filter(moving.distance == "Small village range (1.2 km)") %>% 
  mutate(experiment.condition = "Archaeological\nstarting locations")

EXPA.a_percentiles <- EXPA_percentiles %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Archaeological\nstarting locations")


EXPR.a <- EXPR %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I or II land")

EXPR.a_percentiles <- EXPR_percentiles %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I or II land")


EXPRR.a <- EXPRR %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I, II, or IIIa land")

EXPRR.a_percentiles <- EXPRR_percentiles %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I, II, or IIIa land")


range1 <- rbind(EXPA.a, EXPR.a, EXPRR.a) 
range1_percentiles <- rbind(EXPA.a_percentiles, EXPR.a_percentiles, EXPRR.a_percentiles)


ggplot(range1, aes(x=step, y=valley.hierarchy, color=as.factor(valley)))+
  theme_bw()+
  facet_grid(probability.attack~experiment.condition)+
  geom_errorbar(inherit.aes = FALSE, data = range1_percentiles,
                aes(x=step, ymin = percent25, ymax = percent75, color=as.factor(valley)),
                width=0, linewidth=1.25, alpha = 0.1)+
  geom_line(data = range1_percentiles,
            aes(x = step, y = percent25, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = range1_percentiles,
            aes(x = step, y = percent75, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = range1_percentiles, 
            aes(x = step, y = percent50, color = as.factor(valley)), 
            linetype = "solid", linewidth = 1) +
  # geom_point(alpha = 0.01)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text.x  = element_text(size=15),
        axis.text.y =element_text(size=25),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30))+
  scale_y_continuous(breaks=seq(0, 10, 2))+ 
  xlab("Time step")+
  ylab("Average hierarchy within each valley")+
  scale_color_manual(values=c("#999933", "#CC6677","#88CCEE"),
                     name="Valley:  ",
                     labels=c("Etla/ Central  ","Ocotlán/ Zimatlán  ", "Tlacolula"))
  # ggtitle("Comparing test conditions, village.range = 1")



#############################################################

#### village.range = 10, to compare conditions side-by-side ####

EXPA.b <- EXPA %>% 
  filter(moving.distance == "Medium village range (11.4km)") %>% 
  mutate(experiment.condition = "Archaeological\nstarting locations")

EXPA.b_percentiles <- EXPA_percentiles %>% 
  filter(moving.distance == "Medium village range (11.4km)")%>% 
  mutate(experiment.condition = "Archaeological\nstarting locations")


EXPR.b <- EXPR %>% 
  filter(moving.distance == "Medium village range (11.4km)")%>% 
  mutate(experiment.condition = "Located on\nClass I or II land")

EXPR.b_percentiles <- EXPR_percentiles %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I or II land")


EXPRR.b <- EXPRR %>% 
  filter(moving.distance == "Medium village range (11.4km)")%>% 
  mutate(experiment.condition = "Located on\nClass I, II, or IIIa land")

EXPRR.b_percentiles <- EXPRR_percentiles %>% 
  filter(moving.distance == "Small village range (1.2 km)")%>% 
  mutate(experiment.condition = "Located on\nClass I, II, or IIIa land")


range2 <- rbind(EXPA.b, EXPR.b, EXPRR.b) 
range2_percentiles <- rbind(EXPA.b_percentiles, EXPR.b_percentiles, EXPRR.b_percentiles)


ggplot(range2, aes(x=step, y=valley.hierarchy, color=as.factor(valley)))+
  theme_bw()+
  facet_grid(probability.attack~experiment.condition)+
  geom_errorbar(inherit.aes = FALSE, data = range2_percentiles,
                aes(x=step, ymin = percent25, ymax = percent75, color=as.factor(valley)),
                width=0, linewidth=1.25, alpha = 0.1)+
  geom_line(data = range2_percentiles,
            aes(x = step, y = percent25, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = range2_percentiles,
            aes(x = step, y = percent75, color = as.factor(valley)),
            linetype = "dashed")+
  geom_line(data = range2_percentiles, 
            aes(x = step, y = percent50, color = as.factor(valley)), 
            linetype = "solid", linewidth = 1) +
  # geom_point(alpha = 0.01)+
  theme(plot.title=element_text(hjust=0.5,
                                size=24),
        strip.text.x = element_text(size=25),
        strip.text.y = element_text(size=25),
        strip.background = element_rect(fill="white"),
        axis.text.x  = element_text(size=15),
        axis.text.y =element_text(size=25),
        axis.title = element_text(size=32),
        legend.title = element_text(size=30,
                                    hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30))+
  scale_y_continuous(breaks=seq(0, 10, 2))+ 
  xlab("Time step")+
  ylab("Average hierarchy within each valley")+
  scale_color_manual(values=c("#999933", "#CC6677","#88CCEE"),
                     name="Valley:  ",
                     labels=c("Etla/ Central  ","Ocotlán/ Zimatlán  ", "Tlacolula"))
 # ggtitle("Comparing test conditions, village.range = 10")

