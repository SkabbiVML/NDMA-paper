# Script to produce Figures 1c and 1d

setwd("./data/")

library(ggsci)
library(dplyr)

Seq_stats <- read.csv("SeqStats.csv", header = T)


############## CpGs vs Time ########################

library(ggplot2)
library(scales)
P1 <- ggplot(Seq_stats, aes(x = Minutes, y = CpG_sites))

P2 <- P1 + geom_smooth(mapping = aes(linetype = "r1"),
                       method = "loess",
                       formula = y ~ x, 
                       se = TRUE,
                       color = "black",
                       show.legend = F)


Q1 <- P2 + geom_point(size=6, shape = 1) +
  theme_classic(base_size=15) +
  scale_x_continuous(name="Sequencing time (min)", breaks = c(10,30,60,90,120,150,180)) + 
  geom_hline(yintercept = 3500, linetype="dashed") +
  scale_y_log10(name="Number of CpG sites detected", label = number, breaks = c(1000,3500,10000,35000,100000))
 Q1

###########################################

################# Time verus Subclassifier Error rate #################
 
 P5 <- ggplot(Seq_stats, aes(x = Minutes, y = Subclass.OOB.error))
 
 P6 <- P5 + geom_smooth(mapping = aes(linetype = "r1"),
                        method = "loess",
                        formula = y ~ x, 
                        se = TRUE,
                        color = "black",
                        show.legend = F)
 
 Q3 <- P6 + geom_point(size=6, shape = 1) +
   
   theme_classic(base_size=15) +
   scale_fill_gradientn(colors=blues9,name="Sequencing\ntime (min)") +
   scale_x_continuous(name="Sequencing time (min)", breaks = c(10,30,60,90,120,150,180)) +
   scale_y_continuous(name="Out-of-bag error rate (%)")
 
 Q3
#######################################
library(tidyr)
MCF_Seq_stats <- Seq_stats %>% filter(Minutes==30) %>% select(Patient, CpG_sites, Subclass.OOB.error, MCF.OOB.error) %>%
                               gather("MCF.OOB.error", "Subclass.OOB.error", key = "Classifier", value = "OOB_Error")

############# MCF versus subclass error #####
p7 <- ggplot(MCF_Seq_stats, aes(x=reorder(Classifier,-OOB_Error), y = OOB_Error, fill = CpG_sites))

Q6 <- p7 + 
  geom_jitter(size=8, width = 0.05, height = 0, alpha = 0.8, shape = 21, stroke = 1)+
  scale_fill_viridis_c(direction = -1,limits = c(1000, 11000), breaks = c(2000, 4000, 6000, 8000, 10000)) +
  scale_y_continuous(limits = c(0,10), name="Out-of-bag error rate (%)")+
  #theme(legend.position=c(.80, .25))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
  guides(fill = guide_colourbar(barheight = 10))+
  labs(fill = "CpG sites")+
  scale_x_discrete(name="Classifier", labels = c("Subclass","MCF")) +
  theme_bw(15) 
Q6

########################
library(gridExtra)
grid.arrange(Q1,  Q3, Q6, nrow = 1)
