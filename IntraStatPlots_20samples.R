# Script to produce Figures 1d and 1e

setwd("D:/IntraopR")

library(ggsci)
library(dplyr)

Seq_stats <- read.csv("SeqStats.csv", header = T)

#Seq_stats <- mutate(Seq_stats, sum_seq = Mean_read_length * Read_num)

############## CpGs vs Read numbers ########################
library(ggplot2)

P1 <- ggplot(Seq_stats, aes(x = log10(CpG_sites), y = log10(sum_seq), fill = Minutes)) 

P2 <- P1 + geom_smooth(mapping = aes(linetype = "r1"),
                 method = "loess",
                 formula = y ~ x, 
                 se = TRUE,
                 color = "black",
                 show.legend = F)

Q1 <- P2 + geom_point(size=5, stroke = 1, shape = 21, alpha = 0.8) +
#ggtitle("Coverage (bp) as function of total CpG sites") +
  theme_bw(base_size=15) +
  scale_fill_gradientn(colors=blues9,name="Sequencing\ntime (min)") +
    theme(legend.position="none")+
  scale_y_continuous(name="Total nucleotides analyzed (log10)") +
  scale_x_continuous(name="Total CpG sites discovered (log10)")
Q1

###########################################

################# Read Number vers Error rate #################
P5 <- ggplot(Seq_stats, aes(x = log10(CpG_sites), y = Subclass.OOB.error, fill = Minutes))

P6 <- P5 + geom_smooth(mapping = aes(linetype = "r1"),
                       method = "loess",
                       formula = y ~ x, 
                       se = TRUE,
                       color = "black",
                       show.legend = F)

Q3 <- P6 + geom_point(size=5, stroke = 1, shape = 21, alpha = 0.8) +
  #ggtitle("Out-of-Bag error rate as a function of CpG sites")+
  theme_bw(base_size=15) +
  scale_fill_gradientn(colors=blues9,name="Sequencing\ntime (min)") +
  theme(legend.position=c(.75, .75))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
  scale_x_continuous(name="Total CpG sites analyzed (log10)") +
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
