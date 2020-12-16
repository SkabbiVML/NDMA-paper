# This script is to generate figure 1a

# Data is imported and turned into a tibble so all dplyr functions function properly

setwd("~/IntraopR") # set working directory to where FULL_Nanopore_Final.csv is located

library(dplyr)
FULL <- read.csv("FULL_Nanopore_FINAL.csv", header=T) # 105 patients
FULL <- as_tibble(FULL)

P <- FULL %>% group_by(Category, Cohort) %>% tally() # summarize groups

# Change the names so that they make more sense
P$Category <- gsub("Training set", "Validation set", P$Category) 
P$Category <- gsub("Double_Blind", "Double-blind", P$Category)
P$Category <- gsub("Difficult cases", "Inconclusive frozen", P$Category)
P$Category <- gsub("Intraop", "Intraoperative", P$Category)

# order the samples correctly in the barplot so they match how they are introduced in the text
P <- P %>%
  mutate(Category = factor(Category, levels=c("Intraoperative","Inconclusive frozen","Double-blind","PNET","Prospective","Validation set")))

# Make the plot
library(ggplot2)
G <- ggplot(P, aes(x=Category, y=n, fill = Cohort, label = n))

G + geom_bar(position="stack", stat="identity", colour="black")+
  theme_bw(base_size = 22)+
  scale_fill_manual(values=c("#cccccc", "#696969"))+
  #xlab("Patient cohort")+
  xlab("")+
  ylab("")+
  labs(fill = "")+
  coord_flip()+
  theme(legend.position=c(.75, .35))+
  geom_text(size = 5, position = position_stack(vjust = 0.5))


  
