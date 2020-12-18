setwd("D:/IntraopR")


# SunburstR
library(sunburstR)
library(htmltools)
library(d3r)
library(tidyr)
library(dplyr)


FULL <- read.csv("FULL_Nanopore_FINAL.csv", header=T)
FULL <- as_tibble(FULL)

Cohort <- FULL %>% filter(Cohort == "Pediatric") # use this to make any subset for plotting
Cohort <- FULL %>% filter(Category == "PNET") # use this to make any subset for plotting



#dat <- select_(FULL, "Simp_Patho_diagnosis","Nanopore_Capper","MCF_class_Capper", "Concordance")
dat <- select_(Cohort, "Simp_Patho_diagnosis","Nanopore_Capper","MCF_class_Capper", "Concordance")

dat$size <- 1

dat <- dat[,c(3,2,1,4,5)] #select columns and re-order. This creates the order of rings

# Get a list of all the unique entities in the data that need colors
Uniques <- data.frame(
  name=c(as.character(dat$Simp_Patho_diagnosis),
         as.character(dat$Nanopore_Capper),
         as.character(dat$MCF_class_Capper),
         as.character(dat$Concordance)
  ) %>% unique()
)

tree <- d3_nest(dat, value_cols = "size")
labels <- Uniques$name



Colormap <- read.csv("ColorMapFull.csv", header = T)
colnames(Colormap)<-c("Tissue", "name", "colorCode")

FindColors <- left_join(Uniques, Colormap, by = "name")

sb1 <- sunburst(tree, 
                width="100%", 
                height=500,
                count = T,
                percent = F,
                sumNodes = T,
                colors = list(range = FindColors$colorCode, domain = labels),
                #withD3=TRUE
                #legendOrder = labels
)
sb1
