# This script generates figure 1a, a sankey of all samples, how they were classified by pathology and NDMA and if they are concordant

setwd("~/IntraopR")



library(dplyr)
library(tidyr)
library(tibble)

# Data is imported and turned into a tibble so all dplyr functions function properly
FULL <- read.csv("FULL_Nanopore_FINAL.csv", header=T)
FULL <- as_tibble(FULL)

# # If subsetting the data
# Cohort <- FULL %>% filter(Cohort == "Pediatric") # use this to make any subset for plotting
# Cohort <- FULL %>% filter(Category == "Intraop") # use this to make any subset for plotting

# select the relevant columns for creating the flow chart. All IDs here should be represented in the colormap below 
links <- select(FULL, "Simp_Patho_diagnosis","Nanopore_Capper", "Concordance")
#links <- select_(Cohort, "Simp_Patho_diagnosis","Nanopore_Capper", "Concordance")

#
links <- links %>% unite(First_key, Simp_Patho_diagnosis, Nanopore_Capper, sep="->", remove = F) %>%
  unite(Second_key, Nanopore_Capper, Concordance, sep="->", remove = F) %>%
  select(First_key, Second_key, Concordance) %>% 
  gather(Keys, Concordance) %>%
  select(Concordance) %>% group_by(Concordance) %>% tally(sort = TRUE) %>%
  separate(Concordance, c("Source","Target"), sep="->")

#change the column names to match the Sankey inbut
colnames(links)<-c("Source", "Target", "value")

# For some reason the coloring fails if there is a space in the name
links$Source <- gsub(" ", "_", links$Source)

# Create a node data frame: it lists all entities involved in the Sankey
nodes <- data.frame(
  name=c(as.character(links$Source), 
         as.character(links$Target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe. Need to reformat it.
links$IDsource <- match(links$Source, nodes$name)-1 
links$IDtarget <- match(links$Target, nodes$name)-1

# Load the color map from Capper et al
Colormap <- read.csv("ColorMapFull.csv", header = T)
colnames(Colormap)<-c("name", "colorCode")
Colormap$name <- gsub(" ", "_", Colormap$name)

FindColors <- left_join(nodes, Colormap, by = "name")

color_scale <- data.frame(
  range = FindColors$colorCode,
  domain = FindColors$name,
  nodes = nodes,
  stringsAsFactors = FALSE
)

library(networkD3)
# Make the network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name",
                   fontSize = 22,
                   fontFamily = "sans-serif",
                   nodeWidth = 15,
                   nodePadding = 15,
                   sinksRight=T,
                   margin = c(top=50,right=0, left=0,bottom=50),
                   iterations = 100,
                   colourScale = JS(
                     sprintf(
                       'd3.scaleOrdinal()  
  .domain(%s)
  .range(%s)
',
                       jsonlite::toJSON(color_scale$domain),
                       jsonlite::toJSON(color_scale$range)
                     )
                   )
)
p