# To produce Sankey plot of surgical impact, Figure 2e

setwd("./data/")

DF <- as_tibble(read.csv("SeqStats.csv", header = T))

library(tidyr)
library(dplyr)

DF2 <- DF %>% filter(Minutes==30) %>% 
            select(Patient,Nanopore_Capper,Simp_Patho_diagnosis,Conclusive,Potential.intraoperative.consequence) %>%
            distinct()

DF2 <- filter(DF2,Patient != "NDMA_105") # This sample does not contain any info on intraop frozen section. Discard from plot

#Reorder columns for plotting
DF3 <- DF2[,c(3,2,4,5)]

DF3$Conclusive <- gsub("Yes", "Conclusive", DF3$Conclusive)
DF3$Conclusive <- gsub("No", "Inconclusive", DF3$Conclusive)

# For some reason the coloring fails if there is a space in the name
DF3$Simp_Patho_diagnosis <- gsub(" ", "_", DF3$Simp_Patho_diagnosis)

links <- DF3 %>% unite(First_key, Simp_Patho_diagnosis, Nanopore_Capper, sep="->", remove = F) %>%
  unite(Second_key, Nanopore_Capper, Conclusive,   sep="->", remove = F) %>%
  unite(Third_key, Conclusive, Potential.intraoperative.consequence,sep="->", remove = F) %>%
  select_(1,3,5) %>%
  gather(Keys, Potential.intraoperative.consequence) %>%
  select_(2) %>% group_by(Potential.intraoperative.consequence) %>% tally(sort = TRUE) %>%
  separate(Potential.intraoperative.consequence, c("Source","Target"), sep="->")

#change the column names to match the Sankey inbut
colnames(links)<-c("Source", "Target", "value")

nodes <- data.frame(
  name=c(as.character(links$Source), 
         as.character(links$Target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe. Reformat it.
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
                   fontSize = 15,
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
#p

library(htmlwidgets)

onRender(
  p,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20);
  }
  '
)

