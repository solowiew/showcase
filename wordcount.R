rm(list = ls())
# Preliminaries
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# Load Data (Source: University of Toronto, The G7 Research Group, URL: "http://www.g7.utoronto.ca/summit/index.htm" )
files <- list.files("C:/Users/Home/Desktop/G7_TXT")

# Write a function that takes the name of a file 
# and returns the number of word uses in this file
GetWordCount <- function(file){
fileName <- glue("C:/Users/Home/Desktop/G7_TXT/", file, sep = "")
fileName <- trimws(fileName)
fileText <- glue(read_file(fileName)) # read in the new file
# excluding special characters from text
fileText <- gsub("\\$", "", fileText) 
fileText <- gsub("international environment", "", fileText)
fileText <- gsub("friendly environment", "", fileText) 
fileText <- gsub("political environment", "", fileText) 
fileText <- gsub("policy environment", "", fileText) 
fileText <- gsub("business environment", "", fileText) 
fileText <- gsub("welcoming environment", "", fileText) 
fileText <- gsub("stable environment", "", fileText) 
fileText <- gsub("non-inflatory environment", "", fileText) 
fileText <- gsub("macroeconomic environment", "", fileText) 
fileText <- gsub("investment climate", "", fileText) 
fileText <- gsub("business climate", "", fileText) 
fileText <- gsub("Atomic Energy Agency", "", fileText) 
# tokenize, search for an exact word, prepare output
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
environment <- str_count(tokens, "environment") # mere occurrences of the pattern (e.g. cognates)
climate <- str_count(tokens, "climate")
energy <- str_count(tokens, "energy") 
trade <- str_count(tokens, "trade")
file <- file
wordcount <- tibble(environment, climate, energy, trade, file)
return(wordcount)
} # end of the function

wordcounts <- data_frame() # file to put the output in

# get the word uses for each file in our output data frame
for(i in files){wordcounts <- rbind(wordcounts, GetWordCount(i))}

# Prepare final output
wordcounts <- mutate(wordcounts, separate(wordcounts, file, c("year", NA), sep = "_"))
as.numeric(wordcounts$year)
wordcounts <- wordcounts[, c(6, 5, 1, 2, 3, 4)]
View(wordcounts)

# Save final output
setwd("C:/Users/Home/Desktop")
write.csv(wordcounts, "G7_WORD_COUNTS.csv") # preprint version

# Data Visualization
library(tidyverse)
library(ggthemes)
library(gridExtra)
data <- wordcounts
data$environmentCS <- cumsum(data$environment)
data$climateCS <- cumsum(data$climate)
data$energyCS <- cumsum(data$energy)
data$tradeCS <- cumsum(data$trade)
data <- data[, c(1,2,7,8,9,10)]
colnames(data) <- c("year", "file", "Environment", "Climate", "Energy", "Trade")

dataViz <- gather(data, key = "wordEnv", value = "wordcountEnv", 3)
dataViz <- gather(dataViz, key = "wordCli", value = "wordcountCli", 3)
dataViz <- gather(dataViz, key = "wordEne", value = "wordcountEne", 3)
dataViz <- gather(dataViz, key = "wordTra", value = "wordcountTra", 3)

# Linear scale
g1 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountEnv, group=wordEnv, color = wordEnv), color="#69b379", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Environment")+
  labs(tag = "(а)")+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

g2 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountCli, group=wordCli, color = wordCli), color="#699db3", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Climate")+
  labs(tag = "(б)")+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

g3 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountEne, group=wordEne, color = wordEne), color="#3a3d3c", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Energy")+
  labs(tag = "(в)")+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

# Log scale
g4 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountEnv, group=wordEnv, color = wordEnv), color="#69b379", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(aes(x=year, y = wordcountEnv), color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Environment")+
  labs(tag = "(г)")+
  scale_y_log10()+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

g5 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountCli, group=wordCli, color = wordCli), color="#699db3", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(aes(x=year, y = wordcountEnv), color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Climate")+
  labs(tag = "(д)")+
  scale_y_log10(breaks = c(1, 10, 100))+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

g6 <- ggplot(data = dataViz, aes(x = year, y = wordcountTra, group = wordTra, color = wordTra)) +
  geom_line(data = dataViz, aes(x = year, y = wordcountEne, group=wordEne, color = wordEne), color="#3a3d3c", size=2) +
  geom_line(aes(color=wordTra), color="grey", size=2, alpha = 0.3) +
  geom_rangeframe(aes(x=year, y = wordcountEnv), color = "#272928")+
  theme_tufte()+
  theme(plot.title = element_text(color = "#272928", size=14),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.tag = element_text(color = "#272928", vjust = -3))+
  ggtitle("Energy")+
  labs(tag = "(е)")+
  scale_y_log10(breaks = c(1, 10, 100))+
  scale_x_discrete(breaks = c(1975,1990,2005,2021),limits = factor(c(1975:2021)))

# Visualization (save as 600x395)
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)
# github.com/solowiew
