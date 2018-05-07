# packages ----
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(sf)
library(leaflet)
library(tigris)
# importing ----
nuforc <- read.csv("~/R/ufos/nuforc.csv")
# sorting ----
shape <- nuforc %>%
  count(shape, sort = TRUE)
mystate <- nuforc %>%
  count(state, sort = TRUE)
year <- nuforc %>%
  count(year, sort = TRUE)
# graphing ----
shape_plot <- shape %>%
  top_n(20) %>%
  ggplot(aes(reorder(shape, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(y = "count", x = "shape") +
  ggtitle("Shape of UFOs spotted in the U.S.") +
  theme_minimal()
mystate %>%
  top_n(20) %>%
  ggplot(aes(reorder(state, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE)
year %>%
  top_n(20) %>%
  ggplot(aes(reorder(year, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE)
# trying this again ----
utf1 <- read.delim("~/Desktop/datasets/hopefullyUTF8/utf1.txt", header=FALSE)
utf2 <- read.csv("~/Desktop/datasets/hopefullyUTF8/utf2.txt", header=FALSE)
utf3 <- read.csv("~/Desktop/datasets/hopefullyUTF8/utf3.txt", header=FALSE)
one <- data_frame(utf1$V1)
two <- data_frame(utf2$V1)
three <- data_frame(utf3$V1)
texty1_df <- data.frame(text = one)
texty1_dl <- unnest_tokens(texty1_df, 
                          input = text, 
                          output = line, 
                          token = "sentences", 
                          to_lower = F)
texty2_df <- data.frame(text = two)
texty2_dl <- unnest_tokens(texty2_df, 
                          input = text, 
                          output = line, 
                          token = "sentences", 
                          to_lower = F)
# with one ----
texty <- one$`utf1$V1`
texty_df <- data.frame(text = texty)
texty_dl <- unnest_tokens(texty_df, 
                          input = text, 
                          output = line, 
                          token = "sentences", 
                          to_lower = F)
texty_dl2 <- texty_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
texty_dl3 <- texty_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
texty_words <- texty_dl3 %>% 
  count(word, sort = TRUE)
# with two ----
texty2 <- two$`utf2$V1`
texty2_df <- data.frame(text = texty2)
texty2_dl <- unnest_tokens(texty2_df, 
                          input = text, 
                          output = line, 
                          token = "sentences", 
                          to_lower = F)
texty2_dl2 <- texty2_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
texty2_dl3 <- texty2_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
texty2_words <- texty2_dl3 %>% 
  count(word, sort = TRUE)

# with three ----
texty3 <- three$`utf3$V1`
texty3_df <- data.frame(text = texty3)
texty3_dl <- unnest_tokens(texty3_df, 
                          input = text, 
                          output = line, 
                          token = "sentences", 
                          to_lower = F)
texty3_dl2 <- texty3_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
texty3_dl3 <- texty3_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
texty3_words <- texty3_dl3 %>% 
  count(word, sort = TRUE)
# FINALLY ----
words_words_words <- texty_dl3 %>%
  rbind.data.frame(texty2_dl3)
words_words_words <- words_words_words %>%
  rbind.data.frame(texty3_dl3)
words_words_words2 <- words_words_words %>%
  count(word, sort = TRUE)

wordcloud2(words_words_words2 %>% top_n(100))

# mapping ----
state_abbrvs <- read_excel("~/Desktop/datasets/state_abbrvs.xlsx")
mystates <- state %>%
  inner_join(state_abbrvs, by = "state")

us <- st_read(system.file("shape/us.shp", package = "sf"), quiet = TRUE)

# mapping with Walsh's help ----
states(cb = FALSE, resolution = "500k", year = NULL) -> statesDF
st_as_sf(statesDF) -> statesDF
as.data.frame(statesDF) -> statesDF2

names(mystates)[names(mystates) == 'full_name'] <- 'name'
names(statesDF2)[names(statesDF2) == 'NAME'] <- 'name'

mystatesDF2 <- mystates %>%
  inner_join(statesDF2, by = "name")

sightings_map <- mystatesDF2 %>% ggplot() +
  geom_sf(fill = mystatesDF2$n)

sightings_map + theme(panel.grid.major = element_line(colour = 'transparent'),
                     axis.title.x=element_blank(), 
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(), 
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     panel.background=element_blank(),
                     panel.border=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank())

statesDF %>% 
  ggplot() +
  geom_sf(aes(fill = statesDF2$ALAND)) 

gc()

# googlevis ----
library(readxl)
state_sightings <- read_excel("/Applications/state_sightings.xlsx")
install.packages("googleVis")
library(googleVis)
map <- gvisGeoChart(state_sightings, "state", "sightings/pop.", options=list(region="US", 
                                                                             displayMode="regions", 
                                                                             resolution="provinces",
                                                                             width=600, height=400))
plot(map)

# code for markdown ----
library(readxl)
state_sightings <- read_excel("/Applications/state_sightings.xlsx")
suppressPackageStartupMessages(library(googleVis))
op <- options(gvis.plot.tag=NULL)
head(state_sightings)
map <- gvisGeoChart(state_sightings, "state", "sightings/pop.", options=list(region="US", displayMode="regions", resolution="provinces", width=600, height=400))
print(map)