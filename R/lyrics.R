library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(janeaustenr)
library(textdata)

get_sentiments("afinn")

# read lyrics and unnest text into word
# number is the position of each song in the album
# number>0 denotes the score for a song
lyrics <- read_csv(file="./Data/lyrics.csv") %>% 
  group_by(Album) %>% 
  mutate(Number = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word,Lyrics) 

# calculate average sentiment for each album 
# number = 0 denotes the score for an album
afinn_album <- lyrics %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Album, Year) %>% 
  summarise(sentiment = mean(value)) %>% 
  mutate(Song = "", Number=0) %>% 
  select(Album,Year, Song, Number, sentiment)
  
#calculate average sentiment for each song
afinn_song <- lyrics %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Album, Year, Song, Number) %>% 
  summarise(sentiment = mean(value))

# union album and song into afinn
# the data are not tidy
# but it is to trick Tableau to display the musical symbols:
# create number=0 to position the G musical symbol
# (mean score of the album) at beginning of each album
afinn <- rbind(afinn_album, afinn_song) %>% 
  arrange(Album, Number) %>% 
  mutate(symbol=if_else(Number == 0, "mean", 
                        if_else(sentiment>0, "positive", 
                                if_else(sentiment<0, "negative", "zero"))))
  
# write output into csv file
write_csv(afinn,file="./Data/afinn.csv")
