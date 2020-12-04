library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(ggridges)


Sys.setenv(SPOTIFY_CLIENT_ID = '5964d300017d4643a56fedb1e42b561e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '307841367f1c4457baea58d6bf6584ff')

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')

beatles

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(10) %>% 
  as_tibble()


joy <- get_artist_audio_features('joy division')
joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(10) %>% 
  as_tibble()

ggplot(joy, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Joy Division's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")

