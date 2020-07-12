library(readr)
library(rlist)
library(tidyverse)

source("C:/Users/PRAPAWA/Documents/R Scripts/Spotify_Songs/functions.R")

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
View(spotify_songs)
str(spotify_songs)
#convert duration_ms to minute
spotify_songs$duration_min <- spotify_songs$duration_ms/60000
spotify_songs$duration_ms <- NULL

numericCols <- unlist(lapply(spotify_songs, is.numeric))  
spotify_songs_numFeatures <- spotify_songs[,numericCols]
#check correlations
cor.df <- cor(spotify_songs_numFeatures)
View(cor.df)


#filter the "pop" songs with only their danceability and liveliness features
spotify_songs_pop <- spotify_songs %>% filter(playlist_genre=="pop") %>%
                                       select(track_id,track_name,track_artist,danceability,valence)
dim(spotify_songs_pop)

#check for duplicates and remove
spotify_songs_pop %>% group_by(track_id,track_name,track_artist) %>%
                      summarise(duplicate_count=n()) %>%
                      filter(duplicate_count > 1)

#remove duplicate and keep only 1 row of each track/artist
spotify_songs_pop <- distinct(spotify_songs_pop)

View(spotify_songs_pop)
#plot the top 10 danceable tracks against their valence scores
spotify_songs_pop %>% top_n(danceability,n=10) %>% 
                      ggplot(aes(danceability,valence)) +
                      geom_point(aes(colour=paste0(track_name," by ",track_artist)))


#create a new dataframe that has the trackname in one column
#and the properties danceability and valence as
#a vector in the other column named "Properties"
popSongs_properties <- spotify_songs_pop %>% mutate(properties = mapply(c, danceability,valence, SIMPLIFY = F)) %>%
  select(track_name,track_artist, properties)


closestMatchesPop <- findSongs(popSongs_properties,artist="Daft Punk",closest="Y",song="Around the World")
closestMatchesPop <- as.data.frame(closestMatches)

#cool. Let's see how close these songs actually are to Anaconda
spotify_songs_pop %>% filter((track_name %in% closestMatches$track_name & track_artist %in% closestMatches$track_artist) | (track_name=="Around the World" & track_artist=="Daft Punk")) %>%
                      ggplot(aes(danceability,valence)) +
                      geom_point(aes(colour=(track_name)))


distance1 <- distanceBetween("Ironic - 2015 Remaster","Alanis Morissette","Hollaback Girl","Gwen Stefani",popSongs_properties)
distance2 <- distanceBetween("Ironic - 2015 Remaster","Alanis Morissette","Don't Let the Sun Go Down on Me","George Michael",popSongs_properties)

#Lets plot

spotify_songs_pop %>% filter(track_name %in% c("Ironic - 2015 Remaster","Hollaback Girl","Don't Let the Sun Go Down on Me") & track_artist %in% c("Alanis Morissette","Gwen Stefani","George Michael")) %>% 
  ggplot(aes(danceability,valence)) +
  geom_point(aes(colour=(track_name)))

#extrapolating the above logic for all properties
numCols <- names(numericCols[numericCols])
spotify_songs_properties <- spotify_songs %>% mutate(properties = mapply(c,track_popularity,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_min, SIMPLIFY = F)) %>%
                            select(track_name,track_artist, properties)

closestMatchesAll <- findSongs(spotify_songs_properties,artist="Simon & Garfunkel",closest="Y",song="The Boxer")
closestMatchesAll <- as.data.frame(closestMatchesAll)


distance1 <- distanceBetween("Ironic - 2015 Remaster","Alanis Morissette","Hollaback Girl","Gwen Stefani",spotify_songs_properties)
distance2 <- distanceBetween("Ironic - 2015 Remaster","Alanis Morissette","Ping Pong","Armin van Buuren",spotify_songs_properties)

if(distance1 < distance2){
  print("Ironic by Alanis Morissette is more similar to Hollaback Girl by Gwen Stefani than it is to Ping Pong by Armin van Buuren")
}
