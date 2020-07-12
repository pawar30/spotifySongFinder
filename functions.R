
#df - dataframe with track_name, track_artist and track_properties
#closest - boolean value that determines whether to return songs 
#          that match most/least
#song - string represeting the song being matched
#n - number of songs to return. Default value is 10
findSongs <- function(df,closest,song,num=10){
  
  song.properties <- df[which(df$track_name==song),]$properties[[1]]
  if(closest=="N"){
    num = num
  }else{
    num= -num
  }
  dist.df <- df %>% mutate(distance = mapply(function(x, y){
    dist(rbind(x,y),method = "euclidean")
  }, properties, song.properties)) %>%
    top_n(num) %>%
    select(track_name,track_artist, distance) %>%
    arrange(distance) 
  
  
  dist.df
}

#This function determines the euclidean distance 
#between two word vectors
#song 1 - string representing the first song
#song1Artist - string representing the artist of the first song
#song 1 - string representing the second song
#song1Artist - string representing the artist of the second song
#df - dataframe with track_name, track_artist and track_properties
distanceBetween <- function(song1,song1Artist, song2, song2Artist, df){
  
  song1Properties <- df[df$track_name==song1 & df$track_artist==song1Artist,]$properties[[1]]
  song2Properties <- df[df$track_name==song2 & df$track_artist==song2Artist,]$properties[[1]]
  
  songSimilarity <- dist(rbind(song1Properties,song2Properties),method = "euclidean")
  as.numeric(songSimilarity)
}
