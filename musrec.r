library(shiny)
library(recommenderlab)
library(tibble)
require(codetools)
library(Matrix)
library(dplyr)
library(tidyr)


#loading, cleaning and organizing datasets
artists <- read.delim("~/capstonep/hetrec2011-lastfm-2k/artists.dat")
artists<-subset(artists, select =c(1,2) )
user_artists <- read.delim("~/capstonep/hetrec2011-lastfm-2k/user_artists.dat")
colnames(artists) <- c("artistID", "name")
tags <- read.delim("~/capstonep/hetrec2011-lastfm-2k/tags.dat")
user_tag_artists <- read.delim("~/capstonep/hetrec2011-lastfm-2k/user_taggedartists.dat")
user_artists <- inner_join(user_artists, artists, by="artistID")

#second df for additional use 
user_artistsdf<-read.delim("~/capstonep/hetrec2011-lastfm-2k/user_artists.dat")
user_artistsdf<- inner_join(user_artistsdf, artists, by="artistID")
uaID <- user_artistsdf %>%
  unique() %>%
  select(artistID, name)

user_artists <- select(user_artists, -artistID)
user_artists$name <- strtrim(user_artists$name, 100)
user_artists[16060,]$name <- NA

# filtering out artists that have less htan 4000 listens
user_artists_mx <- user_artists %>%
  filter(weight > 4000) %>%
  spread(key = name, value = weight) %>%
  as.matrix()
#removing column
user_artists <- user_artists_mx[,-1]

# similiarity matrix using recommenderlab package
user_artists_rec <- new("realRatingMatrix", data = as(user_artists, "CsparseMatrix"))
sim_item <- similarity(user_artists_rec, method = "cosine", which = "items")
sim_item_mx <- as.matrix(sim_item)
artists_recomm <- as.data.frame(sim_item_mx)
artists_recomm<- rownames_to_column(artists_recomm, var="artist")

#sparsematrix
user_artists_smx<-user_artists %>%
  spread(key = name, value= weight) %>%
  as.matrix()
user_artists2<-user_artists_smx[,c(-1)]

realRatingMatrix<- as(user_artists2, "realRatingMatrix")
uRaitings<-realRatingMatrix[rowCounts(realRatingMatrix)>50, colCounts(realRatingMatrix)>50]


#create test and training sets
train<-sample(x=c(TRUE, FALSE), size = nrow(uRaitings), replace = TRUE, prob = (c(.80, .20)))
rTrain<-uRaitings[train, ]
rTest<-uRaitings[!train, ]
# model provides top 10 recommendation
rec1<-Recommender(data= raiting.train, method="IBCF", parameter = list(k=10))
recP<-predict(object=rec1, newdata=rTest, n=10)
recP@items[1]

#using the tag dataset
user_tag_artists <- inner_join(user_tag_artists, tags, by = "tagID")
user_tag_artists <- left_join(user_tag_artists, uaID, by = "artistID")


tag_count <- user_tag_artists %>% 
  count(tagValue) %>% 
  arrange(desc(n)) %>%
  filter(n >= 1800)

user_tag_artists <- user_tag_artists %>%
  filter(tagValue %in% tag_count$tagValue)

#content based filtering using tag frequecy 
#not too beneficial for users. 
artist_tags <- user_tag_artists %>% group_by(tagValue, name) %>% tally()
alltags <- artist_tags$tagValue %>% unique() %>% as.character()




