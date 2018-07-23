library(reshape2)
library(data.table)
library(dplyr)

#data loading
ratings <- read.csv("data/movie_rating.csv")

#data processing and formatting
movie_ratings <-  as.data.frame(acast(ratings, title ~ critic, value.var = "rating"))

#similarity calculation
sim_users <- cor(movie_ratings[,1:6],use = "complete.obs")

#sim_users[colnames(sim_users) == 'Toby']
sim_users[ ,6]

#predicting the unknown values

#seperating the non rated movies of Toby
rating_critic  <- setDT(movie_ratings[colnames(movie_ratings)[6]],keep.rownames = TRUE)[]
names(rating_critic) <-  c('title','rating')
titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
ratings_t <- ratings[ratings$title %in% titles_na_critic,]
#add similarity values for each user as new variable
x <- (setDT(data.frame(sim_users[,6]),keep.rownames = TRUE)[])
names(x) <- c('critic','similarity')
ratings_t <-  merge(x = ratings_t, y = x, by = "critic", all.x = TRUE)
#mutiply rating with similarity values
ratings_t$sim_rating <- ratings_t$rating * ratings_t$similarity
#predicting the non rated titles
ratings_t %>% group_by(title) %>% summarise(n = sum(sim_rating)/sum(similarity)) %>% arrange(desc(n))

library("recommenderlab")	 	 
# Loading to pre-computed affinity data	 
affinity.data <-read.csv("data/movie_rating.csv")
affinity.matrix <- as(affinity.data,"realRatingMatrix")

Rec.model <- Recommender(affinity.matrix,method="IBCF")  
Rec.model2 <- Recommender(affinity.matrix,method="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=5, minRating=1))
as(predict(Rec.model, affinity.matrix["Toby",], n=3), 'list')
as(predict(Rec.model2, affinity.matrix["Toby",], n=3), 'list')



m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=''),
                          item=paste("i", 1:10, sep='')))
m

r <- as(m, 'realRatingMatrix')
r
getRatingMatrix(r)
r_m <- normalize(r)
r_m
getRatingMatrix(r_m)


data(Jester5k)
Jester5k
r <- sample(Jester5k, 1000)
r
getRatings(r)
hist(getRatings(r), breaks=100)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)
hist(rowCounts(r), breaks=50)
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r
getModel(r)$topN
recom <- predict(r, Jester5k[1001:1002], n=5)
as(recom, 'list')

recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")
as(recom3, "matrix")[,1:20]
recom <- predict(r, Jester5k[1001:1002], type="ratingMatrix")
recom
as(recom, "matrix")[,1:10]