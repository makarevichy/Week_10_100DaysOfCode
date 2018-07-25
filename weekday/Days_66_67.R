library(tibble)
library(reshape2)
library(recommenderlab)
df <- tribble(
  ~x, ~y, ~z,
  'Tom', 'Star Wars', 5,
  
  'Jane', 'Titanic', 4,
  
  'Bill', 'Batman', 3,
  
  'Jane', 'Star Wars', 2,
  
  'Bill', 'Titanic', 3
)

df <- acast(df, x ~ y, value.var = 'z')
df <- as(df, 'realRatingMatrix')
model <- Recommender(df, method = 'UBCF')
pre <- predict(model, df[2], n = 1)
as(pre, 'list')

data("Jester5k")
model <- evaluationScheme(data = Jester5k,
                          method = 'split',
                          train = 0.9,
                          k = 3,
                          given = 3,
                          goodRating = 5)
model
pred <- evaluate(model, method = 'POPULAR', n = c(1, 3, 5, 10))
plot(pred, legend="topright")

pred <- evaluate(model, method = list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL)
))
plot(pred, legend="topright")
avg(pred)