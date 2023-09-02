#imdb movies and players database

options(scipen = 999)

#Read CSV
imdb.movies<- read.csv("imdb_movies.csv")
players.df<-read.csv("imdb_players.csv")

# remove rows with missing values
imdb.movies.nomissing <- 
  imdb.movies[is.na(imdb.movies$total.gross) == FALSE, ]

#Convert budget into numeric
imdb.movies$budget<- as.numeric(as.character(imdb.movies$budget))
class(imdb.movies$budget)
summary(imdb.movies$budget)

#New column for budget higher than 1 million dollars
imdb.movies$is.over.1m<- imdb.movies$budget>=1000000

#count true/false
#720 movies with budget higher than 1 million dollars
table(imdb.movies$is.over.1m)

#creating data frame that counts each specific role for each movie
n.actors<- as.data.frame.matrix(table(players.df$id, players.df$role))
head(n.actors)

#correlation between budget and total gross
cor(is.na(imdb.movies$budget) == FALSE , is.na(imdb.movies$total.gross) == FALSE)

#the average budget for each genre which contains the strings "Action" or "Comedy"
aggregate(budget ~ genre, data = imdb.movies[grep("Action|Comedy", imdb.movies$genre), ], mean)

# mean gross per genre
table(imdb.movies$genre)
aggregate(total.gross ~ genre, data = imdb.movies, FUN = "mean")

# compute number of directors per movie, convert the result into data.frame called "numDirectors.df" and label the columns.
numDirectors <- table(players.df[players.df$role == "Director",]$id)
numDirectors.df <- as.data.frame(numDirectors)
names(numDirectors.df) <- c("id", "numDir")

#merge the datasets by id
merged.data <- merge(imdb.movies, numDirectors.df, 
                     by.x = "id", 
                     by.y = "id", 
                     all = T)

# data partitioning
set.seed(1)

train.rows <- sample(1:dim(imdb.movies)[1], dim(imdb.movies)[1]*0.6)
train.data <- imdb.movies[train.rows, ]
valid.data <- imdb.movies[-train.rows, ]

