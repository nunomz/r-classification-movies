# load sales dataset
df_sales <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/archive/Highest Holywood Grossing Movies.csv", sep = ",")

View(df_sales)

# drop useless columns: 
df = subset(df_sales, select = -c(X,Movie.Info,Distributor,Release.Date,Genre))

View(df)

# separate release year from name
library(stringr)
df$ReleaseYear <- str_sub(df$Title,-5,-2)
df$primaryTitle = str_sub(df$Title,1,nchar(df$Title)-7)

# reorder df, drop domestic and int sales keeping only worldwide
movie_sales <- df[, c("primaryTitle","ReleaseYear","World.Sales..in...","Movie.Runtime","License")]
View(movie_sales)


#------------------------------
# merge with title basics
#------------------------------

# load title_basics dataset
title_basics <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/title.basics.tsv/data.tsv", sep = "\t")
View(title_basics)

# drop useless obs
df_titles <- title_basics[which(title_basics$titleType == "movie"),]

# merge
title_sales <- merge(df_titles, movie_sales, by = 'primaryTitle')
View(title_sales)

# data treatment
title_sales <- title_sales[which(title_sales$ReleaseYear == title_sales$startYear),]
print(summary(title_sales))
title_sales <- title_sales[, c("primaryTitle","tconst","ReleaseYear","isAdult","runtimeMinutes","genres","World.Sales..in...")]
colnames(title_sales) <- c("primaryTitle","tconst","releaseYear","isAdult","runtimeMinutes","genres","worldwideSalesDollars")

View(title_sales)

# merge with ratings
df_sales_ratings <- merge(title_sales, df_ratings, by = 'tconst')


#------------------------------
# data visualization
#------------------------------

library(ggplot2)

graph1<-ggplot(title_sales, aes( x = title_sales$worldwideSalesDollars, y = title_sales$releaseYear))

graph1 + geom_col()

#------------------------------
# get more movies to assemble unsuccessful movies
#------------------------------

# load title_ratings dataset
title_ratings <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/title.ratings.tsv/data.tsv", sep = "\t")
print(summary(title_ratings))

# reduce number of movies till ~4000
df_ratings = subset(title_ratings, title_ratings$numVotes >= 50000)
View(df_ratings)

#use df_titles
df_titles <- df_titles[, c("primaryTitle","tconst","startYear","isAdult","runtimeMinutes","genres")]
colnames(df_titles) <- c("primaryTitle","tconst","releaseYear","isAdult","runtimeMinutes","genres")
df_titles$worldwideSalesDollars <- NA
View(df_titles)
df_titles_rat <- merge(df_titles, df_ratings, by = 'tconst')
View(df_titles_rat)

#------------------------------
# create df with unsuccessful and successful movies
#------------------------------ 

df_sales_all <- rbind(df_titles_rat, df_sales_ratings)

#------------------------------
# add necessary attributes
#------------------------------

# load title_crew dataset
title_crew <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/title.crew.tsv/data.tsv", sep = "\t")

# join df_sales_all and title_crew to get directors and writers of each movie
df_complete_crew <- merge(df_sales_all, title_crew, by = 'tconst')
View(df_complete_crew)

# load title_principals dataset
title_principals <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/title.principals.tsv/data.tsv", sep = "\t")

# remove everything but actors from principals
df_principals = subset(title_principals, title_principals$category == 'actor' | title_principals$category == 'actress' | title_principals$category == 'self')

# assemble mainCast
library(dplyr)

df_principals_fixed <- df_principals %>%
  group_by(tconst) %>%
  summarise(mainCast = toString(unique(nconst)))

View(df_principals_fixed)

View(df_complete_crew)

# merge mainCast into complete_crew
df_complete <- merge(df_complete_crew, df_principals_fixed, by = 'tconst')

write.csv(df_complete,"C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/complete.csv", row.names = FALSE)

df_complete <- read.csv("C:/Users/onuno/Documents/3º ano/2S/Data Mining/datasets/complete.csv", sep = ",")


# delete useless dfs
rm(df_sales)
rm(df)


