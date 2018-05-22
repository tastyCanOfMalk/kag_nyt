# https://www.kaggle.com/kennethbollen/ny-times-bestsellers-with-goodreads-user-rating

library(tidyverse)

x <- read.csv("df_books_v2.csv")
x <- x[complete.cases(x),]

# check distribution of ratings
library(ggplot2)
ggplot(x, aes(x=x$ratings)) + geom_histogram()
ggplot(x, aes(x=x$ratings)) + geom_histogram(binwidth=.01)

# compare fiction vs non fiction
library(ggjoy)
ggplot(x, aes(x= ratings, y=Type, fill=Type)) + 
  geom_density_ridges(rel_min_height=.01) +
  scale_x_continuous(expand = c(0.01, 0), name="Rating", limits=c(2.5,5) ) +
  scale_y_discrete(expand = c(0.01, 0))

# density vs year?
# need to extract year only from date column
x$Best_Seller_Week <- as.Date(x$Best_Seller_Week) # convert from factor to date
x.Best_Seller_Year <- lubridate::year(x$Best_Seller_Week) # create year only column
x.Best_Seller_Month <- lubridate::month(x$Best_Seller_Week) # create month only column
x.2 <- cbind(x, 
             "Best_Seller_Month" = as.factor(x.Best_Seller_Month),
             "Best_Seller_Year" = as.factor(x.Best_Seller_Year))
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# ratings by month
ggplot(x.2, aes(x=x.2$ratings, y=x.2$Best_Seller_Month)) +
  geom_density_ridges(rel_min_height=.01) +
  scale_x_continuous(expand = c(0.01, 0), name="Rating", limits=c(2.5,5)) +
  scale_y_discrete(expand = c(0.01, 0), name="Month", labels=months)

# seggregate by fiction vs non-fiction
ggplot(x.2, aes(x=x.2$ratings, y=x.2$Best_Seller_Month, fill=Type)) +
  geom_density_ridges(rel_min_height=.01, alpha=.9, scale=1.1) +
  scale_x_continuous(expand = c(0.01, 0), name="Rating", limits=c(2.5,5)) +
  scale_y_discrete(expand = c(0.01, 0), name="Month", labels=months)

# try to determine sex from name
library(gender)



x.Author <- as.character(x$Author) # take names
x.Author <- gsub("\\.", "", x.Author) # remove periods from names
# x.Author <- gsub("\\. ", "", x.Author) # remove periods from names
x.Author <- sapply(strsplit(x.Author, " "), `[`, 1) # extract first names

df.Author <- as.data.frame(x.Author)


x.Gender <- gender(x.Author, method="ssa") # match gender
xg.1<-ggplot(x.Gender, aes(x=x.Gender$gender)) + 
  geom_histogram(stat="count") +
  scale_y_continuous(limits=c(0,1000)) +
  xlab("gender") +
  theme(legend.position = "none")
x.Gender1 <- gender(x.Author, method="ipums") # match gender
xg.2<-ggplot(x.Gender1, aes(x=x.Gender1$gender)) + 
  geom_histogram(stat="count") +
  scale_y_continuous(limits=c(0,1000)) +
  xlab("gender") +
  theme(legend.position = "none")
x.Gender2 <- gender(x.Author, method="napp") # match gender
xg.3<-ggplot(x.Gender2, aes(x=x.Gender2$gender)) + 
  geom_histogram(stat="count") +
  scale_y_continuous(limits=c(0,1000)) +
  xlab("gender") +
  theme(legend.position = "none")
x.Gender3 <- gender(x.Author, method="kantrowitz") # match gender
xg.4<-ggplot(x.Gender3, aes(x=x.Gender3$gender)) + 
  geom_histogram(stat="count") +
  scale_y_continuous(limits=c(0,1000)) +
  xlab("gender") +
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(xg.1,xg.2,xg.3,xg.4)


#how long was on best seller

# each occurence per row = 1 week on NYT BS

x.count <- x %>% count(Title) # count each title occurence
x.count <- x.count[order(-x.count$n),] # reorder list
x.count.50 <- x.count[1:50,] # top 50 longest on list
# x.count.1 <- x.count[x.count$n ==1,] # single weekers

ggplot(x.count.50, aes(x=reorder(Title, n), y=n))+geom_bar(stat="identity")+coord_flip()
# ggplot(x.count.1, aes(x=reorder(Title, n), y=n))+geom_bar(stat="identity")+coord_flip()
       
#try a wordcloud of titles
library(wordcloud)
library(tm)
wordcloud(x$Title)# needs words filtered


