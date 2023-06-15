library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr) 
#question a
data(iris)
boxplot(Sepal.Length ~ Species, data = iris, main = "Sepal Length by Species",
        xlab = "Species", ylab = "Sepal Length")

boxplot(Petal.Length ~ Species, data = iris, main = "Petal Length by Species",
        xlab = "Species", ylab = "Petal Length")
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species, 
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Scatterplot of Sepal Length vs. Petal Length")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 1)



#question b
library(imager)
flip <- function(image) {
  w<- dim(image)[2]
  h <- dim(image)[1]
  
  flipped <- imager(width = width, height = height)
  for (row in 1:h) {
    for (col in 1:w) {
      flipped[row, col] <- image[row, w - col + 1]
    }
  }
  
  return(flipped)
  #question c
  library(MASS)
  
  data(ships)
  
  accidents<- table(ships$type)
  

  barplot(accidents, 
          main = "Number of Accidents by Ship Type", 
          xlab = "Ship Type", 
          ylab = "Number of Accidents",
          col = "steelblue",
          ylim = c(0, max(accidents_count) + 5))
  

  text(x = barplot(accidents) - 0.25, 
       y = accidents_count + 2,
       labels = accidents,
       pos = 3)
}
#question d
title <- c()
titles <- c()
view <- c()
views <- c()
answer <- c()
answers <- c()
vote <- c()
votes <- c()
urls_codes <- 1:13854
for (i in 1:13854)
{
  urls_codes[i] <- paste("https://stats.stackexchange.com/questions?tab=newest&page=", urls_codes[i], sep = '')
}
for (i in 1:13854)
{
  html <- read_html(urls_codes[i])
  titles <- html %>% html_elements("#questions .s-link") %>% html_text()
  views <- html %>% html_elements(".s-post-summary--stats-item__emphasized~ .s-post-summary--stats-item+ .s-post-summary--stats-item .s-post-summary--stats-item-number") %>% html_text()
  answers <- html %>% html_elements(".s-post-summary--stats-item__emphasized+ .s-post-summary--stats-item .s-post-summary--stats-item-number") %>% html_text()
  votes <- html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text()
  title = append(title, titles)
  view = append(view, views)
  answer = append(answer, answers)
  vote = append(vote, votes)
}
Data_frame <- data.frame("title"=title,"views"=view,"answers"=answer,"votes"=vote)

#question e
pull_tablet <- function() {
  if (runif(1) < 0.5) {
    
    return(1)
  } else {
    
    return(0)
  }
}

calculateaverage_days <- function(iterations) {
  total_days <- 0
  
  for (i in 1:iterations) {
    num_days <- 0
    bottle <- rep(1, 100) 
    
    while (pull_tablet() == 0) {
      num_days <- num_days + 1
      bottle <- c(bottle, 0.5)
    }
    
    total_days <- total_days + num_days
  }
  
  average_days <- total_days / num_iterations
  return(average_days)
}


num_iterations <- 10000


average_days <- calculateaverage_days(num_iterations)
print(paste("Average number of days:", round(average_days, 2)))

