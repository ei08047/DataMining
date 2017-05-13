# TODO's
# get number of reviews (done)
# get all reviews and its score 
# save as data frame and write to file

library(rvest)
# Store web url
getMovieUrl <-  function(id){
  url <- paste("http://www.imdb.com/title/", id, sep = "")
  url
}
#get movie by id
movieById<- function(id){
  url <- getMovieUrl(id)
  url
  movie <- read_html(url)
  movie
}

getRating <- function(movie){
  #Scrape the website for the movie rating
  rating <- movie %>% 
    html_nodes("strong span") %>%
    html_text()  %>%  as.numeric()
  rating
}
getNumReviews <- function(movie){
  #Scrape the website for the numReviews
  numReviews <- (movie %>% html_nodes(".subText a:nth-child(1)") %>% html_text())[[2]]
  numReviews <- strsplit(numReviews, " user")[[1]] %>%  as.numeric()
  numReviews
}
# Scrape the website for the cast
getCast <- function(movie){
  cast <- movie %>%
    html_nodes("#titleCast .itemprop span") %>%
    html_text()
  cast
}
#Scrape the website for the url of the movie poster
getPoster<- function(movie){
  poster <- movie %>%
    html_nodes("#img_primary img") %>%
    html_attr("src")
  poster  
}
# Extract the first review
getReview <- function(movie){
  review <- movie %>%
    html_nodes("#titleUserReviewsTeaser p") %>%
    html_text()
  review
}
##navigate reviews
navReviews <- function(session){
  reviews_links <- jump_to(session,url = "reviews?start=")
  reviews_links
}



##navigate to user reviews page 1
##all_pages <- follow_link(all_reviews,css="td a:nth-child(1)")
##print(all_reviews$url , length(all_reviews) )
##all_reviews

id <- 'tt1490017'
movie <- movieById(id)
url <- getMovieUrl(id)
session <- html_session(url)
session$url

rating <- getRating(movie)
numReviews <- getNumReviews(movie)
cast <- getCast(movie)
review <- getReview(movie)

page <- 20
all_pages <- navReviews(session,page)
pageReviews <- scrapReviews(all_pages) 

nextPage <- navNextPage(session,all_pages)
session$url
##TODO back and save to file
##all_reviews <- navReviews(session)
 
 
 #TODO
 
all_reviews_html[1]
all_reviews_html[2]
all_reviews_html[3]
all_reviews_html[4]
all_reviews_html[5]
all_reviews_html[6]
all_reviews_html[7]
all_reviews_html[8]
all_reviews_html[9]
all_reviews_html[10]
##navigate to user reviews page 1
all_pages <- follow_link(all_reviews,css="td a:nth-child(1)")
all_pages_html <- html(all_pages) %>% html_nodes("#tn15content div+ p")
##navigate to user reviews page 2
all_pages <- follow_link(all_reviews,css="td a:nth-child(2)")
all_pages_html <- html(all_pages) %>% html_nodes("#tn15content div+ p")


all_pages <-  follow_link(movie_session,css=".quicklink:nth-child(5)")

start <- 0
count <- 0
while (count!= numReviews) {
  start <- start + 10
  splitted_url <- unlist(strsplit(all_pages$url,"?")[1])
  print(splitted_url)
  splitted_url <- gsub(" " , "",(paste(splitted_url,"=",start)[1]) )
  all_pages <- jump_to(all_pages ,splitted_url )
  
  print(all_pages$url)
  all_pages_html <- read_html(all_pages) %>% html_nodes("#tn15content div+ p")
  
  print(all_pages_html)
  count <- count + length(all_pages_html)
}
print(count)

##df <- data.frame(a = c(1:5), b = (1:5)^2)

##write.table(mydata, "c:/mydata.txt", sep="\t")


