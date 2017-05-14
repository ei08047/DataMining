# TODO's
# get number of reviews (done)
# get all reviews and its score (score missing)
# save as data frame and write to file (done)

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
  numReviews <- (movie %>% html_nodes(".titleReviewbarItemBorder a:nth-child(1)" ) %>% html_text() )[[1]]
  numReviews <- strsplit(numReviews, " user")[[1]] #%>%  as.numeric()
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


##prepare url
prepareReviewUrl <- function(page){
  url_review_page <- gsub(" " , "",(paste("reviews?start=",page))[1])
  url_review_page
}
##navigate reviews
navReviews <- function(session,url_review_page){
  review_page <- jump_to(session,url_review_page)
  review_page
}
##scrap reviews
scrapReviews <- function(review_page){
  reviews <- review_page %>%  html_nodes("#tn15content div+ p") %>% html_text()
  reviews
} 
##get all
getAllreviews <- function(id){
  
  movie <- movieById(id)
  url <- getMovieUrl(id)
  session <- html_session(url)
  page <- 0
  count<- 0
  myList <- list()
  filename <- paste("C:/Users/ei08047/Desktop/DataMining/web_text_mining/reviews/" ,id,".txt")
  while(count != numReviews)
  {
    url_review_page <- prepareReviewUrl(page)
    url_review_page
    review_page <- navReviews(session,url_review_page)
    review_page$url
    reviews <- scrapReviews(review_page)
    count <- count + length(reviews)
    page <- page + 10
    print(reviews)
    myList[[length(myList)+1]] <- reviews
  }
  write.table(myList, filename, sep="\t")
  
}


id <- 'tt1490017'
movie <- movieById(id)

rating <- getRating(movie)
numReviews <- getNumReviews(movie)
poster <- getPoster(movie)
cast <- getCast(movie)
review <- getReview(movie)




