library(rvest)

id <- 'tt1490017'
# Store web url
t <- paste("http://www.imdb.com/title/", id, sep = "")
movie <- html(t)
#Scrape the website for the movie rating
rating <- movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating
# Scrape the website for the cast
cast <- movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast
#Scrape the website for the url of the movie poster
poster <- movie %>%
  html_nodes("#img_primary img") %>%
  html_attr("src")
poster
# Extract the first review
review <- movie %>%
  html_nodes("#titleUserReviewsTeaser p") %>%
  html_text()
review