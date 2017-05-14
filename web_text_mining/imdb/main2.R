library(XML)
library(rvest)

query = "born to kill"

##submit form (GET)
submit_form2 <- function(session, form){
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q <- paste(q, collapse = '&')
  q <- gsub(" ", "+", q)
  url <- paste(url, q, sep = '')
  html_session(url)
}

getMoviesByTitle <-function(query){
  if(query!="") {
    print(query)
    session <- html_session("http://www.imdb.com")
    form <- html_form(session)[[1]]
    form <- set_values(form, q = query, s="tt")
    session1 <- submit_form2(session, form)
    tdist <- html_node(session1, "table.findList") %>% html_table(header = FALSE)
    tdist
  }
}
getMoviesIds <- function(query){
  if(query!="") {
    print(query)
    session <- html_session("http://www.imdb.com")
    form <- html_form(session)[[1]]
    form <- set_values(form, q = query, s="tt")
    session1 <- submit_form2(session, form)
    mov <- html_nodes(session1, ".result_text > a")  
    ids <- html_attr(mov,"href")
    tmp <- regexpr("(?:\\/\\w*\\/)(\\w*)", ids)
    ids <- regmatches(ids, tmp)
    ids <- gsub("/title/", "",ids)
    ids
  }
}

