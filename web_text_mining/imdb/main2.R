# Submit the form on indeed.com for a job description and location using html_form() and set_values()
query = "born to kill"
session <- html_session("http://www.imdb.com")
form <- html_form(session)[[1]]
form <- set_values(form, q = query, s="tt")


submit_form2 <- function(session, form){
  library(XML)
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

session1 <- submit_form2(session, form)





tdist <- html_node(session1, "table.findList") %>% html_table(header = FALSE)

