library(data.table)   # Required for rbindlist
library(dplyr)        # Required to use the pipes %>% and some table manipulation commands
library(magrittr)     # Required to use the pipes %>%
library(rvest)        # Required for read_html
library(RSelenium)    # Required for webscraping with javascript

url <- "https://www.goodreads.com/book/show/18619684-the-time-traveler-s-wife#other_reviews"
book.title <- "The time traveler's wife"
output.filename <- "GR_TimeTravelersWife.csv"

startServer()
remDr <- remoteDriver(browserName = "firefox", port = 4444) # instantiate remote driver to connect to Selenium Server
remDr$open() # open web browser
remDr$navigate(url)

global.df <- data.frame(book = character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)

# Main loop going through the website pages
for(t in 1:98){
  
  #Extracting the reviews from the page
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  reviews.html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
  reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
  reviews.text <- unlist(reviews.list)
  
  # Cleaning the reviews with Regex
  reviews.text2 <- gsub("[^A-Za-z\\-]|\\.+"," ",reviews.text) # Removing all characters that are not letters, dash or periods
  reviews.clean <- gsub("\n|[ \t]+"," ",reviews.text2)  # Removing the end of line characters and extra spaces
  
  n <- floor(length(reviews)/2)
  reviews.df <- data.frame(book = character(n), 
                           reviewer = character(n), 
                           rating = character(n), 
                           review = character(n), 
                           stringsAsFactors = F)
  
  # Populating a data frame with the relevant fields
  for(j in 1:n){
    reviews.df$book[j] <- book.title
    
    #Isolating the name of the author of the review
    auth.rat.sep <- regexpr(" rated it | marked it | added it ", reviews.clean[2*j-1])
    reviews.df$reviewer[j] <- substr(reviews.clean[2*j-1], 5, auth.rat.sep-1)
    
    #Isolating the rating
    rat.end <- regexpr("· | Shelves| Recommend| review of another edition", reviews.clean[2*j-1])
    if (rat.end==-1){rat.end=nchar(reviews.clean[2*j-1])}
    reviews.df$rating[j] <- substr(reviews.clean[2*j-1], auth.rat.sep+10, rat.end-1)
    
    #Removing the beginning of each review that was repeated on the html file
    short.str <- substr(reviews.clean[2*j], 1, 50)
    rev.start <- unlist(gregexpr(short.str, reviews.clean[2*j]))[2]
    if (is.na(rev.start)){rev.start <- 1}
    rev.end <- regexpr("\\.+more|Blog", reviews.clean[2*j])
    if (rev.end==-1){rev.end <- nchar(reviews.clean[2*j])}
    reviews.df$review[j] <- substr(reviews.clean[2*j], rev.start, rev.end-1)
  }
  
  global.lst <- list(global.df, reviews.df)
  global.df <- rbindlist(global.lst)
  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  NextPageButton$clickElement()
  Sys.sleep(3)
}   
#end of the main loop

write.csv(global.df, output.filename)