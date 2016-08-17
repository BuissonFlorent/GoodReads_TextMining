library(data.table)   # Required for rbindlist
library(dplyr)        # Required to use the pipes %>% and some table manipulation commands
library(magrittr)     # Required to use the pipes %>%
library(rvest)        # Required for read_html
library(RSelenium)    # Required for webscraping with javascript

url <- "https://www.goodreads.com/book/show/18619684-the-time-traveler-s-wife#other_reviews"
BookTitle <- "The time traveler's wife"
OutputFileName <- "GR_TimeTravelersWife.csv"

startServer()
remDr <- remoteDriver(browserName="firefox", port=4444) # instantiate remote driver to connect to Selenium Server
remDr$open() # open web browser
remDr$navigate(url)

global_df <- data.frame(book=character(),author=character(),rating=character(),review=character(), stringsAsFactors = F)

# Main loop going through the website pages
for(t in 1:98){
  
  #Extracting the reviews from the page
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  reviews_html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
  reviews_list <- lapply(reviews_html, function(x){read_html(x) %>% html_text()} )
  reviews_text <- unlist(reviews_list)
  
  # Cleaning the reviews with Regex
  reviews_text2<- gsub("[^A-Za-z\\-]|\\.+"," ",reviews_text) # Removing all characters that are not letters, dash or periods
  reviews_clean <- gsub("\n|[ \t]+"," ",reviews_text2)  # Removing the end of line characters and extra spaces
  
  n=floor(length(reviews)/2)
  reviews_df=data.frame(book=character(n),author=character(n),rating=character(n),review=character(n), stringsAsFactors = F)
  
  # Populating a data frame with the relevant fields
  for(j in 1:n){
    reviews_df$book[j]=BookTitle
    
    #Isolating the name of the author of the review
    auth_rat_sep=regexpr(" rated it | marked it | added it ",reviews_clean[2*j-1])
    reviews_df$author[j]=substr(reviews_clean[2*j-1],5,auth_rat_sep-1)
    
    #Isolating the rating
    rat_end=regexpr("· | Shelves| Recommend| review of another edition",reviews_clean[2*j-1])
    if (rat_end==-1){rat_end=nchar(reviews_clean[2*j-1])}
    reviews_df$rating[j]=substr(reviews_clean[2*j-1],auth_rat_sep+10,rat_end-1)
    
    #Removing the beginning of each review that was repeated on the html file
    short_str=substr(reviews_clean[2*j], 1, 50)
    rev_start=unlist(gregexpr(short_str, reviews_clean[2*j]))[2]
    if (is.na(rev_start)){rev_start=1}
    rev_end=regexpr("\\.+more|Blog",reviews_clean[2*j])
    if (rev_end==-1){rev_end=nchar(reviews_clean[2*j])}
    reviews_df$review[j]=substr(reviews_clean[2*j],rev_start,rev_end-1)
  }
  
  global_lst=list(global_df, reviews_df)
  global_df=rbindlist(global_lst)
  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  NextPageButton$clickElement()
  Sys.sleep(3)
}   
#end of the main loop

write.csv(global_df,OutputFileName)