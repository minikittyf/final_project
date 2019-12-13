#' Search Article Based on Topic.
#'
#' Get the top 10 articles' information based on topic and searchdate and return article's category, headline, abstract, lead paragraph, url, published year, published month, published day, total words of article in a dataframe.
#' @param topic A Character.
#' @param searchdate A numeric number in date formate: yearmonthdate.
#' @return A dataframe inclde article's category, headline, abstract, lead paragraph, url, published year, published month, published day, total words of article based on \code{topic} and \code{searchdate}.
#' @export
#' @examples
#' search_article('new york', 20191231)
#'


search_article <- function(topic='new york', searchdate=20191231){
  library(httr)
  library(stringr)
  library(tidyr)
  library(dplyr)

  if(!is.character(topic)){
    stop("input must be a character")
  }
  if(!is.numeric(searchdate)){
    stop("input must be a numeric")
  }

  abstract <- list()
  web_url <- list()
  lead_paragraph <- list()
  headline <- list()
  public_date <- list()
  author <- list()
  words <- list()
  category <- list()
  search_url <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"
  query_paramater <- list(q=topic,end_date=searchdate,'api-key'=Sys.getenv("NYT_KEY"))
  search_r <- GET(search_url, query=query_paramater)
  c <- content(search_r)
  for(i in 1:10){
    abstract[[i]] <- c$response$docs[[i]]$abstract
    web_url[[i]] <- c$response$docs[[i]]$web_url
    lead_paragraph[[i]] <- c$response$docs[[i]]$lead_paragraph
    headline[[i]] <- c$response$docs[[i]]$headline$main
    public_date[[i]] <- c$response$docs[[i]]$pub_date
    words[[i]] <- c$response$docs[[i]]$word_count
    category[[i]] <- c$response$docs[[i]]$news_desk
    category[sapply(category, function(x){length(x)==0})] <- NA
  }

  output <- data.frame('category' = unlist(category), 'headline'=unlist(headline), 'abstract' = unlist(abstract), 'lead_paragraph' = unlist(lead_paragraph), 'url' = unlist(web_url), 'date' = unlist(public_date), 'words' = unlist(words))

  public_date <- str_extract(output[,6], "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") #clean the date in the dataset
  output$date <- public_date

  new_output <- output %>%
    group_by(category) %>%
    separate(date, c('year', 'month', 'day'), sep = '-') %>%
    arrange(words)

  return(new_output)}


#' Search Most Popular Articles.
#'
#' Get the most popular articles in specifc type and recent 1 day, 7 day or 30 day and put the information of most popular article's url, category, author, title, abstract, published date in a dataframe.
#' @param type A Character: choose from emailed, shared, viewed.
#' @param period A Numeric number: choose from 1,7,30.
#' @return A dataframe inclde article's category, title, abstract, url, author, published date based on \code{topic} and \code{searchdate}.
#' @export
#' @examples
#' mostpopular('emailed', 1)
#'


mostpopular <- function(type='emailed', period=1){
  library(httr)
  library(stringr)
  library(tidyr)
  library(jsonlite)
  library(dplyr)

  if(!is.character(type)){
    stop("input must be a character")
  }
  if(!is.numeric(period)){
    stop("input must be a numeric")
  }

  search_url <- 'https://api.nytimes.com/svc/mostpopular/v2/'
  base_url <- paste(search_url, type, '/', period, '.', 'json', sep = '')
  search_r <- GET(base_url, query = list('api-key'=Sys.getenv("NYT_KEY")))
  search_c <- content(search_r)
  search_nyt <- fromJSON(toJSON(search_c))
  url <- search_nyt$results$url
  section <- search_nyt$results$section
  author <- search_nyt$results$byline
  author[sapply(author, function(x){length(x)==0})] <- NA
  title <- search_nyt$results$title
  abstract <- search_nyt$results$abstract
  abstract[sapply(abstract, function(x){length(x)==0})] <- NA
  date <- search_nyt$results$published_date

  new_result <- data.frame('url' = unlist(url),'category' = unlist(section), 'author' = unlist(author),'title' = unlist(title), 'abstract' = unlist(abstract), 'date' = unlist(date))

  return(new_result)
  }


#' Get Comments.
#'
#' Get article's top 25 comments based on article url, and return a dataframe include user id, user name, user location, and user comments.
#' @param your_url A Character.
#' @return A dataframe include user id, user name, user location, and user comments based on \code{your_url}.
#' @export
#' @examples
#' comments('https://www.nytimes.com/2019/12/08/opinion/donald-trump-impeachment.html')

comments <- function(your_url){

  library(httr)
  library(jsonlite)

  if(!is.character(your_url)){
    stop("input must be a character")
  }

  base <- 'https://api.nytimes.com/svc/community/v3/user-content/url.json'
  query_list <- list('api-key' = Sys.getenv("NYT_KEY"), offset=0, url = your_url)
  com_r <- GET(base, query=query_list)
  com_c <- content(com_r)
  com_nyt <- fromJSON(toJSON(com_c))
  userid <- com_nyt$results$comments$userID
  username <- com_nyt$results$comments$userDisplayName
  userlocation <- com_nyt$results$comments$userLocation
  comment <- com_nyt$results$comments$commentBody
  rec <- com_nyt$results$comments$recommendations
  comment_data <- data.frame('userid' = unlist(userid), 'username' = unlist(username), 'location' = unlist(userlocation), 'comment' = unlist(comment), 'recommendation' = unlist(rec))
  return(comment_data)
}

#' Find most recommend comments.
#'
#' Get the most recommend comments based on article url
#' @param your_url A Character.
#' @return the most recommend user comments based on \code{your_url}.
#' @export
#' @examples
#' best_recommend('https://www.nytimes.com/2019/12/08/opinion/donald-trump-impeachment.html')


best_recommend <- function(your_url){
  library(tidyr)
  library(dplyr)

  if(!is.character(your_url)){
    stop("input must be a character")
  }

  rec_data <- comments(your_url)
  best <- rec_data %>%
    filter(recommendation == max(recommendation))
  best_rec <- best$comment
  return(best_rec)
}



#' Section Rank.
#'
#' Get recent ranked articles' information in specific section.
#' @param search_section A Character.(the character should choose from: Africa, Americas, ArtandDesign, Arts, AsiaPacific, Automobile, Baseball, Books, Business, Climate, CollegeBasketball, CollegeFootball, Dance, Dealbook, DiningandWine, Economy, Education, EnergyEnvironment, Europe, FashionandStyle, Golf, Health, Hockey, HomePage, Jobs, Lens, MediaandAdvertising, MiddleEast, MostEmailed, MostShared, MostViewed, Movies, Music, NYRegion, Obituaries, PersonalTech, Politics, ProBasketball, ProFootball, RealEstate, Science, SmallBusiness, Soccer, Space, Sports, SundayBookReview, Sunday-Review, Technology, Television, Tennis, Theater, TMagazine, Travel, Upshot, US, Weddings, Well, YourMoney).
#' @return A dataframe inclde article's title, link, description, published date, and their rank based on \code{search_section}.
#' @export
#' @examples
#' rss_rank('Arts')
#'

rss_rank <- function(search_section='Arts'){
  library(xml2)
  library(stringr)
  library(httr)
  # Get XML
  base <- 'https://api.nytimes.com/services/xml/rss/nyt/'
  specific_url <- paste(base,search_section,'.xml', sep='')
  resp_xml <- GET(specific_url)
  # Examine returned text with content()
  xml_text <- content(resp_xml, as = "text")
  # Turn rev_text into an XML document
  rss_xml <- read_xml(xml_text)
  title <- xml_find_all(rss_xml, xpath = '/rss/channel/item/title') %>%
    xml_text()
  link <- xml_find_all(rss_xml, xpath = '/rss/channel/item/link') %>%
    xml_text()
  description <- xml_find_all(rss_xml, xpath = '/rss/channel/item/description') %>%
    xml_text()
  public_date <- xml_find_all(rss_xml, xpath = '/rss/channel/item/pubDate') %>%
    xml_text()
  date <- str_extract(public_date, "[A-Z][a-z][a-z], [0-9][0-9] [A-Z][a-z][a-z] [0-9][0-9][0-9][0-9]") #clean the date in the dataset
  rank <- c(1: length(title))
  rss_data <- data.frame('title' = title, 'link' = link, 'description' = description, 'public_date' = date, 'rank' = rank)
  return(rss_data)
}





#' Search ALL Article.
#'
#' Get all articles in specific year and month and get the information: article's category, head, url, published date, total words of article in a dataframe.
#' @param year A series of Character.
#' @param month A numeric number in date formate: yearmonthdate.
#' @return A dataframe inclde article's category, head, url, published date, total words of article based on \code{year} and \code{month}.
#' @export
#' @examples
#' specific_articles(2019, 1)
#'

specific_articles <- function(year=2019, month=1){

  library(httr)
  library(jsonlite)
  library(stringr)
  library(plyr)

  if(!is.numeric(year)){
    stop("input must be a numeric")
  }

  if(!is.numeric(month)){
    stop("input must be a numeric")
  }


  search_url <- 'https://api.nytimes.com/svc/archive/v1/'
  new_url <- paste(search_url, year, '/', month, '.', 'json', sep = '')
  search_r <- GET(new_url, query = list('api-key'=Sys.getenv("NYT_KEY")))
  search_c <- content(search_r)
  search_nyt <- fromJSON(toJSON(search_c))
  head <- search_nyt$response$docs$headline$main
  head[sapply(head, function(x){length(x)==0})] <- NA
  abstract <- search_nyt$response$docs$abstract
  category <- search_nyt$response$docs$news_desk
  date <- search_nyt$response$docs$pub_date
  url <- search_nyt$response$docs$web_url
  words <- search_nyt$response$docs$word_count
  category[sapply(category, function(x){length(x)==0})] <- NA
  search_data <- data.frame('head' = unlist(head), 'category' = unlist(category), 'date' = unlist(date), 'url' = unlist(url), 'words'= unlist(words))
  public_date <- str_extract(search_data[,3], "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") #clean the date in the dataset
  search_data$date <- public_date
  print(length(category))
  return(search_data)
  }

