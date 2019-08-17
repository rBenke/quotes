# TODO: 
#-- Database to save all the informations
#-- Latent Dirichlet allocation to distinguish which category has
#   particular quote: film, movie, person 

library(rvest)
library(tidyverse)
# https://github.com/lmu-applied-r/2019-CYOSS/blob/master/presentations/appliedR_talk2.pdf
# https://www.w3schools.com/xml/xpath_syntax.asp

# The beginning link shouls be made using last title in database (firstly sorted by title)
# beginnig_link = "https://pl.wikiquote.org/w/index.php?title=Specjalna:Wszystkie_strony"
extend_database <- function(beginnig_link = "https://pl.wikiquote.org/w/index.php?title=Specjalna:Wszystkie_strony"){
  #prepare containers
  all_quotes_lst <- list()
  scraped_links <- list()
  next_page <- ""
  
  scraped_links_lst <- scrap_links(link = beginnig_link)
  quotes_lst <- scrap_qoutes(links_vec = scraped_links_lst[[2]][1])
  all_quotes_lst <- append(all_quotes_lst, quotes_lst)
  next_page <- scraped_links_lst[[1]]
  for(i in 1:3){
    scraped_links_lst <- scrap_links(link = next_page)
    quotes_lst <- scrap_qoutes(links_vec = scraped_links_lst[[2]][1])
    all_quotes_lst <- append(all_quotes_lst, quotes_lst)
    next_page <- scraped_links_lst[[1]]
  }
  list("output" = all_quotes_lst, "scraped_links" = scraped_links, "next_page" = next_page)
}

scrap_links <- function(link){
  raw_page_html <- read_html(link)
  
  next_page <- html_nodes(raw_page_html, xpath = "//*[@id=\"mw-content-text\"]/div[2]/a") %>% 
    html_attr("href")
  # if page has "previous page" and "next page" links
  if(length(next_page)==2) next_page <- next_page[2]
  
  next_page <- paste0("https://pl.wikiquote.org", next_page)
  
  links2qoutes <- html_nodes(raw_page_html, xpath = "//*[@id=\"mw-content-text\"]/div[3]/ul/li/a") %>%
    html_attr("href") 
  links2qoutes <- paste0("https://pl.wikiquote.org", links2qoutes) 
  list("next" = next_page, "links" = links2qoutes)
}

scrap_qoutes <- function(links_vec){
  output <- list()
  for(i in 1:length(links_vec)){
    link <- links_vec[i]
    raw_page_html <- read_html(link)
    
    page_quotes_vec <- html_nodes(raw_page_html, xpath = "//*[@id=\"mw-content-text\"]/div/ul/li/text()") %>% html_text(trim = TRUE) 
    #clean sentence which are not quotes
    #TODO: try to write better xPath
    zobacz_tez_ind <- which(substr(page_quotes_vec,-1,10) == "Zobacz też")
    zrodlo_ind <- which(substr(page_quotes_vec,-1,6) == "Źródło")
    if(length(not_qoutes <- c(zobacz_tez_ind, zrodlo_ind))>0){
      page_quotes_vec <- page_quotes_vec[-not_qoutes]
    }
    
    # the author or title (movie/series/book)
    page_heading_str <- html_nodes(raw_page_html, xpath = "//*[@id=\"firstHeading\"]") %>% html_text()
    
    # the description of author or title (movie/series/book)
    title_description_str <- html_nodes(raw_page_html, xpath = "//*[@id=\"mw-content-text\"]/div/p") %>% html_text(trim = TRUE) 
    
    output <- append(output,list(list("author" = page_heading_str, "quotes" = page_quotes_vec, "title_description" = title_description_str)))
  }
  output
}
