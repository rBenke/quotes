# TODO: 
#-- Latent Dirichlet allocation to distinguish which category has
#   particular quote: film, movie, person 
p_load("rvest", "magrittr", "stringi")

# inspiration for this app:
# https://github.com/lmu-applied-r/2019-CYOSS/blob/master/presentations/appliedR_talk2.pdf
# xPath tutorial:
# https://www.w3schools.com/xml/xpath_syntax.asp

# 
extend_database <- function(beginnig_link = "https://pl.wikiquote.org/w/index.php?title=Specjalna:Wszystkie_strony", db_connection = NULL, n_page = 1){
 #TODO: beginning link should be set to last title in database (firstly sorted by title)
  
  # select only first link from every page - do not generate unnecessery movement
  selected_links_vec <- TRUE # set logical TRUE to get all
  #prepare containers
  if(is.null(db_connection)){
    all_quotes_lst <- list()
    scraped_links <- list()
    next_page <- ""
  }
  # Scrap first page
  scraped_links_lst <- scrap_links(link = beginnig_link)
  quotes_lst <- scrap_qoutes(links_vec = scraped_links_lst[[2]][selected_links_vec])
  next_page <- scraped_links_lst[[1]]
  # Save links and quotes from first page
  if(is.null(db_connection)){ # database not available
  all_quotes_lst <- append(all_quotes_lst, quotes_lst)
  scraped_links[[1]] <- scraped_links_lst[[2]][selected_links_vec]
  } else { # save to DB
    links_selected <- scraped_links_lst[[2]][selected_links_vec]
    quotes_selected <- quotes_lst[selected_links_vec]
    
    for (i in 1:length(links_selected)){
      # insert into links
      author_processed <- db_insert_processing(quotes_selected[[i]]$author)
      insert_links_query <- "INSERT INTO `links` (`id`, `link`, `title`) VALUES( NULL," 
      insert_links_query <- paste0(insert_links_query, "\"", links_selected[i], "\",")
      insert_links_query <- paste0(insert_links_query, "\"", author_processed, "\"")
      insert_links_query <- paste0(insert_links_query, ");")
      
      dbSendQuery(db_connection, insert_links_query)
      tit_descri_processed <- db_insert_processing(quotes_selected[[i]]$title_description)
      for (j in 1:length(quotes_selected[[i]]$quotes)){
        # insert into quotes
        insert_quotes_query <- "INSERT INTO `quotes` (`id`,`quotes`,`nchar`,`title`,`description`) VALUES( NULL," 
        insert_quotes_query <- paste0(insert_quotes_query, "\"", db_insert_processing(quotes_selected[[i]]$quotes[j]), "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", nchar(quotes_selected[[i]]$quotes[j]), "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", author_processed, "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", tit_descri_processed, "\"")
        insert_quotes_query <- paste0(insert_quotes_query, ");")
        
        dbSendQuery(db_connection, insert_quotes_query)
      }
    }
  }
  #------------------------------------------------------
  # Scrap next pages in loop
  for(iter in 1:n_page){
    scraped_links_lst <- scrap_links(link = next_page)
    quotes_lst <- scrap_qoutes(links_vec = scraped_links_lst[[2]][selected_links_vec])
    next_page <- scraped_links_lst[[1]]
    
    if(is.null(db_connection)){
    all_quotes_lst <- append(all_quotes_lst, quotes_lst)
    scraped_links <- append(scraped_links, list(scraped_links_lst[[2]][selected_links_vec]))
    } else{ # save to DB
      links_selected <- scraped_links_lst[[2]][selected_links_vec]
      quotes_selected <- quotes_lst[selected_links_vec]
      
      for (i in 1:length(links_selected)){
        author_processed <- db_insert_processing(quotes_selected[[i]]$author)
        # insert into links
        insert_links_query <- "INSERT INTO `links` (`id`, `link`, `title`) VALUES( NULL," 
        insert_links_query <- paste0(insert_links_query, "\"", links_selected[i], "\",")
        insert_links_query <- paste0(insert_links_query, "\"", author_processed, "\"")
        insert_links_query <- paste0(insert_links_query, ");")
        
        dbSendQuery(db_connection, insert_links_query)
        tit_descri_processed <- db_insert_processing(quotes_selected[[i]]$title_description)
        for (j in 1:length(quotes_selected[[i]]$quotes)){
          # insert into quotes
          insert_quotes_query <- "INSERT INTO `quotes` (`id`,`quotes`,`nchar`,`title`,`description`) VALUES( NULL," 
        insert_quotes_query <- paste0(insert_quotes_query, "\"", db_insert_processing(quotes_selected[[i]]$quotes), "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", nchar(quotes_selected[[i]]$quotes[j]), "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", author_processed, "\",")
        insert_quotes_query <- paste0(insert_quotes_query, "\"", tit_descri_processed, "\"")
        insert_quotes_query <- paste0(insert_quotes_query, ");")
        
        dbSendQuery(db_connection, insert_quotes_query)
      }
      }
    }
  }
  
  if(is.null(db_connection)){
  list("output" = all_quotes_lst, "scraped_links" = scraped_links, "next_page" = next_page)
  } else {
    TRUE
  }
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

db_insert_processing <- function(sentence){
  sentence <- stri_replace_all(sentence, fixed = "\"", replacement = "'")
  sentence
}