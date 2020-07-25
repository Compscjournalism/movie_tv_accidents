library(XML)
library(RSelenium)
library(RCurl)
library(tidyverse)

#to get all the movies stored with a linked URL for further data on film-specific level
central_url_list <- "https://en.wikipedia.org/wiki/List_of_film_and_television_accidents"
page <- getURL(central_url_list)
tpage <- htmlParse(page)
entries <- xpathSApply(tpage, "/html/body//li")
film_database <- data.frame()
for(index in seq(1, length(entries))){
#return href of film and film title
general_info_df <- xpathSApply(entries[[index]], "./i/a", xmlAttrs)
if(typeof(general_info_df) == "character"){
href <- as.character(general_info_df[1,1])
title <- as.character(general_info_df[2,1])
wiki_list_description <- xpathSApply(entries[[index]], ".", xmlValue)
film_database <- dplyr::bind_rows(film_database, as.data.frame(t(c(href, title, wiki_list_description))))
print(title)
}}


film_level_data <- data.frame()
for(index in seq(1, nrow(film_database))){
#get film specific data for the stored movies

get_film_url <- paste0("https://en.wikipedia.org", film_database$V1[index])
page <- try(getURL(get_film_url))
tpage <- htmlParse(page)
info_box_x <- as.vector(xpathSApply(tpage, "//table[contains(@class, 'infobox vevent')]//tr/th", xmlValue))
info_box_y <- as.vector(xpathSApply(tpage, "//table[contains(@class, 'infobox vevent')]//tr/td", xmlValue))
info_box <- as.data.frame(cbind(info_box_x, info_box_y))
info_box <- info_box %>% 
  mutate(film_url = film_database$V1[index],
         film_name = film_database$V2[index])


film_level_data <- dplyr::bind_rows(film_level_data, info_box)
}

#dataset cleaning TBD


