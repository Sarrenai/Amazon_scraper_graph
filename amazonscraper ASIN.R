library(rvest)
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(ggiraph)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(data.table)
library(pacman)
library(patchwork)
library(jpeg)
library(httr)
library(stringr)
library(magrittr)
library(devtools)
library(htmltools)
library(ggfittext)

pacman::p_load(RCurl, XML, dplyr, rvest)

scrape_amazon <- function(ASIN, page_num){
  url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews)
  #page title
  doc %>%
    html_elements("[class='a-size-large a-text-ellipsis']") %>%
    html_text2() -> page_title
  
  
  #review title
  
doc %>%
  html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
  html_text() -> review_title

#review text
doc %>%
  html_nodes("[class='a-size-base review-text review-text-content']") %>%
  html_text2() -> review_text

#number of stars in review
doc %>%
  html_nodes("[data-hook='review-star-rating']") %>%
  html_text() -> review_star 

#date
date <- doc %>%
  html_nodes("#cm_cr-review_list .review-date") %>%
  html_text() %>%
  gsub(".*on ", "", .) %>%
  gsub(',', '', .) %>%
  mdy() 
  

#return a tibble
tibble(page_title,
       review_title,
       review_text,
       review_star,
       date,
       page = page_num) %>% return()

                  
                  
}


ASIN <- "B07C24V3SD" # Specify ASIN 

#Title
scrapeTitle <- function(ASIN){
  url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=1")
  
  doc <- read_html(url_reviews)
  
  page_title <- doc %>%
    html_elements("[class='a-size-large a-text-ellipsis']") %>%
    html_text2() -> page_title
  
}
scrapeTitle(ASIN = ASIN) -> output_list

page_range <- 1:20 # Change as required

# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!
match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))

lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(1) # Take a one second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(1) # Take an additional one second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> output_list
#List1 <- dplyr::bind_rows(output_list)
List1 <- Map(c, output_list) 
RbList1 <- rbindlist(List1)
DT1 <- as.data.frame(RbList1) %>%
  arrange(date) %>%
  distinct(review_text, .keep_all = TRUE)

DT1$review_star <- gsub(" .*$", "", DT1$review_star)


#labels
label_data <- DT1
label_data$id <- as.numeric(row.names(DT1))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
label_data$date<-as.factor(label_data$date)
label_data$review_star<- as.numeric(label_data$review_star)
label_data$review_title<- as.character(label_data$review_title)
label_data$review_text<- gsub("'", '', label_data$review_text)

view(label_data)

DT1$onclick <- paste0("alert(\"", label_data$review_text, "\")")


#circle graph


p <- ggplot(DT1, aes(x=as.numeric(row.names(DT1)), y= as.numeric(review_star), fill = as.numeric(review_star), tooltip = as.character(review_title), data_id = as.numeric(row.names(DT1)))) +
  geom_bar_interactive(stat="identity") +
  aes(onclick = onclick)+
  scale_fill_gradient(low="black", high="blue")+
  ylim(-3, 6) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    aspect.ratio = 1.1,
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(as.numeric(row.names(DT1)), y=review_star+0.5, label=date, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=1.6, angle= label_data$angle, inherit.aes = FALSE) + 
  
  #annotate('text', x=0, y=-3, label = label_data$page_title, size = 3, geom_fit_text(reflow = TRUE)) 
 #wraps central text
   annotate('text', x=0, y=-3, label = stringr::str_wrap(label_data$page_title, width = 15))


tooltip_css <- "background-color:transparent; font-style:italic; font-weight:bold; color:red;"
p <- p + guides(fill=guide_legend(title="Rating"))
p1 <- girafe(ggobj = p, 
             options = list(
               #opts_selection(only_shiny = FALSE),
               opts_hover(css="fill:thistle;"),
               opts_tooltip(css = tooltip_css)
             )
) 
p1


