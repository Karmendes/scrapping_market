
library(httr)
library(xml2)
library(tidyverse)


# get order
get_order_mundial <- function(Item){
  # Item
  Item <- "Colorado"
  # change blank space
  Item <- Item %>% str_replace_all(fixed(" "),"+")
  # GET
  html <- GET(
    paste("https://www.supermercadosmundial.com.br/busca?q=",Item,sep = "")) %>%
    read_html()
  html
} 

# Check if the search is null
check_search <- function(html){
  html %>% xml_find_all('//*[@class = "col-md-12"]') %>%
    xml_text() %>%
    str_remove_all("\n") %>% str_squish() %>%
    str_detect("Encontramos") %>% detect(isTRUE)
}

# generate table
get_table_product_and_price_mundial <- function(html){
  # Products
  Produto <- html %>%
    xml_find_all('//*[@class = "name-product"]') %>%
    xml_text() 
  # Price
  price <- html %>%
    xml_find_all('//*[@class = "price-product"]') %>%
    xml_text() %>% str_remove_all("\n") %>% str_squish()
  
  tibble(Produtos = Produto,
         Price = price)
}

# get table mundial
get_table_mundial <- function(Item){
  # get order
  html <- get_order_mundial(Item)
  # check if the order exists
  check <- check_search(html)
  # create table
  if(check){
    get_table_product_and_price_mundial(html)
  }else{
    tibble(Produtos = "",
           Price = "price")
  }
}




