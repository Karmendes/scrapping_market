


# get page

get_order_prix <- function(Item){
  # remove black space
  Item <- Item %>% str_replace_all(fixed(" "), "%20")
  # Pegar produtos e preços disponíveis
  html <- GET(paste("https://www.superprix.com.br/",Item,sep = "")) %>%
    read_html()
  html
}

get_products_prix <- function(html){
  # Pegando produtos
  produtos <- html %>% 
    xml_find_all('//h3') %>%
    xml_text()
  # Pegando apenas os que coincidem com o pedido
  produtos <- produtos %>% str_subset("\n") %>%
    str_squish()
  produtos
}

get_prices_prix <- function(html){
  # Pegando preços
  price <- html %>% 
    xml_find_all('//*[@class = "price"]') %>%
    xml_text()
  # Pegando apenas os que coincidem com o pedido
  price <- price %>% str_squish() %>% str_remove_all("Por: ")
  price
}

table_prix <- function(produtos,price){
  # Tabelando
  tibble(
    Produto = produtos,
    Price = price
  )
}

get_table_prix <- function(Item){
  # GET
  html <- get_order_prix(Item)
  # get products
  produtos <- get_products_prix(html)
  # get prices
  price <- get_prices_prix(html)
  # take off products 
  x <- length(produtos) - length(price)
  if(x >0){
    produtos <- produtos[1:(length(produtos)-x)]
  }
  # table
  table_prix(produtos,price)
}
