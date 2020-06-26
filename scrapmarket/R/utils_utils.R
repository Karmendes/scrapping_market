




get_order <- function(Item){
  # remove black space
  Item <- Item %>% stringr::str_replace_all(stringr::fixed(" "), "")
  # Pegar produtos e preços disponíveis
  html <- httr::GET(paste("https://www.zonasul.com.br/busca/",Item,sep = "")) %>%
    xml2::read_html()
  html
}

get_products <- function(html){
  # Pegando produto
  produto <- xml2::xml_find_all(html,'//*[@class = "informativo"]') %>%
    xml2::xml_children() %>% xml2::xml_text() 
  # slicing vector
  produto <- produto[seq(2,length(produto),by = 3)]
  # Removing r/n
  produto <- produto %>%
    stringr::str_remove_all("\r\n")
  # Remover espaço em branco
  produto[produto != ""]
  produto
}

get_prices <- function(html){
  # Pegando preços
  price <- xml2::xml_find_all(html,
                              '//*[@data-price]') %>%
    xml2::xml_text() %>% stringr::str_squish()
  price
}

get_discounts <- function(html){
  # Possiveis descontos
  desconto <- xml2::xml_find_all(html,
                                 '//*[@data-desconto]')
  # asserthat
  if(length(desconto) > 0){
    # Preço
    price_desc <- desconto %>% 
      xml2::xml_text() %>% stringr::str_squish()
    # Produtos
    name_descs <- desconto %>% xml2::xml_parent() %>%
      xml2::xml_parent() %>% xml2::xml_siblings() %>%
      xml2::xml_children() %>% xml2::xml_text()
    # slicing vector
    name_descs <- name_descs[seq(2,length(name_descs),by = 3)]
    # list
    list(name_descs = name_descs,
         price_desc = price_desc)
  }else{
    NULL
  }
}


table_zona_sul <- function(produto,price,list_discount){
  # Tabela prod and price
  df_products <- tibble::tibble(
    Produto = produto,
    Price = price
  )
  if(!is.null(list_discount)){
    # tabela de descontos
    df_desc <- tibble::tibble(
      Produto = list_discount$name_descs,
      Price_desc = list_discount$price_desc
    )
    # join dfs
    df_agg <- df_products %>%
      dplyr::left_join(df_desc,by = "Produto") %>%
      dplyr::mutate(Price_desc = ifelse(is.na(Price_desc),"Sem",Price_desc))
    df_agg
  }else{
    df_products
  }
  
}



get_table_zs <- function(Item){
  # GET
  html <- get_order(Item)
  # products
  produto <- get_products(html)
  # price
  price <- get_prices(html)
  # list disconts
  list_discount <- get_discounts(html)
  # table
  table_zona_sul(produto,price,list_discount)
}




# get order
get_order_mundial <- function(Item){
  # Item
  Item <- "Colorado"
  # change blank space
  Item <- Item %>% stringr::str_replace_all(stringr::fixed(" "),"+")
  # GET
  html <- httr::GET(
    paste("https://www.supermercadosmundial.com.br/busca?q=",Item,sep = "")) %>%
    xml2::read_html()
  html
} 

# Check if the search is null
check_search <- function(html){
  html %>% xml2::xml_find_all('//*[@class = "col-md-12"]') %>%
    xml2::xml_text() %>%
    stringr::str_remove_all("\n") %>% stringr::str_squish() %>%
    stringr::str_detect("Encontramos") %>% purrr::detect(isTRUE)
}

# generate table
get_table_product_and_price_mundial <- function(html){
  # Products
  Produto <- html %>%
    xml2::xml_find_all('//*[@class = "name-product"]') %>%
    xml2::xml_text() 
  # Price
  price <- html %>%
    xml2::xml_find_all('//*[@class = "price-product"]') %>%
    xml2::xml_text() %>% stringr::str_remove_all("\n") %>% stringr::str_squish()
  
  tibble::tibble(Produtos = Produto,
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
    tibble::tibble(Produtos = "",
                   Price = "price")
  }
}


utils::globalVariables("Price_desc")


