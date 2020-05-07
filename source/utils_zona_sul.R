



get_order <- function(Item){
  # remove black space
  Item <- Item %>% str_replace_all(fixed(" "), "")
  # Pegar produtos e preços disponíveis
  html <- GET(paste("https://www.zonasul.com.br/busca/",Item,sep = "")) %>%
    read_html()
  html
}

get_products <- function(html){
  # Pegando produto
  produto <- xml_find_all(html,'//*[@class = "informativo"]') %>%
    xml_children() %>% xml_text() 
  # slicing vector
  produto <- produto[seq(2,length(produto),by = 3)]
  # Removing r/n
  produto <- produto %>%
    str_remove_all("\r\n")
  # Remover espaço em branco
  produto[produto != ""]
  produto
}

get_prices <- function(html){
  # Pegando preços
  price <- xml_find_all(html,
                        '//*[@data-price]') %>%
    xml_text() %>% str_squish()
  price
}

get_discounts <- function(html){
  # Possiveis descontos
  desconto <- xml_find_all(html,
                           '//*[@data-desconto]')
  # asserthat
  if(length(desconto) > 0){
  # Preço
  price_desc <- desconto %>% 
    xml_text() %>% str_squish()
  # Produtos
  name_descs <- desconto %>% xml_parent() %>%
    xml_parent() %>% xml_siblings() %>%
    xml_children() %>% xml_text()
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
  df_products <- tibble(
    Produto = produto,
    Price = price
  )
  if(!is.null(list_discount)){
    # tabela de descontos
    df_desc <- tibble(
      Produto = list_discount$name_descs,
      Price_desc = list_discount$price_desc
    )
    # join dfs
    df_agg <- df_products %>%
      left_join(df_desc,by = "Produto") %>%
      mutate(Price_desc = ifelse(is.na(Price_desc),"Sem",Price_desc))
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