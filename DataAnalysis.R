library(knitr)
library(stringr)
library(plyr)
library(dplyr) # mutate_if


path.prefix <- "GourmetDB/"
path.suffix <- ".csv"

get_file_print_info <- function(path.name) {
  
  file <- read.csv(paste0(path.prefix, path.name, path.suffix), header = TRUE)
  print("-------- nrows ----------------")
  print(nrow(file))
  
  print("-------- str ------------------")
  print(str(file))
  
  print("-------- summary --------------")
  print(summary(file))
  
  print("-------- na ------------------")
  print(unlist(lapply(file, function(x) any(is.na(x)))))
  
  print("-------- colnames ------------")
  print(colnames(file))
  
  #Removing additional space
  colwise(str_trim)(producto) 
  
  return(file)
  
}

merge_dfs_left_outer <- function(df1, df2, by.clause) {
  df <- merge(df1, df2, by=by.clause, all.x = TRUE, path.suffix = FALSE) 
  
  # Cleaning the colnames
  colnames(df) <- str_replace_all(colnames(df), '.x', '')
  colnames(df) <- str_replace_all(colnames(df), '.y', '')
  
  # Removing duplicated columns
  df <- df[, !duplicated(colnames(df), fromLast = TRUE)] 
  
  print(colnames(df))
  return(df)
}

# Reading all the datasets available and printing info about them
#---------------------------------------------------------------------
ticket.cabecera <- get_file_print_info("cabeceraticket")
ticket.lineas <- get_file_print_info("lineasticket")
cliente <- get_file_print_info("cliente")
familia <- get_file_print_info("familia")
pais <- get_file_print_info("pais")
pedido <- get_file_print_info("pedido")
producto <- get_file_print_info("producto")
promocion <- get_file_print_info("promocion")
proveedor <- get_file_print_info("proveedor")
regiongeografica <- get_file_print_info("regiongeografica")
seccion <- get_file_print_info("seccion")
subfamilia <- get_file_print_info("subfamilia")
tienda <- get_file_print_info("tienda")


# To define association rules, my initial approach is: to merge ticket dataframes (cabecera + lineas) and afterwards merge products as well
# then to define a distribution table regarding some indicators such as product name to finally come up with the binarization / apriori / eclat, etc
# definetely by CODVENTA - CODCABECERA is a fraudster =P
ticket <- merge_dfs_left_outer(ticket.lineas, ticket.cabecera, c("CODVENTA")) 
ticket_with_product <- merge_dfs_left_outer(ticket, producto, c("CODPRODUCTO"))

# Checking if sold price > cost price (and if sold price = recommended sell price)
tp <- ticket_with_product
tp$VENDIDOPOR <- tp$IMPORTETOTAL / tp$TOTALUNIDADES
inconsistences <- ticket_with_product[ticket_with_product[, "IMPORTETOTAL" / "TOTALUNIDADES"] > "COSTE"]
data[data[, "Var1"]>10, ]
