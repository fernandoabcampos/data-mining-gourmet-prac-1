library(knitr)

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
  #kable(data.frame(variables=names(file),clase=as.vector(file)))
  return(file)
  
}


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
