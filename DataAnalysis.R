library(knitr)
library(stringr)
library(plotrix) # pie3d
library(arules)
library(arulesViz)


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
  #colwise(str_trim)(producto) 
  
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

# Trim to eliminate additional blank space before merging
ticket.cabecera$NOMBRETIENDA <- str_trim(ticket.cabecera$NOMBRETIENDA)
ticket.lineas$NOMBRETIENDA <- str_trim(ticket.lineas$NOMBRETIENDA)
ticket.lineas$CODPRODUCTO <- str_trim(ticket.lineas$CODPRODUCTO)
producto$DESCRIPCIÓN <- str_trim(producto$DESCRIPCIÓN)

# To define association rules, my initial approach is: to merge ticket dataframes (cabecera + lineas) and afterwards merge products as well
# then to define a distribution table regarding some indicators such as product name to finally come up with the binarization / apriori / eclat, etc
# definetely by CODVENTA - CODCABECERA is a fraudster =P
ticket <- merge_dfs_left_outer(ticket.lineas, ticket.cabecera, c("CODVENTA")) 
ticket_with_product <- merge_dfs_left_outer(ticket, producto, c("CODPRODUCTO"))

# Now, lets study the data / first reducing the name to be easier to work
tp <- ticket_with_product
head(tp)
nrow(tp)
summary(tp)
unlist(lapply(tp, function(x) any(is.na(x))))
res <- sapply(tp, class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

# Distribution by NOMBRETIENDA, NOMBREPAIS, FORMAPAGO, MARCA (obviously this is gonna show 6 [head] first elements)
dist_pais <- table(tp$NOMBREPAIS)
head(table(tp$NOMBRETIENDA))
head(dist_pais)
head(table(tp$FORMAPAGO))
head(table(tp$MARCA))

lbls <- paste(names(dist_pais),  ": " , dist_pais, sep= "" ) 
pie3D(dist_pais
      , radius= 0.9
      , labelcex=  0.7
      , labels = lbls
      , explode= 0.1
      , main= "Distribución por País" , col = rainbow(length(lbls)
      ))




# It makes sense to split by CODVENTA as CODCLIENTE is not properly filled - and the CODVENTA will give us an understanding of what was sold together
ppv <- split (x = tp[,  "CODPRODUCTO" ], f = tp$CODVENTA) 
ppv <- lapply (ppv, unique)
ppv <- as (ppv,  "transactions" )

class(ppv)
head(ppv)
inspect(head(ppv))

ppvt2 <- data.frame(TId = tp$CODVENTA, items = c(paste0("Producto=", tp$CODPRODUCTO), paste0("Tienda=", tp$NOMBRETIENDA)))
ppvt2 <- lapply(ppvt2, unique)
ppvt2 <- as(ppvt2, "transactions")
class(ppvt2)
head(ppvt2)
inspect(head(ppvt2))


?split
str(tp[, c("CODPRODUCTO", "NOMBRETIENDA")])

colnames(df) <- c("COD_PRODUCT", "STORE", "SELL_ID")
str(df)
print(unlist(lapply(df, function(x) any(is.na(x)))))
ppvt <- split(x = df[, c("COD_PRODUCT", "STORE")], f = df$SELL_ID)
ppvt <- lapply (ppvt, unique)
ppvt <- as (ppvt,  "transactions" )



tp$NOMBRETIENDA <- as.factor(tp$NOMBRETIENDA)
tp$CODPRODUCTO <- as.factor(tp$CODPRODUCTO)
str(tp)

print(unlist(lapply(ppvt, function(x) any(is.na(x)))))

ppvt <- lapply (ppvt, unique)
ppvt <- as (ppvt,  "transactions" )
class(ppvt)
head(ppvt)
inspect(head(ppvt))

showMethods("coerce", classes="transactions")





rules_ppv <- apriori(ppv, parameter=list(support= 0.001 , confidence= 0.4 ))

items(rules_ppv)
inspect(rules_ppv)

rules_conf <- sort (rules_ppv, by = "confidence" , decreasing = TRUE ) 
inspect(rules_conf)
rules_lift <- sort (rules_ppv, by = "lift" , decreasing = TRUE ) 
inspect(rules_lift)
plot(rules_ppv)

summary(quality(rules_ppv))

class(rules_ppv[1:3])




df2 <- count(tp, c('CODPRODUCTO','DESCRIPCIÓN'))
head(df2[order(df2$freq, decreasing =  TRUE ), ], n = 10)



producto.quantity <- aggregate(data.frame(count = tp$DESCRIPCIÓN), list(value = tp$DESCRIPCIÓN, code=tp$CODPRODUCTO), length)
producto.a <- aggregate(data.frame(count = tp$CODPRODUCTO), list(value = tp$CODPRODUCTO), length)
# How many products are?
nrow(producto)
nrow(producto.quantity) 
nrow(producto.a) 

head(producto.quantity[order(producto.quantity$count, decreasing =  TRUE ), ], n = 10)
head(producto.a[order(producto.a$count, decreasing =  TRUE ), ], n = 10)


nrow(tp[tp$DESCRIPCIÓN == 'Tinto Reserva 95', ])

?kable

# Checking if sold price <= cost price (and NO DISCOUNT)
tp <- ticket_with_product
tp$VENDIDOPOR <- tp$IMPORTETOTAL / tp$TOTALUNIDADES
tp$COSTETOTAL <- tp$COSTE * tp$TOTALUNIDADES
inconsistences <- tp[tp$VENDIDOPOR <= tp$COSTE, ]
inconsistences <- inconsistences[inconsistences$NOMBREPROMOCION == "", ]
inconsistences$PERDIDA <- inconsistences$COSTETOTAL - inconsistences$IMPORTETOTAL
sum(inconsistences$PERDIDA)



summary(quality(rules))

#write.csv(tp, file = "tp.csv")

?kable

# Checking if sold price > cost price (and if sold price = recommended sell price)

