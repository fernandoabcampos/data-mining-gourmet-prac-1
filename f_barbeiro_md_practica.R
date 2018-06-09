library(knitr)
library(stringr)
library(plotrix) # pie3d
library(arules)
library(arulesViz)
library(lubridate)
library(clustMixType)
library(C50)

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
  file <- as.data.frame(sapply(file, toupper))
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
ticket.cabecera$CODCLIENTE <- str_trim(ticket.cabecera$CODCLIENTE)
ticket.lineas$NOMBRETIENDA <- str_trim(ticket.lineas$NOMBRETIENDA)
ticket.lineas$CODPRODUCTO <- str_trim(ticket.lineas$CODPRODUCTO)
ticket.lineas$CODVENTA <- str_trim(ticket.lineas$CODVENTA)
producto$DESCRIPCIÓN <- str_trim(producto$DESCRIPCIÓN)
producto$NOMBRESUBFAMILIA <- str_trim(producto$NOMBRESUBFAMILIA)
subfamilia$NOMBRESUBFAMILIA <- str_trim(subfamilia$NOMBRESUBFAMILIA)
subfamilia$NOMBREFAMILIA <- str_trim(subfamilia$NOMBREFAMILIA)
familia$NOMBREFAMILIA <- str_trim(familia$NOMBREFAMILIA)
familia$NOMBRESECCIÓN <- str_trim(familia$NOMBRESECCIÓN)
cliente$CODCLIENTE <- str_trim(cliente$CODCLIENTE)



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
dist_tienda <- table(tp$NOMBRETIENDA)
head(dist_tienda)
head(table(tp$FORMAPAGO))
head(table(tp$MARCA))

lbls <- paste(names(dist_tienda),  ": " , dist_tienda, sep= "" ) 
pie3D(dist_tienda
      , radius= 0.9
      , labelcex=  0.7
      , labels = lbls
      , explode= 0.1
      , main= "Distribución de ventas por Tienda" , col = rainbow(length(lbls)
      ))




# It makes sense to split by CODVENTA as CODCLIENTE is not properly filled - and the CODVENTA will give us an understanding of what was sold together


####################################################### ASSOCIATION RULES ##########################################################################################################################

ppv <- split (x = tp[,  "CODPRODUCTO" ], f = tp$CODVENTA) 
ppv <- lapply (ppv, unique)
ppv <- as (ppv,  "transactions" )
new_label <- do.call(paste, c(producto[match(itemLabels(ppv), producto$CODPRODUCTO), c("CODPRODUCTO", "DESCRIPCIÓN", "MARCA")], sep = " - ")) 
itemLabels(ppv) <- new_label
class(ppv)
inspect(head(ppv))
rules_ppv <- apriori(ppv, parameter=list(support= 0.001 , confidence= 0.4 ))


plot(rules_ppv)
items(rules_ppv)
inspect(rules_ppv)

rules_conf <- sort (rules_ppv, by = "confidence" , decreasing = TRUE ) 
inspect(rules_conf)
rules_lift <- sort (rules_ppv, by = "lift" , decreasing = TRUE ) 
inspect(rules_lift)
summary(quality(rules_ppv))



df <- tp[, c("CODPRODUCTO", "NOMBRETIENDA", "CODVENTA")]
write.csv(df, file = "df.csv", row.names=FALSE)

ppvt <- read.transactions(file = "df.csv", rm.duplicates =  TRUE , skip =  1 , sep = "," ) 
new_label_t <- ifelse(!is.na(match(itemLabels(ppvt), producto$CODPRODUCTO)), do.call(paste, c(producto[match(itemLabels(ppvt), producto$CODPRODUCTO), c("CODPRODUCTO", "DESCRIPCIÓN", "MARCA")], sep = " - ")), itemLabels(ppvt))
itemLabels(ppvt) <- new_label_t
inspect(head(ppvt))
rules_ppvt <- apriori(ppvt, parameter=list(support= 0.002 , confidence= 0.5 ))

items(rules_ppvt)
inspect(rules_ppvt)

rules_conft <- sort (rules_ppvt, by = "confidence" , decreasing = TRUE ) 
inspect(rules_conft)
rules_liftt <- sort (rules_ppvt, by = "lift" , decreasing = TRUE ) 
inspect(rules_liftt)
summary(quality(rules_ppvt))

plot(rules_ppvt)


####################################################### CLUSTERING ##########################################################################################################################

year <- lubridate::year(Sys.Date())
joven <- cliente[(year - cliente$ANYONACIMIENTO) <= 38, ]
nrow(joven)
cliente$ANYONACIMIENTO <- as.numeric(substr(cliente$FECHANACIMIENTO, 0, 4))
cliente$GRUPOEDAD <- ifelse((year - cliente$ANYONACIMIENTO) <= 29, "JOVEN", ifelse((year - cliente$ANYONACIMIENTO) <= 65, "ADULTO", "MAYOR"))
cliente$GRUPOEDAD <- as.factor(cliente$GRUPOEDAD)

cliente$NUMEROHIJOS <- as.numeric(cliente$NUMEROHIJOS)
# Before employing k-means (or whichever clustering), see the data types
res <- sapply (cliente, class) 
kable ( data.frame ( variables= names (res), clase= as.vector (res)))

print(unlist(lapply(cliente, function(x) any(is.na(x))))) # NAs NUMEROHIJOS
cliente[is.na(cliente)] <- 0


# Reducting the dimensions
colnames(cliente)
x <- cliente[, c("SEXO", "ESTADOCIVIL", "PROFESIÓN", "NUMEROHIJOS", "REGION", "NACIONALIDAD", "GRUPOEDAD")]
str(x)

x$PROFESIÓN <- as.factor(gsub("ECONOMISTAS,ABOGADOS & ADMIN.EMPRESAS", "ECON, ABOG./ADMIN", x$PROFESIÓN))
x$PROFESIÓN <- as.factor(gsub("INGENIEROS & ESPECIALISTAS", "INGEN./ESPEC.", x$PROFESIÓN))
x$PROFESIÓN <- as.factor(gsub("DOCTORES & PROFESIONALES DE LA SALUD", "DR./PROFES. SALUD", x$PROFESIÓN))
x$PROFESIÓN <- as.factor(gsub("ARCHITECTOS,DECORADORES & HUMANISTAS", "ARCH, DECOR./HUM.", x$PROFESIÓN))
x$PROFESIÓN <- as.factor(gsub("GERENTES & DIRECTIVOS", "GERENTE/DIRECTIVO", x$PROFESIÓN))

people <- x[x$SEXO!="EMPRESA",]

# apply k prototypes
l <- lambdaest(people)
kpres <- kproto(people, 4, lambda = l)
clprofiles(kpres, people)


####################################################### DECISION TREE ##########################################################################################################################

unlist(lapply(tp, function(x) any(is.na(x)))) 

# Filtrando CODCLIENTE que estan rellenados
tpc <- tp[tp$CODCLIENTE!="", ]
tc <- merge_dfs_left_outer(tpc, cliente, c("CODCLIENTE"))

# Valor que será la categoría que queremos predecir
tc$CHEQUE <- as.factor(ifelse(tc$FORMAPAGO == "CHEQUE", "SI", "NO"))

# Drastically reducing dimensions
tc <- tc[, c("CODVENTA", "HORA", "NOMBREPAIS", "REGION", "CHEQUE")]

TO <- "ASCII//TRANSLIT"
tc$NOMBREPAIS <- as.factor(iconv(tc$NOMBREPAIS, to = TO))
tc$REGION <- as.factor(iconv(tc$REGION, to = TO))
head(tc)

# Obtain train set (80%) and test set (20%)
lines  <- nrow(tc)
colnames(tc)
str(tc)
ntrain <- round(lines * 0.8)       # number of training examples
tindex <- sample(lines, ntrain)    # indices of training samples (random)
xtrain <- tc[tindex,2:4]          # data are in columns 2:4 - "HORA", "NOMBREPAIS", "REGION"
xtest  <- tc[-tindex,2:4]         # data are in columns 2:4 - "HORA", "NOMBREPAIS", "REGION"
ytrain <- tc[tindex,5]            # labels are in column 5 - "CHEQUE"
ytest  <- tc[-tindex,5]           # labels are in column 5 - "CHEQUE"

summary(xtest)
summary(xtrain)

model_market <- C50::C5.0(xtrain, ytrain, rules = TRUE)#, trials = 10)
summary(model_market)

# MOSTRAR EL ARBOL OBTENIDO
plot(model_market)
cat(model_market$rules)

estimated_ytest <- predict(model_market, xtest, type="class")
accuracy <- sum(ytest==estimated_ytest)/length(ytest)
error <- 1- accuracy
print(accuracy)
print(error)


####################################################### EXTRACTING GENERAL KNOWLEDGE ##########################################################################################################################
tp$VENDIDOPOR <- tp$IMPORTETOTAL / tp$TOTALUNIDADES
tp$COSTETOTAL <- tp$COSTE * tp$TOTALUNIDADES
inconsistences <- tp[tp$VENDIDOPOR <= tp$COSTE, ]
inconsistences <- inconsistences[inconsistences$NOMBREPROMOCION == "", ]
inconsistences$PERDIDA <- inconsistences$COSTETOTAL - inconsistences$IMPORTETOTAL
sum(inconsistences$PERDIDA)

