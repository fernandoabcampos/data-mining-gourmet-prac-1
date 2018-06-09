library(C50)

unlist(lapply(tp, function(x) any(is.na(x)))) 

# Altough CODCLIENTE doesnt have NAs, the values are not properly informed, filtering to work with the ones which are.
tpc <- tp[tp$CODCLIENTE!="", ]

nrow(tpc)
head(tpc, n = 10)
ticket_with_client <- merge_dfs_left_outer(tpc, cliente, c("CODCLIENTE"))
ticket_with_client_product_subfamily <- merge_dfs_left_outer(ticket_with_client, subfamilia, c("NOMBRESUBFAMILIA"))
ticket_with_client_product_family <- merge_dfs_left_outer(ticket_with_client_product_subfamily, familia, c("NOMBREFAMILIA"))

tcf <- ticket_with_client_product_family
nrow(tcf)
head(tcf)
colnames(tcf)
# APPROACH 1
#tcf.COD_VENTA_VINOS <- tcf[tcf$NOMBRESECCIÓN == "VINOS", c("CODVENTA")]
#tcf$VINO_VENTA <- as.factor(ifelse(tcf$CODVENTA %in% tcf.COD_VENTA_VINOS, "VENDIDO", "NO VENDIDO"))

# APPROACH 2 MAS VALIDO
#tcf$VINO_VENTA <- as.factor(ifelse(tcf$NOMBRESECCIÓN == "VINOS", "VENDIDO", "NO VENDIDO"))




tcf$CHEQUE <- as.factor(ifelse(tcf$FORMAPAGO == "CHEQUE", "SI", "NO"))
tcf$CODCLIENTE <- as.factor(tcf$CODCLIENTE)
tcf$NOMBRETIENDA <- as.factor(tcf$NOMBRETIENDA)

# Reducing dimensions
# tcf <- tcf[, c("CODVENTA","PROFESIÓN", "FORMAPAGO", "CODCLIENTE", "HORA", "CODPRODUCTO", "NOMBRETIENDA", "NOMBREPAIS", "REGION", "SEXO", "ESTADOCIVIL", "CHEQUE")]
# tcf <- tcf[, c("CODVENTA","PROFESIÓN", "FORMAPAGO", "CODCLIENTE", "HORA", "NOMBRETIENDA", "NOMBREPAIS", "REGION", "SEXO", "ESTADOCIVIL", "CHEQUE")]
#tcf <- tcf[, c("CODVENTA","PROFESIÓN", "FORMAPAGO", "HORA", "NOMBRETIENDA", "NOMBREPAIS", "REGION", "SEXO", "ESTADOCIVIL", "CHEQUE")]
#tcf <- tcf[, c("CODVENTA","PROFESIÓN", "FORMAPAGO", "HORA", "NOMBRETIENDA", "NOMBREPAIS", "REGION", "ESTADOCIVIL", "CHEQUE")]
tcf <- tcf[, c("CODVENTA", "FORMAPAGO", "PROFESIÓN", "HORA", "NOMBREPAIS", "REGION", "CHEQUE")]

unique(tcf$PROFESIÓN)
TO <- "ASCII//TRANSLIT"
tcf$PROFESIÓN <- as.factor(gsub("&", "y", iconv(tcf$PROFESIÓN, to = TO)))
#tcf$CODPRODUCTO <- as.factor(iconv(tcf$CODPRODUCTO, to = TO))
tcf$NOMBRETIENDA <- as.factor(iconv(tcf$NOMBRETIENDA, to = TO))
tcf$NOMBREPAIS <- as.factor(iconv(tcf$NOMBREPAIS, to = TO))
tcf$FORMAPAGO <- as.factor(iconv(tcf$FORMAPAGO, to = TO))
tcf$REGION <- as.factor(iconv(tcf$REGION, to = TO))

str(tcf)

tcf$ESTADOCIVIL <- as.factor(ifelse(nchar(as.character(tcf$ESTADOCIVIL)) == 0, "NO APLICA", str_trim(tcf$ESTADOCIVIL)))
unlist(lapply(tcf, function(x) any(is.na(x)))) 
#tcf <- tcf[1:2000, ]
# Obtain train set (80%) and test set (20%)
lines  <- nrow(tcf)
colnames(tcf)
str(tcf)
ntrain <- round(lines * 0.8)       # number of training examples
tindex <- sample(lines, ntrain)    # indices of training samples (random)
xtrain <- tcf[tindex,4:6]          # data are in columns 1:3 - "PROFESIÓN", "FORMAPAGO", "REGION", "SEXO", "ESTADOCIVIL"
xtest  <- tcf[-tindex,4:6]         # data are in columns 1:3 - "PROFESIÓN", "FORMAPAGO", "REGION", "SEXO", "ESTADOCIVIL"
ytrain <- tcf[tindex,7]            # labels are in column 4 - "VINO_VENTA"
ytest  <- tcf[-tindex,7]           # labels are in column 4 - "VINO_VENTA"

summary(xtest)
summary(xtrain)


model_market <- C50::C5.0(xtrain, ytrain)#, trials = 10)
summary(model_market)

# MOSTRAR EL ARBOL OBTENIDO
plot(model_market)
               