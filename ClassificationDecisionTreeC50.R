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
tcf.COD_VENTA_VINOS <- tcf[tcf$NOMBRESECCIÓN == "VINOS", c("CODVENTA")]
tcf$VINO_VENTA <- as.factor(ifelse(tcf$CODVENTA %in% tcf.COD_VENTA_VINOS, "VENDIDO", "NO VENDIDO"))

# Reducing dimensions
tcf <- tcf[, c("CODVENTA", "DESCRIPCIÓN", "PROFESIÓN", "FORMAPAGO", "REGION", "SEXO", "ESTADOCIVIL", "VINO_VENTA")]
str(tcf)


tcf$ESTADOCIVIL <- as.factor(ifelse(nchar(as.character(tcf$ESTADOCIVIL)) == 0, "NO APLICA", str_trim(tcf$ESTADOCIVIL)))


# Obtain train set (80%) and test set (20%)
lines  <- nrow(tcf)
colnames(tcf)
str(tcf)
ntrain <- round(lines * 0.8)       # number of training examples
tindex <- sample(lines, ntrain)    # indices of training samples (random)
xtrain <- tcf[tindex,4:7]          # data are in columns 1:3 - "PROFESIÓN", "FORMAPAGO", "REGION", "SEXO", "ESTADOCIVIL"
xtest  <- tcf[-tindex,4:7]         # data are in columns 1:3 - "PROFESIÓN", "FORMAPAGO", "REGION", "SEXO", "ESTADOCIVIL"
ytrain <- tcf[tindex,8]            # labels are in column 4 - "VINO_VENTA"
ytest  <- tcf[-tindex,8]           # labels are in column 4 - "VINO_VENTA"

summary(xtest)
summary(xtrain)

model_market <- C50::C5.0(xtrain, ytrain)
summary(model_market)
               