library(C50)

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

