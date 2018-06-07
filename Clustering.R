install.packages("lubridate")
library(knitr)
library(lubridate)
library(clustMixType)
library(stringr)

year <- lubridate::year(Sys.Date())

cliente$ANYONACIMIENTO <- as.numeric(substr(cliente$FECHANACIMIENTO, 0, 4))
cliente$GRUPOEDAD <- ifelse((year - cliente$ANYONACIMIENTO) <= 29, "JOVEN", ifelse((year - cliente$ANYONACIMIENTO) <= 65, "ADULTO", "MAYOR"))
cliente$GRUPOEDAD <- as.factor(cliente$GRUPOEDAD)

cliente$NUMEROHIJOS <- as.numeric(cliente$NUMEROHIJOS)
# Before employing k-means (or whichever clustering), see the data types
res <- sapply (cliente, class) 
kable ( data.frame ( variables= names (res), clase= as.vector (res)))

print(unlist(lapply(cliente, function(x) any(is.na(x))))) # NAs NUMEROHIJOS
cliente[is.na(cliente)] <- 0

unicos <- unique(cliente[c("PROFESIÓN")])
unicos <- sapply(unicos, str_trim)


map[[key]] <- 4

unicos <- str_trim(unicos)
# Reducting the dimensions
colnames(cliente)
x <- cliente[, c("SEXO", "ESTADOCIVIL", "PROFESIÓN", "NUMEROHIJOS", "REGION", "NACIONALIDAD", "GRUPOEDAD")]
str(x)

# apply k prototypes
l <- lambdaest(x)
kpres <- kproto(x, 4, lambda = l)


# in real world clusters are often not as clear cut
# by variation of lambda the emphasize is shifted towards factor / numeric variables
kpres <- kproto(x, 2)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 0.1)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 25)
clprofiles(kpres, x)


