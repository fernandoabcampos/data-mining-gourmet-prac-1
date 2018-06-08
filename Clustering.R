install.packages("lubridate")
library(knitr)
library(lubridate)
library(clustMixType)
library(stringr)

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




