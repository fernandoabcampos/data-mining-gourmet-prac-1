install.packages("lubridate")
library(lubridate)

year <- lubridate::year(Sys.Date())

cliente$ANYONACIMIENTO <- as.numeric(substr(cliente$FECHANACIMIENTO, 0, 4))
cliente$GRUPOEDAD <- ifelse((year - cliente$ANYONACIMIENTO) <= 29, "JOVEN", ifelse((year - cliente$ANYONACIMIENTO) <= 65, "ADULTO", "MAYOR"))

