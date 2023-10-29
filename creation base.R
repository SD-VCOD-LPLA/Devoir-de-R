#création base
install.packages("RMySQL")
library(RMySQL)

#créer connexion entre base R et abse SQL

con<-dbConnect(MySQL(),
                user="sql11646646",
                password="2zLzyEfkSi",
                host="sql11.freesqldatabase.com",
                dbname="sql11646646")
summary(con)
dbGetInfo(con)
 
#créer table 
dbWriteTable(con,"tablere",data)

#connaitre les tables de la BDD
dbListTables(con)


