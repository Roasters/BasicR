library(RMySQL)
?RMySQL
con<-dbConnect(MySQL(), user="root", password="1234", host="127.0.0.1", dbname="rprogramming")
dbListTables(con)
df<-dbGetQuery(con, "select * from rtest2")
dbGetQuery(con, "select name from rtest2")
str(df)
