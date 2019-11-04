# 1.

titanic <- read.csv("titanic.csv")

nameExtract <- function(name) {
  str_extract(name, "\\b(\\w+)\\.")
}
titanic$NameId <- sapply(titanic$Name, nameExtract)

IdNum = 1:length(table(titanic$NameId))
nameDict <- data.frame(IdNum, row.names = names(table(titanic$NameId)))

nameInvert <- function(name) {
  nameDict[name, ]
}

titanic$NameIdNum <- sapply(titanic$NameId, nameInvert)
titanic$NameIdNum <- as.factor(titanic$NameIdNum)

# 2.

library(dplyr)
top5 <- titanic %>% 
  group_by(NameIdNum) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  head(5)
library(ggplot2)
ggplot(data = top5, aes(x=NameIdNum, y=count)) +
  geom_col()+
  
# 3.
