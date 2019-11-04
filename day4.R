sapply # list
lapply # vector
?tapply  # (벡터, 그룹색인, 그룹단위적용함수)
sapply(iris[, 1:4], function(x){x>3})

rep(1,10)
tapply(1:10, rep(1,10), sum) 
tapply(1:10, 1:10%%2==1, sum) 

str(iris)
tapply(iris$Sepal.Length, iris$Species, mean)

install.packages("doBy")
library(doBy)
quantile(iris$Sepal.Length, seq(0,1,0.1))

summaryBy(Sepal.Length~Species, iris, FUN=mean)
order(iris$Sepal.Length) # return index, not value
iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = T),]
orderBy(~Sepal.Length+Sepal.Width, iris)
sample(1:45, 6) # 비복원추출
sample(1:45, 6, replace=TRUE) #복원추출

NROW(iris)

iris[sample(NROW(iris), NROW(iris)),]
sampleBy(~Species, frac=0.1 , data=iris)

split(iris, iris$Species)
class(split(iris, iris$Species))  # list

subset(iris, Species=="setosa" & Sepal.Length>=5.0)
subset(iris, select=c(Species, Sepal.Length))
subset(iris, select=-c(Species, Sepal.Length))

names(iris)
iris[, !names(iris) %in% c("Species", "Sepal.Length")]


# merge
x<-data.frame(name=c("a", "b", "c"), math=c(1,2,3))
y<-data.frame(name=c("c", "b", "a"), eng=c(4,5,6))

cbind(x, y) # 단순 컬럼 연결
merge(x, y) # name 기준 병합

x<-data.frame(name=c("a", "b", "c"), math=c(1,2,3))
y<-data.frame(name=c("c", "b", "d"), eng=c(4,5,6))

merge(x, y)
merge(x, y, all=T)


x<-c(5,2,1,44,3)
sort(x, decreasing=T)

x<-c(5,2,1,44,3)
order(x, decreasing=T)


data = list()
n = 10
for (c in 1:n){
  data[[c]] = data.frame(Index=c, myChar=sample(letters, 1), z=runif(1))
}

runif(1) # create a random number within the range of the arg

# Merging multiple dataframes
# 1. rbind
do.call(rbind,data)

# 2. ldply  (l: list, d: dataframe)
library(plyr)
ldply(data, rbind)

# 3. rbindlist
library(data.table)
rbindlist(data)
# rbindlist is the fastest(written in C language)

with(iris,{
  mean(Sepal.Length)
})


which(iris$Species=="setosa")

x<-c(1,1,4,4,4,3,3,1,3,3,3)
names(which.max(table(x)))
which.max(table(x))
max(table(x))


library(mlbench)
data("Ozone")
head(Ozone)
plot(Ozone$V8, Ozone$V9, xlab="Temp1", ylab = "Temp2", main="Ozone", pch="o", cex=.5, col="#FE8E1E",
     col.axis="#0000ff", col.lab="#990000", xlim=c(25,93))  # pch: shape, cex: size
help(par)

cars 
plot(cars, type="o")  # type: l, b, o ....


# speed로 그룹화 -> dist 평균 출력
x<-tapply(cars$dist, cars$speed, mean)
plot(names(x), x, xlab="Speed", ylab="Dist Mean", type='l', lty="dashed")


par() # reset par instance

myPar <-par(mfrow=c(1, 2)) # makes two cols
plot(Ozone$V8, Ozone$V9, main="Ozone")
plot(Ozone$V8, Ozone$V9, main = "Ozone2")
par(myPar) # go back to when myPar was not declared


library(ggplot2)

ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point(col="#990000") +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.y = element_line(colour="#ABABAB"),
        panel.grid.major.x = element_line(colour="#ABABAB"),
        axis.text=element_text(size=15, face= "bold"),
        axis.title=element_text(size=15,face="bold")) +
  xlab("displ") +
  ylab("hwy")

library(dplyr)
df<-mpg %>% 
  group_by(drv) %>% 
  summarise(meanHwy=mean(hwy))

ggplot(data=df, aes(x=reorder(drv, -meanHwy), y=meanHwy)) +
  geom_col()+
  scale_fill_gradient("", low="#ffffff", high="#d35400")


ggplot(data=mpg, aes(x=cty)) +geom_bar(col = "#cb4335", fill =  "#fdedec")
#geom_col : plot mean
#geom_bar : plot frequency

economics
ggplot(data=economics, aes(x=date, y=unemploy)) +
  geom_line()
