var1 = c(1,3,5,7,9)
print(var1)

c(1:5)

var3 <- seq(1,5)
var3


var4 <- seq(1,5, by=3)
var4


str1<-"a"
str2<-"text"
str3<-"hello world"
str4<-c(str1, str2, str3)

c(str1, str2)

x<-c(1,2,3)
mean(x)
max(x)
paste(str4, collapse=",") # concatenate

library(ggplot2)

x<-c("a", "a", "b", "c")
qplot(x)

mpg
str(mpg)
mpg$year<-as.factor(mpg$year)
str(mpg)

qplot(mpg$hwy)
qplot(data=mpg, x=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", colour=drv)

#Inter-Quantile Range

#1. 
a<-80
b<-90
c<-50
#2.
mean(c(a,b,c))


#DataFrame
eng <- c(90, 100, 70, 60)
math <- c(50, 60, 100, 9)
class(eng)
df <- data.frame(eng)
class(df)

df2 <- data.frame(eng, math, row.names = c('a','b','c','d'))

class <- c(1,1,2,3)
df3<-data.frame(eng, math, class)

mean(df3$eng)

df4 <- data.frame(eng = c(90, 100, 70, 60),
                  math = c(50, 60, 100, 9))

names <- c("Grape", "Apple", "Pear")
Price <- c(1000,2000,500)
Quantity <- c(20, 10, 5)
fruit <- data.frame(Price, Quantity, row.names=names)
fruit

mean(fruit$Price)
mean(fruit$Quantity)


install.packages("readxl")
library(readxl)
df<-read_excel("Data/excel_exam.xlsx")
df$id <- as.factor(df$id)
df$class <- as.factor(df$class)
mean(df$science)

df2<-read_excel("Data/excel_exam_novar.xlsx", col_names=FALSE)

df3<-read_excel("Data/excel_exam_sheet.xlsx", sheet = 3)

df4<-read.csv("Data/csv_exam.csv")
df4$id <- as.factor(df$id)
df4$class <- as.factor(df$class)


df5<-data.frame(a=c(1,2,3),
                b=c(1,2,3),
                c=c(1,2,3))
write.csv(df5, file="./Data/mydf.csv")

# for small file size and fast writing
save(df5, file="./Data/mydf_s.rda")
rm(df5)
load("mydf_s.rda") # load rda file

exam <- read.csv("Data/csv_exam.csv")
head(exam)
tail(exam)
View(exam)
dim(exam)
exam$id <- as.factor(exam$id)
exam$class <- as.factor(exam$class)
summary(exam)

class(mpg) # [1] "tbl_df"     "tbl"        "data.frame" : 세 가지 모두 다 해당된다는 말

summary(mpg)


df<-data.frame(var1=c(1,2,1),
           var2=c(2,3,2))
df_new <- df
library(plyr)
df_new <- rename(df_new, replace=c("var2"='v2'))

install.packages("dplyr")
library(dplyr)
df_new <- rename(df_new, v1=var1)

mydf<-data.frame(eng=c(70,80,90),
                 math=c(50,60,70))
mydf$sum<-mydf$eng+mydf$math

mpg$tot <- (mpg$hwy + mpg$cty)/2
mpg$test<-ifelse(mpg$tot >= 23, "gr_h", "gr_l")
table(mpg$test) # frequency
qplot(mpg$test)

mpg$grade <- ifelse(mpg$tot>=28, 'A', ifelse(mpg$tot>=20, 'B', 'C'))

exam <- read.csv("Data/csv_exam.csv")
str(exam)
#filter: extract data
exam %>%filter(class==3)          #ctrl+shift+m : filter symbol(pipe symbol)
exam %>%filter(class!=3 & science >= 50)          
exam %>%filter(math>=70 | science >= 50)          
exam %>%filter(class %in% c(1,4,5))  
class3 <- exam %>%filter(class==3) 

mpg3 <- mpg %>% filter(displ<=3)
mpg5 <- mpg %>% filter(displ>=5)
mean(mpg3$hwy)
mean(mpg5$hwy)


table(mpg$manufacturer)
volks <- mpg %>% filter(manufacturer == "volkswagen")
audi <- mpg %>% filter(manufacturer == "audi")
mean(volks$cty)
mean(audi$cty)

hyun <- mpg %>% filter(manufacturer == "hyundai")
chev <- mpg %>% filter(manufacturer == "chevrolet")
nis <- mpg %>% filter(manufacturer == "nissan")
mean(c(hyun$cty, chev$cty, nis$cty))

mpg_3 <- mpg  %>% filter(manufacturer %in% c("hyundai", "chevrolet", "nissan"))
mean(mpg_3$cty)


#extract column

exam %>% select(-science, -math)

exam %>% 
  filter(class == 3)%>% 
  select(math) %>%   # %>% allows chaining
  head(2)

exam %>% arrange(math)
exam %>% arrange(desc(math)) # reversed
exam %>% arrange(math, english) 
exam %>% arrange(class, english) 
