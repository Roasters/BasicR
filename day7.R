library(devtools)
install_github("cardiomoon/kormaps2014") # Download a package from github
library(kormaps2014)
str(changeCode(korpop1))
library(ggiraphExtra)
library(ggplot2)
library(dplyr)

korpop1<-rename(korpop1, pop="총인구_명", name="행정구역별_읍면동")
str(changeCode(kormap1))

ggChoropleth(data=korpop1, # 지도에 표시할 데이터
             aes(fill=pop, # 색상별로 표현할 변수
                 map_id=code, #  지역 기준 변수수
                 tooltip=name), # 지도 위 표시할 지역명
             map=kormap1, # 지도와 관련한 정보
             interactive = T)
        
str(changeCode(tbc))
ggChoropleth(data=tbc, # 지도에 표시할 데이터
             aes(fill=NewPts, # 색상별로 표현할 변수
                 map_id=code, #  지역 기준 변수수
                 tooltip=name), # 지도 위 표시할 지역명
             map=kormap1, # 지도와 관련한 정보
             interactive = T)

library(plotly)
p<-ggplot(data=mpg, aes(x=displ, 
                     y=hwy,
                     col=drv)) +
  geom_point()
ggplotly(p)  # interactive plot

str(diamonds)

ggplotly(ggplot(data=diamonds, aes(x=cut,
                          fill=clarity)) +
  geom_bar(position = "dodge"))


# Time series graph
library(dygraphs)
library(xts)
eco<-xts(economics$unemploy, order.by=economics$date)
head(eco)
dygraph(eco) 

dygraph(eco) %>% 
  dyRangeSelector()

ecoA<-xts(economics$psavert, order.by=economics$date) # savings rate
ecoB<-xts(economics$unemploy/1000, order.by=economics$date)

eco2<-cbind(ecoA, ecoB)
colnames(eco2) <- c("psavert", "unemploy")

dygraph(eco2) %>% 
  dyRangeSelector()


exam <- read.csv("Data/csv_exam.csv")
exam[2,]
exam[exam$class==1,]
exam[exam$math>=80,]
exam[exam$class==2 & exam$english>=70,]

exam[,1]
exam[,"id"]

exam[1,3]
exam[5, "math"]
exam[exam$math>=50, "english"]
exam[exam$math>=50, c("english", "science")]

exam[, ]

exam %>% 
  filter(exam$math>=50 & exam$english>=80) %>% 
  mutate(totMean=(math+english+science)/3) %>% 
  group_by(class) %>% 
  summarise(myMean=mean(totMean))
exam$totMean<-(exam$math + exam$english + exam$science)/3
aggregate(data=exam[exam$math>=50 & exam$english>=80,], totMean~class, mean)

mpg$totMean <- (mpg$hwy + mpg$cty)/2
aggregate(data=mpg[mpg$class %in% c("compact", "suv"),], totMean~class, mean)

var1<-c(1,2,3,1,2)
var2<-factor(c(1,2,3,1,2))
var1 + 3
var2 + 3
levels(var1)
levels(var2)
var3 <- c('a','b','b','c')
var4 <- factor(c('a','b','b','c'))
class(var3)
class(var4)


myvector<-c(1:6, 'a')

mylist<-list(1:6, 'a')

obj1 <- 1:4
obj2 <- 6:10
obj3 <- list(obj1, obj2)

# vector indexing -> []
# list indexing -> [[]]
mylist<-list(obj1,obj2,obj3)

myvector<-c(1:6, 'a')
mylist<-list(1:6, 'a')
unlist(mylist)
unlist(myvector)

name1 <- "Donald"
myspace <- " "
name2 <- "Trump"
list(name1, myspace, name2)
unlist(list(name1, myspace, name2))

mydata<-data.frame(name=c("a","b","c","d"), gender=c(2,1,1,2))
attr(mydata$name, "what the variable means")<-"이름"
attr(mydata$gender, "what the variable means")<-"성별"
mydata$gender.character<-attr(mydata$gender, "what the variable means")<-"성별"

mylist<-list(1:4, 6:10, list(1:4, 6:10))
lapply(mylist[[3]], mean)
lapply(mylist, mean)
lapply(mylist[c(1,2)], mean)
lapply(mylist[c(1,2, c(1,2))], mean)

wordlist<-c("the", "is", "a", "the")
doc1freq<-c(3, 4, 2, 4)
doc2freq<-rep(1,4)
tapply(doc1freq, wordlist, length)
tapply(doc2freq, wordlist, length)
tapply(doc1freq, wordlist, sum)
tapply(doc2freq, wordlist, sum)


sent1 <- c("earth", "to", "earth")
sent2 <- c("ashes", "to", "ashes")
sent3 <- c("dust", "to", "dust")
# 한 문장에서 to는 한 번, to가 아닌 단어는 2번
myfreq<-c(rep(1, length(sent1)), rep(1, length(sent2)), rep(1, length(sent3)))
tapply(myfreq, c(sent1, sent2, sent3), sum)

mysent <- "Learning R is so interesting"
mywords<-strsplit(mysent, split=" ")

myletters = list()
for(i in 1:5) {
  myletters[i] = strsplit(mywords[[1]][i], split="")
}

mywords2 = list()
for(i in 1:5) {
  mywords2[i] = paste(myletters[[i]], collapse="")
}

rWiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

rParagraphs <- strsplit(rWiki, split="\n")
rSents <- strsplit(rParagraphs[[1]], split="\\. ")
rWords <- list()
for(i in 1:length(rSents)){
  rWords[[i]] <- strsplit(rSents[[i]], split=" ")
}


mysent <- "Learning R is so interesting"
loc.begin <- as.vector(regexpr("ing", mysent)) 
loc.length <- attr(regexpr("ing", mysent),
     "match.length")
loc.end <- loc.begin + loc.length - 1

loc.begin <- as.vector(gregexpr("ing", mysent)[[1]])

regexec('interestin(g)', mysent)

mysentences<-unlist(rSents)
mytemp<-regexpr("software", mysentences)
gregexpr("software", mysentences)

regexec("software", mysentences)

my.begin<-as.vector(mytemp)
my.begin[my.begin == -1] <- NA
my.length <- attr(mytemp, "match.length")
my.end <- my.begin + my.length - 1

mylocs<-matrix(NA, nrow=length(my.begin), ncol=2)
colnames(mylocs)<-c("Begin", "End")
rownames(mylocs) <- paste("sentence", 1:length(my.begin), sep=".")

cbind(my.begin, my.end)
mylocs[,1] <- my.begin
mylocs[,2] <- my.end

grep("software", rSents)
grepl("software", rSents)

sent1 <- rSents[[1]][1]


mypattern <- regexpr('ing', mysentences)
regmatches(mysentences, mypattern, invert=TRUE)

substr(mysentences, 1, 30)

my2sentence <- c("Learning R is so interesting", "He is a fascinating singer")
gregexpr("ing\\b", my2sentence)
