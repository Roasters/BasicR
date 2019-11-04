library(dplyr)

exam <- read.csv("Data/csv_exam.csv")

exam %>%
  mutate(tot = math + english + science) %>% 
  head

exam %>%
  mutate(tot = math + english + science,
         avg = tot/3) %>% 
  head

exam %>%
  mutate(res = ifelse(science>=60, "pass", "fail")) %>% 
  head

exam %>%
  mutate(tot = math + english + science,
         avg = tot/3) %>% 
  arrange(tot) %>% 
  head

exam %>% summarise(meanMath=mean(math))

# to get mean score from each class : group_by
exam %>% 
  group_by(class) %>% 
  summarise(meanMath=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(meanMath=mean(math),
            medianMath=median(math),
            sumMath=sum(math),
            count=n())

library(ggplot2)
mpg %>% 
  group_by(manufacturer, drv) %>% 
  head(20)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  summarise(hwyAvg=mean(hwy),
            ctyAvg=mean(cty)) %>% 
  arrange(desc(hwyAvg), desc(ctyAvg)) %>% 
  head(5)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  mutate(avg=(hwy+cty)/2) %>% 
  summarise(meanAvg=mean(avg)) %>%
  arrange(desc(meanAvg)) %>% 
  head(5)

mid<-data.frame(sid=c(100,200,300,400,500),
                score=c(90,90,50,70,100))
fin<-data.frame(sid=c(100,200,300,400,600),
                score=c(70,80,60,80,90))
tot<-left_join(mid, fin, by=c("sid" = "sid2")) # Use "by" when specifying the axis
tot
right_join(mid, fin, by=c("sid" = "sid2"))

tName<-data.frame(teacher=c("aaa", "bbb", "ccc", "ddd", "eee"),
                  class=c(1,2,3,4,5))
exam_new<-left_join(exam, tName, by="class")

exam_all<-bind_rows(mid, fin)
exam_all


# Processing missing value
df <- data.frame(sex=c("f", "m", NA, "m", "f"),
                 score=c(50,40,40,30,NA))
is.na(df)
table(is.na(df))

# remove rows including NA
df %>% 
  filter(!is.na(score))

dfnomiss <- df %>% 
  filter(!is.na(sex) & !is.na(score))

dfnomiss2 <- na.omit(df)

mean(df$score, na.rm=TRUE)

exam[c(2,5,10), "science"] <- NA
exam %>% summarise(meanSci=mean(science, na.rm=T))

# replace missing value with average

sciMean<-mean(exam$science, na.rm=T)
exam$science <- ifelse(is.na(exam$science), sciMean, exam$science)

# outlier : above or below iqr*1.5 or 0.3%

data <- data.frame(g=c(1,2,3,-3,6),
                   s=c(5,4,3,2,3))
data$g<-ifelse(data$g<=0, NA, data$g)

boxplot(mpg$hwy)$stats
?boxplot

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

# analysis without na

mpg %>% 
  group_by(drv) %>% 
  summarise(hMean=mean(hwy, na.rm=T))

x<-c("a","b","c")
str(x)
x<-as.factor(x)

# Transformation of data structure (matrix -> dataframe)
a<-matrix(1:6, ncol=3)
as.data.frame(a)

as.factor(c("m","f"))

x<-1:5
class(x)

ifelse(x %% 2 == 0, "even", "odd")

for (i in 1:10) {
  print(i)
}


x<-data.frame(a=c(1,2,3), b=c('a', NA, 'c'), c=c('a','b',NA))
na.omit(x)
na.pass(x)
na.exclude(x)

df<-data.frame(x=1:5, y=seq(2,10,2))
df[3,2] = NA
resid(lm(y~x, data=df, na.action=na.omit))
resid(lm(y~x, data=df, na.action=na.exclude))


f<-function(a,b){
  print(a)
  print(b)
}

head(iris)
head(iris3,50)

library(help=datasets) #내장된 데이터셋
