library(foreign)
library(readxl)
library(dplyr)
library(ggplot2)
raw_welfare<-read.spss("Koweps.sav", to.data.frame=TRUE)
welfare<-raw_welfare
str(welfare)
View(welfare)
summary(welfare)

welfare<-rename(welfare,
                sex=h10_g3,
                birth=h10_g4,
                marriage=h10_g10,
                religion=h10_g11,
                income=p1002_8aq1,
                code_job=h10_eco9,
                code_region=h10_reg7)
welfare$age <- (2019 - welfare$birth)


welfare$ageGroup <- with(welfare, {
  ifelse(age>=60, "60대 이상", 
         ifelse(age>=50, "50대", 
                ifelse(age>=40, "40대", 
                       ifelse(age>=30, "30대", 
                              ifelse(age>=20, "20대", "20대 미만")))))
})

welfare$sex <- ifelse(welfare$sex == 1, "Male", "Female")

ageGroupIncome <- welfare %>% 
  group_by(ageGroup) %>% 
  filter(!is.na(income)) %>% 
  summarise(meanIncome=mean(income))

ggplot(data=ageGroupIncome, aes(x=ageGroup, y=meanIncome)) +
  geom_col()


sexIncome <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageGroup, sex) %>% 
  summarise(meanIncome=mean(income))

ggplot(data=sexIncome, aes(x=ageGroup, y=meanIncome, fill=sex)) +
  geom_col()
  
ggplot(data=sexIncome, aes(x=ageGroup, y=meanIncome, fill=sex)) +
  geom_col(position = "dodge")

sexAge <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(meanIncome=mean(income))
ggplot(data=sexAge, aes(x=age, y=meanIncome, col=sex)) +
  geom_line()

code2job <- read_excel("Data/Koweps_Codebook.xlsx", col_names = T, sheet = 2)
welfare <- left_join(welfare, code2job, id="code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

jobIncome <- welfare %>% 
  group_by(job) %>% 
  filter(!is.na(code_job) & !is.na(income)) %>%
  summarise(meanIncome=mean(income))

top10 <- jobIncome %>% 
  arrange(desc(meanIncome)) %>% 
  head(10)

ggplot(data=top10, aes(x=job, y=meanIncome)) +
  geom_col() +
  coord_flip()
ggplot(data=top10, aes(x=reorder(job, meanIncome), y=meanIncome)) +  # The 1st arg of reorder() shouldn't be a continuous data
  geom_col() +
  coord_flip()
ggplot(data=top10, aes(x=reorder(job, -meanIncome), y=meanIncome)) +
  geom_col() +
  coord_flip()

bottom10 <- jobIncome %>% 
  arrange(meanIncome) %>% 
  head(10)
ggplot(data=bottom10, aes(x=reorder(job, -meanIncome), y=meanIncome)) +
  geom_col() +
  coord_flip() +
  ylim(0,200)

maleJob<-welfare %>% 
  group_by(job) %>% 
  filter(!is.na(job) & sex=="Male") %>%
  summarise(male=n()) %>% 
  arrange(desc(male))
femaleJob<-welfare %>% 
  group_by(job) %>% 
  filter(!is.na(job) & sex=="Female") %>%
  summarise(female=n()) %>% 
  arrange(desc(female))

maleIncome <- welfare %>% 
  group_by(job) %>% 
  filter(!is.na(job) & !is.na(income) & sex=="Male") %>%
  summarise(meanIncome=mean(income))
maleIncome %>% 
  arrange(desc(meanIncome)) %>% 
  head(10)

femaleIncome <- welfare %>% 
  group_by(job) %>% 
  filter(!is.na(code_job) & !is.na(income) & sex=="Female") %>%
  summarise(meanIncome=mean(income))
femaleIncome %>% 
  arrange(desc(meanIncome)) %>% 
  head(10)

welfare$religion <- as.factor(welfare$religion)
welfare$religion <- ifelse(welfare$religion==1, "yes", "no")

table(welfare$marriage)
# Does having a religion affect divorce?
welfare$groupMarriage<-ifelse(welfare$marriage==1, "married", ifelse(welfare$marriage==3, "divorced", NA))
religionMarriage<-welfare %>% 
  filter(!is.na(groupMarriage)) %>% 
  group_by(religion, groupMarriage) %>%
  summarise(n=n()) %>% 
  mutate(totGroup=sum(n)) %>%
  mutate(pct=round(n/totGroup*100, 1))
           
listRegion<-data.frame(code_region=c(1:7),
           region=c("서울", "수도권", "경남", "경북", "충남", "강원/충북", "전라/제주"))

welfare<-left_join(welfare, listRegion, id="code_region")

regionAge<-welfare %>% 
  group_by(region, ageGroup) %>% 
  summarise(n=n()) %>% 
  mutate(totGroup=sum(n)) %>% 
  mutate(pct=round(n/totGroup*100, 1))

ggplot(data=regionAge, aes(x=region, y=pct, fill=ageGroup))   +
  geom_col() +
  coord_flip()


library(ggiraphExtra)
library(tibble)
library(maps)
str(USArrests)
crime<-rownames_to_column(USArrests, var="State")
crime$State <- tolower(crime$State)

statesMap<-map_data("state")
str(statesMap)

library(mapproj)
ggChoropleth(data=crime, 
             aes(fill=Murder, 
                 map_id=State),
             map=statesMap)

plot(iris$Sepal.Width, iris$Sepal.Length, cex=.5, pch=20, xlab='width', ylab='length', main='Iris')
x<-seq(0, 2*pi, 0.1)
plot(x, sin(x), cex=0.1)
lines(x, sin(x))

plot(cars, xlim=c(0, 25))
abline(a=5, b=3.5, col="red")
abline(h=mean(cars$dist))
abline(v=mean(cars$speed))
 
plot(4:6, 4:6)
# text(5,5,"x")
text(5, 5, "00", adj = c(0,0))
text(5, 5, "01", adj = c(0,1))
text(5, 5, "10", adj = c(1,0))
text(5, 5, "11", adj = c(1,1))

plot(cars, cex=.5)
text(cars$speed, cars$dist, pos=4)

plot(cars, cex=.5)
identify(cars$speed, cars$dist)

plot(iris$Sepal.Width, iris$Sepal.Length, pch=20)
points(iris$Petal.Width, iris$Petal.Length, pch="+", col="#ff0000")
legend("topright", legend=c("Sepal", "Petal"), pch=c(20, 43), col=c("black", "red"), bg="gray")
