library(ggplot2)
ggplot(data=mpg, aes(x=drv, y=cty)) +
  geom_boxplot()

# Text Mining
# Bayes Theory -> Bayesian filter, RNN

# Analysis
# 1) Morphological Analysis
# 2) POS Tagging
# 3) Frequency Table
# 4) Visualization

# Machine Learning
# 5) Algorithm Selection
# 6) Model Selection
# 7) Prediction / Classification / Categorization

library(rJava)
library(KoNLP)

useNIADic()  # For using the dictionary
txt<-readLines("Data/hiphop.txt")

library(stringr)
library(dplyr)

tolower("Eye for eye")
toupper("Eye for eye")
nchar('korea')  # number of characters

mysentence <- "Learning R is so interesting"
mystr<-strsplit(mysentence, split=" ")
mystr[[1]][1]

strsplit(mystr[[1]][5], "")

myLetters = list(rep(NA, 5))

for (i in 1:5) {
  myLetters[i]<-strsplit(mystr[[1]][i], "")
}

paste("a", "b", sep = "")
paste(myLetters[[1]], collapse = "$")

myWords <- list()
for (i in 1:5) {
  myWords[i]<-paste(myLetters[[i]], collapse="")
}
myWords

mySentence <- paste(myWords, collapse=" ")

rwiki <- "R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing.[6] The R language is widely used among statisticians and data miners for developing statistical software[7] and data analysis.[8] Polls, data mining surveys, and studies of scholarly literature databases show substantial increases in popularity in recent years.[9]. as of May 2019, R ranks 21st in the TIOBE index, a measure of popularity of programming languages.[10]
A GNU package,[11] source code for the R software environment is written primarily in C, Fortran and R itself,[12] and is freely available under the GNU General Public License. Pre-compiled binary versions are provided for various operating systems. Although R has a command line interface, there are several graphical user interfaces, such as RStudio, an integrated development environment."

rwiki_para<-strsplit(rwiki, split="\n")
rwiki_sent<-strsplit(rwiki_para[[1]], split="\\.")

strsplit(rwiki_sent[[1]][1], split=" ")

test = "R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing"
str_replace_all(test, pattern="\\W", replacement = " ")

txt <- readLines("Data/hiphop.txt")
txt <- str_replace_all(txt, "\\W", " ")
extractNoun("멀티캠퍼스는 여기에 위치함.")

nouns <- extractNoun(txt)

wordCount<-table(unlist(nouns))  # nouns is a list so it needs to be vectorized to make it into a table
df<-as.data.frame(wordCount, stringsAsFactors = F)

# df$word <-as.character(df$word)
df<-rename(df, word=Var1, freq=Freq)
df<-filter(df, nchar(word)>=2)

df %>% 
  arrange(desc(freq)) %>% 
  head(30)

library(wordcloud)
pal<-brewer.pal(8, "Dark2")
wordcloud(df$word, df$freq, min.freq = 5, max.words=100, colors = pal, scale=c(3,1), random.order=F)


tw<-as.data.frame(read.csv("Data/twitter.csv", header=TRUE, fileEncoding = "UTF-8"))
tw<-rename(tw, Content=내용, Date=작성일, Account=계정이름, Number=번호)
tw$Content <- str_replace_all(tw$Content, "\\W", " ")

twWords <- extractNoun(tw$Content)
wordCount <- table(unlist(twWords))
dftw <- data.frame(wordCount, stringsAsFactors=F)
dftw<- rename(dftw, word=Var1, freq=Freq)
top20<-dftw %>% 
  arrange(desc(freq)) %>% 
  head(20)

wordcloud(dftw$word, dftw$freq, max.words=80, colors=pal, random.order=F)

order<-arrange(top20, freq)$word
ggplot(data=top20, aes(x=word, y=freq)) +
  ylim(0, 2500) +
  geom_col() +
  geom_text(aes(label=freq), hjust=0.2) +
  scale_x_discrete(limit=order) +
  coord_flip()

pal<-brewer.pal(9,"Pastel1")
wordcloud(dftw$word, dftw$freq, max.words=150, colors=pal, random.order=F)

# SPSS
library(foreign)
library(readxl)
raw_welfare<-read.spss("Koweps.sav", to.data.frame=TRUE)
welfare<-raw_welfare
str(welfare)
View(welfare)
summary(welfare)

library(dplyr)

# Income difference based on sex
welfare<-rename(welfare,
  sex=h10_g3,
  birth=h10_g4,
  marriage=h10_g10,
  religion=h10_g11,
  income=p1002_8aq1,
  code_job=h10_eco9,
  code_region=h10_reg7)

welfare$sex <- ifelse(welfare$sex==0, NA, welfare$sex)
welfare$sex<-ifelse(welfare$sex==1, 'Male', 'Female')

table(is.na(welfare$income))
table(welfare$sex[is.na(welfare$income)])

welfare$income<-ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

sexIncome<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(meanIncome=mean(income))
ggplot(data=sexIncome, aes(x=sex, y=meanIncome)) +
  geom_col()

# Income difference based on age

welfare$age <- (2019 - welfare$birth)

ageIncome<-welfare %>% 
  group_by(age) %>% 
  filter(!is.na(income)) %>% 
  summarise(meanIncome=mean(income)) %>% 
  filter(!is.na(meanIncome))

ggplot(data=ageIncome, aes(x=age, y=meanIncome)) +
  geom_line() +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.minor = element_line(colour="#ABABAB")) 

welfare %>% 
  group_by(age) %>% 
  filter(!is.na(income)) %>% 
  summarise(meanIncome=mean(income)) %>% 
  filter(!is.na(meanIncome)) %>% 
  arrange(desc(meanIncome))


