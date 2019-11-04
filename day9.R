library(tm)
myText <- c("software environment", "software  environment", "software\tenvironment")

library(stringr)
str_split(myText, "\\s+")

# sapply() : input <- list, output -> vector
sapply(str_split(myText, pattern = "\\s"), length)
lapply(str_split(myText, pattern = "\\s"), length)

sapply(str_split(myText, pattern = "\\s"), str_length)

myText <- "The 45th President of the United States, Donald Trump, 
states that he knows how to play trump with the former president"
str_split(myText, "\\s+")
myWord <- str_extract_all(myText, "\\w+")
# myWord <-unlist(str_extract_all(myText, boundary(type="word")))
myWord<-str_replace(myWord, "Trump", "Trump_unique_")
myWord<-str_replace(myWord, "States", "States_unique_")

myText <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
str_split(myText, "\\s+")
myText2 <- str_replace_all(myText, "\\d+\\s+", "")
myText2 <- str_split(myText2, "\\s+")

str_c(myText2[[1]], collapse=" ")

myText3 <- str_replace_all(myText, "\\d+\\s+", "_number_ ")
myText3 <- str_split(myText3, "\\s+")

myText <- "Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the Internet."
str_split(myText, "\\. ")
myText2 <- str_replace_all(myText, "-", " ")
myText2 <- str_replace(myText2, "[A-Z].+et al\\.\\s+\\(\\d+\\)", "_reference_")
myText2 <- str_replace(myText2, "\\.\\s*", "")

myStopWords <- "(\\bthe )|(\\ba )|(\\ban )"
myText <- c("She is an actress", "She is the actress")
str_replace_all(myText, myStopWords, "")

library(tm)
stopwords(kind="es")
stopwords(kind="SMART")

myText <- str_replace_all(myText, "(\\bam )|(\\bare )|(\\bis )|(\\bwas )|(\\bwere )|(\\bbeen )", "be ")

myStemmer.func <- function(text) {
  myText <- str_replace_all(text, "(\\bam )|(\\bare )|(\\bis )|(\\bwas )|(\\bwere )|(\\bbeen )", "be ")
  print(myText)
}
test <- c("I am a boy. You are a boy. He might be a boy.")
myStemmer.func(test)


# n-gram
mytext <- "The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
mytext <- str_replace_all(mytext, "\\bUnited States", "United_States")
myword <- unlist(str_extract_all(mytext, "\\w+"))

for (i in 1:(length(myword)-1)) {
  print(c(myword[i], myword[i+1]))
}

myTextLocation <- "papers/"
mypaper <- VCorpus(DirSource(myTextLocation))
mypaper[[1]]$content
mypaper[[1]]$meta$author
meta(mypater[[2]], tag='author') <- "G. D. Hong"

myfunc <- function(x) {
  str_extract_all(x, "\\w+[^\\w^\\s]\\w+")
}
mypuncts <- lapply(mypaper, myfunc)

mydigit <- function(x) {
  str_extract_all(x, "\\d+")
}
mydigits <- lapply(mypaper, mydigit)

myupper <- function(x) {
  str_extract_all(x, "\\b[A-Z]\\w+")
}
myuppers <- lapply(mypaper, myupper)

mycorpus <- tm_map(mypaper, removeNumbers)

removePunctuation("hello....")


library(SnowballC)
wordStem(c("learn", "learns", "learning", "states", "mathematics"))

cleaned <- tm_map(mypaper, stemDocument)
cleaned[[1]]$content
mycorpus[[1]]$content

mytempfunc <- function(corp, targ, repl){
  newobj <- tm_map(corp, 
                   content_transformer(function(x, pattern) 
                     gsub(pattern, repl, x)),
                   targ)
  print(newobj)
}

mycorpus <- mytempfunc(mycorpus,"-collar","collar")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)o-)","co")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)ross-)","cross")
mycorpus <- mytempfunc(mycorpus,"e\\.g\\.","for example")
mycorpus <- mytempfunc(mycorpus,"i\\.e\\.","that is")
mycorpus <- mytempfunc(mycorpus,"\\'s","")
mycorpus <- mytempfunc(mycorpus,"s¡¯","s")
mycorpus <- mytempfunc(mycorpus,"ICD-","ICD")
mycorpus <- mytempfunc(mycorpus,"\\b((i|I)nter-)","inter")
mycorpus <- mytempfunc(mycorpus,"K-pop","Kpop")
mycorpus <- mytempfunc(mycorpus,"\\b((m|M)eta-)","meta")
mycorpus <- mytempfunc(mycorpus,"\\b((o|O)pt-)","opt")
mycorpus <- mytempfunc(mycorpus,"\\b((p|P)ost-)","post")
mycorpus <- mytempfunc(mycorpus,"-end","end")
mycorpus <- mytempfunc(mycorpus,"\\b((w|W)ithin-)","within")
mycorpus <- mytempfunc(mycorpus,"=","is equal to")
mycorpus <- mytempfunc(mycorpus,"and/or","and or")
mycorpus <- mytempfunc(mycorpus,"his/her","his her")
mycorpus <- mytempfunc(mycorpus,"-"," ")

mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, content_transformer(tolower)) # because tolower works on str data type and mycorpus is a corpus type
mycorpus <- tm_map(mycorpus, removeWords, words=stopwords('SMART'))
mycorpus <- tm_map(mycorpus, stemDocument, language="en")


# TF/IDF
# DTM
dtm.e <- DocumentTermMatrix(mycorpus)
inspect(dtm.e[1:3, 50:60])
