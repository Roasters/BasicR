library(RWeka)
library(tm)
mytext <- c("The United States comprises fifty states.",  "In the United States, each state has its own laws.", "However, federal law overrides state law in the United States.")
mytemp <- VCorpus(VectorSource(mytext)) # from a vector
#VCorpus(DirSource("Path"))
ngram.tdm <- TermDocumentMatrix(mytemp)

bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

ngram.tdm <- TermDocumentMatrix(mytemp, control=list(tokenize=bigramTokenizer))
ngram.tdm$dimnames$Terms

# Frequency of each word occured in all the docs
bigramList <- apply(ngram.tdm[,], 1, sum) # margin=1 : compute by rows
sort(bigramList, decreasing = T)


library(rJava)
library(KoNLP)
library(stringr)

mytextLocation <- "논문/"
mypaper <- VCorpus(DirSource(mytextLocation))
myKorean <- mypaper[[19]]$content

mytext <- str_replace_all(myKorean, "\\([A-Za-z]+\\)", "")
mytext <- str_replace_all(mytext, "·|‘|’", "")
noun.mytext <- extractNoun(mytext)


numExtract <- function(text) {
  str_extract_all(text, "\\d+")
}
mydigits <- lapply(mypaper, numExtract)

mycorpus <- tm_map(mypaper, removeNumbers)

mytempfunct <- function(myobj, oldexp, newexp) {
  tm_map(myobj, content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp) # x <- myobj, pattern <- oldexp
}
mycorpus <- mytempfunct(mycorpus, "\\(|[A-Za-z]+|\\)|·|‘|’|-|_|\\?|/|\\.|,", "")

paste(extractNoun(mycorpus[[1]]$content), collapse=" ")

myNounFun<-function(mytext){
  myNounList<-paste(extractNoun(mytext),
                    collapse = " ")
  return(myNounList)
  #print(myNounList)
}
#mycorpus[[3]]$content
myNounListRes<-myNounFun(mycorpus[[3]]$content)
myNounCorpus <- mycorpus
for (i in 1:length(mycorpus)) {
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

imsi <- myNounCorpus
for (i in 1:length(myNounCorpus)) {
  myNounCorpus[[i]]$content <- str_replace_all(imsi[[i]]$content, "커뮤니\\w+", "커뮤니케이션")
}

dtm.k <- DocumentTermMatrix(myNounCorpus)
dtm.k
imsi <- myNounCorpus
for (i in 1:length(myNounCorpus)) {
  myNounCorpus[[i]]$content <- str_replace_all(imsi[[i]]$content, "위키리크스\\w+", "위키리크스")
}

dtm.k <- DocumentTermMatrix(myNounCorpus)
colnames(dtm.k)

word.freq <- apply(dtm.k[,], 2, sum)
sort.word.freq <- sort(word.freq, decreasing = T)
cumsum.word.freq <- cumsum(sort.word.freq)
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
prop.word.freq[20]

plot(1:length(word.freq), prop.word.freq, type='l')


library(wordcloud)
library(RColorBrewer)
mypal <- brewer.pal(8, "Dark2")
wordcloud(names(word.freq), freq=word.freq, min.freq = 5, col=mypal, random.order=FALSE, scale=c(4,0.2))
          

# KMeans Clustering

teens <- read.csv("sns.csv")
table(teens$gender)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
