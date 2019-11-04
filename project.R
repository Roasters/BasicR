library(rvest)
library(stringr)
library(tm)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyr)

url_default = "https://www.reddit.com"
htxt = read_html("https://www.reddit.com/r/MachineLearning/hot/")
links = htxt %>% html_nodes(".SQnoC3ObvgnGjWt90zD9Z") %>% html_attr("href")

allComments = c()
for (link in links) {
  url = paste(url_default, link, sep="")
  htmlTxt = read_html(url)
  comment = html_nodes(htmlTxt, ".rz6fp9-10")
  allComments = c(allComments, html_text(comment))
}

my.df.text<-data_frame(comment.id=1:length(allComments), doc=allComments)
my.df.text.word<-my.df.text %>% 
  unnest_tokens(word, doc)

temp = tolower(my.df.text.word$word)
# words = unlist(str_split(text, pattern=" "))
words_meaningful = ifelse(temp %in% stopwords(kind="SMART"), NA, temp)
my_words = str_replace_all(words_meaningful, "[^a-z]+", "")
my_words = ifelse(my_words == "", NA, my_words)
my.df.text.word$word = my_words
table_df = data.frame(table(my.df.text.word$word))
table_df = table_df[words_df$Freq >= 4, ]

cmap = brewer.pal(8, "Pastel1")
wordcloud(table_df$Var1, freq=table_df$Freq, min.freq = 4, col = cmap, random.order = FALSE, scale=c(4,0.2))

myresult.sa<-my.df.text.word %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, comment.id, sentiment) %>% 
  spread(sentiment,n,fill=0)
myresult.sa

myagg<-summarise(group_by(myresult.sa, comment.id),
                 pos.sum=sum(positive),
                 neg.sum=sum(negative),
                 pos.sent=pos.sum-neg.sum)
myagg
