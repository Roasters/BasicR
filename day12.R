library(rvest)

url_default <- "https://movie.daum.net/moviedb/grade?movieId=111292&type=netizen&page="
htxt <- read_html(url)

review <- html_nodes(htxt, ".desc_review")

html_text(review)

library(stringr)

cnt = html_text(html_nodes(htxt, ".txt_menu"))

allReviews = c()
for (page in 1:24){
  url = paste(url_default, page, sep="")
  htmlTxt = read_html(url)
  review = html_nodes(htmlTxt, ".desc_review")
  allReviews = c(allReviews, html_text(review))
  print(page)
}

write.table(allReviews, "review.txt")


url_default = "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=161967&target=before&page="
allReviews = c()
for (page in 1:40) {
  url = paste(url_default, page, sep="")
  htmlTxt = read_html(url, encoding = "cp949")
  review = html_nodes(htmlTxt, ".list_netizen")
  review = html_nodes(review, ".title")
  reviews = html_text(review)
  allReviews = c(allReviews, reviews)
  # print(review)
}
write.table(allReviews, "review2.txt")


url = "https://namu.wiki/w/%EA%B8%B0%EC%83%9D%EC%B6%A9"
htmlTxt = read_html(url)
content = html_text(html_nodes(htmlTxt, ".wiki-heading-content"))

library(KoNLP)
useSejongDic()
unlist(extractNoun(content))

text = sapply(content, extractNoun)
text = unlist(text)

text = Filter(function(x){nchar(x)>=2}, text)
text = gsub("\\d+", "", text)

library("twitteR")
library("ROAuth")
library("base64enc")

# 트위터 계정 발급키 입력


# oauth 인증 파일 저장
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
# 콘솔 창에 1(yes) 선택

#키워드 저장
keyword <-enc2utf8("기생충")

# 크롤링할 트위터 수(n=1000)와 언어(lang="ko") 
data<- searchTwitter(keyword, n=1000, lang="ko")

length(data)
