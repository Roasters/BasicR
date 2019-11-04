r_wiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
library(stringr)

str_extract(r_wiki, "software environment")

str_extract_all(r_wiki, "software environment")

#unlist함수 적용효과 => simplify = TRUE
str_extract_all(r_wiki, "software environment", simplify = TRUE)

#첫글자가 대문자로 시작되는 단어들을 추출
myextract<-str_extract_all(r_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
table(myextract)

#등장 위치
str_locate(r_wiki,"software environment")
str_locate_all(r_wiki,"software environment")

#첫글자가 대문자로 시작...단어 위치?
mylocate<-str_locate_all(r_wiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
head(mylocate)
dim(mylocate[[1]])

class(mylocate[[1]])
mydata<-data.frame(mylocate[[1]])
mydata

myextract<-str_extract_all(r_wiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
myextract

mydata$myword<-myextract[[1]]
mydata

mydata$myword.length <- mydata$end - mydata$start +1
mydata

#"software environment" =>software_environment 
str_replace(r_wiki,"software environment","software_environment")
temp<-str_replace_all(r_wiki,"software environment","software_environment")
temp

table(str_extract_all(r_wiki,"software_environment|software|environment"))
table(str_extract_all(temp,"software_environment|software|environment"))

r_wiki
temp<-str_replace_all(r_wiki,"R\\b",
                      "R_computer.language_")
temp<-str_replace_all(r_wiki,"C\\b",
                      "C_computer.language_")

temp
#\\b : ~시작 또는 ~끝
#R로 끝나는...

r_wiki_para<-str_split(r_wiki, "\n")
class(r_wiki_para)
r_wiki_sent<-str_split(r_wiki_para[[1]], "\\. ")
#class(r_wiki_para[[1]])

my2sentences<-unlist(r_wiki_sent)[c(4,7)]
#my2sentences
#각 문장의 단어수를 출력
my2sentences[1] #5개
my2sentences[2]

mylength1<-length(unlist(str_split(
  my2sentences[1], " ")))
mylength2<-length(unlist(str_split(
  my2sentences[2], " ")))
mylength1; mylength2

my2sentences
myfixed.short<-str_split_fixed(my2sentences, " ", 5)
myfixed.long<-str_split_fixed(my2sentences, " ", 13)
myfixed.long



length.sentences<-rep(NA,
                      length(unlist(r_wiki_sent)))


#반복문을 사용하여 단어수를 계산
for(i in 1:length(length.sentences)){
  length.sentences[i]<-length(unlist(str_split(unlist(r_wiki_sent)[i]," ")))
}
length.sentences

max.length.sentences<-max(length.sentences)
#최대단어수 기준으로 문장*단어 행렬 구성
#7*21 행렬
#----------------------------------------
#        word1   word2 . . .  word21
# sent1    R      is   
# ...
# sent7

#최대 단어수 기준 행렬
sent.word.matrix<-str_split_fixed(unlist(r_wiki_sent), " ", max.length.sentences)
class(sent.word.matrix)
mydata<-data.frame(sent.word.matrix)
head(mydata)

#열 이름을 word1~word21로 변경
#행 이름을 sent.1~sent.7로 변경
#paste함수

rownames(mydata)<-paste('sent', 1:length(unlist(r_wiki_sent)), sep=".")
mydata
colnames(mydata)<-paste('word', 1:max.length.sentences, sep=".")
mydata

mydata[,1]
mydata[3,1:10]

str_count(r_wiki, "R")
str_count(r_wiki_para[[1]],"R")
str_count(unlist(r_wiki_sent),"R")

str_count(unlist(r_wiki_sent),"(s|S)tat[[:lower:]]{1,}")
r_wiki_sent

name<-c("Joe", "Jack", "Jackie", "Jefferson")
donation<-c("$1","$111","$11111","$111111")
mydata<-data.frame(name,donation)
mydata

name2<-str_pad(mydata$name, width = 15, side="right", pad=" ")
donation2<-str_pad(mydata$name, width = 15, side="both", pad="~")
mydata2<-data.frame(name2,donation2)
mydata2

rep("soft", 3)
str_dup("soft",3)

str_sub(unlist(r_wiki_sent)[1],1,30)

#글자 수 계산
str_length(unlist(r_wiki_sent))
nchar(unlist(r_wiki_sent))

#패딩 제거
name3<-str_trim(mydata2$name2,side='right')
donation3<-str_trim(str_replace_all(mydata2$donation2,'~',' '), side='both')
mydata3<-data.frame(name3,donation3)
mydata3

#문자열 데이터 전처리
#품사 분석, 감성 분석
#웹사이트 텍스트 추출(스크래핑) -> 감성분석
#A회사->제품 댓글 추출->데이터 전처리->감성 분석

#Corpus(말뭉치:분석가가 관심갖는 영역(domain,법률,의료,...)의 텍스트 문서 집합)텍스트 데이터 사전처리
library(tm)

#공란처리























#1.연습
#2.r_wiki 데이터에서 'So'라는 표현 추출







