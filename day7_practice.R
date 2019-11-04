# 1.

mypattern <- gregexpr('stat\\w+', mysentences)
regmatches(mysentences, mypattern)

# 2.


mypattern <- gregexpr('\\b\\w+\\b', tolower(mysentences))
myWords <- regmatches(tolower(mysentences), mypattern)

table(unlist(myWords))

# 3.

mypattern <- gregexpr('\\w', tolower(mysentences))
myLetters <- regmatches(tolower(mysentences), mypattern)
myLetters <- unlist(myLetters)

myLetterDF <- as.data.frame(table(myLetters))

length(myLetterDF$myLetters)


# 4.

myLetterDF %>% 
  arrange(desc(Freq)) %>% 
  head(1)

# 5.


ggplot(data=myLetterDF, aes(x=myLetters, y=Freq)) +
  geom_col()


