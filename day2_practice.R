str(midwest)

# 1)
new_mpg <- mpg %>% select(class, cty)
head(new_mpg)


# 2)
mean((mpg %>% filter(class=='suv'))$cty)
mean((mpg %>% filter(class=='compact'))$cty)


# 3)
mpg %>% 
  filter(manufacturer == "audi") %>% 
  select(hwy) %>% 
  arrange(desc(hwy)) %>% 
  head(5)

# 4)
library(ggplot2)
df <- data.frame(midwest)
str(df)
summary(df)

# 5)
library(dplyr)
df <- rename(df, replace = c("poptotal"="total"))
df <- rename(df, replace = c("popasian"="asian"))

# 6) 
df$portion = (df$asian/df$total) * 100
qplot(df$portion, geom='histogram')

# 7)
portion_mean = mean(df$portion)
df$size <- ifelse(df$portion>portion_mean, "large", "small")

# 8)
qplot(df$size)
table(df$size)
