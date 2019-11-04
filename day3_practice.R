library(ggplot2)

mpg_new <- mpg

# 1-1)

mpg_new$sumHC <- mpg_new$hwy + mpg_new$cty

# 1-2)

mpg_new$avgHC <- mpg_new$sumHC/2

# 1-3)

mpg_new %>% 
  group_by(model) %>% 
  summarise(meanHC=mean(avgHC)) %>% 
  arrange(desc(meanHC)) %>% 
  head(3)

# 1-4)

library(dplyr)

mpg %>% 
  mutate(sumHC=hwy+cty) %>% 
  mutate(avgHC=sumHC/2) %>% 
  group_by(model) %>% 
  summarise(meanHC=mean(avgHC)) %>% 
  arrange(desc(meanHC)) %>% 
  head(3)

# 1-5)

mpg %>% 
  group_by(class) %>% 
  summarise(meanCTY=mean(cty))

# 1-6)

mpg %>% 
  group_by(class) %>% 
  summarise(meanCTY=mean(cty)) %>% 
  arrange(desc(meanCTY))

# 1-7)

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(meanHWY=mean(hwy)) %>% 
  arrange(desc(meanHWY)) %>% 
  head(3)

# 1-8)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="compact") %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

# 2-1)

midwest$kidratio <- (1 - (midwest$popadults / midwest$poptotal)) * 100

# 2-2)

midwest %>% 
  group_by(county) %>% 
  arrange(desc(kidratio)) %>% 
  select(kidratio) %>% 
  head(5)

# 2-3)

midwest$grade <- ifelse(midwest$kidratio>=40, "large", 
                        ifelse(midwest$kidratio>=30, "middle", "small"))
table(midwest$grade)

# 2-4)

midwest$asianratio <- (midwest$popasian / midwest$poptotal) * 100

midwest %>% 
  arrange(asianratio) %>% 
  select(c(state, county, asianratio))

# 3)

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA 

# 3-1)

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# 3-2)

mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(meanHWY=mean(hwy)) %>% 
  arrange(desc(meanHWY))

# 4)

mpg <- as.data.frame(ggplot2::mpg) 
mpg[c(10, 14, 58, 93), "drv"] <- "k" 
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) 

# 4-1)

mpg$drv <- ifelse(mpg$drv %in% c("f", "r", "4"), mpg$drv, NA) 

# 4-2)

boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty>26 | mpg$cty<9, NA, mpg$cty)
boxplot(mpg$cty)

# 4-3)

mpg %>% 
  na.omit(drv) %>%
  na.omit(cty) %>% 
  group_by(drv) %>% 
  summarise(meanCTY=mean(cty))
