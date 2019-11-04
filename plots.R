setwd("C:/Users/SJH/Downloads")
dataset <- read.csv("data.csv", header=TRUE)

# setEPS()
# postscript("LinMod.eps")
# plot(dataset$p_mean, dataset$q_mean, pch = 20, col = "grey60", xlab = "Quality of Life", ylab = "Smartphone Addiction")
# abline(a = fit1$coefficients[1], b = fit1$coefficients[2], lwd = 2)
# dev.off()

plot(dataset$p_mean, dataset$q_mean, pch = 20, col = "grey60", xlab = "Quality of Life", ylab = "Smartphone Addiction")
abline(a = fit1$coefficients[1], b = fit1$coefficients[2], lwd = 2)
ggplot(data=dataset, aes(dataset$q_mean)) + 
geom_histogram(aes(fill=..count..), col="#f0b27a") + 
scale_fill_gradient("Score", low="#FFA07A", high="#b03a2e") + 
theme(panel.background = element_rect(fill = '#ffffff'))


ggplot(dataset2, aes(x=sex, y = count)) +
  ylim(0, 40) +
  geom_segment( aes(x=sex, xend=sex, y=0, yend=count), color="grey", size=1) +
  geom_point( color="orange", size=5) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Count") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
dataset3 = data.frame(grade = c("1st", "2nd", "3rd", "4th"), count = c(5, 5, 21, 19))
ggplot(dataset3, aes(x=grade, y = count)) +
  ylim(0, 25) +
  geom_segment( aes(x=grade, xend=grade, y=0, yend=count), color="grey", size=1) +
  geom_point( color="orange", size=5) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Count") +
  theme(axis.text=element_text(size=12, face= "bold"),
        axis.title=element_text(size=14,face="bold"))

ggplot(dataset, aes(x = grade)) + geom_bar(aes(fill=..count..)) +
  ylab("Counts") +
  scale_x_discrete("Grades", breaks = 1:4, labels=c("1st", "2nd", "3rd", "4th"), limits = 1:4) + 
  scale_fill_gradient("Counts", low="#f39c12", high="#d68910") +
  theme(axis.text=element_text(size=12, face= "bold"),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.minor = element_line(colour="#f1f1f1"))

ggplot(dataset, aes(x=as.factor(sex))) + geom_bar()

