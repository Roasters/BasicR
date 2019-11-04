dataset <- read.csv("result.csv", header=TRUE)
library(ggplot2)

#QOL
ggplot(data=dataset, aes(dataset$DOM_MEAN)) + 
  geom_histogram(aes(fill=..count..), col="#08539d") +
  scale_fill_gradient("Counts", low="#ffffff", high="#d35400") + 
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.y = element_line(colour="#f1f1f1"),
        axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  xlab("Quality of Life") +
  ylab("Counts")

# Smartphone
ggplot(data=dataset, aes(dataset$s_mean)) + 
  geom_histogram(aes(fill=..count..), col="#34495e") +
  scale_fill_gradient("", low="#ffffff", high="#138d75") + 
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.y = element_line(colour="#f1f1f1"),
        axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  xlab("Smartphone Addiction") +
  ylab("Counts")

# Interpersonal
ggplot(data=dataset, aes(dataset$p_mean)) + 
  geom_histogram(aes(fill=..count..), col="#e74c3c") +
  scale_fill_gradient("", low="#ffffff", high="#273746") + 
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.y = element_line(colour="#f1f1f1"),
        axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  xlab("Interpersonal Stress") +
  ylab("Counts")
  

ggplot(dataset, aes(x=p_mean, y=DOM_MEAN)) + 
  geom_point(color="#2471a3", fill="#f4d03f", size=5, shape=21) + 
  # geom_smooth(method="lm", se=FALSE, fullrange=FALSE, level=0.95, col="#cb4335") +
  geom_abline(intercept = 15.212, slope=-1.226, colour="#2471a3", linetype='solid', size=1.5) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.minor = element_line(colour="#f1f1f1")) +
  xlab("Interpersonal Stress") +
  ylab("Quality of Life") +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major = element_line(colour="#f1f1f1"),
        axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold"))
#reg: smartphone
ggplot(dataset, aes(x=s_mean, y=DOM_MEAN)) + 
  geom_point(color="#e74c3c", fill="#34495e", size=5, shape=21) + 
  # geom_smooth(method="lm", se=FALSE, fullrange=FALSE, level=0.95, col="#cb4335") +
  geom_abline(intercept = 17.229, slope=-1.314, colour="#34495e", linetype='solid', size=1.5) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.minor = element_line(colour="#f1f1f1")) +
  xlab("Smartphone Addiction") +
  ylab("Quality of Life") +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major = element_line(colour="#f1f1f1"),
        axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold"))

# Grade
ggplot(dataset, aes(x = grade)) + geom_bar(aes(fill=..count..), col = "#cb4335", fill =  "#fdedec") +
  ylab("Counts") +
  scale_x_discrete("Grades", breaks = 1:4, labels=c("1st", "2nd", "3rd", "4th"), limits = 1:4) + 
  #scale_fill_gradient("Counts", low="#2980b9", high="#34495e") +
  theme(axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.y = element_line(colour="#f1f1f1"))

# Sex
ggplot(dataset, aes(x = sex)) + geom_bar(aes(fill=..count..), col = "#34495e", fill = "#ebf5fb") +
  ylab("Counts") +
  xlab("")+
  ylim(0, 40) +
  scale_x_discrete(breaks = 1:2, labels=c("Male", "Female"), limits = 1:2) + 
  #scale_fill_gradient(low="#138d75", high="#34495e") +
  theme(axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.minor = element_line(colour="#f1f1f1"))

# Phone
ggplot(dataset, aes(x = phone)) + geom_bar(aes(fill=..count..), col = "#1e8449", fill = "#e9f7ef") +
  ylab("Counts") +
  xlab("Hours") +
  ylim(0, 25) +
  scale_x_discrete(breaks = 1:5, labels=c("0 ~ 2", "2 ~ 4", "4 ~ 6", "6 ~ 8", "8 ~"), limits = 1:5) + 
  #scale_fill_gradient(low="#f39c12", high="#d35400") +
  theme(axis.text=element_text(size=20, face= "bold"),
        axis.title=element_text(size=22,face="bold")) +
  theme(panel.background = element_rect(fill = '#ffffff'),
        panel.grid.major.x = element_line(colour="#f1f1f1")) +
  coord_flip()
