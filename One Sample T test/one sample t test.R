library(ggpubr)
my_data = read.csv("E:/Kuliah/github/R/Uji Banding/One Sample T test/data.csv")

ggboxplot(my_data$Bakteri, 
          ylab = "bakteri", xlab = FALSE,
          ggtheme = theme_minimal())

#normality test
shapiro.test(my_data$Bakteri)

ggqqplot(my_data$Bakteri, ylab = "Bakteri",
         ggtheme = theme_minimal())

#One sample T test
result <- t.test(my_data$Bakteri, mu = 200)
result
