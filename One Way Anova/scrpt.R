my_data = read.csv("E:/Kuliah/github/R/Uji Banding/One Way/data.csv")
my_data$Metode = factor(my_data$Metode, levels = c("1","2","3"), 
                       labels = c("aromaterapi","Merendam kaki","kombinasi"))

class(my_data$Metode)
group1<-subset(my_data,group = "aromaterapi")
group2<-subset(my_data, group = "Merendam kaki")
group2<-subset(my_data, group= "kombinasi")

#normality test aromaterapi method
shapiro.test(group1$Tekanan.Darah)
qqnorm(group1$Tekanan.Darah)
qqline(group1$Tekanan.Darah)

#normality test merndam kaki method
shapiro.test(group2$Tekanan.Darah)
qqnorm(group2$Tekanan.Darah)
qqline(group2$Tekanan.Darah)

#normality test kombinasi method
shapiro.test(group3$Tekanan.Darah)
qqnorm(group3$Tekanan.Darah)
qqline(group3$Tekanan.Darah)

#homogenity test using bartlett
bartlett.test(Tekanan.Darah~ Metode, data = my_data)

#homogenity test using levene
library(lawstat)
levene.test(Tekanan.Darah, group = Metode, data = my_data)

#Oneway Anova
model1 <- lm(Tekanan.Darah~ Metode, data = my_data)
anova(model1)
