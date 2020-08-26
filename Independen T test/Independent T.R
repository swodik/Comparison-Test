library(dplyr)
my_data = read.csv("E:/Kuliah/github/R/Uji Banding/Independen/data.csv")
my_data$group = factor(my_data$group, levels = c("1","2"), labels = c("suntik","pil"))
summary(my_data)

#mean and standard deviation 2 groups
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(nilai, na.rm = TRUE),
    sd = sd(nilai, na.rm = TRUE)
  )
#Visualization
library(ggpubr)
ggboxplot(my_data, x = "group", y = "nilai", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "nilai", xlab = "Groups")

#Normality Test with shapiro
shapiro.test(my_data$nilai)
with(my_data, shapiro.test(nilai[group == "suntik"]))
with(my_data, shapiro.test(nilai[group == "pil"]))

#Homogenity Test with levene test
library(lawstat)
levene.test(my_data$nilai, group = my_data$group)

#Homogenity Test with var.test
vari.test = var.test(nilai~ group, data = my_data)
vari.test

#Independent T test Two side
result = t.test(nilai~group, data = my_data, var.equal = TRUE)
result

#Independent T test One side
result2 = t.test(nilai~group, data = my_data, var.equal = TRUE, 
                 alternative = "greater")
result3 = t.test(nilai~group, data = my_data, var.equal = TRUE, 
                 alternative = "less")
result2
result3