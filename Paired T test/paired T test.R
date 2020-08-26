my_data = read.csv("E:/Kuliah/github/R/Uji Banding/Paired T test/data.csv")
my_data$group = factor(my_data$group, levels = c("sebelum","sesudah"), 
                       labels = c("sebelum","sesudah"))
summary(my_data)

#mean and standard deviation
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

#plot
library(ggpubr)
ggboxplot(my_data, x = "group", y = "value", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("sebelum", "sesudah"),
          ylab = "value", xlab = "Groups")

library(PairedData)
# Subset value before treatment
sebelum <- subset(my_data,  group == "sebelum", value,
                 drop = TRUE)
# subset weight data after treatment
sesudah <- subset(my_data,  group == "sesudah", value,
                drop = TRUE)

# Plot paired data
pd <- paired(sebelum, sesudah)
plot(pd, type = "profile") + theme_bw()

#normality test with shapiro.test
d = with(my_data, value[group == "sebelum"] - value[group =="sesudah"])
shapiro.test(d)

#t.test with 2 side
result = t.test(sebelum,sesudah, paired = TRUE)
result

#t.test with 1 side
result2 = t.test(sebelum,sesudah, paired = TRUE, alternative = "greater")
result3 = t.test(sebelum,sesudah, paired = TRUE, alternative = "less")
result2
result3