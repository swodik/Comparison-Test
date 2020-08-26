#input data
library(car)
library(ggpubr)
library(lsmeans)
library(multcompView)
library(lawstat)

my_data = read.csv("E:/Kuliah/github/R/Uji Banding/Two Way Anova/data.csv")
my_data$Gender = factor(my_data$Gender, levels = c("1","2"),
                       labels = c("laki-laki","perempuan"))
my_data$Dosis = factor(my_data$Dosis, levels = c("1","2"),
                       labels = c("rendah","tinggi"))

#plot
ggboxplot(my_data, x = "Gender", y = "Score", color = "Dosis",
          palette = c("#00AFBB", "#E7B800"))

ggline(my_data, x = "Gender", y = "Score", color = "Dosis",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

#two way anova using interaction
model1<-lm(Score~Dosis*Gender,data = my_data)
Anova(model1,type = "III")

#two way anova without interaction
model2<-lm(Score~Dosis+Gender,data = my_data)
Anova(model2,type = "II")

#normality test using shapiro
shapiro.test(my_data$Score)

#homogenity using levene
leveneTest(Score~Dosis*Gender, data=my_data)

#posthoc wuth interaction
posthoc<-lsmeans(model1,
                 pairwise~Dosis*Gender,
                 adjust="Tukey")
posthoc

#posthoc without interaction
posthoc2<-lsmeans(model2,
                  pairwise~Gender,
                  adjust="Tukey")
posthoc2
    