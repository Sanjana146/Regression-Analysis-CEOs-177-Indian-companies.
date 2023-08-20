require(coefplot)
library(car)
install.packages("readxl")
library("readxl")

salary<-read_excel("C:\\Users\\sanjana\\OneDrive\\Desktop\\SNU MONSOON 2022 COURSES\\DOM 207\\DOM207 MPSS\\DOM207MINIPROJECT3-SANJANA ADITYA\\SalaryData.xlsx")
head(salary)

salarys1<-lm(Salary~College+Grad+Age+Tenure+CeoTen+Sales+Profits+Mktval+Profmarg,data=salary)
summary(salarys1)

#CHECKING FOR NORMALCY
shapiro.test(salary$Age)
#Age is not rejected as normal.
shapiro.test(salary$Tenure)
shapiro.test(log(salary$Tenure))
shapiro.test(1/salary$Tenure)
#Tenure is not normal and is not transforming to Normal
shapiro.test(salary$CeoTen)
shapiro.test(log(salary$CeoTen+1))
#CeoTen initially rejected as not normal has been transformed to normal. 
shapiro.test(salary$Sales)
shapiro.test(log(salary$Sales))
#Salary initially rejected as not normal has been transformed to normal.
shapiro.test(salary$Profits)
shapiro.test(log(salary$Profits+463+1))
shapiro.test(sqrt(salary$Profits+463+1))
shapiro.test((salary$Profits+463+1)^2)
#Profits is not normal and is not transforming to Normal
shapiro.test(salary$Mktval)
shapiro.test(log(salary$Mktval))
shapiro.test(sqrt(salary$Mktval))
shapiro.test((salary$Mktval)^2)
shapiro.test(log10(salary$Mktval))
#Mktval is not normal and is not transforming to Normal
shapiro.test(salary$Profmarg)
shapiro.test(log(salary$Profmarg))
shapiro.test(log(salary$Profmarg+1))
shapiro.test(log(salary$Profmarg+1+203.0769))
shapiro.test(log10(salary$Profmarg+1+203.0769))
shapiro.test((salary$Profmarg+1+203.0769)^2)
#Profmarg is not normal and is not transforming to Normal

salarys1<-lm(log(Salary)~College+Grad+Age+Tenure+log(CeoTen+1)+log(Sales)+Mktval+Profmarg+Profits,data=salary)
summary(salarys1)

vif(salarys1)
cor(salary)


salarys1<-lm(log(Salary)~College+Grad+Age+Tenure+log(CeoTen+1)+log(Sales)+Profmarg,data=salary)
summary(salarys1)
vif(salarys1)
salarys1<-lm(log(Salary)~College+Grad+Age+Tenure+log(CeoTen+1)+log(Sales)+Profits+Profmarg,data=salary)
summary(salarys1)
vif(salarys1)
salarys1<-lm(log(Salary)~College+Grad+Age+Tenure+log(CeoTen+1)+log(Sales)+Mktval+Profmarg,data=salary)
summary(salarys1)
vif(salarys1)

salarys1<-lm(log(Salary)~College+Grad+Tenure+log(CeoTen+1)+log(Sales)+Mktval+Age,data=salary)
summary(salarys1)
salarys1<-lm(log(Salary)~College+Grad+Tenure+log(CeoTen+1)+log(Sales)+Mktval+Profmarg,data=salary)
summary(salarys1)

#FINAL RESULT
salarys1<-lm(log(Salary)~College+Grad+Tenure+log(CeoTen+1)+log(Sales)+Mktval+Profmarg,data=salary)
summary(salarys1)
coef(salarys1)
coefplot(salarys1)
