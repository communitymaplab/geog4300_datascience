#
#
#
#
#
#
#
#
#| label: setup
library(tidyverse)
#
#
#
#
#
test1<-data.frame(var1=rbeta(5000,7,2))
test2<-data.frame(var2=rnorm(5000,0,1))
#
#
#
#
#
ggplot(test1,aes(x=var1)) + 
  geom_histogram()


#
#
#
#
#


ggplot(test2,aes(x=var2)) + 
  geom_histogram()
 

ggplot(test1,aes(sample=var1)) +
  stat_qq()+
  stat_qq_line()

ggplot(test2,aes(sample=var2)) +
  stat_qq()+
  stat_qq_line()
#
#
#
#
#
shapiro.test(test1$var1) #skewed data
shapiro.test(test2$var2) #normal data
#
#
#
#
#
#
acsdata<-read_csv("data/ACSCtyData_2022ACS.csv")


#
#
#
#
#
