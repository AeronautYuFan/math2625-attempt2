#install.packages('DescTools')
#install.packages('Rfit')
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = "ComputerModern", regular = "cmunrm.ttf") #for consistent formatting in LaTex
showtext_auto()

hr <- read.table('hrPaired.txt', header = TRUE)
hrDiffs <- hr$flightHR - hr$controlHR

summary(hr) #summary stats
sd(hr$controlHR)
mad(hr$controlHR)
sd(hr$flightHR)
mad(hr$flightHR)

summary(hrDiffs) #summary of differences
sd(hrDiffs)
mad(hrDiffs)

hist(hrDiffs, main = "", col = "lightgreen",
     ylab = "Number of Participants", 
     xlab = "Difference in Heart Rate",
     family = "ComputerModern")

#paired t-test -- all produce the same results, but we prefer the third for consistency in hypotheses
with(hr, t.test(flightHR, controlHR, paired = TRUE)) #another way of writing
t.test(hrDiffs) #t-test of differences
t.test(hr$flightHR, hr$controlHR, paired = TRUE) #paired t-test

#wilcoxon signed rank test
with(hr, wilcox.test(flightHR, controlHR, paired = TRUE, conf.int = TRUE))
