library(showtext)
library(DescTools)
library(Rfit)
font_add(family = "ComputerModern", regular = "cmunrm.ttf")
showtext_auto()
#some of the code is for changing fonts. not necessary,
#but it looks gourgeous in latex so why not

hr <- read.table('hrPaired.txt', header = TRUE)
hrDiffs <- hr$flightHR - hr$controlHR

summary(hr)
sd(hr$controlHR)
sd(hr$flightHR)
summary(hrDiffs)
sd(hrDiffs)

hist(hrDiffs, main = "", col = "lightgreen",
     ylab = "Number of Participants", 
     xlab = "Difference in Heart Rate",
     family = "ComputerModern")
#hist(hr)

with(hr, t.test(flightHR, controlHR, paired = TRUE)) #another way of writing paired t test
t.test(hrDiffs) #t test of differences
t.test(hr$flightHR, hr$controlHR, paired = TRUE) #paired t test

#wilcoxon sign test
with(hr, wilcox.test(flightHR, controlHR, paired = TRUE, conf.int = TRUE)) #wilcoxon sign test

HodgesLehmann(hrDiffs)
