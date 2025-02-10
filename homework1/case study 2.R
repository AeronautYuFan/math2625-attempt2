#install.packages('DescTools')
#install.packages('Rfit')
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = "ComputerModern", regular = "cmunrm.ttf") #for consistent formatting in LaTex
showtext_auto()

#case study 2: PDI & MDI in DCHA vs low-flow
heart   <- read.table('heart.txt', header = TRUE)
dcha    <- subset(heart, treatment == 'DCHA')
lowflow <- subset(heart, treatment == 'Low-flow')

#dcha summary statistics
summary(dcha)
sd(dcha$pdi)
sd(dcha$mdi)
mad(dcha$pdi)
mad(dcha$mdi)
hist(dcha$pdi, main = "", col = "lightblue",
     ylab = "Participants", 
     xlab = "Psychomotor Development Index (PDI)",
     family = "ComputerModern")
hist(lowflow$pdi, main = "", col = "lightgreen",
     ylab = "Participants", 
     xlab = "Psychomotor Development Index (PDI)",
     family = "ComputerModern")

#low-flow summary statistics
summary(lowflow)
sd(lowflow$pdi)
sd(lowflow$mdi)
mad(lowflow$pdi)
mad(lowflow$mdi)
hist(dcha$mdi, main = "", col = "darkblue",
     ylab = "Participants", 
     xlab = "Mental Development Index (MDI)",
     family = "ComputerModern")
hist(lowflow$mdi, main = "", col = "darkgreen",
     ylab = "Participants", 
     xlab = "Mental Development Index (MDI)",
     family = "ComputerModern")

#two sample t-testing
t.test(pdi ~ treatment, heart)
t.test(mdi ~ treatment, heart)

#t.test(dcha$pdi, lowflow$pdi) alternate code which produces the same results
#t.test(dcha$mdi, lowflow$mdi) alternate code which produces the same results

#wilcoxon rank sum testing
wilcox.test(pdi ~ treatment, data = heart, conf.int = TRUE)
wilcox.test(mdi ~ treatment, data = heart, conf.int = TRUE)
