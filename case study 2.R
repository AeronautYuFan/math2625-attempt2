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
hist(dcha$pdi)
hist(lowflow$pdi)

#low-flow summary statistics
summary(lowflow)
sd(lowflow$pdi)
sd(lowflow$mdi)
mad(lowflow$pdi)
mad(lowflow$mdi)
hist(dcha$mdi)
hist(lowflow$mdi)

#two sample t-testing
t.test(pdi ~ treatment, heart)
t.test(mdi ~ treatment, heart)

#t.test(dcha$pdi, lowflow$pdi) alternate code which produces the same results
#t.test(dcha$mdi, lowflow$mdi) alternate code which produces the same results

#wilcoxon rank sum testing
wilcox.test(pdi ~ treatment, data = heart, conf.int = TRUE)
wilcox.test(mdi ~ treatment, data = heart, conf.int = TRUE)
