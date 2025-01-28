#case study 2
#PDI in DCHA vs low-flow
#MDI in DCHA to low-flow
heart   <- read.table('heart.txt', header = TRUE)
dcha    <- subset(heart, treatment == 'DCHA')
lowflow <- subset(heart, treatment == 'Low-flow')

summary(dcha)
sd(dcha$pdi)
sd(dcha$mdi)
mad(dcha$pdi)
mad(dcha$mdi)
hist(dcha$pdi)
hist(lowflow$pdi)

summary(lowflow)
sd(lowflow$pdi)
sd(lowflow$mdi)
mad(lowflow$pdi)
mad(lowflow$mdi)
hist(dcha$mdi)
hist(lowflow$mdi)

t.test(pdi ~ treatment, heart)
t.test(mdi ~ treatment, heart)

#t.test(dcha$pdi, lowflow$pdi)
#t.test(dcha$mdi, lowflow$mdi)

#t1 <- sum((dcha$pdi)*rank(abs(heart$pdi)))
#t2 <- sum((lowflow$pdi)*rank(abs(heart$pdi)))

wilcox.test(pdi ~ treatment, data = heart, conf.int = TRUE)
wilcox.test(mdi ~ treatment, data = heart, conf.int = TRUE)
