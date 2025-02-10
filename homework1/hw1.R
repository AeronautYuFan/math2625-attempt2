# biostats hw 1 code

#case study 1




#case study 2
data2 = read.table("heart.txt", header=TRUE)
head(data2)

dcha    <- subset(data2, treatment == 'DCHA')
lowflow <- subset(data2, treatment == 'Low-flow')

summary(dcha$pdi)
summary(lowflow$pdi)

t.test(dcha$pdi, lowflow$pdi) # p = 0.0264
# sensitivity using mann-whitney u test:
wilcox.test(dcha$pdi, lowflow$pdi) # p = 0.0189


t.test(dcha$mdi, lowflow$mdi)

wilcox.test(dcha$mdi, lowflow$mdi)


