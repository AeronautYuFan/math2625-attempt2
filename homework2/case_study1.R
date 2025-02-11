# case study 1
library(ISwR)

data("vitcap2")

# subsets of the different groups for histogram construction
noExposure = subset(vitcap2, group == '3')
midExposure = subset(vitcap2, group == '2')
longExposure = subset(vitcap2, group == '1')

#histograms
hist(noExposure$vital.capacity, breaks = 12, col = rgb(0.68, 0.85, 0.90, alpha = 0.5)) #lightblue
hist(midExposure$vital.capacity, breaks = 12, col = rgb(0.56, 0.93, 0.56, alpha = 0.5) , add = T)
hist(longExposure$vital.capacity, breaks = 12, col = rgb(1, 1, 0, alpha = 0.5), add = T)

summary(vitcap2$vital.capacity)
summary(noExposure$vital.capacity)
summary(midExposure$vital.capacity)
summary(longExposure$vital.capacity)

sd(vitcap2$vital.capacity)
sd(longExposure$vital.capacity)
sd(midExposure$vital.capacity)
sd(noExposure$vital.capacity) 

kruskal.test(vital.capacity ~ group, data = vitcap2)

model = aov(vital.capacity ~ group, data = vitcap2)

anova(model)

plot(vital.capacity ~ age, data = vitcap2)

with(vitcap2, cor.test(age, vital.capacity, method = 'spearman' ))
with(noExposure, cor.test(age, vital.capacity, method = 'spearman' ))
with(midExposure, cor.test(age, vital.capacity, method = 'spearman' ))
with(longExposure, cor.test(age, vital.capacity, method = 'spearman' ))
