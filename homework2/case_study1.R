# case study 1
library(ISwR)

data("vitcap2")

# subsets of the different groups for histogram construction
noExposure = subset(vitcap2, group == "3")
midExposure = subset(vitcap2, group == "2")
longExposure = subset(vitcap2, group == "1")

#histograms
hist(noExposure$vital.capacity, breaks = 12, col = rgb(0.68, 0.85, 0.90, alpha = 0.5)) #lightblue
hist(midExposure$vital.capacity, breaks = 12, col = rgb(0.56, 0.93, 0.56, alpha = 0.5) , add = T)
hist(longExposure$vital.capacity, breaks = 12, col = rgb(1, 1, 0, alpha = 0.5), add = T)

summary(noExposure)
summary(midExposure)
summary(longExposure)

# replace this with variance if possible but it appears variances are unequal
# so we have to use kruskall wallis over anova
sd(noExposure$vital.capacity) 
sd(longExposure$vital.capacity)
sd(midExposure$vital.capacity)

kruskal.test(vital.capacity ~ group, data = vitcap2)

model = aov(vital.capacity ~ group, data = vitcap2)

anova(model)
