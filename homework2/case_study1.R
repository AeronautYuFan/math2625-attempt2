# case study 1
library(ISwR)

data("vitcap2")

# subsets of the different groups for histogram construction
noExposure = subset(vitcap2, group == '3')
midExposure = subset(vitcap2, group == '2')
longExposure = subset(vitcap2, group == '1')

exposures = list(vitcap2 = vitcap2, 
                 longExposure = longExposure,
                 midExposure = midExposure, 
                 noExposure = noExposure)

# loop through each exposure group and get summary stats
for (group_name in names(exposures)) {
  group_data = exposures[[group_name]]
  
  vital_sd <- sd(group_data$vital.capacity)
  n <- length(group_data$vital.capacity)
  
  margin = qt(0.975, df = n - 1) * (vital_sd / sqrt(n))  # margin for 95% CI
  
  # print results
  print(paste('Group:', group_name, '( N =', n, ')')) # for future reference: paste() used to concatenate strings
  print(paste('  Mean Age:', round(mean(group_data$age), 3))) #prints mean age
  print(paste('  Mean Vital Capacity:', round(mean(group_data$vital.capacity), 3),
              '| SD:', round(vital_sd, 3)))
  print(paste('  95% CI MoE:', round(margin, 3) ))
  print(paste('  Median VC:', median(group_data$vital.capacity)) )
  print(paste('  MAD:', mad(group_data$vital.capacity)) )
  print('')
}




hist(vitcap2$vital.capacity, col = "lightblue", breaks = 20)
# 3 individuals
hist(noExposure$vital.capacity, breaks = 20, col = rgb(0.68, 0.85, 0.90, alpha = 0.5)) #lightblue
hist(midExposure$vital.capacity, breaks = 20, col = rgb(0.56, 0.93, 0.56, alpha = 0.5) , add = T)
hist(longExposure$vital.capacity, breaks = 20, col = rgb(1, 1, 0, alpha = 0.5), add = T)

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
