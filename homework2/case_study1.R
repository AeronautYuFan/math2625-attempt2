#install.packages('DescTools')
#install.packages('Rfit')
library(ISwR) # data source

# formatting stuff (add latex font)
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff

# case study 1

data('vitcap2')

# subsets of the different groups for histogram construction
# this block creates dataframes to store the different treatment arms and also puts them in a list
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

# histogram of data
hist(vitcap2$vital.capacity, col = 'lightblue', 
     breaks = 20, ylab = 'Number of Workers',
     xlab = 'Observed Vital Capacity (Liters)',
     main = '')
# create a histogram from the total dataset

#boxplot of the different groups
boxplot(vital.capacity ~ group, data = vitcap2, 
        main = '',
        xlab = 'Vital Capacity (in Liters)', 
        ylab = 'Exposure to Cadmium', 
        col = c('lightpink', 'lightgreen', 'beige'), horizontal = T,
        names = c('Long Term', 'Short Term', 'Control'))

kruskal.test(vital.capacity ~ group, data = vitcap2)
# insignificant value found


# plotting correlation
plot(vital.capacity ~ age, data = vitcap2, 
     col = c("darkred", "darkgreen", "tan"),
     pch = 16,
     xlab = 'Age', ylab = 'Vital Capacity (Liters)', 
     main = '')

legend("topright", legend = c("Long-Term", "Mid-Term", "Control"),
       col = c("darkred", "darkgreen", "tan"), pch = 16)

#it may be useful to show this stuff too because we are examining them
plot(vital.capacity ~ age, data = longExposure, col = 'darkred',
     xlab = 'Age', ylab = 'Vital Capacity (Liters)')

plot(vital.capacity ~ age, data = midExposure, col = 'darkgreen',
     xlab = 'Age', ylab = 'Vital Capacity (Liters)')

plot(vital.capacity ~ age, data = noExposure, col = 'tan',
     xlab = 'Age', ylab = 'Vital Capacity (Liters)')






with(vitcap2, cor.test(age, vital.capacity, method = 'spearman' ))
with(noExposure, cor.test(age, vital.capacity, method = 'spearman' ))
with(midExposure, cor.test(age, vital.capacity, method = 'spearman' ))
with(longExposure, cor.test(age, vital.capacity, method = 'spearman' ))
