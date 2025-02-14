# formatting stuff (add latex font)
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff

cf <- read.table('cf.txt', header = TRUE)

# for separating dataframes and parsing through to get summaries
vLow = subset(cf, bmp == 'Very Low')
low = subset(cf, bmp == 'Low')
norm = subset(cf, bmp == 'Near Normal')
bodyMasses = list(all = cf, veryLow = vLow, someLow = low, nearNorm = norm )

#loop for summary stats
for (group_name in names(bodyMasses)) {
  
  #mean age
  #mean wt
  #median wt
  #mean fev1
  #median fev1
  group_data = bodyMasses[[group_name]]
  wt_sd <- sd(group_data$weight)
  fev_sd = sd(group_data$fev1)
  n <- length(group_data$age)
  wt_margin = qt(0.975, df = n - 1) * (wt_sd / sqrt(n))
  fev_margin = qt(0.975, df = n - 1) * (fev_sd / sqrt(n))
  
  print(paste('Group:', group_name, '( N =', n, ')')) # for future reference: paste() used to concatenate strings
  print(paste('  Mean Age:', round(mean(group_data$age), 3))) #prints mean age
  print(paste('  Mean Weight:', round(mean(group_data$weight), 3)))
  print(paste('  Weight 95% CI:', round(wt_margin, 3)))
  print(paste('  Median Weight:', round(median(group_data$weight), 3)))
  
  print('')
  print(paste('  Mean FEV:', round(mean(group_data$fev1), 3)))
  print(paste('  FEV 95% CI:', round(fev_margin, 3)))
  print(paste('  Median FEV:', round(median(group_data$fev1), 3)))
  print('')
  
}

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

hist(cf$weight, col = 'lightblue',
     xlab = 'Weight (kg)', ylab = 'Number of Participants',
     main = '')

hist(cf$fev1, col = 'beige',
     xlab = 'Forced Expiratory Volume (liters)', ylab = 'Number of Participants',
     main = '')

boxplot(weight ~ bmp, data = cf, 
        main = '',
        xlab = 'Weight (kg)', 
        ylab = 'BMP Level', 
        col = c('lightpink', 'beige', 'lightgreen'), horizontal = T,
        names = c('Very Low', 'Low', 'Near Normal'))

boxplot(fev1 ~ bmp, data = cf, 
        main = '',
        xlab = 'Forced Expiratory Volume (liters)', 
        ylab = 'BMP Level', 
        col = c('lightpink', 'beige', 'lightgreen'), horizontal = T,
        names = c('Very Low', 'Low', 'Near Normal'))

#spearmanns
with(cf, cor.test(fev1, weight, method = 'spearman'))

with(cf, cor.test(fev1, age, method = 'spearman'))
with(cf, cor.test(age, weight, method = 'spearman'))

#kendalls
with(cf, cor.test(fev1, weight, method = 'kendall'))

with(cf, cor.test(fev1, age, method = 'kendall'))
with(cf, cor.test(age, weight, method = 'kendall'))
