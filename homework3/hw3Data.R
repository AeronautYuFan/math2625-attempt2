#install.packages('epitools')
library(epitools)
#### Case Study 1 ####
##### 12 week progression #####
prog12  <- matrix(c(161, 96, 327, 148), nrow = 2, dimnames = list(Treatment = c('Ocrelizumab', 'Placebo'), Progression = c('Yes', 'No')))

chisq.test(prog12)

oddsratio.wald(prog12)



##### 24 week progression #####
prog24  <- matrix(c(144, 87, 344, 157), nrow = 2, dimnames = list(Treatment = c('Ocrelizumab', 'Placebo'), Progression = c('Yes', 'No')))

chisq.test(prog24)
oddsratio.wald(prog24)

##### 120 week worsened 25-foot walk performance #####
foot25  <- matrix(c(190, 135, 298, 109), nrow = 2, dimnames = list(Treatment = c('Ocrelizumab', 'Placebo'), Worsened = c('Yes', 'No')))

chisq.test(foot25)
oddsratio.wald(foot25)



#### Case Study 2 ####
##### Case vs First Genetic Marker, rs8034191_2 #####
first   <- matrix(c(311, 508, 165, 413, 431, 125), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), Allele = c('AA', 'AB', 'BB')))

##### Case vs Second Genetic Marker, rs1051730_1 #####
secon   <- matrix(c(307, 495, 182, 404, 440, 125), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), Allele = c('AA', 'AB', 'BB')))

##### Case vs Smoking Status #####
smoke   <- matrix(c(92, 502, 390, 161, 554, 254), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), SmokeStatus = c('Never', 'Former', 'Current')))

