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


#install.packages('epitools')
library(epitools)

#### Case Study 2 ####
##### Case vs First Genetic Marker, rs8034191_2 #####
first   <- matrix(c(311, 508, 165, 413, 431, 125), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), Allele = c('AA', 'AB', 'BB')))
chisq.test(first)

# AB AA comparison (AA in first col)
ab_aa_comp = matrix(c(first[1,1], first[1,2], first[2,1], first[2,2]), nrow = 2)

# BB AA comp (AA in first col)
bb_aa_comp = matrix(c(first[1,1], first[1,3], first[2,1], first[2,3]), nrow = 2)

ab_bb_comp = matrix(c(first[2,1], first[2,3], first[2,1], first[2,3]), nrow = 2)

oddsratio(ab_aa_comp, conf.level = 0.975)

oddsratio(bb_aa_comp, conf.level = 0.975)



##### Case vs Second Genetic Marker, rs1051730_1 #####
secon   <- matrix(c(307, 495, 182, 404, 440, 125), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), Allele = c('AA', 'AB', 'BB')))

chisq.test(secon)

# AB AA comparison (AA in first col)
ab_aa_comp2nd = matrix(c(secon[1,1], secon[1,2], secon[2,1], secon[2,2]), nrow = 2)

# BB AA comp (AA in first col)
bb_aa_comp2nd = matrix(c(secon[1,1], secon[1,3], secon[2,1], secon[2,3]), nrow = 2)

oddsratio(ab_aa_comp2nd, conf.level = 0.975)

oddsratio(bb_aa_comp2nd, conf.level = 0.975)


##### Case vs Smoking Status #####
smoke   <- matrix(c(92, 502, 390, 161, 554, 254), nrow = 2, byrow = TRUE, 
                  dimnames = list(Case = c('Case', 'Control'), SmokeStatus = c('Never', 'Former', 'Current')))

chisq.test(smoke)

# nonsmoker quitter comparison (nonsmoker in first col)
nonsmoke_quit = matrix(c(smoke[1,1], smoke[1,2], smoke[2,1], smoke[2,2]), nrow = 2)

# nonsmoker smoker comparison (nonsmoker in first col)
nonsmoke_smoke = matrix(c(smoke[1,1], smoke[1,3], smoke[2,1], smoke[2,3]), nrow = 2)


oddsratio(nonsmoke_quit, conf.level = 0.975)
oddsratio(nonsmoke_smoke, conf.level = 0.975)
