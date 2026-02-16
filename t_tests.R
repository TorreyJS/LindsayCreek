################################################################################
# t tests! Site 1 vs all other sites
#
# Orig: 1/8/26 
################################################################################
library(openxlsx)

vars <- read.xlsx("LC_FullData.xlsx.xlsx")

std_test <- vars %>%
  mutate(site_grp = case_when(position == "HW" ~ "Headwaters",
                              T ~ "Others"))

## 1: DO and Nitrate
# Fig 6: DO sat and NO3 POS relationship w/ all sites, NO relationship w/o Site 1

# t-tests 3 ways: DO, NO3, and DO/NO3

t.test(DOsat ~ site_grp, data = std_test) # t = -4.61, df = 5.24, p < 0.01
t.test(NO3 ~ site_grp, data = std_test) # t = -13.44, df = 7.28, p < 0.001
std_test$DO_NO3_ratio <- std_test$DOsat / std_test$NO3
t.test(DO_NO3_ratio ~ site_grp, data = std_test) # t = 1.89, df = 4.01, p = 0.13

## 2: Nitrate and bacterial phyla
# Fig S2

# t-tests 3 ways for each of 6 phyla: Cyano, Bdello, Proteo, Plancto, Bact, Patesci
## ACTUALLY only phyla and ratio--NO3 will be the same regardless of phyla...

# Cyano:
t.test(Cyanobacteria ~ site_grp, data = std_test) # t = 7.15, df = 6.34, p < 0.001
t.test(NO3 ~ site_grp, data = std_test) # t(7.28) = -13.44, p < 0.001
std_test$Cyano_NO3_ratio <- std_test$Cyanobacteria / std_test$NO3
t.test(Cyano_NO3_ratio ~ site_grp, data = std_test) # t(5.02) = 6.47, p < 0.01

# Bdellovibrionota:
t.test(Bdellovibrionota ~ site_grp, data = std_test) # t(19.81) = -8.88, p < 0.001
t.test(NO3 ~ site_grp, data = std_test) # t(7.28) = -13.44, p < 0.001 --right, don't do this again
std_test$Bdello_NO3_ratio <- std_test$Bdellovibrionota / std_test$NO3
t.test(Bdello_NO3_ratio ~ site_grp, data = std_test) # t(5.19) = 1.16, p = 0.30

# Proteobacteria:
t.test(Proteobacteria ~ site_grp, data = std_test) # t(15.94) = -9.02, p < 0.001
std_test$Proteo_NO3_ratio <- std_test$Proteobacteria / std_test$NO3
t.test(Proteo_NO3_ratio ~ site_grp, data = std_test) # t(5.03) = 3.07, p < 0.05

# Planctomycetota:
t.test(Planctomycetota ~ site_grp, data = std_test) # t(12.80) = -5.33, p < 0.001
std_test$Plancto_NO3_ratio <- std_test$Planctomycetota / std_test$NO3
t.test(Plancto_NO3_ratio ~ site_grp, data = std_test) # t(5.73) = 2.23, p = 0.07

# Bacteroidota:
t.test(Bacteroidota ~ site_grp, data = std_test) # t(5.64) = -2.06, p = 0.09
std_test$Bact_NO3_ratio <- std_test$Bacteroidota / std_test$NO3
t.test(Bact_NO3_ratio ~ site_grp, data = std_test) # t(5.01) = 2.33, p = 0.07

# Patescibacteria:
t.test(Patescibacteria ~ site_grp, data = std_test) # t(6.80) = -2.88, p < 0.05
std_test$Pates_NO3_ratio <- std_test$Patescibacteria / std_test$NO3
t.test(Pates_NO3_ratio ~ site_grp, data = std_test) # t(5.09) = 2.61, p < 0.05

