################################################################################
# Lindsay Creek Stats
#
# 5/7/25
################################################################################
# use "LC_FullData.xlsx"
dat <- read.xlsx("LC_FullData.xlsx")

# model structure is simple: lm(y ~ site + month, data = data)

################################################################################
# stats:
# model structure is simple: lm(y ~ site + month, data = data)
colnames(dat)
# variables: discharge, turbidity, temp, DO, DOsat, cond, ph, DOC, TN, NO3, PO4,
# coliform, Ecoli, CowM2, HF183, Actinobacteriota, Bacteroidota, Bdellovibrionota,
# Cyanobacteria, Firmicutes, Myxococcota, Patescibacteria, Planctomycetota, Proteobacteria,
# Rare, Verrucomicrobiota, rk_ratio, ShanDiv


##### look at the distribution for each variable:
library(tidyverse)

# check to make sure columns are correct
dat_long <- dat %>%
  select(c(6:14, 16:21,26:37)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(dat_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables")

# normal-ish: actino, bact, bdello, DOC, firm, human, no3, pates, ph, plancto, po4, proteo, rare, temp, tn, turbid, verruco
# right skew/log: cyano, discharge, ecoli, myxo, rK
# left skew: coliform, DO, DOsat, shandiv
# bimodal: cond, cow

## what variables are correlated?
library(GGally)

d2 <- dat[,c(6:14,16,17,19,26:37)]
  
ggpairs(dat, columns = c(6:14,16,17,19,26:37)) #, mapping = ggplot2::aes(color = position))
# messy! DO and DOsat are correlated, unsurprisingly

## model: lm(y ~ position + month)

### For just the normal variables first:
normvars <- names(dat)[c(26:28,13,30,21,16,17,32,33,12,34,35,8,14,7,36)]

### loop for the normies:
models <- list()

for (var in normvars) {
  formula <- as.formula(paste(var, "~ position + month"))
  models[[var]] <- lm(formula, data = dat)
}

### ANOVA for each variable:
anova_results <- lapply(models, anova)
anova_results[[1]] # can get each variable

## OR, get them all in one dataframe:
library(dplyr);library(purrr)

anova_df <- imap_dfr(anova_results, ~ 
                       as.data.frame(.x) %>%
                       mutate(response = .y, term = rownames(.x)) %>%
                       select(response, term, Df, `F value`, `Pr(>F)`)
)
anova_df$`Pr(>F)` <- format(anova_df$`Pr(>F)`, scientific = FALSE, digits = 4)
anova_df$`Pr(>F)` <- as.numeric(anova_df$`Pr(>F)`)

write.xlsx(anova_df, "anova_normal variables.xlsx")

anova_sigonly <- anova_df %>%
  filter(`Pr(>F)` < 0.05)
str(anova_sigonly)
anova_sigonly$`Pr(>F)` <- round(anova_sigonly$`Pr(>F)`, digits = 4)
write.xlsx(anova_sigonly, "anova_normal vars_sig only.xlsx")

## emmeans:
library(emmeans)

# Create a list to store pairwise comparisons
pairwise_results <- list()

# Loop through each model
for (var in names(models)) {
  # Get the model
  model <- models[[var]]
  
  # Get pairwise comparisons for 'position' (change this to 'model' after!)
  pairwise_results[[var]] <- emmeans(model, pairwise ~ month)
}

pairwise_results[[1]] # put in index number

####### log variables (5)
logvars <- names(dat[,c(29,6,19,31,37)])
### loop for the normies:
logmodels <- list()

for (var in logvars) {
  formula <- as.formula(paste("log(", var, ")", "~ position + month"))
  logmodels[[var]] <- lm(formula, data = dat)
}

### ANOVA for each variable:
log_anova_results <- lapply(logmodels, anova)
log_anova_results[[1]] # can get each variable

anova_log_df <- imap_dfr(log_anova_results, ~ 
                       as.data.frame(.x) %>%
                       mutate(response = .y, term = rownames(.x)) %>%
                       select(response, term, Df, `F value`, `Pr(>F)`)
)
anova_log_df$`Pr(>F)` <- format(anova_log_df$`Pr(>F)`, scientific = FALSE, digits = 4)
anova_log_df$`Pr(>F)` <- as.numeric(anova_log_df$`Pr(>F)`)

write.xlsx(anova_log_df, "anova_log variables.xlsx")

anova_log_sigonly <- anova_log_df %>%
  filter(`Pr(>F)` < 0.05)
anova_log_sigonly$`Pr(>F)` <- round(anova_log_sigonly$`Pr(>F)`, digits = 4)

# Create a list to store pairwise comparisons
log_pairwise_results <- list()

# Loop through each model
for (var in names(logmodels)) {
  # Get the model
  m2 <- logmodels[[var]]
  
  # Get pairwise comparisons for 'position' (change this to 'model' after!)
  log_pairwise_results[[var]] <- emmeans(m2, pairwise ~ month, type = "response")
}

log_pairwise_results[[4]] # put in index number

################################################################################
## left skew variables: coliform, DO, DOsat, Shandiv
hist(dat$coliform)
summary(dat$coliform)
unique(dat$coliform)
# coliform might be best talked about qualitatively

hist(dat$DOsat)
summary(dat$DOsat)
unique(dat$DOsat)
# DO and DOsat we can do stats on (probably extremely related)
plot(dat$DO ~ dat$DOsat) # yep

library(MASS)
boxcox(lm(DO ~ position + month, data = dat), lambda = seq(-0, 6, 0.1))
# looks like we need a power transformation. Raise to 3
dat$DO_3 <- dat$DO^3

DOmodel <- lm(DO_3 ~ position + month, data = dat)
hist(DOmodel$residuals)# actually looks nice
shapiro.test(DOmodel$residuals)#heyooo!
anova(DOmodel) # both sig
emm <- emmeans(DOmodel, pairwise ~ month) # use this to get sig contrasts
emm_summary <- summary(emm$emmeans)
emm_summary %>%
  mutate(response_scale = emmean^(1/3)) # use this to get actual emmean estimates

## same process for DOsat:
boxcox(lm(DOsat ~ position + month, data = dat), lambda = seq(-0, 6, 0.1))
# looks like we need a power transformation. Raise to 4
dat$DOsat_4 <- dat$DOsat^4

DOsatmodel <- lm(DOsat_4 ~ position + month, data = dat)
hist(DOsatmodel$residuals)# looks okay
shapiro.test(DOsatmodel$residuals)#ehhhhhhhhh close
anova(DOsatmodel) # only position is sig
emm <- emmeans(DOsatmodel, pairwise ~ position) # use this to get sig contrasts
emm_summary <- summary(emm$emmeans)
emm_summary %>%
  mutate(response_scale = emmean^(1/4)) # use this to get actual emmean estimates


hist(dat$ShanDiv) # 
summary(dat$ShanDiv)
unique(dat$ShanDiv)
# this is fine to do stats on, too
## same process as abovet:
boxcox(lm(ShanDiv ~ position + month, data = dat), lambda = seq(0,10, 0.5))
# looks like we need a power transformation. Raise to 4
dat$ShanDiv_4 <- dat$ShanDiv^4

shanmodel <- lm(ShanDiv_4 ~ position + month, data = dat)
hist(shanmodel$residuals)# looks okay
shapiro.test(shanmodel$residuals)#beautiful
anova(shanmodel) # only position is sig
emm <- emmeans(shanmodel, pairwise ~ position) # use this to get sig contrasts
emm_summary <- summary(emm$emmeans)
emm_summary %>%
  mutate(response_scale = emmean^(1/4)) # use this to get actual emmean estimates

#################################################################################
## finally, the two bimodal weirdos: conductivity, cow gene

hist(dat$cond)
summary(dat$cond)
boxcox(lm(cond ~ position + month, data = dat), lambda = seq(-10,0, 0.5)) #raise to -4
dat$cond_4 <- dat$cond^(-4)

condmod <- lm(cond_4 ~ position + month, data = dat)
hist(condmod$residuals) # hm
shapiro.test(condmod$residuals) # it's okay ish
anova(condmod) # position is sig

emm <- emmeans(condmod, pairwise ~ position) # use this to get sig contrasts
emm_summary <- summary(emm$emmeans)
emm_summary %>%
  mutate(response_scale = emmean^(-1/4)) # use this to get actual emmean estimates

## COW
hist(dat$Mean_Cow)
summary(dat$Mean_Cow)
#way too many NAs to assess with stats

summary(dat$Mean_Human)


################################################################################
## regressions, for data exploration:

# 1. discharge vs all others
library(dplyr);library(purrr);library(broom)

results <- dat %>%
  select(-c(1:5)) %>%                             # Exclude the first 5 metadata columns
  select(where(is.numeric)) %>%                   # Ensure all selected columns are numeric
  map_df(~tidy(lm(.x ~ dat$discharge)),     # Regress each variable on discharge
         .id = "response_var") %>%
  filter(term == "dat$discharge")           # Keep only the slope terms


results_r2 <- dat %>%
  select(-c(1:5)) %>%
  select(where(is.numeric)) %>%
  map(~summary(lm(.x ~ dat$discharge))) %>%
  map_dfr(~tibble(r.squared = .x$r.squared), .id = "response_var")

# Combine results
full_results <- left_join(results, results_r2, by = "response_var")

sig_res <- full_results %>% filter(p.value < 0.05)

################################################################################
## How much response does precipitation explain??
precip <- read.xlsx("PRISM/monthly temp and precip.xlsx")
p2 <- precip %>%
  pivot_wider(names_from = type, values_from = value)
# reformat so can merge
p2 <- p2 %>%
  mutate(month = case_when(month == "5" ~ "May",
         month == "6" ~ "June",
         month == "7" ~ "July",
         month == "8" ~ "Aug",
         month == "9" ~ "Sep",
         month == "10" ~ "Oct")) %>%
  mutate(position = case_when(
    site_id == "MS" ~ "4",
    site_id== "HW" ~ "1",
    site_id == "CCE" ~ "2",
    site_id == "SK" ~ "3",
    site_id == "GUN" ~"5",
    site_id == "ODOM" ~ "6"))

colnames(p2)[4:5] <- c("precip","airtemp")

dat2 <- left_join(dat, p2, by = c('month', 'position'))
library(purrr);library(broom)

precip_corrs <- dat2 %>%
  select(-c(1:5,22:25,38)) %>%                             # Exclude the first 5 metadata columns
  select(where(is.numeric)) %>%                   # Ensure all selected columns are numeric
  map_df(~tidy(lm(.x ~ dat2$precip)),     # Regress each variable on discharge
         .id = "response_var") %>%
  filter(term == "dat2$precip")           # Keep only the slope terms

precip_r2 <- dat2 %>%
  select(-c(1:5,38)) %>%
  select(where(is.numeric)) %>%
  map(~summary(lm(.x ~ dat2$precip))) %>%
  map_dfr(~tibble(r.squared = .x$r.squared), .id = "response_var")

# Combine results
full_precip <- left_join(precip_corrs, precip_r2, by = "response_var")

sig_precip <- full_precip %>% filter(p.value < 0.05)

# specifically, does discharge rely on precipitation?
dp <- lm(discharge ~ precip, data = dat2)
hist(dp$residuals)

anova(dp) # p=0.97

# does precip vary by stream position? I doubt it
precipmodel <- lm(precip ~ position, data = dat2)
anova(precipmodel)# not even a little bit p=0.98
summary(precipmodel)


#####################
## single variable model:
# model structure is simple: lm(y ~ site + month, data = data)

dat$DOC_TDN <- dat$DOC/dat$TN

dat$position <- factor(dat$position, levels = c("HW","CCE","SK","MAIN","GUN","ODOM"))

ggplot(data = dat, aes(x = position, y = rk_ratio))+
  geom_boxplot()

dis <- dat %>%
  group_by(position) %>%
  summarise(mean = mean(DOsat, na.rm = T))

mod <- (lm(rk_ratio ~ position + month, data = dat))
anova(mod) # position***, month p = 0.54
emmeans(mod, pairwise ~ position) # 1>3,4,5,6 

