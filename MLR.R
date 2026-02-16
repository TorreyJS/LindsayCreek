################################################################################
# Multiple Linear Regression - Lindsay Creek
#
#
################################################################################
# Load packages
library(MASS)       # for stepAIC (optional)
library(MuMIn)      # for dredge()
library(car)        # for vif()
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

# bring in ALL data (including precip)
dat1 <- read.xlsx("LC_FulLData.xlsx")

# option 1: remove May -- no microbial data
#nomay <- final2 %>% filter(month != 5)

### OR -- only compare chemical to chemical

### pull out only certain columns
# i.e., for NO3 look at only chem data (bac rel abund sums to 1 = collinear)
# also--no microbe data for May, so ONLY biogeochem data!
test2 <- dat1[,c(6:15,34,35)]
test2 <- na.omit(test2) # lose 2 obs

# define variables
vars <- colnames(test2)[-9] # -NO3 (column 9)
#vars_all <- colnames(testdf)
form <- as.formula(paste("NO3 ~", paste(vars, collapse = " + ")))
#------------------------------------------------------------
# 1. Fit a global model with all predictors
#------------------------------------------------------------
global_model <- lm(form, data = test2)

#------------------------------------------------------------
# 2. Check multicollinearity with Variance Inflation Factors
#------------------------------------------------------------
alias(global_model)

vif_values <- vif(global_model)
print(vif_values)
?vif


# Optionally, identify variables with VIF > 3
high_vif <- names(vif_values[vif_values > 3])
cat("Variables with high VIF:\n")
print(high_vif) # temp, cond, ph, DOC, TN, ppt, tmean

# If you have any highly collinear variables, remove them and refit:
vars2 <- vars[!vars %in% high_vif]

form2 <- as.formula(paste("NO3 ~", paste(vars2, collapse = " + ")))

global_model <- lm(form2, data = test2)




### correlation plots
# cors <- cor(test2[, c("discharge", "turbidity", "temp", "DOsat",
#                       "cond", "ph", "DOC", "TN", "PO4", "Ecoli", "Mean_Cow", "Mean_Human")],
#             use = "complete.obs")
# 
# cors2 <- cor(testdf[,c(12,18:28)], use = "complete.obs")
# 
# library(corrplot)
# 
# corrplot(cors, method = "color", type = "upper",
#          addCoef.col = "black", # add correlation values
#          tl.col = "black", tl.srt = 45, # text label color and rotation
#          col = colorRampPalette(c("blue", "white", "red"))(200))
# 
# corrplot(cors2, method = "color", type = "upper",
#          addCoef.col = "black", # add correlation values
#          tl.col = "black", tl.srt = 45, # text label color and rotation
#          col = colorRampPalette(c("blue", "white", "red"))(200))

#------------------------------------------------------------
# 3. Enable MuMIn to handle missing data correctly
#------------------------------------------------------------
options(na.action = "na.fail")  # required by dredge()

#------------------------------------------------------------
# 4. Use dredge() to compare all possible models
#------------------------------------------------------------
dredge_results <- dredge(global_model)

# View model ranking
print(dredge_results)

#------------------------------------------------------------
# 5. Select best model(s)
#------------------------------------------------------------
# Best model by AICc
best_model <- get.models(dredge_results, subset = 2)[[1]]
summary(best_model) # discharge, DOsat, turbidity are the best predictors of NO3
# adj r2 = 0.33, DOsat is the only significant term (p < 0.001)
# discharge p = 0.08, turbidity p = 0.12
# DOsat is the most important!


test3 <- final2[,c(2,3,6:15,34,35)]
test3 <- na.omit(test3)
test3 <- test3 %>%
  mutate(Site = case_when(position == "HW" ~ "Site 1",
                          position == "CCE" ~ "Site 2",
                          position == "SK" ~ "Site 3",
                          position == "MAIN" ~ "Site 4",
                          position == "GUN" ~ "Site 5",
                          position == "ODOM" ~ "Site 6"))


sitecols <- c("#2F357C", "#9D9CD5", "#F6B3B0", "#BF3729", "#F5BB50", "#355828")

## plot:
test3_outlier <- test3 %>% filter(DOsat>25)
test3_nohw <- test3_outlier %>% filter(Site != "Site 1")
coef(lm(DOsat ~ NO3, data = test3_nohw)) #
summary(lm(DOsat ~ NO3, data = test3_nohw)) # p = 0.26, R2=0.01
summary(lm(DOsat ~ NO3, data = test3_outlier)) # p < 0.001, R2=0.47


### shade for nitrate and DO in BAD ZONES???

ggplot(data = test3_outlier, aes(y = DOsat, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(fill = Site, shape = Site), size = 3)+
  geom_line(aes(y = 105.91 - 1.80 * NO3), linetype = "dashed", color = "black",
            linewidth = 0.75)+
  theme_minimal()+
  labs(y = "Dissolved Oxygen Saturation (%)", x = "Nitrate Concentration (ppm)")+
  scale_fill_manual(values = sitecols, name = "")+
  scale_shape_manual(values = c(21,22,23,24,25,8), name = "")+
  guides(fill = guide_legend(nrow = 1),
         shape = guide_legend(nrow = 1))+
  geom_text(y = 55, x = 2.5, label = "paste('p < 0.001;', ' R'^2, ' = 0.47')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 95, x = 2.5, label = "paste('p  = 0.26;', ' R'^2, ' = 0.01')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

summary(lm(NO3 ~ DOsat, data = test2)) # p < 0.001, r2 = 0.27


ggsave("Figures/NO3_vs_DOsat_v2.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")


###############################################################################
#### look at some microbial correlations
nomay <- final3 %>% filter(month != 5)

nomay <- nomay %>%
  mutate(Site = case_when(position == "HW" ~ "Site 1",
                          position == "CCE" ~ "Site 2",
                          position == "SK" ~ "Site 3",
                          position == "MAIN" ~ "Site 4",
                          position == "GUN" ~ "Site 5",
                          position == "ODOM" ~ "Site 6"))

nomay$Bdellovibrionota <- nomay$Bdellovibrionota*100
nomay$Cyanobacteria <- nomay$Cyanobacteria*100
nomay$Proteobacteria <- nomay$Proteobacteria*100
nomay$Planctomycetota <- nomay$Planctomycetota*100

sitecols <- c("#2F357C", "#9D9CD5", "#F6B3B0", "#BF3729", "#F5BB50", "#355828")

nomay2 <- nomay %>% filter(position != "HW")

## plot:
summary(lm(Bdellovibrionota ~ NO3, data = nomay)) # p < 0.001, r2 = 0.67
summary(lm(Bdellovibrionota ~ NO3, data = nomay2)) # p = 0.01, r2 = 0.22
coef(lm(Bdellovibrionota ~ NO3, data = nomay2)) # int = 0.288, slope = 0.214


b <- ggplot(data = nomay, aes(y = Bdellovibrionota, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y = 0.288+0.214 * NO3), color = "black", linewidth = 0.75, 
            linetype = "dashed")+
  theme_minimal()+
  labs(y = "Bdellovibrionota \nRelative Abundance (%)", x = "")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 0.6, x = 3.5, label = "paste('p < 0.001;', ' R'^2, ' = 0.67')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 1.2, x = 2, label = "paste('p = 0.01;', ' R'^2, ' = 0.22')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        panel.border = element_rect(fill = NA))



ggsave("Figures/NO3_vs_Bdello.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")


# Cyano:
summary(lm(NO3 ~ Cyanobacteria, data = nomay)) # p < 0.001, r2 = 0.38
summary(lm(NO3 ~ Cyanobacteria, data = nomay2)) # p = 0.002, r2 = 0.33
coef(lm(NO3 ~ Cyanobacteria, data = nomay)) # 7.67 and -0.092


c <- ggplot(data = nomay, aes(y = Cyanobacteria, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y = 7.67-0.092 * NO3), color = "black", linewidth = 0.75, 
            linetype = "dashed")+
  theme_minimal()+
  labs(y = "Cyanobacteria \nRelative Abundance (%)", x = "")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 25, x = 5.7, label = "paste('p < 0.001;', ' R'^2, ' = 0.38')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 10.5, x = 2.5, label = "paste('p = 0.002;', ' R'^2, ' = 0.33')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        panel.border = element_rect(fill = NA))

ggsave("Figures/NO3_vs_Cyano.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")


# Proteo:
summary(lm(Proteobacteria ~ NO3, data = nomay)) # p = 0.004, r2 = 0.23
summary(lm(Proteobacteria ~ NO3, data = nomay2)) # p = 0.053, r2 = 0.12
coef(lm(Proteobacteria ~ NO3, data = nomay2)) # 70.37 and -2.71

pr <- ggplot(data = nomay, aes(y = Proteobacteria, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y = 70.37 -2.71 * NO3), color = "black", linewidth = 0.75, linetype = "dashed")+
  theme_minimal()+
  labs(y = "Proteobacteria \nRelative Abundance (%)", x = "")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 47, x = 2.25, label = "paste('p = 0.004;', ' R'^2, ' = 0.23')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 52, x = 2.25, label = "paste('p = 0.053;', ' R'^2, ' = 0.12')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        panel.border = element_rect(fill = NA))

ggsave("Figures/NO3_vs_Proteo.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")


# Plancto:
summary(lm(Planctomycetota ~ NO3, data = nomay)) # p = 0.02, r2 = 0.14
summary(lm(Planctomycetota ~ NO3, data = nomay2)) # p = 0.22, r2 = 0.02
coef(lm(Planctomycetota ~ NO3, data = nomay2)) # 2.77 and -0.16


pl<-ggplot(data = nomay, aes(y = Planctomycetota, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y = 2.77 - 0.16*NO3), linewidth = 0.75, linetype = "dashed", color = "black")+
  theme_minimal()+
  labs(y = "Planctomycetota \nRelative Abundance (%)", x = "")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 1, x = 5, label = "paste('p = 0.02;', ' R'^2, ' = 0.14')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 2.3, x = 5, label = "paste('p = 0.22;', ' R'^2, ' = 0.02')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "none",
        panel.border = element_rect(fill = NA))

ggsave("Figures/NO3_vs_Plancto.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")

library(cowplot)

plot_grid(c, b, pr, pl, ncol = 2, rel_heights = c(0.8,0.8,1.2,1.2))

ggsave("Figures/NO3_vs_microbes.png", height = 10, width = 12, dpi = 350,
       units = "in", bg = "white")


## is nitrate just correlated with every phylum?? (no)
nomay$Actinobacteriota <- nomay$Actinobacteriota*100
summary(lm(Actinobacteriota~NO3, data = nomay)) # p = 0.21

nomay$Firmicutes <- nomay$Firmicutes*100
summary(lm(Firmicutes~NO3, data = nomay)) # p = 0.07

nomay$Myxococcota <- nomay$Myxococcota*100
summary(lm(Myxococcota~NO3, data = nomay)) # p = 0.07
summary(lm(Rare~NO3, data = nomay)) # p = 0.08
summary(lm(Verrucomicrobiota~NO3, data = nomay)) # p = 0.16


### two other phyla are sig:
nomay$Bacteroidota <- nomay$Bacteroidota*100
summary(lm(Bacteroidota~NO3, data = nomay)) # p = 0.03, r2 = 0.13

nomay$Patescibacteria <- nomay$Patescibacteria*100
summary(lm(Patescibacteria~NO3, data = nomay)) # p = 0.03, r2 = 0.13



# look at shape of bact and patesc relationship
nomay2 <- nomay %>% filter(position != "HW")


# bac = positive
summary(lm(Bacteroidota~NO3, data = nomay)) # p = 0.03, r2 = 0.13
summary(lm(Bacteroidota~NO3, data = nomay2)) # p = 0.05, r2 = 0.12
coef(lm(Bacteroidota~NO3, data = nomay2)) # 26.001 and -1.08

bac <- ggplot(data = nomay, aes(y = Bacteroidota, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y=26.001-1.08*NO3), linetype = "dashed", color = "black", linewidth = 0.75)+
  theme_minimal()+
  labs(y = "Bacteroidota \nRelative Abundance (%)", x = "Nitrate Concentration (ppm)")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 12.5, x = 2.5, label = "paste('p = 0.03;', ' R'^2, ' = 0.13')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 20, x = 2.5, label = "paste('p = 0.05;', ' R'^2, ' = 0.12')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA))


ggsave("Figures/NO3_vs_Bacteroid.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")


# patesc = positive
summary(lm(Patescibacteria~NO3, data = nomay)) # p = 0.03, r2 = 0.13
summary(lm(Patescibacteria~NO3, data = nomay2)) # p = 0.46, r2 = 0
coef(lm(Patescibacteria~NO3, data = nomay2)) # 2.134 and -0.07

pat <- ggplot(data = nomay, aes(y = Patescibacteria, x = NO3))+
  geom_smooth(method = "lm", se = F, linetype = "solid", color = "#2F357C")+
  geom_point(aes(color = Site), size = 3)+
  geom_line(aes(y=2.134-0.07*NO3), linetype = "dashed", color = "black", linewidth = 0.75)+
  theme_minimal()+
  labs(y = "Patescibacteria \nRelative Abundance (%)", x = "Nitrate Concentration (ppm)")+
  scale_color_manual(values = sitecols, name = "")+
  guides(color = guide_legend(nrow = 1))+
  geom_text(y = 1.15, x = 4.5, label = "paste('p = 0.03;', ' R'^2, ' = 0.13')",
            parse = TRUE, size = 4.5, color = "#2F357C")+
  geom_text(y = 2, x = 4.5, label = "paste('p = 0.46;', ' R'^2, ' = 0')",
            parse = TRUE, size = 4.5, color = "black")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size =12),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA))


ggsave("Figures/NO3_vs_Patesc.png", height = 5, width = 6, units = "in",
       dpi = 350, bg = "white")

library(cowplot)
plot_grid(c, b, pr, pl, bac, pat, ncol = 2, 
          rel_heights = c(0.8,0.8,1))

ggsave("Figures/NO3_vs_microbes_6.png", height = 15, width = 12, dpi = 350,
       units = "in", bg = "white")


## what about the sum of K and r? 
# K = Verruco, Plancto
# r = Proteo, bacter

nomay <- nomay %>%
  mutate(K_strat = ((Verrucomicrobiota*100) + Planctomycetota),
         r_strat = (Proteobacteria + Bacteroidota))

summary(lm(K_strat ~ NO3, data = nomay)) # p = 0.08
summary(lm(r_strat ~ NO3, data = nomay)) # p = 0.003, R2 = 0.25

