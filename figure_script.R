################################################################################
# Water chemistry/nutrient plots (and E. coli, coliform)
#
# 5/9/25
################################################################################
library(dplyr);library(plotrix);library(openxlsx);library(ggplot2)
# use "dat4c" or "full data_TS.xlsx" (same file, depends what's loaded)

#install.packages("MetBrewer")  # Only if you haven't installed it yet
library(MetBrewer)

setwd("C:/Users/torreys/OneDrive - University of Idaho/Mentor and Collab/Josie")

# View the Renoir palette (default is 10 colors)
renoir_colors <- met.brewer("Renoir")

# dat <- dat4c
dat <- read.xlsx("full data_TS.xlsx")

colnames(dat)

# Start with coliform, E. coli, Cow/Human -- these go with microbial data
dat <- dat %>%
  mutate(position = case_when(position == "HW" ~ "1",
                          position == "CCE" ~ "2",
                          position == "SK" ~ "3",
                          position == "GUN" ~ "5",
                          position == "MAIN" ~ "4",
                          position == "ODOM" ~ "6"))

dat$month <- factor(dat$month, levels = c("May","June","July","Aug","Sep","Oct"))

chemdat <- dat[,c(1:23)]
gaypal <- c("#8B0000","#D2691E","#D4AF37","#228B22","#1E3F66","#800080")
sitepal <- c('#2F357C', '#9D9CD5', '#F6B3B0', '#BF3729', '#F5BB50', '#355828')

## E. coli
ecol <- chemdat %>% filter(month != "May")

ggplot(data = ecol, aes(x = month, y = Ecoli, color = position, group = position, 
                        shape = position, fill = position))+
  geom_point(size = 3.5, color = "black")+
  geom_hline(yintercept=235, linetype = "dashed", color = "black")+
  geom_line(linewidth = 1)+
  labs(y = expression(italic("E. coli") ~ "CFU 100 mL"^-1), x = "Month")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_shape_manual(values = c(21,22,23,24,25,8), name = "Site")+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))+
  guides(shape = guide_legend(nrow = 1))+
  guides(color = guide_legend(nrow = 1))
  


ggsave("Figures/Ecoli_235.png", height = 6, width = 5, units = "in",
       dpi = 350, bg = "white")

sum(dat$Ecoli > 235, na.rm = TRUE) # 18/30 instances of E coli > reg limit

ecol <- dat %>% filter(Ecoli>235)


## Coliform
cd2 <- chemdat %>% filter(month != "May")
ggplot(data = cd2, aes(x = month, y = coliform, fill = position, group = position))+
  geom_col(position = position_dodge())+
  labs(y = "Coliform", x = "Month")+
  scale_fill_manual(values = gaypal, name = "Position")+
  theme_minimal()
# this figure doesn't show anything useful at all

## Human gene:
geneplot <- read.xlsx("MST scaled to copiesper100ml.xlsx")
gp2 <- geneplot %>%
  group_by(Target.Name, month, site) %>%
  summarise(copies = mean(copies_per_100, na.rm = T),
            SE = std.error(copies_per_100, na.rm = T),
            repcount = sum(!is.na(copies_per_100)),
            num_possible = n()) %>%
  mutate(site = case_when(site == "HW" ~ "1",
                          site == "CCE" ~ "2",
                          site == "SK" ~ "3",
                          site == "GUN" ~ "5",
                          site == "MAIN" ~ "4",
                          site == "ODOM" ~ "6"),
         Month = case_when(month == 6 ~ "June",
                           month == 7 ~ "July",
                           month == 8 ~ "Aug",
                           month == 9 ~ "Sep",
                           month == 10 ~ "Oct"))

gp2$Month <- factor(gp2$Month, levels = c("June","July","Aug","Sep","Oct"))

# Ensure only sites with >1 rep are plotted by changing 1 and 0 reps to NA
gp3 <- gp2 %>%
  mutate(copies = case_when(repcount < 2 ~ NA_real_, TRUE ~ copies),
         SE     = case_when(repcount < 2 ~ NA_real_, TRUE ~ SE))

human <- gp3 %>% filter(Target.Name == "HF183")

ggplot(data = human, aes(x = Month, y = copies, color = site, 
                        fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=copies+SE, ymin=copies-SE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression("Gene copies 100 mL"^-1), x = "Month")+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()+
  ggtitle("Human Tracer")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none")

ggsave("Figures/Human gene.png", height = 3, width = 4, units = "in", dpi = 350,
       bg = "white")

## Cow gene:
cow <- gp3 %>% filter(Target.Name == "CowM2")

## Cow, all points:
ggplot(data = cow, aes(x = Month, y = copies, color = site, 
                         fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=copies+SE, ymin=copies-SE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression(italic("CowM2")~"copies 100 mL"^-1), x = "Month")+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()

## Cow, without SKs:
cow2 <- cow %>% filter(site != "3")

ggplot(data = cow2, aes(x = Month, y = copies, color = site, 
                       fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=copies+SE, ymin=copies-SE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression(italic("CowM2")~"copies 100 mL"^-1), x = "Month")+
  scale_fill_manual(values = renoir_colors[c(2,4,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,8,10,12)], name = "Site")+
  theme_minimal()

ggsave("Figures/cow gene wo SK.png", height = 5, width = 7, units = "in", dpi = 350,
       bg = "white")

## Both genes on one plot

ggplot(data = gp3, aes(x = Month, y = copies, color = site, 
                       fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=copies+SE, ymin=copies-SE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression("Gene copies 100 mL"^-1), x = "Month")+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()+
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12))+
  facet_wrap(.~`Target.Name`, ncol=1, scales = "free_y")

ggsave("Figures/Genes.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

#### SUM of both genes--just for kicks

gpsum <- gp3 %>%
  group_by(Month, site) %>%
  summarise(totalgene = sum(copies),
         totalSE = std.error(copies))

ggplot(data = gpsum, aes(x = Month, y = totalgene, color = site, 
                       fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=totalgene+totalSE, ymin=totalgene-totalSE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression("Gene copies 100 mL"^-1), x = "Month")+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()+
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12))

####### FIGURE 3b: BROKEN Y AXIS
library(ggbreak);library(ggpattern)

ggplot(data = cow, aes(x = Month, y = copies, color = site, 
                       fill = site, group = site))+
  geom_col(position = position_dodge(width = 0.9), color = "black")+
  geom_errorbar(aes(ymax=copies+SE, ymin=copies-SE), 
                position = position_dodge(width = 0.9), width = 0.3)+
  labs(y = expression("Gene copies 100 mL"^-1), x = "Month")+
  scale_y_break(c(120,750), scales = 0.5)+
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()+
  ggtitle("Bovine Tracer")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none")

ggsave("Figures/Cow_broken_y.png", height = 3, width = 4, units = "in",
       dpi = 350, bg = "white")

## plot cyanobacteria at HW and SK against their respective DOsat []s:

hw <- dat %>% filter(position == "1 - HW")
sk <- dat %>% filter(position == "3 - SK")

hm <- lm(DOsat ~ Cyanobacteria, data = hw)
anova(hm) # HW: 0.78; SK:. 0.17
summary(hm)$r.squared #HW = 0.03, SK: 0.4

ggplot(data = sk, aes(x = DOsat, y = Cyanobacteria, color = month))+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = TRUE, color = "blue")+
  ggtitle("Steve Kauder Property")+
  scale_color_manual(values = gaypal, name = "month")+
  theme_minimal()

ggsave("Figures/SK_cyano_DO.png", height = 5, width = 7, units = "in", dpi = 350,
       bg = "white")


## plot ALL DOsat vs ALL cyanobacteria:
ggplot(data = dat, aes(x = DOsat, y = Cyanobacteria*100, color = month))+
  geom_point(size = 4)+
  labs(y = "Cyanobacteria Relative Abundance (%)", x = "Dissolved Oxygen Saturation (%)")+
  geom_smooth(method = "lm", se = T, color = "blue")+
  scale_color_manual(values = gaypal, name = "Month")+
  theme_minimal()

ggsave("Figures/DOsat vs Cyano.png", heigh = 5, width = 7, units = "in", dpi = 350,
       bg = "white")

hm <- lm(DOsat ~ Cyanobacteria, data = dat)
anova(hm) # p=0.004
summary(hm)$r.squared # R2 = 0.23


################################################################################
### Chemical Variables: (n=10)

## 1 - Discharge: Cubic Feet per Second

ggplot(data = dat, aes(x = month, y = discharge, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(discharge)), linewidth = 1)+
  labs(y = "Discharge (cfs)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/1_Discharge.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 2 - Turbidity: Formazin Nephelometric Unit

ggplot(data = dat, aes(x = month, y = turbidity, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(turbidity)), linewidth = 1)+
  labs(y = "Turbidity (FNU)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/2_Turbidity.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 3 - Temperature

ggplot(data = dat, aes(x = month, y = temp, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(temp)), linewidth = 1)+
  labs(y = expression(paste("Temperature (", degree, "C)")), x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/3_Temperature.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 4 - DO sat: 

ggplot(data = dat, aes(x = month, y = DOsat, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(DOsat)), linewidth = 1)+
  labs(y = "Dissolved Oxygen Saturation (%)", x = "Month")+
  scale_color_manual(values = sitepal, name = "Position")+
  theme_minimal()

ggsave("Figures/4_DO sat.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 5 - Conductivity: uS/cm

ggplot(data = dat, aes(x = month, y = cond, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(cond)), linewidth = 1)+
  labs(y = expression(paste("Conductivity (", mu, "S cm"^{-1}, ")")), x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/5_Conductivity.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 6 - pH

ggplot(data = dat, aes(x = month, y = ph, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(ph)), linewidth = 1)+
  labs(y = "pH", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/6_pH.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 7 - DOC: presumably ppm??

ggplot(data = dat, aes(x = month, y = DOC, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(DOC)), linewidth = 1)+
  labs(y = "Dissolved Organic Carbon (ppm)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/7_DOC.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 8 - TN: ppm

ggplot(data = dat, aes(x = month, y = TN, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(TN)), linewidth = 1)+
  labs(y = "Total Nitrogen (ppm)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/8_TN.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## 9 - NO3

ggplot(data = dat, aes(x = month, y = NO3, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(NO3)), linewidth = 1)+
  labs(y = "Nitrate (ppm)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/9_NO3.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

# limit is 10 ppm
dat_noHW <- dat %>%
  filter(position != "1 - HW")

mean(dat_noHW$NO3) #7.33
hw <- dat %>%
  filter(position == "1 - HW")
mean(hw$NO3) #1.87

## 9b: nitrate vs TN
ggplot(data = dat, aes(x = TN, y = NO3, color = position, shape = month))+
  geom_point(size = 3)+
  scale_color_manual(values = gaypal)+
  theme_minimal()

nn <- lm(TN ~ NO3, data = dat)
anova(nn)
summary(nn) #adj R2 = .76, p<0.001

# or:
dat <- dat %>%
  mutate(n_as_no3 = NO3/TN)

ggplot(data = dat, aes(x = month, y = n_as_no3, fill = position))+
  geom_col(position = position_dodge(width = 0.9))+
  scale_fill_manual(values = gaypal)+
  theme_minimal()
range(dat$n_as_no3) #47-99%
mean(dat$n_as_no3) #77% avg

n_mod <- lm(n_as_no3 ~ month + position, data = dat)
anova(n_mod) # no sig diffs btwn month, position
emmeans(n_mod, pairwise ~ month) # nothing
emmeans(n_mod, pairwise ~ position) # yup, nothing

## 10 - PO4

ggplot(data = dat, aes(x = month, y = PO4, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(data = subset(dat, !is.na(PO4)), linewidth = 1)+
  labs(y = "Phosphate (ppm)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/10_PO4.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

#### Figure 2:
# paneled stream characteristics (bar plots) with NO3, pH, DOC, DOsat (drop outlier)
renoir_colors <- met.brewer("Renoir")
library(plotrix)

dat <- dat %>%
  mutate(Site = case_when(position == "HW" ~ 1,
                          position == "CCE" ~ 2,
                          position == "SK" ~ 3,
                          position == "MAIN" ~ 4,
                          position == "GUN" ~ 5,
                          position == "ODOM" ~ 6))%>%
  mutate(Site = as.factor(Site))
## nitrate
no3 <- dat %>%
  group_by(Site) %>%
  summarise(no3 = mean(NO3, na.rm = T),
            SE = std.error(NO3, na.rm = T))

nitrateplot <- ggplot(data = no3, aes(x = Site, y = no3, fill = Site, color = Site))+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = no3+SE, ymin = no3-SE), width = 0.5) + 
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  labs(x = "Site", y = "Nitrate (ppm)")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

## pH
ph <- dat %>%
  group_by(Site) %>%
  summarise(phmn = mean(ph, na.rm = T),
            SE = std.error(ph, na.rm = T))

ggplot(data = ph, aes(x = Site, y = phmn, fill = Site, color = Site))+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = phmn+SE, ymin = phmn-SE), width = 0.5) + 
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  labs(x = "Site", y = "pH")+
  coord_cartesian(ylim = c(6,9))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

### buuuut, Laurel wants an axis break. So.
library(ggbreak)

phplot <- ggplot(data = ph, aes(x = Site, y = phmn, fill = Site, color = Site))+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = phmn + SE, ymin = phmn - SE), width = 0.5) + 
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  labs(x = "Site", y = "pH")+
  #scale_y_cut(c(0, 6.5), scales = c(0.01,0.5)) +
  coord_cartesian(ylim = c(6,9))+
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))
# she needs a little shoppin' in ppt, but good enough
ggsave("Figures/fig2_ph_splity.png", height = 3, width = 3.5, units = "in",
       dpi = 350, bg = "white")

## DOC
doctn <- dat %>%
  group_by(Site) %>%
  summarise(CN = mean(DOC/TN, na.rm = T),
            SE = std.error(DOC/TN, na.rm = T))

DOCplot <- ggplot(data = doctn, aes(x = Site, y = CN, fill = Site, color = Site))+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = CN+SE, ymin = CN-SE), width = 0.5) + 
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  labs(x = "Site", y = "DOC:TN")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

## DOsat
dosat <- dat %>%
  filter(id != 7) %>% #gets rid of May GUN outlier
  group_by(Site) %>%
  summarise(dos = mean(DOsat, na.rm = T),
            SE = std.error(DOsat, na.rm = T))

dosatplot <- ggplot(data = dosat, aes(x = Site, y = dos, fill = Site, color = Site))+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = dos+SE, ymin = dos-SE), width = 0.5) + 
  scale_fill_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)])+
  labs(x = "Site", y = "Dissolved Oxygen Saturation (%)")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

library(gridExtra);library(ggplotify)

figure2 <- as.ggplot(grid.arrange(nitrateplot, phplot, dosatplot, DOCplot, ncol = 2))

ggsave("Figures/Figure2_2.png", figure2, width = 7, height = 7, units = "in",
       dpi = 350, bg = "white")

##################################



# regression: E. coli and NO3

ggplot(data = dat2, aes(x = Ecoli, y = NO3, shape = month, color = position, group = position))+
  geom_point(size = 2)+
  stat_smooth()

dat2 <- dat %>% filter(position != "1")

reg <- lm(Ecoli ~ NO3, data = dat2)
anova(reg)
summary(reg)


library(emmeans)
library(multcomp)   # for cld()

# Example: variable = Shannon, grouping variable = site
dat$position <- factor(dat$position, levels = c("HW","CCE","SK","MAIN","GUN","ODOM"))
model <- lm(DOC_TDN ~ position, data = dat)

# Estimated marginal means
emm <- emmeans(model, ~ position)

# Get pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")

# Get letters of significance
cld(emm, Letters = letters, adjust = "tukey")

