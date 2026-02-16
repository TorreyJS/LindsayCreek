################################################################################
# Relative Abundance/microbial plots
#
# 5/8/25
################################################################################
library(openxlsx);library(ggplot2)

rk_phyla <- read.xlsx("rk_phyla_for plotting.xlsx")

unique(rk_phyla$phylum)

rk_phyla <- rk_phyla %>%
  mutate(site = case_when(site == "HW" ~ "Site 1",
                         site == "CCE" ~ "Site 2",
                         site == "SK" ~ "Site 3",
                         site == "GUN" ~ "Site 5",
                         site == "MAIN" ~ "Site 4",
                         site == "ODOM" ~ "Site 6"),
         month = case_when(month == '6' ~ "June",
                  month == '5' ~ "May",
                  month == '7' ~ "July",
                  month == '8' ~ "August",
                  month == '9' ~ "September",
                  month == '10' ~ "October"))

rk_phyla$month <- factor(rk_phyla$month, levels = c("May","June","July","August","September","October"))

# SITE ORDER: HW, CCE, SK, GUN, MS, ODOM
gaypal <- c("#8B0000","#D2691E","#D4AF37","#228B22","#1E3F66","#800080")
library(MetBrewer)
renoir_colors <- met.brewer("Renoir")
redon <- met.brewer("Redon")

### Myxococcota:
myx <- rk_phyla %>%
  filter(phylum == "Myxococcota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
          summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = myx, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Myxococcota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Actinobacteriota:
act <- rk_phyla %>%
  filter(phylum == "Actinobacteriota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = act, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Actinobacteriota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Bacteroidota:
bac <- rk_phyla %>%
  filter(phylum == "Bacteroidota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = bac, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Bacteroidota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Bdellovibrionota:
bdell <- rk_phyla %>%
  filter(phylum == "Bdellovibrionota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = bdell, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Bdellovibrionota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Cyanobacteria:
cyan <- rk_phyla %>%
  filter(phylum == "Cyanobacteria")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = cyan, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Cyanobacteria.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Firmicutes:
firm <- rk_phyla %>%
  filter(phylum == "Firmicutes")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = firm, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Firmicutes.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Patescibacteria:
pat <- rk_phyla %>%
  filter(phylum == "Patescibacteria")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = pat, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Patescibacteria.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Planctomycetota:
planc <- rk_phyla %>%
  filter(phylum == "Planctomycetota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = planc, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Planctomycetota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Proteobacteria:
pro <- rk_phyla %>%
  filter(phylum == "Proteobacteria")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = pro, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Proteobacteria.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Rare:
rare <- rk_phyla %>%
  filter(phylum == "Rare")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = rare, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Rare.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

###############################################################################
### Verrucomicrobiota:
ver <- rk_phyla %>%
  filter(phylum == "Verrucomicrobiota")%>%
  group_by(site, month) %>%
  mutate(Abundance = Abundance*100) %>% #convert from decimal to percent
  summarise(meanab = mean(Abundance, na.rm = T),
            SEab = std.error(Abundance, na.rm = T))


ggplot(data = ver, aes(x = month, y = meanab, color = site, group = site))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymax=meanab+SEab, ymin=meanab-SEab), width = 0.2)+
  geom_line(linewidth = 1)+
  labs(y = "Relative Abundance (%)", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/Verrucomcrobiota.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

## all sites
library(ggsci)
npg_colors <- (pal_npg("nrc", alpha =1)(10))
npg_colors2 <- c(npg_colors, "grey", "black")

rk_agg <- rk_phyla %>%
  group_by(site, month, phylum) %>%
  summarise(meanab = mean(100*Abundance, na.rm = T),
            SE = std.error(100*Abundance, na.rm = T)) %>%
  mutate(month = case_when(month == "August" ~ "Aug",
                           month == "September" ~ "Sep",
                           month == "October" ~ "Oct",
                           TRUE ~ month))

rk_agg$month <- factor(rk_agg$month, levels = c("May","June","July","Aug","Sep","Oct"))

  
ggplot(data = rk_agg, aes(x=month, y = meanab, group = phylum, fill = phylum))+
  geom_col()+
  theme_minimal()+
  labs(x="Month",y="Relative Abundance (%)")+
  scale_fill_manual(values = redon[c(9,2:8,1,10,11)], name = "Phylum")+
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())+
  facet_wrap(.~site, ncol = 3)

ggsave("Figures/allsites_phyla abundance_Barplot.png", height = 6, width = 9, units = "in",
       dpi = 350, bg = "white")

################################################################################
## r:K figures
# very brief searching (w/Chat) suggest Verruco are still K, Proteo and Bact are r

ggplot(data = dat, aes(x = month, y = rk_ratio, color = position, group = position))+
  geom_point(size = 3)+
  geom_line(linewidth = 1)+
  labs(y = "r:K ratio", x = "Month")+
  scale_color_manual(values = gaypal, name = "Position")+
  theme_minimal()

ggsave("Figures/rK ratio.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

#### what percent of total bacteria is classified as r-strategist?
dat <- dat %>%
  mutate(rstrat = Proteobacteria + Bacteroidota,
         other = Actinobacteriota+Bdellovibrionota+Cyanobacteria+Firmicutes+Myxococcota+Patescibacteria+Planctomycetota+Rare+Verrucomicrobiota)

range(dat$Verrucomicrobiota*100)

######### r:K correlations:
library(tidyr)
correlations <- dat %>%
  select(where(is.numeric)) %>%
  summarise(across(-rk_ratio, ~ cor(.x, dat$rk_ratio, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "correlation")

correlations %>% arrange(desc(abs(correlation)))

## and plots:
numeric_vars <- dat %>%
  select(where(is.numeric)) %>%
  select(-c(rk_ratio, id, n_Cow, n_Human, SE_Cow, SE_Human, NH4, Mean_Cow, coliform))

# Create one plot per variable using purrr::imap
library(purrr)
plots <- imap(numeric_vars, ~ {
  ggplot(dat, aes_string(x = .y, y = "rk_ratio")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8) +
    labs(title = paste("rk_ratio vs", .y),
         x = .y,
         y = "rk_ratio") +
    theme_minimal()
})

plots

# what about r:K on the y and DOC:TDN (or TDN:DOC) on the x?
dat <- dat %>%
  mutate(CN = DOC/TN,
         NC = TN/DOC)

ggplot(data = dat, aes(x = CN, y = rk_ratio))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  labs(y = "r:K ratio", x = "DOC:TN")+
  #scale_color_manual(values = gaypal, name = "Month")+
  theme_minimal()

ggplot(data = dat, aes(x = NC, y = rk_ratio))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  labs(y = "r:K ratio", x = "DOC:TN")+
  #scale_color_manual(values = gaypal, name = "Month")+
  theme_minimal()

cn <- lm(rk_ratio ~ CN, data = dat)
anova(cn) # p=0.97
summary(cn)$r.squared # R2 = 0

nc <- lm(rk_ratio ~ NC, data = dat)
anova(nc) # p=0.72
summary(nc)$r.squared # R2 = 0.004
