################################################################################
# Lindsay Creek PCAs
#
# 5/14/25
################################################################################
# use "LC_FullData.xlsx"

dat <- read.xlsx("LC_FullData.xlsx")

#### PCA: 
# make sure all variables are numeric (no NAs, no metadata)--check column #s
dat_pca <- dat[,c(6:14,16,17,26:37)]
dat_pca <- na.omit(dat_pca)

pca_result <- prcomp(dat_pca, scale. = TRUE)

pca_scores <- as.data.frame(pca_result$x)

dat_clean <- dat[c(1:14,16,18:36),]
pca_scores$position <- dat_clean$position
pca_scores$month <- dat_clean$month

library(ggplot2)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = position, shape = month)) +
  geom_point(size = 3) +
  labs(
    x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"), #29.7
    y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)") #18.4
  ) +
  theme_minimal()

# Get loadings
loadings <- as.data.frame(pca_result$rotation[, 1:2])
loadings$variable <- rownames(loadings)
loadings <- loadings %>%
  mutate(type = case_when(variable %in% c("discharge", "PO4","NO3","DO","ph", "DOsat",
                                          'cond','temp','turbidity',"DOC", "TN") ~ "chemical",
                          TRUE ~ "biological"))

library(ggrepel)
# Plot with loadings as arrows
renoir_colors <- met.brewer("Renoir")

pca_scores <- pca_scores %>%
  mutate(Position = case_when(position == "HW" ~ "1 - HW",
                              position == "CCE" ~ "2 - CCE",
                              position == "SK" ~ "3 - SK",
                              position == "GUN" ~ "4 - GUN",
                              position == "MAIN" ~ "5 - MAIN",
                              position == "ODOM" ~ "6 - ODOM"),
         Month = factor(month, levels = c("May","June","July","Aug","Sep","Oct")))

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, aes(shape = Month, color = position)) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1*12, yend = PC2*12), 
               arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * 12.2, y = PC2 * 12.2, label = variable),
                  color = ifelse(loadings$type == "chemical", "#1f78b4", "#e31a1c"),
                  size = 4, max.overlaps = 20, show.legend = FALSE)+
  labs(x = "PC1 (29.7%)", y = "PC2 (18.4%)")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()

#ggsave("Figures/PCA of all data.png", height = 8, width = 8, units = "in",
#       bg = "white", dpi = 350)

## make two versions: one for month, one for site
# also add ellipses, r2, p-value, and adjust labels

## MONTH:

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, aes(color = Month)) +
  xlim(-10, 10)+
  ylim(-10,10)+
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1*25, yend = PC2*25), 
               arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
  stat_ellipse(aes(color = Month), level = 0.95, linetype = "dashed", linewidth = 0.7)+
  geom_text(data = loadings,
            aes(x = PC1*26, y = PC2*26, label = variable),
            color = ifelse(loadings_label$type == "chemical", "#1f78b4", "black"),
            size = 4, show.legend = FALSE)+
  labs(x = "PC1 (29.7%)", y = "PC2 (18.4%)")+
  scale_color_manual(values = gaypal)+
  theme_minimal()

ggsave("Figures/Month PCA_ugly.png", height = 5, width = 7, units = "in", 
       dpi = 350, bg = "white")

### SITE: this is more interesting to me!
loadings <- loadings %>% filter(variable != "DO")

labeldf <- data.frame(
  variable = c("Firmicutes", "Cyanobacteria", "cond","rk_ratio","PO4","temp",
               "DOC","Patescibacteria","Proteobacteria","Bdellovibrionota", 
               "TN", "NO3", "Myxococcota", "DOsat", "turbidity", "Bacteroidota",
               "discharge", "ph", "Actinobacteriota","Planctomycetota", 
               "Verrucomicrobiota", "Rare"),
  var_name = c("Firmicutes", "Cyanobacteria", "Conductivity", "r:K ratio", "PO4","Temp.",
               "DOC", "Patescibacteria","Proteobacteria", "Bdellovibrionota", 
               "Total N", "NO3", "Myxococcota", "DO sat.", "Turbidity", "Bacteroidota",
               "Discharge", "pH", "Actinobacteriota", "Planctomycetota",
               "Verrucomicrobiota", "Rare Phyla"),
  x = c(-1, -7.5, -5, -4.2, -0.8, -0.9, 
        0.4, 4, 5.5, 6.3, 
        6, 6, 6.25, 6.3, 4.8, 5.8, 
        4.6, 6, 0.2, 7.6, 
        7.3, 2.3),
  y = c(-1.4, -3.2, 3.6, 7, 5.2, 3, 
        6.5, 7.5, 6.65, 5.8, 
        5, 4, 1.9, 1, 0.1, -0.8, 
        -1.65, -3, -2.4, -4.4, 
        -5.4, -4.7)
)

loadings_label <- left_join(loadings, labeldf, by = "variable")

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, aes(color = position)) +
  xlim(-11.2, 9)+
  ylim(-8, 8)+
  stat_ellipse(aes(color = position), level = 0.95, linetype = "dashed", linewidth = 0.7)+
  geom_segment(data = loadings_label,
               aes(x = 0, y = 0, xend = PC1*20, yend = PC2*20), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = ifelse(loadings$type == "chemical", "#1f78b4", "black"),
               linewidth = 0.8) +
  geom_label(data = loadings_label,
             aes(x = x, y = y, label = var_name),
             color = ifelse(loadings$type == "chemical", "#1f78b4", "black"),
             size = 4, show.legend = FALSE)+
  labs(x = "PC1 (29.7%)", y = "PC2 (18.4%)")+
  scale_color_manual(values = renoir_colors[c(2,4,6,8,10,12)], name = "Site")+
  theme_minimal()

ggsave("Figures/Position PCA.png", height = 5, width = 7, units = "in",
       dpi = 350, bg = "white")

# for reference (to align labels)
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, aes(color = Position)) +
  xlim(-11, 9)+
  ylim(-8, 8)+
  stat_ellipse(aes(color = Position), level = 0.95, linetype = "dashed", linewidth = 0.7)+
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1*20, yend = PC2*20), 
               arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
  geom_text(data = loadings,
            aes(x = PC1*21, y = PC2*21, label = variable),
            color = ifelse(loadings$type == "chemical", "#1f78b4", "#e31a1c"),
            size = 4, show.legend = FALSE)+
  labs(x = "PC1 (29.7%)", y = "PC2 (18.4%)")+
  scale_color_manual(values = gaypal)+
  theme_minimal()

### get R2 and p values for PCA:
library(vegan)
# center and scale the data
dat_scale <- scale(dat_pca)
class(dat_scale)
dat_meta <- dat[c(1:14,16,18:36),2:3]

str(dat_scale)
#run PERMANOVA
adonis_result <- adonis2(dat_scale ~ month, data = dat_meta, method = "euclidean",
                         permutations = 999)
adonis_result # R2 = 49.7% of variance explained, P<0.001 = positions differ

# for month: 0.219 and p=0.027
