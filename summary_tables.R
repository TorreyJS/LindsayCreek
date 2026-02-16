################################################################################
# Lindsay Creek Summary Tables
#
# 5/8/25
################################################################################
library(dplyr);library(plotrix);library(openxlsx)
# use "dat4c" or "full data_TS.xlsx"

dat <- read.xlsx("full data_TS.xlsx")

### get mean by position for each variable:
selected_cols <- c(6:21,26:37)

pos_sum <- dat %>%
  group_by(position) %>%
  summarise(across(
    .col = all_of(names(dat)[selected_cols]),
    .fns = list(mean = ~mean(.x, na.rm = T),
                se = ~std.error(.x, na.rm = T)),
    .names = "{.col}_{.fn}"
  ))

write.xlsx(pos_sum, "Mean values by position_ignore MST.xlsx")

### and now for each month:
month_sum <- dat %>%
  group_by(month) %>%
  summarise(across(
    .col = all_of(names(dat)[selected_cols]),
    .fns = list(mean = ~mean(.x, na.rm = T),
                se = ~std.error(.x, na.rm = T)),
    .names = "{.col}_{.fn}"
  ))

write.xlsx(month_sum, "Mean values by month_ignore MST.xlsx")


####### MST
mstdat <- read.xlsx("MST scaled to copiesper100ml.xlsx")

## drop anything where only 1 rep had data (keep where 2 reps had data)
mst_sum <- mstdat %>%
  group_by(Target.Name, month, site) %>%
  summarise(copiesper100 = mean(copies_per_100, na.rm = T),
            SE = std.error(copies_per_100, na.rm = T),
            repcount = sum(!is.na(copies_per_100)),
            num_possible = n())

## how often was each gene found?
mst_sum %>% 
  group_by(Target.Name) %>%
  summarise(count = sum(!is.na(copiesper100))) # 13 of 30 cow, 24 of 30 human

## but which gene was more abundant?
mst_sum %>% 
  group_by(Target.Name) %>%
  summarise(avg = mean(copiesper100, na.rm = T)) # 95.6 cow, 23.8 human
# same question, without the huge outlier at SK:
mst_sum %>%
  filter(copiesper100 <700) %>%
  group_by(Target.Name) %>%
  summarise(avg = mean(copiesper100, na.rm = T)) # 34.5 cow, 23.8 human


## remove observations where only 1 rep wasn't "NA":
mst_filtered <- mst_sum %>% filter(repcount >1)

write.xlsx(mst_filtered, "MST_scaled_singles removed.xlsx")

pos_sum2 <- mst_filtered %>%
  group_by(site, Target.Name) %>%
  summarise(genecopiesper100 = mean(copiesper100, na.rm = T),
            SE = std.error(copiesper100, na.rm = T))

write.xlsx(pos_sum2, "mean values for MST by position.xlsx")

month_sum2 <- mst_filtered %>%
  group_by(month, Target.Name) %>%
  summarise(genecopiesper100 = mean(copiesper100, na.rm = T),
            SE = std.error(copiesper100, na.rm = T))

write.xlsx(month_sum2, "mean values for MST by month.xlsx")


## cute little publication figures
# 1. merge MST and chem data
# 1a: monthly 

mst_month_wide <- month_sum2 %>%
  pivot_wider(names_from = Target.Name, values_from = c(genecopiesper100, SE)) %>%
  mutate(month = case_when(month == "6" ~ "June",
                           month == "7" ~ "July",
                           month == "8" ~ "Aug",
                           month == "9" ~ "Sep",
                           month == "10" ~ "Oct"))

month_both <- left_join(month_sum, mst_month_wide, by = "month")

library(dplyr);library(tidyr);library(stringr)

# Step 1: Round all numeric values to 2 decimals
month_round <- month_both %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
colnames(month_round)[58:61] <- c("CowGene_mean", "HumanGene_mean", "CowGene_se", "HumanGene_se")
month_round <- month_round[,-c(30:33)]
colnames(month_round)[52:53] <- c("rk_mean","rk_se")


# Step 2: Pivot longer to separate variable, stat (mean or se), and value
tidy_table <- month_round %>%
  pivot_longer(
    cols = -month,
    names_to = c("variable", "stat"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    formatted = paste0(mean, " ± (", se, ")")
  ) %>%
  select(month, variable, formatted) %>%
  pivot_wider(
    names_from = variable,
    values_from = formatted
  )

write.xlsx(tidy_table, "Monthly Summary_cute.xlsx")

## by position
# 1b: position 

mst_site_wide <- pos_sum2 %>%
  pivot_wider(names_from = Target.Name, values_from = c(genecopiesper100, SE))
colnames(mst_site_wide)[1] <- "position"


pos_both <- left_join(pos_sum, mst_site_wide, by = "position")

# Step 1: Round all numeric values to 2 decimals
pos_round <- pos_both %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
colnames(pos_round)[58:61] <- c("CowGene_mean", "HumanGene_mean", "CowGene_se", "HumanGene_se")
pos_round <- pos_round[,-c(30:33)]
colnames(pos_round)[52:53] <- c("rk_mean","rk_se")


# Step 2: Pivot longer to separate variable, stat (mean or se), and value
tidy_pos <- pos_round %>%
  pivot_longer(
    cols = -position,
    names_to = c("variable", "stat"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    formatted = paste0(mean, " ± (", se, ")")
  ) %>%
  select(position, variable, formatted) %>%
  pivot_wider(
    names_from = variable,
    values_from = formatted
  )

write.xlsx(tidy_pos, "Position Summary_cute.xlsx")


# merge dat and mst_filtered to get a huge summary table


## calculations for in-text results:
# 1. conductivity for sites 1-2 in June-Sept (mean +/- SE)
dat %>% filter(month %in% c("June","July","Aug","Sept") & position %in% c("HW","CCE"))%>%
  summarise(MEAN = mean(cond, na.rm = T),
            SE = std.error(cond, na.rm = T))
# 1b. conductivity for sites 3-6 in the same months:
dat %>% filter(month %in% c("June","July","Aug","Sept") & position %in% c("SK","MAIN","ODOM","GUN"))%>%
  summarise(MEAN = mean(cond, na.rm = T),
            SE = std.error(cond, na.rm = T))
# october, all sites:
dat %>% filter(month == "Oct")%>%
  summarise(MEAN = mean(cond, na.rm = T),
            SE = std.error(cond, na.rm = T))
