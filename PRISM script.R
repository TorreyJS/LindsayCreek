################################################################################
# PRISM data for Lindsay Creek
#
# 6/13/25
################################################################################
library(prism);library(raster);library(sf);library(dplyr);library(openxlsx)
library(terra)

prism_set_dl_dir("PRISM")  # Replace with your actual folder

# get precip data:
get_prism_monthlys(type = "ppt", years = 2022, mon = 5:10, keepZip = FALSE)

# get temp data:
get_prism_monthlys(type = "tmean", years = 2022, mon = 5:10, keepZip = FALSE)


# field site info:
coords <- read.csv("LC site coords.csv")
# convert to a spatial object:
coords_sf <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)
site_ids <- coords$site

crs(coords_sf) 
crs(may_ppt_raster) # mismatch
 
# reproject site coords to match data
coords_sf_nad83 <- st_transform(coords_sf, crs = crs(may_ppt_raster))

## trying
# Function to extract data
extract_prism_data <- function(type) {
  # Get full PRISM data directory path
  prism_dir <- prism_get_dl_dir()
  
  # List PRISM folders
  folder_names <- prism_archive_ls()
  matching_folders <- folder_names[grepl(type, folder_names)]
  
  # Prepend full path
  matching_paths <- file.path(prism_dir, matching_folders)
  
  results <- purrr::map_dfr(matching_paths, function(folder_path) {
    print(folder_path)
    print(list.files(folder_path))  # Debug step
    
    bil_file <- list.files(
      folder_path,
      pattern = "\\.bil$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(bil_file) == 0) {
      warning("No .bil file found in: ", folder_path)
      return(NULL)
    }
    
    r <- terra::rast(bil_file)
    vals <- terra::extract(r, vect(coords_sf_nad83)) %>%
      pull(2)
    
    folder_name <- basename(folder_path)
    year <- as.numeric(stringr::str_sub(folder_name, -10, -7))
    month <- as.numeric(stringr::str_sub(folder_name, -6, -5))
    
    tibble(
      site_id = site_ids,
      year = year,
      month = month,
      type = type,
      value = vals
    )
  })
  
  return(results)
}



# Extract and combine all data
ppt_data <- extract_prism_data("ppt")
tmean_data <- extract_prism_data("tmean")

all_data <- bind_rows(ppt_data, tmean_data)

## #YAYAYAYAYAYAYAYYYYYYYYYYYYYYYYY
write.xlsx(all_data, "PRISM/monthly temp and precip.xlsx")

tp <- read.xlsx("PRISM/monthly temp and precip.xlsx")

## quick look at temp/precip:
library(ggplot2)
all_data$site_id <- factor(all_data$site_id, levels = c("HW","CCE","SK","MS","GUN","ODOM"))

ggplot(data = all_data, aes(x = month, y = value, color = type, group = type))+
  geom_point()+
  geom_line()+
  facet_grid(.~site_id)

library(plotrix)
prism_summary <- all_data %>%
  group_by(type, month) %>%
  summarise(mean_val = mean(value, na.rm = T),
            SE = std.error(value, na.rm = T)) %>%
  mutate(type = case_when(type == "ppt" ~ "Precipitation (mm)",
                              type == "tmean" ~ "Air Temperature (\u00B0C)"),
         Month = case_when(month == "5" ~ "May",
                           month == "6" ~ "June",
                           month == "7" ~ "July",
                           month == "8" ~ "Aug",
                           month == "9" ~ "Sep",
                           month == "10" ~ "Oct"))

prism_summary$Month <- factor(prism_summary$Month, levels = c("May","June","July","Aug","Sep","Oct"))

library(MetBrewer)
renoir_colors <- met.brewer("Renoir")
redon <- met.brewer("Redon")

ggplot(data = prism_summary, aes(x = Month, y = mean_val, fill = type, 
                                 shape = type, group = type))+
  geom_line(aes(linetype = type, color = type), linewidth = 0.75)+
  geom_errorbar(aes(ymin = mean_val-SE, ymax = mean_val+SE, color = type), width = 0.25)+
  geom_point(aes(fill = type), color = "black", size = 4)+
  scale_fill_manual(values = renoir_colors[c(3,9)], name = "")+
  scale_color_manual(values = renoir_colors[c(3,9)], name = "")+
  scale_linetype_manual(values = c("solid", "dashed"),name = "")+
  scale_shape_manual(values = c(21, 24), name = "")+
  theme_minimal()+
  ylab("Mean of all sites")+
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())

ggsave("Figures/mean temp and precip.png", height = 4, width = 4, units = "in",
       dpi = 350, bg = "white")

# I guess some quick stats:
library(tidyr);library(emmeans)
alld_wide <- all_data %>%
  pivot_wider(names_from = type, values_from = value)
alld_wide$month <- factor(alld_wide$month)

tmod <- lm(tmean ~ month, data = alld_wide) # ignore site for this
hist(tmod$residuals)
shapiro.test(tmod$residuals) # 0.0004
anova(tmod) # month p<0.001, F5,30 = 2118.3
emmeans(tmod, pairwise ~ month) # all months are sig diff I guess

pmod <- lm(ppt ~ month, data = alld_wide) # ignore site for this
hist(pmod$residuals)
shapiro.test(pmod$residuals) # 0.0005
anova(pmod) # month p<0.001, F5,30 = 139.2
emmeans(pmod, pairwise ~ month) # all months are sig diff except July vs Aug and Sep vs Oct


## summaries:
monthlyprecip <- alld_wide %>%
  group_by(month) %>%
  summarise(meanppt = mean(ppt, na.rm = T),
            SEppt = std.error(ppt, na.rm = T))







################################################################################
### what about DAILY data????
# only D2 is available, which is difference from mean--can look into this further
prism_set_dl_dir("PRISM/Daily")

# download
# Precipitation
get_prism_dailys(
  type = "ppt", 
  minDate = "2022-05-01", 
  maxDate = "2022-10-31", 
  keepZip = FALSE
)

# Mean temperature
get_prism_dailys(
  type = "tmean", 
  minDate = "2022-05-01", 
  maxDate = "2022-10-31", 
  keepZip = FALSE
)

# list all the downloaded folders:
prism_daily_dirs <- prism_archive_ls()

# Function to extract values:
extract_prism_daily <- function(folder_path, site_coords) {
  # Get .bil file inside the folder
  bil_file <- list.files(
    folder_path,
    pattern = "\\.bil$",  # match any .bil file
    full.names = TRUE
  )
  
  # Skip folder if no .bil file found
  if (length(bil_file) != 1) {
    message("No .bil file found in: ", folder_path)
    return(NULL)
  }
  
  # Read raster
  r <- raster(bil_file)
  
  # Reproject coords to raster CRS
  site_coords_proj <- st_transform(site_coords, crs = crs(r))
  values <- extract(r, as(site_coords_proj, "Spatial"))
  
  # Pull info from folder name
  folder_name <- basename(folder_path)
  variable <- str_extract(folder_name, "ppt|tmean")
  date_str <- str_extract(folder_name, "\\d{8}")  # daily uses YYYYMMDD
  date <- as.Date(date_str, format = "%Y%m%d")
  
  tibble(
    site_name = site_coords_proj$site_name,
    date = date,
    variable = variable,
    value = values
  )
}

# run the function:
daily_data <- lapply(prism_daily_dirs, extract_prism_daily, site_coords = coords_sf_wgs84) %>%
  bind_rows()








# ############## SINGLE MONTH TEST EXAMPLE:
# ## May precipitation:
# may_ppt_path <- "PRISM/PRISM_ppt_stable_4kmM3_202205_bil/PRISM_ppt_stable_4kmM3_202205_bil.bil"
# may_ppt_raster <- raster(may_ppt_path) 
# 
#
# # extract precip values
# ppt_values <- raster::extract(may_ppt_raster, as(coords_sf_nad83, "Spatial"))
# 
# # join back to site data
# result <- coords_sf_nad83 %>%
#   st_drop_geometry() %>%
#   mutate(may_2022_ppt_mm = ppt_values)
# 
# #####################################3


dailyprecip <- read.xlsx("PRISM/daily precip.xlsx", sheet = 2)

dailyprecip$date <- as.Date(as.numeric(dailyprecip$Date), origin = "1899-12-30")
dailyprecip <- dailyprecip %>%
  mutate(month = format(date, "%B"))
library(ggplot2)
ggplot(dailyprecip, aes(x = date, y = `ppt.(inches)`))+
  geom_point()

rainydays <- dailyprecip %>% filter(`ppt.(inches)` > 0)
str(rainydays$date)

sample_dates <- as.Date(c("2022-05-26", "2022-06-23", "2022-07-29", 
                          "2022-08-26", "2022-09-30", "2022-10-28"))

# create a window around the sample dates  (5 days before)

# make a dataframe of all windows
windows <- tibble(
  sample_date = sample_dates,
  start_date  = sample_dates - 4,
  end_date    = sample_dates
)

hits <- dailyprecip %>%
  rowwise() %>%
  filter(
    `ppt.(inches)` > 0,
    any(date >= windows$start_date & date <= windows$end_date)
  )

hits <- hits %>%
  mutate(sample_period = format(date, "%B"))

rain_summary <- hits %>%
  group_by(sample_period) %>%
  summarise(total_precip = sum(`ppt.(inches)`))%>%
  mutate(precip_mm = total_precip*25.4)


### precip figure (SI)
library(MetBrewer)

cols <- met.brewer("Tsimshian")

dailyprecip$month <- factor(dailyprecip$month, levels = c("May","June","July",
                                                          "August","September",
                                                          "October","November"))

ggplot(dailyprecip, aes(x = date, y = `ppt.(inches)`*25.4))+
  geom_line(linewidth = 1.15)+
  #scale_color_manual(values = cols)+
  geom_vline(xintercept = sample_dates, linetype = "dashed", color = "black")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line())+
  labs(x = "Date", y = "Precipitation (mm)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")

ggsave("Figures/precip_and_samples_bw.png", height = 4, width = 6, units = "in",
       dpi = 350, bg = "white")
