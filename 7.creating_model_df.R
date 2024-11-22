rm(list=ls())
library(WDI)
################################################ flags
path <- "./data/combined.feather"
combined <- arrow::read_feather(path) %>% select("country", "cropname", "yield", "year", "iso_a3")
flags <- read_csv("./data/faostat_all_flags.csv")[,c(3,4,8,10,13,14)]
colnames(flags) <- c("iso_a3", "fao", "item", "year", "flag", "flag_desc")
flags <- flags %>% filter(flag_desc != "Official figure")

crop_key<- cbind(sort(unique(flags$item)), sort(unique(combined$cropname)))
crop_key[7,1] <- "Potatoes"
crop_key[8,1] <- "Other pulses n.e.c."
colnames(crop_key) <- c("item", "cropname")
flags <- left_join(flags, crop_key, by="item", copy=TRUE) %>% select(-c(item, flag, flag_desc)) %>% mutate(flag = 1)

country_key <- read_csv("./data/country_key.csv")
flags <- left_join(flags, country_key[,c("country", "fao", "iso_a3")], by = c("fao", "iso_a3")) %>% filter(!is.na(country)) %>% select(-"fao")

flags_data <- left_join(combined, flags, by=c("country", "iso_a3", "cropname", "year")) 
flags <- flags_data %>% group_by(country, cropname, iso_a3) %>% 
  mutate(count = n(),
         flags = sum(flag, na.rm=T),
         flag_pct = flags/count) %>% select(country, cropname, iso_a3, flag_pct) %>% distinct()


################################################ gdp per cap, not just gdp
gdp_data <- WDI(indicator = "NY.GDP.PCAP.CD",  
  start = 2000, end = 2020, extra = TRUE) %>% select(-c(iso2c, lastupdated, region, capital, longitude, latitude, lending, status)) %>% filter(income!="Aggregates"| is.na(income))
colnames(gdp_data) <- c("wb", "iso_a3", "year", "gdp_cap", "inc_class")
gdp_data$year <- as.numeric(as.character(gdp_data$year))

same_class <-gdp_data %>% group_by(wb) %>% count(inc_class) %>%
  pivot_wider(names_from = inc_class, values_from = n, values_fill = 0)
diff_class <- same_class %>% filter(if_any(everything(), ~ . != 0 & . != 21))
gdp_data <- gdp_data %>% group_by(wb, iso_a3, inc_class) %>% filter(!all(is.na(gdp_cap))) %>% 
  summarise(gdp_cap_trend= lm(gdp_cap~year)$coefficient[2],
            avg_gdp_cap = mean(gdp_cap, na.rm=T))

gdp <- left_join(gdp_data, country_key[,c("wb", "country", "iso_a3")], by="iso_a3") %>% filter(!is.na(country)) %>% select(-c("wb.x", "wb.y"))

gdp <- gdp %>%
  mutate(inc_class = if_else(iso_a3 == "VNM", "Lower middle income", inc_class),
         inc_class = if_else(iso_a3 == "CZE", "High income", inc_class))

################################################ old stuff
################################################ yield/csif variables
path <- "./data/combined.feather"
combined <- arrow::read_feather(path)
combined <- combined %>% group_by(country,cropname,whichlag, iso_a3) %>%
  summarise(avg_csif = mean(csif, na.rm=T),
            avg_yield = mean(yield, na.rm=T),
            avg_gridcells = mean(gridcells, na.rm=T),
            total_gridcells = sum(gridcells, na.rm=T), 
            max_yield = max(yield, na.rm=T),
            max_csif = max(csif, na.rm=T))

path <- "./data/combined_regs.feather"
regs <- arrow::read_feather(path)
regs$resid_var<-as.numeric(regs$resid_var)
regs$r2 <- as.numeric(regs$r2)

x<- regs$resid_var
b <- MASS::boxcox(lm(x ~ 1))
lambda <- b$x[which.max(b$y)]
regs$resid_var_bc <- (x ^ lambda - 1) / lambda

x<- regs$r2
b <- MASS::boxcox(lm(x ~ 1))
lambda <- b$x[which.max(b$y)]
regs$r2_bc <- (x ^ lambda - 1) / lambda

################################################ corruption
corrupt <- read_csv("./data/corruption.csv") %>% select(-matches("Rank|Sources|Standard"))
corrupt <- reshape2::melt(corrupt, id=1:2, variable.name= "year", value.name="corrupt_score")
colnames(corrupt) <- tolower(colnames(corrupt))
corrupt$year <- as.numeric(gsub("CPI score |CPI Score", "", corrupt$year))
corrupt_data <- corrupt %>% group_by(country, iso3) %>% 
  mutate(corrupt_trend = lm(corrupt_score~year)$coefficient[2],
         corrupt_change = max(corrupt_score, na.rm=T) - min(corrupt_score, na.rm=T),
         corrupt_score = mean(corrupt_score, na.rm=T)) %>% select(-year) %>% distinct() %>% 
  rename("country_c" = "country", "iso_a3" = "iso3")
corrupt <- left_join(corrupt_data, country_key[,c("country", "iso_a3")], by="iso_a3") %>% ungroup()%>% select(-country_c)
filter(corrupt, is.na(country))

################################################ modis
path <- "./data/modis_variables.feather"
vars <- arrow::read_feather(path) %>% select(-c(crop, majcrop_rank)) %>% distinct() %>% group_by(across(c(-majcrop))) %>% mutate(hm = str_c(majcrop, collapse="_"))
vars[c("majcrop1", "majcrop2")] <-str_split_fixed(vars$hm, '_', 2)
vars <- vars %>% select(-c(majcrop, hm)) %>% distinct()
vars <-vars %>% mutate(lc = ifelse((majcrop1 == "14" | majcrop1=="12"), "cropland", majcrop1),
       lc= ifelse(majcrop1 %in% c("1", "2", "3", "4", "5"), "forest", lc),
       lc = ifelse(majcrop1 %in% c("6", "7", "8", "9", "10"), "sav/grass/shrub", lc),
       lc = ifelse(majcrop1 == "13", "urban", lc),
       lc = ifelse(majcrop1 == "16", "barren", lc)) 

################################################ joining togeether
df_comb <- purrr::reduce(list(regs, vars, combined), dplyr::left_join, by = c("cropname", "country"))
socials <- full_join(gdp, corrupt, by=c("country", "iso_a3"))
df <- left_join(df_comb, socials, by=c("country", "iso_a3"))
df <- left_join(df, flags, by=c("country", "iso_a3", "cropname"))

colSums(is.na(df))
save(df, file="./data/df_model.Rda")
