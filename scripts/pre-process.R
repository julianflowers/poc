## create single database
## 
## 
## 
# install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(needs)
needs(fs, tidyverse, data.table, readxl, conflicted, curl)
conflicted::conflicts_prefer(dplyr::select, dplyr::filter, purrr::map, dplyr::between)

## load data
#
## go to data directory
data <- here::here("data")
#

## identify data
## needs different functions for xlsx and csv files
#
xl <- fs::dir_ls(data, regexp = "xls")
csv <- fs::dir_ls(data, regexp = "csv")
#

####################################
## read datasets into R
xls <- map(xl, readxl::read_xlsx)
csvs <- map(csv, read_csv, show_col_types = FALSE)
##

## combine into single list
dfs <- c(xls, csvs)
###################################


#dfs$`/Users/julianflowers/proof-of-concept/data/Translated_Population_Data_with_Regions.csv` 
###################################
## look at structure
## area names - AMR data is saudi wide (not stratified be geographical or admnistrative unit) and not split be gender
## ## we want to make key diemnsion names and variable names consistent between datasets

cnames <- map(dfs, colnames)
#cnames

area_names <- c(cnames[2]$`/Users/julianflowers/proof-of-concept/data/Nonfatal Hospitalizations for Injuries data 2023 (8-7-2024).xlsx`[5], 
                cnames[3]$`/Users/julianflowers/proof-of-concept/data/Flu Vaccine Coverage 2023 updated.csv`[11], 
                cnames[5]$`/Users/julianflowers/proof-of-concept/data/Smoking 2022.csv`[2], 
                cnames[7]$`/Users/julianflowers/proof-of-concept/data/Translated_Population_Data_with_Regions.csv`[8]
)

age_names <- c(cnames[1]$`/Users/julianflowers/proof-of-concept/data/AMR 2022 poc.xlsx`[7], 
               cnames[2]$`/Users/julianflowers/proof-of-concept/data/Nonfatal Hospitalizations for Injuries data 2023 (8-7-2024).xlsx`[1],
               cnames[3]$`/Users/julianflowers/proof-of-concept/data/Flu Vaccine Coverage 2023 updated.csv`[3],
               cnames[4]$`/Users/julianflowers/proof-of-concept/data/Fully_Translated_Population_Data.csv`[2],
               cnames[5]$`/Users/julianflowers/proof-of-concept/data/Smoking 2022.csv`[7])

gender_names <- c(
                  cnames[2]$`/Users/julianflowers/proof-of-concept/data/Nonfatal Hospitalizations for Injuries data 2023 (8-7-2024).xlsx`[4],
                  cnames[3]$`/Users/julianflowers/proof-of-concept/data/Flu Vaccine Coverage 2023 updated.csv`[2],
                  cnames[4]$`/Users/julianflowers/proof-of-concept/data/Fully_Translated_Population_Data.csv`[6],
                  cnames[5]$`/Users/julianflowers/proof-of-concept/data/Smoking 2022.csv`[6]
                  )
#########################################


## population data

dfs$`/Users/julianflowers/proof-of-concept/data/Fully_Translated_Population_Data.csv` <- dfs$`/Users/julianflowers/proof-of-concept/data/Fully_Translated_Population_Data.csv` |>
    cbind(dfs$`/Users/julianflowers/proof-of-concept/data/Translated_Population_Data_with_Regions.csv`$Region)

#dfs$`/Users/julianflowers/proof-of-concept/data/Fully_Translated_Population_Data.csv`

dfs1 <- set_names(dfs[c(1:5)], c("amr", "injury", "flu", "pop", "smoking"))

## write datasets to file
dfs1 |>
    write_rds("data/df.rds")

###########################################
###########################################

## resape population data

pops <- dfs1$pop |>
    dplyr::select(Region = 8, Gender, age = `Single Age Group`, Population)

## aggregated regional populations

pops <- pops |>
    group_by(Region, Gender, age) |>
    reframe(Population = sum(Population)) |>
    mutate(age = parse_number(age), 
           age_band = cut(age, seq(0, 105, 5), right = FALSE) ,
           `1844` = dplyr::between(age, 18, 44), 
           `15+` = age >= 15, 
           paed = dplyr::between(age, 0, 18), 
           older = age >= 65)

## write to file
pops |> 
    write_csv("~/proof-of-concept/data/pops_1.csv")
############################################
############################################
## rename age field for indicator data

dfs1$amr <- rename(dfs1$amr, age = colnames(dfs1$amr[7]))
dfs1$injury <- rename(dfs1$injury, age = colnames(dfs1$injury[1]))
dfs1$flu <- rename(dfs1$flu, age = colnames(dfs1$flu[3]))
#dfs1$pop <- rename(dfs1$pop, age = colnames(dfs1$pop[2]))
dfs1$smoking <- rename(dfs1$smoking, age = colnames(dfs1$smoking[7]))

## rename gender field
## 

dfs1$smoking <- dfs1$smoking |>
    rename(Gender = patient_gender)

## rename area field
## 

dfs1$flu <- dfs1$flu |>
    rename(Region = region_en) 
##############################################

## for injury data convert dote of birth to age  - assume admission is 1st June 2023
## 

options(digit = 0)

dfs1$injury <- dfs1$injury |>
    mutate(age = round(ceiling(as.numeric(as.POSIXct("2023-06-01") - age))/365, 0))

## need to make sure that variable types match

# map(dfs1, str)  ## pop age is character field

## now count
## 

## now select age, region, gender and value fields
## 
## 

## write datasets to file
dfs1 |>
    write_rds("data/df.rds")


dfs2_agg <- map(2:3, \(x) dfs1[[x]] |> count(Region, age, Gender) |> mutate(id = names(dfs1[x]))) 

dfs3 <- bind_rows(dfs2_agg) |> drop_na(age) ## now have a long dataset of person counts with age, sex and area for injury, flu vacc and region 
##################################################


## next we need to map directorates to regions for smoking data
## this is quite complicated

if(!require("ggsflabel")) devtools::install_github("yutannihilation/ggsflabel", force = TRUE)

 needs(myScrapers, sf, curl, ggsflabel)

###################################################
## scrape sc data
url <- "https://www.moh.gov.sa/en/Ministry/Projects/TCP/Pages/default.aspx"

scc_dir <- get_page_links(url) %>%
    .[157:176] 

sc_dir_links <- paste0("https://www.moh.gov.sa", scc_dir)

sc_dir_names <- sc_dir_links |>
    basename()

#####################################################

## extract Google maps link of scc for each region and create data frame
sc_loc <- map(sc_dir_links, get_page_links) %>%
    map(\(x) x[grepl("https://goo.gl", x)]) %>%
    set_names(., sc_dir_names) |>
    enframe() |>
    mutate(name = str_remove(name, ".aspx"))

#####################################################

## function to extract lat - long from google map urls
get_coordinates_from_google_maps <- function(url) {
    # Follow the redirect to get the final URL
    url <- url
    response <- HEAD(url, config(followlocation = TRUE))
    final_url <- response$url
    
    # Use a regular expression to find the coordinates in the final URL
    match <- str_match(final_url, "@(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)")
    if (!is.na(match[1,2]) && !is.na(match[1,3])) {
        latitude <- as.numeric(match[1,2])
        longitude <- as.numeric(match[1,3])
        return(list(latitude = latitude, longitude = longitude))
    } else {
        return(NULL)
    }
}

###########################################

## run on scc locations
sc_coords <- sc_loc |>
    unnest(value) |>
    mutate(ll = map(value, get_coordinates_from_google_maps, .progress = TRUE))

## create table of sc clinic locations
sc_ll <- sc_coords |>
    unnest_wider(ll)

## convert to sf file (need to remove missing coordinate values)
sc_ll_sf <- sc_ll |>
    drop_na() |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sc_ll_sf
    read_sf("data/smok_loc.gml")

##############################################

## download KSA shape files

sa_shp <- curl_download("https://data.humdata.org/dataset/41ce9023-1d21-4549-a485-94316200aba0/resource/a0188b1b-2f40-4f27-8a43-25913a7378ca/download/sau_adm_gadm_20210525_shp.zip", destfile = tempfile())

tmpd <- tempdir()

sa_shp_1 <- curl_download("https://data.humdata.org/dataset/41ce9023-1d21-4549-a485-94316200aba0/resource/99834c81-ad34-415e-91c5-af053d8e55b4/download/sau_capp_adm1_1m_ocha.zip", destfile = tempfile())

#sa_pop_d <- curl_download("https://data.humdata.org/dataset/14b288ca-1855-4025-9f01-41cba548e6f6/resource/44baa2f6-b6d8-4018-b9c6-fd81b493ec22/download/sau_general_2020_geotiff.zip", destfile = tempfile())

sa_shp <- unzip(sa_shp, exdir = tmpd)

sa_shp_1 <- unzip(sa_shp_1, exdir = tmpd)

#sa_tif <- unzip(sa_pop_d, exdir = tmpd)

shps <- fs::dir_ls(tmpd, regexp = "shp$")

##########################################################

## boundary polygon file
sa_bound <- read_sf(shps[2]) 

sa_bound |> write_sf("data/ksa_bound.gml")

sc_ll_sf <- read_sf("data/smok_loc.gml")

sa_bound |>
    ggplot() +
    geom_sf(fill = "grey90") +
    ggplot2::geom_sf_label(aes(label = ADM1_EN)) +
    geom_sf(data = sc_ll_sf, aes(colour = name)) +
    theme_void() +
    scale_colour_viridis_d(option = "turbo", name = "Directorates")

# pops$Region |>
#     unique() |>
#     enframe() 
#     
#
############################################################

## create directorate - region lookup

reg_dir_lu <- sa_bound |>
    st_join(sc_ll_sf) |>
    st_drop_geometry() |>
    dplyr::select(ADM1_EN, name) |>
    group_by(ADM1_EN, name) |>
    summarise(n = n()) |>
    ungroup() |>
    group_by(name) |>
    arrange(name) |>
    filter(n == max(n)) |>
    dplyr::select(name, everything())

smok_1 <- dfs1$smoking |>
    mutate(directorate_name = recode(directorate_name, "Qurayyat" = "Al-Qurayyat", 
                                     "Qunfotha" = "AL-Qunfudah", 
                                     "AlAhsa" = "Al-Ahsa", 
                                     "Baha" = "Al-Baha",
                                     "Eastern" = "Eastern-Region", 
                                     "Hafer AlBatin" = "Hafr-Al-Batin",
                                     "Northern Borders" = "Northern-Borders",
                                     "Qassim" = "Al-Qassim", 
                                     "Jouf" = "Al-Jouf"
    )) |>
    left_join(reg_dir_lu, by = c("directorate_name" = "name")) |>
    rename(Region = ADM1_EN) |>
    select(Region, age, Gender) |>
    mutate(id = "Smoking")


## aggregate smoking data
smok_agg <- smok_1 |>
    count(Region, age, Gender, id) |>
    drop_na(age)

smok_agg |>
    write_csv("~/proof-of-concept/data/smoking.csv")

########################################################

## combine datasets and rename regions

dfs_3 <- bind_rows(dfs3, smok_agg) |>
    mutate(Gender = case_when(str_detect(Gender, "^[Ff]" ) ~ "female",
                              str_detect(Gender, "unknown") ~ "unknown",
                              TRUE ~ "male")) |>
    pivot_wider(names_from = Gender, values_from = n)

dfs_3 |> pluck("Region") |> unique()

## now we need to rename Region names to be consistent with the Census names
## 
## 

census_names <- dfs1$pop |> pluck("Region") |> unique()

# [1] "Al Bahah"                  "Al Jawf"                   "Al Hudud ash Shamaliyah"  
# [4] "Ar Riyadh"                 "Al Qasim"                  "Al Madinah al Munawwarah" 
# [7] "Al Mintaqah ash Sharqiyah" "Tabuk"                     "Jazan"                    
# [10] "Ha'il"                     "'Asir"                     "Makkah al Mukarramah"     
# [13] "Najran" 
# 
# [1] "Al Baha"                 "Al Jawf"                 "Al Qassim"              
# [4] "Asir"                    "Hail"                    "Jazan"                  
# [7] "Madinah"                 "Makkah"                  "Najran"                 
# [10] "Northern Frontier"       "Riyadh"                  "Sharqiya"               
# [13] "Makkah Al Mukarramah"    "Tabuk"                   "madina"                 
# [16] "Al Bahah"                "Al Hudud ash Shamaliyah" "Al Madinah"             
# [19] "Al Quassim"              "Ar Riyad"                "Ash Sharqiyah"          
# [22] "Ha'il"                   "Jizan"                   "`Asir"        

dfs_4 <- dfs_3 |>
    mutate(Region = case_when(str_detect(Region, "Baha") ~ "Al Bahah", 
                              str_detect(Region, "Qu?assim") ~ "Al Qasim",
                              str_detect(Region, "Asir") ~ "`Asir",
                              str_detect(Region, "Hail") ~ "Ha'il",
                              str_detect(Region, "[Mm]adin") ~ "Al Madinah al Munawwarah",
                              str_detect(Region, "Jizan") ~ "Jazan",
                              str_detect(Region, "Makka") ~ "Makkah al Mukarramah",
                              str_detect(Region, "Sharqiya" ) ~ "Al Mintaqah ash Sharqiyah",
                              str_detect(Region, "Northern Frontier") ~ "Al Hudud ash Shamaliyah",
                              str_detect(Region, "Riyad") ~ "Ar Riyadh",
                              TRUE ~ Region ))

dfs_4 |>
    #filter(id == "flu") |>
    write_rds("data/counts.rds")
    

## now create age bands with 80+ as terminal age band to match census data
## 
needs(Hmisc)
regional_counts <- dfs_4 |>
    filter(!is.na(age)) |>
    mutate(age_band = cut(age, breaks = seq(0, 118, 5), right = FALSE)) |>
    arrange(age) |>
    #fill(age_band, .direction = "down") |>
    #filter(is.na(age_band))
    group_by(Region, id, age_band) |>
    reframe(sum_f = sum(female, na.rm = TRUE), 
            sum_m = sum(male, na.rm = TRUE))


## fill in the gaps

 
regional_counts_complete <- regional_counts |>
    pivot_wider(names_from = id, values_from = c("sum_f", "sum_m")) |>
    complete(Region, age_band) 


## pops
## 

pop_agg <- pops |>
    mutate(age_band = cut(age, breaks = seq(0, 118, 5), right = FALSE)) |>
    group_by(Region, Gender, age_band) |>
    reframe(sum_pop = sum(Population)) |>
    pivot_wider(names_from = Gender, values_from = sum_pop)

pop_agg |>
    write_csv("~/proof-of-concept/data/populations.csv")

## link agg data and pops
## 

final_poc_data <- regional_counts_complete |>
    filter(!is.na(age_band)) |>
    mutate(Region = recode(Region, "`Asir" = "'Asir")) |>
    left_join(pop_agg, by = c("Region", "age_band")) |>
    pivot_longer(names_to = "Indicator", values_to = "Values", cols = 3:8) |>
    filter(!is.na(Values)) |>
    mutate(Gender = str_extract(Indicator, "_(m|f)"), 
           Indicator = str_remove(Indicator, "sum_(f|m)_"), 
           Gender = str_remove(Gender, "_")) |>
    #mutate(rate = ifelse(Gender == "_f", 100000 * Values / Female, 100000 * Values / Male)) |>
    pivot_wider(names_from = Gender, values_from = Values) 

## add uncertainty
## 

needs(PHEindicatormethods, epitools)
    
    
female_rates <- phe_rate(final_poc_data, x = f, n = Female) |>
    select(Region, age_band, Indicator, value, lowercl, uppercl, pop = Female, count = f) |>
    mutate(gender = "Female")

males_rates <- phe_rate(final_poc_data, x = m, n = Male) |>
    select(Region, age_band, Indicator, value, lowercl, uppercl, pop = Male, count = m) |>
    mutate(gender = "Male")

bind_rows(female_rates, males_rates) |>
    write_csv("~/proof-of-concept/data/pop_rates.csv")

regional_counts_complete |>
    write_csv("~/proof-of-concept/data/regional_counts.csv")

