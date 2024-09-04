#### IMPORT IA DATA ############################################################

source("R/01_startup.R")
CSD <- qread("output/CSD.qs")
CD <- qread("output/CD.qs", nthreads = availableCores())
monthly <- qread("output/monthly.qs", nthreads = availableCores())


# # Download CSVs -----------------------------------------------------------
# 
# links <- read_csv("data/ca_links.csv")
# 
# files <-
#   links$link |> 
#   str_remove(".*canada/") |> 
#   str_remove("\\?.*")
# 
# dates <- 
#   files |> 
#   str_remove("\\/.*")
# 
# types <-
#   files |> 
#   str_remove("^.*\\/") |> 
#   str_remove(".csv.gz")
# 
# paths <- paste0("data/ia/", types, "_", dates, ".csv.gz")
# 
# options(timeout = 600)
# walk2(links$link, paths, download.file)


# Import data -------------------------------------------------------------

paths <- 
  list.files("data/ia", full.names = TRUE) |> 
  str_subset("listing") |> 
  str_subset("hosts", negate = TRUE)

months <-
  paths |> 
  str_remove("data/ia/listings_") |> 
  str_remove(".csv") |> 
  as.Date() |> 
  lubridate::round_date("month") |> 
  (\(x) x %m-% period(1, "month"))()

stopifnot(length(months) == length(unique(months)))

monthly_ia <- 
  paths |> 
  map2(months, \(x, y) {
    x |> 
      read_csv(col_types = cols_only(
        id = col_character(),
        last_scraped = col_date(format = ""),
        host_id = col_character(),
        room_type = col_character(),
        property_type = col_character(),
        latitude = col_double(),
        longitude = col_double(),
        region_name = col_character(),
        region_parent_parent_name = col_character(),
        calculated_host_listings_count_entire_homes = col_double(),
        calculated_host_listings_count_private_rooms = col_double(),
        minimum_minimum_nights = col_double())) |> 
      mutate(property_ID = paste0("ab-", id), .before = id) |> 
      mutate(month = yearmonth(y)) |> 
      select(property_ID, month, host_ID = host_id, listing_type = room_type, 
             property_type, latitude, longitude, 
             province1 = region_name,
             province2 = region_parent_parent_name,
             host_eh = calculated_host_listings_count_entire_homes,
             host_pr = calculated_host_listings_count_private_rooms,
             min_stay = minimum_minimum_nights)
  }) |> 
  bind_rows()


# Clean up ----------------------------------------------------------------

monthly_ia <-
  monthly_ia |> 
  mutate(any_eh = sum(listing_type == "Entire home/apt") > 0, 
         .by = property_ID) |> 
  mutate(listing_type = if_else(any_eh, "Entire home/apt", listing_type)) |> 
  select(-any_eh) |> 
  mutate(multi = host_eh > 1 | host_pr > 2, .before = latitude) |> 
  select(-host_eh, -host_pr)

# Reconcile host differences
hosts <- 
  monthly_ia |> 
  count(property_ID, host_ID) |> 
  summarize(host_ID = first(host_ID[n == max(n)]), .by = property_ID)

# Reconcile lat/lon differences
lat <- 
  monthly_ia |> 
  count(property_ID, latitude) |> 
  summarize(latitude = first(latitude[n == max(n)]), .by = property_ID)

lon <- 
  monthly_ia |> 
  count(property_ID, longitude) |> 
  summarize(longitude = first(longitude[n == max(n)]), .by = property_ID)

monthly_ia <-
  monthly_ia |> 
  select(-host_ID, -latitude, -longitude) |> 
  inner_join(hosts) |> 
  inner_join(lat) |> 
  inner_join(lon) |> 
  relocate(host_ID, .after = month)

rm(hosts, lat, lon)


# Filter to province ------------------------------------------------------

monthly_ia_CA <- monthly_ia

monthly_ia <- 
  monthly_ia |> 
  filter(str_detect(province1, prov_name) | str_detect(province2, prov_name)) |> 
  select(-province1, -province2)


# Attach geographies ------------------------------------------------------

CSD_to_join <- 
  monthly_ia |> 
  slice(1, .by = property_ID) |> 
  strr_as_sf(3347) |> 
  select(property_ID) |>
  mutate(CSDUID = CSD$CSDUID[st_nearest_feature(geometry, CSD)]) |> 
  st_drop_geometry()

CSD_to_join <- 
  CSD_to_join |> 
  inner_join(CSD, by = "CSDUID") |> 
  select(property_ID, CSDUID, city, CMAUID)

tourism_zones <- list(
  van_island = c("5943", "5924", "5926", "5923", "5921", "5919", "5917"),
  vancouver = c("5915", "5929", "5931", "5909", "5927"),
  cariboo = c("5945", "5941"),
  kootenay = c("5901", "5903", "5905", "5939"),
  northern = c("5957", "5959", "5949", "5955", "5951", "5953", "5947"),
  thom_ok = c("5933", "5907", "5937", "5935")) |> 
  enframe() |> 
  unnest(value) |> 
  rename(tourism = name, CDUID = value)

CD <- 
  CD |> 
  left_join(tourism_zones, by = "CDUID") |> 
  relocate(geometry, .after = last_col())

CD_to_join <-
  monthly_ia |>
  slice(1, .by = property_ID) |>
  strr_as_sf(3347) |>
  select(property_ID) |>
  mutate(CDUID = CD$CDUID[st_nearest_feature(geometry, CD)]) |>
  st_drop_geometry()

CD_to_join <-
  CD_to_join |>
  inner_join(CD, by = "CDUID") |>
  select(property_ID, CDUID, tourism)

monthly_ia <-
  monthly_ia |> 
  inner_join(CSD_to_join, by = "property_ID") |> 
  inner_join(CD_to_join, by = "property_ID") |> 
  relocate(city, CSDUID, CDUID, CMAUID, tourism, .after = property_type)

rm(CSD_to_join, CD_to_join)


# Add city_type -----------------------------------------------------------

monthly_ia <-
  monthly_ia |> 
  mutate_city_type()


# Find housing listings ---------------------------------------------------

monthly_ia_all <- monthly_ia

monthly_ia_all <-
  monthly_ia_all |> 
  strr_housing() |> 
  mutate(housing = if_else(property_type %in% c(
    "Entire home", "Private room in home", "Entire rental unit", "Entire condo",
    "Entire residential home", "Private room in residential home",
    "Private room in rental unit", "Tiny home", "Entire condominium (condo)",
    "Entire vacation home", "Private room in condo",
    "Private room in condominium (condo)", "Private room in vacation home",
    "Shared room in home", "Shared room in rental unit",
    "Shared room in residential home", "Private room in earthen home",
    "Home", "condo", "apartment", "house", "townhome", "cottage", "bungalow",
    "studio", "Vacation home"), TRUE, housing)) |> 
  relocate(housing, .after = property_type)

monthly_ia <- 
  monthly_ia_all |> 
  filter(housing) |> 
  select(-housing)

monthly_ia_CA_all <- monthly_ia_CA

monthly_ia_CA_all <-
  monthly_ia_CA_all |> 
  strr_housing() |> 
  mutate(housing = if_else(property_type %in% c(
    "Entire home", "Private room in home", "Entire rental unit", "Entire condo",
    "Entire residential home", "Private room in residential home",
    "Private room in rental unit", "Tiny home", "Entire condominium (condo)",
    "Entire vacation home", "Private room in condo",
    "Private room in condominium (condo)", "Private room in vacation home",
    "Shared room in home", "Shared room in rental unit",
    "Shared room in residential home", "Private room in earthen home",
    "Home", "condo", "apartment", "house", "townhome", "cottage", "bungalow",
    "studio", "Vacation home"), TRUE, housing)) |> 
  relocate(housing, .after = property_type)

monthly_ia_CA <- 
  monthly_ia_CA_all |> 
  filter(housing) |> 
  select(-housing)


# Generate trend coefficients ---------------------------------------------

ia_month <- 
  monthly_ia |> 
  summarize(n_ia = n(), EH_ia = mean(listing_type == "Entire home/apt"), 
            multi_ia = mean(multi), .by = month)

ad_month <- 
  monthly |> 
  summarize(n_ad = n(), scraped = sum(scraped), active = sum(A + R), 
            EH_ad = mean(listing_type == "Entire home/apt"),
            multi_ad = mean(multi), rev = sum(rev),
            FREH = sum(FREH), FREH_3 = sum(FREH_3), .by = month)

coefs_table <- 
  ad_month |> 
  inner_join(ia_month, by = "month") |> 
  mutate_days() |> 
  mutate(month,
         n_trend = n_ad / n_ia,
         scraped_trend = scraped / n_ia,
         a_trend = active / n_ia / days,
         rev_trend = rev / n_ia,
         FREH_trend = FREH / n_ia,
         FREH_3_trend = FREH_3 / n_ia,
         EH_trend = EH_ad / EH_ia,
         multi_trend = multi_ad / multi_ia,
         .keep = "none")

coef_val <- 13:14
coefs <- list()

coefs$scraped <- mean(coefs_table$scraped_trend[coef_val])
coefs$active <- mean(coefs_table$a_trend[coef_val])
coefs$FREH <- mean(coefs_table$FREH_trend[coef_val])
coefs$FREH_3 <- mean(coefs_table$FREH_3_trend[coef_val])
coefs$EH <- mean(coefs_table$EH_trend[coef_val])
coefs$multi <- mean(coefs_table$multi_trend[coef_val])
coefs$rev <- coefs_table$rev_trend[coef_val[2]]

# By city_type
ia_month_type <- 
  monthly_ia |> 
  summarize(n_ia = n(), EH_ia = mean(listing_type == "Entire home/apt"), 
            multi_ia = mean(multi), .by = c(month, city_type))

ad_month_type <- 
  monthly |> 
  summarize(n_ad = n(), scraped = sum(scraped), active = sum(A + R), 
            EH_ad = mean(listing_type == "Entire home/apt"),
            multi_ad = mean(multi), rev_ad = sum(rev),
            FREH = sum(FREH), FREH_3 = sum(FREH_3), .by = c(month, city_type))

coefs_table_type <- 
  ad_month_type |> 
  inner_join(ia_month_type, by = c("month", "city_type")) |> 
  mutate_days() |> 
  mutate(month, city_type,
         n = n_ad / n_ia,
         scraped = scraped / n_ia,
         active = active / n_ia / days,
         FREH = FREH / n_ia,
         FREH_3 = FREH_3 / n_ia,
         EH = EH_ad / EH_ia,
         multi = multi_ad / multi_ia,
         rev = rev_ad / n_ia,
         .keep = "none") |> 
  arrange(city_type, month) |> 
  select(-n)

coefs_type <- 
  coefs_table_type |> 
  summarize(
    across(scraped:multi, \(x) mean(x[coef_val])), 
    rev = rev[coef_val[2]], 
    .by = city_type)


# Calculate GH ------------------------------------------------------------

GH_ia <-
  monthly_ia |> 
  summarize(
    created = as.Date(min(month)),
    scraped = as.Date(max(month) + 1) - 1,
    .by = c(property_ID, host_ID, listing_type, latitude, longitude)) |> 
  strr_as_sf(3347) |> 
  strr_ghost()

st_crs(GH_ia) <- 3347

GH_ia <-
  GH_ia |> 
  select(-data) |> 
  mutate(month = yearmonth(date)) |> 
  slice(1, .by = c(ghost_ID, month)) |> 
  relocate(month, .after = ghost_ID) |> 
  select(-date)

GH_ia <- 
  GH_ia |> 
  st_join(CSD) |>
  mutate_city_type() |> 
  select(-pop_CSD, -dwellings_CSD) |> 
  relocate(geometry, .after = last_col())

GH_ia_CA <-
  monthly_ia_CA |> 
  summarize(
    created = as.Date(min(month)),
    scraped = as.Date(max(month) + 1) - 1,
    .by = c(property_ID, host_ID, listing_type, latitude, longitude)) |> 
  strr_as_sf(3347) |> 
  strr_ghost()

st_crs(GH_ia_CA) <- 3347

GH_ia_CA <-
  GH_ia_CA |> 
  select(-data) |> 
  mutate(month = yearmonth(date)) |> 
  slice(1, .by = c(ghost_ID, month)) |> 
  relocate(month, .after = ghost_ID) |> 
  select(-date)

GH_ia_CA <- 
  GH_ia_CA |> 
  st_join(CSD) |>
  mutate_city_type() |> 
  select(-pop_CSD, -dwellings_CSD) |> 
  relocate(geometry, .after = last_col())


# Save output -------------------------------------------------------------

qsavem(monthly_ia, monthly_ia_all, GH_ia, coefs, coefs_type, 
       monthly_ia_CA, monthly_ia_CA_all, GH_ia_CA, file = "output/ia.qsm")
