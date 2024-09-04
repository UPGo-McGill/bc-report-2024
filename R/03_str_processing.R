#### STR PROCESSING ############################################################

source("R/01_startup.R")
province <- qread("output/province.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())
CD <- qread("output/CD.qs", nthreads = availableCores())
property <- qread("data/property.qs", nthreads = 10)
monthly <- qread("data/monthly.qs", nthreads = 10)


# Import raw monthly file -------------------------------------------------

monthly_raw <- 
  read_csv("data/monthly.csv") |> 
  mutate(property_ID = `Property ID`,
         month = `Reporting Month`,
         host_ID = coalesce(as.character(`Airbnb Host ID`),
                            `HomeAway Property Manager`),
         listing_type = `Listing Type`,
         property_type = `Property Type`,
         country = Country,
         region = State,
         R = `Reservation Days`,
         A = `Available Days`,
         B = `Blocked Days`,
         rev = `Revenue (USD)`,
         latitude = `Latitude`,
         longitude = `Longitude`,
         scraped = `Scraped During Month`,
         .keep = "none") |>
  mutate(month = yearmonth(month)) |>
  filter(property_ID %in% monthly$property_ID) |> 
  select(property_ID, month, property_type, scraped)


# Add `scraped` to monthly ------------------------------------------------

monthly <- 
  monthly |> 
  inner_join(monthly_raw, by = c("property_ID", "month")) |> 
  relocate(scraped, .after = cmhc) |> 
  relocate(property_type, .after = listing_type)


# Add city ----------------------------------------------------------------

CMA_to_join <-
  monthly |>
  slice(1, .by = property_ID) |>
  select(property_ID, CSDUID) |>
  inner_join(CSD, by = "CSDUID") |>
  select(property_ID, CMAUID)

CD_to_join <-
  property |>
  strr_as_sf(3347) |>
  select(property_ID) |>
  mutate(CDUID = CD$CDUID[st_nearest_feature(geometry, CD)]) |>
  st_drop_geometry()

CD_to_join <-
  CD_to_join |>
  inner_join(CD, by = "CDUID") |>
  select(property_ID, CDUID)

monthly <-
  monthly |>
  inner_join(CMA_to_join, by = "property_ID") |>
  inner_join(CD_to_join, by = "property_ID") |>
  relocate(city, CSDUID, CDUID, CMAUID, .after = multi)

rm(CMA_to_join)


# Add city_type -----------------------------------------------------------

monthly <-
  monthly |> 
  mutate_city_type()


# Filter to only Airbnb and housing ---------------------------------------

monthly <- 
  monthly |> 
  filter(str_starts(property_ID, "ab-"))



# Clean up ----------------------------------------------------------------

monthly <- 
  monthly |> 
  select(property_ID, month, host_ID, listing_type, property_type, city,
         city_type, CSDUID, CDUID, CMAUID, R, A, B, FREH, FREH_3, multi,
         rev = revenue, scraped) |> 
  inner_join(select(property, property_ID, latitude, longitude), 
             by = "property_ID") |> 
  relocate(scraped, .after = last_col())
  

# Calculate GH ------------------------------------------------------------

GH <- 
  monthly |> 
  summarize(
    created = as.Date(min(month)),
    scraped = as.Date(max(month) + 1) - 1,
    .by = c(property_ID, host_ID, listing_type, latitude, longitude)) |> 
  strr_as_sf(3347) |> 
  strr_ghost()

GH <-
  GH |> 
  select(-data) |> 
  mutate(month = yearmonth(date)) |> 
  slice(1, .by = c(ghost_ID, month)) |> 
  relocate(month, .after = ghost_ID) |> 
  select(-date)

GH <-
  GH |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  inner_join(select(monthly, property_ID, month, R, A, B), 
             by = c("property_ID", "month")) |> 
  summarize(property_IDs = list(property_ID),
            active_pct = sum(R + A) / sum(R + A + B),
            .by = c(ghost_ID, month, host_ID, listing_count, housing_units, 
                    geometry)) |> 
  relocate(property_IDs, active_pct, .before = geometry)

GH <- 
  GH |> 
  st_set_crs(3347) |> 
  st_join(CSD) |> 
  mutate_city_type() |> 
  select(-pop_CSD, -dwellings_CSD) |> 
  relocate(geometry, .after = last_col())


# Save output -------------------------------------------------------------

qsave(monthly, file = "output/monthly.qs", nthreads = availableCores())
qsave(GH, file = "output/GH.qs", nthreads = availableCores())

