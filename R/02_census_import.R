#### CMHC AND CENSUS PROCESSING ################################################

source("R/01_startup.R")


# Import census data ------------------------------------------------------

province <-
  get_census("CA21", regions = list(PR = prov_ID), level = "PR",
             geo_format = "sf") |>
  st_transform(4326) |>
  select(province = name, province_ID = GeoUID, geometry) |>
  mutate(province = str_remove(province, " \\(.*\\)"))

CMA <-
  get_census("CA21", regions = list(PR = prov_ID), level = "CMA",
             geo_format = "sf") |>
  st_transform(3347) |>
  select(CMA = GeoUID, name_CMA = name, pop_CMA = Population,
         dwellings_CMA = Dwellings, geometry) |>
  mutate(name_CMA = str_remove(name_CMA, " \\(.*\\)"))

CSD <-
  get_census("CA21", regions = list(PR = prov_ID), level = "CSD",
             geo_format = "sf") |>
  st_transform(3347) |>
  select(CSDUID = GeoUID, city = name, CMAUID = CMA_UID, CDUID = CD_UID, 
         pop_CSD = Population, dwellings_CSD = Dwellings, geometry) |>
  as_tibble() |>
  st_as_sf()

CSD <-
  CSD |>
  mutate(city = case_when(
    city == "Langley (DM)" ~ "Langley (Township)",
    city == "Langley (CY)" ~ "Langley (City)",
    city == "North Vancouver (DM)" ~ "North Vancouver (District)",
    city == "North Vancouver (CY)" ~ "North Vancouver (City)",
    .default = city)) |> 
  # This may change for each different province
  filter(!str_detect(city, "\\((IRI)|(TWL)|(TAL)|(IGD)|(S-É)\\)")) |>
  # mutate(type = str_remove(city, "^.*\\("), .after = city) |>
  # mutate(type = str_remove(type, "\\)$")) |>
  mutate(city = str_remove(city, " \\(..\\)$")) |>
  mutate(city = str_remove(city, " \\(.\\)$"))

DA <-
  get_census("CA21", regions = list(PR = prov_ID), level = "DA",
             vectors = c(rent = "v_CA21_4318",
                         tenants = "v_CA21_4313",
                         owners = "v_CA21_4305",
                         entertainment = "v_CA21_6657",
                         accommodation = "v_CA21_6660",
                         tourism_parent = "v_CA21_6606"),
             geo_format = "sf") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(3347) |>
  mutate(tourism = entertainment + accommodation) |>
  select(DA = GeoUID, CSDUID = CSD_UID, CMA = CMA_UID, pop = Population, 
         dwellings = Dwellings, tourism, tourism_parent, rent, tenants, 
         owners) |>
  mutate(area_DA = units::drop_units(st_area(geometry)), .before = geometry) |>
  st_set_agr("constant")

DA <-
  DA |>
  mutate(province_ID = substr(DA, 1, 2)) |>
  inner_join(st_drop_geometry(province), by = "province_ID") |>
  relocate(province, province_ID, .after = CMA)

DA <- 
  DA |> 
  inner_join(select(st_drop_geometry(CSD), CSDUID, city), by = "CSDUID") |> 
  relocate(city, .after = CSDUID)

province <-
  province |>
  select(-province_ID)

DA_union <-
  DA |>
  summarize()

CT <-
  get_census("CA21", regions = list(PR = prov_ID), level = "CT",
             vectors = c(rent = "v_CA21_4318",
                         tenants = "v_CA21_4313",
                         owners = "v_CA21_4305"),
             geo_format = "sf") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(3347) |>
  select(CTUID = GeoUID, dwellings = Dwellings, rent, tenants, owners) |>
  st_set_agr("constant")

CD <- 
  get_census("CA21", regions = list(PR = prov_ID), level = "CD", 
             geo_format = "sf") |> 
  as_tibble() |> 
  select(CDUID = GeoUID, name, geometry) |> 
  st_as_sf() |> 
  st_transform(3347) |>
  st_set_agr("constant")


# Water -------------------------------------------------------------------

water <- 
  read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |> 
  filter(PRUID == prov_ID)


# Save census geometries --------------------------------------------------

qsave(DA, file = "output/DA.qs", nthreads = availableCores())
qsave(DA_union, file = "output/DA_union.qs", nthreads = availableCores())
qsave(CT, file = "output/CT.qs", nthreads = availableCores())
qsave(CD, file = "output/CD.qs", nthreads = availableCores())
qsave(CSD, file = "output/CSD.qs", nthreads = availableCores())
qsave(CMA, file = "output/CMA.qs", nthreads = availableCores())
qsave(province, file = "output/province.qs", nthreads = availableCores())
qsave(water, "output/water.qs", nthreads = availableCores())
