#### 06 Principal-residence requirement ########################################

source("R/01_startup.R")
CSD <- qread("output/CSD.qs")


# Get all CMHC neighbourhoods with/without regs ---------------------------

reg_true <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant)) |> 
  filter(province == "British Columbia") |> 
  filter(reg)

reg_false <- 
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant)) |> 
  filter(province == "British Columbia") |> 
  filter(!reg)


# Figure out which neighbourhoods are now covered -------------------------

pr_cities <- 
  c("Abbotsford", "Anmore", "Belcarra", "Burnaby", "Campbell River", 
    "Central Saanich", "Chilliwack", "Coldstream", "Colwood", "Comox", 
    "Coquitlam", "Courtenay", "Cranbrook", "Cumberland", "Delta", "Duncan", 
    "Esquimalt", "Highlands", "Kamloops", "Kelowna", "Lake Country", "Langford", 
    "Langley (City)", "Langley (Township)", "Maple Ridge", "Metchosin", 
    "Metro Vancouver A (RDA)",
    "Mission", "Nanaimo", "Nelson", "New Westminster", "North Cowichan", 
    "North Saanich", "North Vancouver (City)", "North Vancouver (District)", 
    "Oak Bay", "Parksville", "Penticton", "Pitt Meadows", "Port Alberni", 
    "Port Coquitlam", "Port Moody", "Powell River", "Prince George", 
    "Prince Rupert", "Richmond", "Qualicum Beach", "Saanich", "Salmon Arm", 
    "Sechelt", "Sidney", "Sooke", "Squamish", "Summerland", "Surrey", "Terrace", 
    "Vancouver", "Vernon", "Victoria", "View Royal", "West Vancouver", 
    "White Rock", "Williams Lake")

# pr_nbhd <-
#   cmhc_nbhd |> 
#   filter(province == prov_name) |> 
#   select(id:name_CMA) |> 
#   mutate(cmhc_area = as.numeric(st_area(geometry)), .before = geometry) |> 
#   st_intersection(
#     CSD |> 
#       filter(city %in% pr_cities) |> 
#       select(CSDUID:city) |> 
#       mutate(CSD_area = as.numeric(st_area(geometry)), .before = geometry)) |> 
#   filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |> 
#   mutate(
#     cmhc_prop = as.numeric(st_area(geometry) / cmhc_area),
#     CSD_prop = as.numeric(st_area(geometry) / CSD_area),
#     .before = geometry) |> 
#   filter(CSD_prop > 0.008) |>
#   filter(cmhc_prop > 0.001) |>
#   st_drop_geometry() |> 
#   count(id) |> 
#   arrange(id) |> 
#   pull(id)
  
# Just do it manually: all nbhds except
pr_exclude <- c("7583001", "7265002", "7265001", "7190001")


# Construct final table ---------------------------------------------------

pr_table <-
  cmhc_nbhd |> 
  filter(province == prov_name) |> 
  select(id:name_CMA) |> 
  mutate(pr_before = id %in% reg_true$id,
         pr_after = !id %in% pr_exclude,
         .before = geometry) |> 
  st_drop_geometry()

qsave(pr_table, "output/pr_table.qs")
