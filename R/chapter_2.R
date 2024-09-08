#### Chapter 2: The effect of principal-residence requirements on rents ########

source("R/01_startup.R")

DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())
dd <- qread("data/dd.qs", nthreads = availableCores())
md <- qread("data/md.qs", nthreads = availableCores())
dr <- qread("data/dr.qs", nthreads = availableCores())
qload("data/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("data/monthly_sept.qs")
pr_table <- qread("output/pr_table.qs")

source("R/05_imputation.R")

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))


# Canada DiD --------------------------------------------------------------

# Number of cities
reg |> 
  filter(reg) |> 
  count(name_CSD)

md$main$rent_log |> aggte(type = "simple", alp = 0.0000000001)

did_effects <- 
  tibble(treat = md$main$rent_log$group,
         year = md$main$rent_log$t,
         att = md$main$rent_log$att) |> 
  filter(year >= treat)

# Get tenant count to normalize per-neighbourhood rents
tenant_count <- 
  monthly_sept |> 
  st_drop_geometry() |> 
  # Impute missing values
  impute() |> 
  # Update tenant_count to reflect trend in universe
  mutate(tenant_count = universe * tenant_count[year == 2021] / 
           universe[year == 2021], .by = id) |> 
  select(id, year, tenant_count)

did_rent_dif <-
  dd$main |> 
  mutate(id = as.character(id)) |> 
  # Add effects to data
  inner_join(did_effects, by = c("year", "treat")) |> 
  # Create counterfactual for rent_log in the absence of treatment
  mutate(rent_log_cf = rent_log - att) |> 
  # Un-standardize rent_log_cf
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  # Exponentiate rent_log_cf_raw to get counterfactual rent
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  # Exponentiate rent_log_raw to get actual rent
  mutate(rent_raw = exp(rent_log_raw)) |>
  # Get rent difference
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, treat, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

# Average rent decrease in first year
did_rent_dif |> 
  filter(year == treat) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1))

# Average rent decrease in 2023
did_rent_dif |> 
  filter(year == 2023) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif))


# Figure 2.1: Parallel trends ---------------------------------------------

fig_2_1 <- 
  aggte(md$main$rent_log, type = "dynamic") |> 
  ggdid() +
  ggtitle(NULL) +
  theme_minimal() +
  scale_x_continuous(name = "Years after regulation", 
                     breaks = c(-6, -4, -2, 0, 2, 4)) + 
  scale_y_continuous(name = "Change in rent") +
  scale_color_brewer(name = NULL, palette = "Dark2", labels = c(
    "Before regulation", "After regulation")) +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_2_1.png", fig_2_1, width = 8, height = 4, units = "in")


# Principal-residence restrictions are working in BC ----------------------

# Use Van-adjusted DiD model
did_effects_van <- 
  tibble(treat = md$van$rent_log$group,
         year = md$van$rent_log$t,
         att = md$van$rent_log$att) |> 
  filter(year >= treat)

did_rent_dif_van <-
  dd$van |> 
  mutate(id = as.character(id)) |> 
  # Add effects to data
  inner_join(did_effects_van, by = c("year", "treat")) |> 
  # Create counterfactual for rent_log in the absence of treatment
  mutate(rent_log_cf = rent_log - att) |> 
  # Un-standardize rent_log_cf
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  # Exponentiate rent_log_cf_raw to get counterfactual rent
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  # Exponentiate rent_log_raw to get actual rent
  mutate(rent_raw = exp(rent_log_raw)) |>
  # Get rent difference
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, treat, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

# Number of neighbourhoods/cities
did_rent_dif_van |> 
  inner_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA))) |> 
  inner_join(select(reg, id, name_CSD)) |> 
  filter(year == treat, province == prov_name) |> 
  summarize(n = n(), n_CSD = length(unique(name_CSD)))

# Average rent decrease in first year
did_rent_dif_van |> 
  left_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA))) |> 
  filter(year == treat, province == prov_name) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif))

# Average rent decrease in 2023
did_rent_dif_van |> 
  left_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA))) |> 
  filter(year == 2023, province == prov_name) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif))

# Results from BC-specific DiD
dd$bc <- 
  dr$main |> 
  inner_join(select(mutate(reg, date = if_else(
    name_CSD == "Vancouver", as.Date("2019-08-31"), date)), id, date, reg), 
    by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         id = as.numeric(paste0("1111", id))) |> 
  filter(treat == 0 | treat > 2017) |> 
  filter(province == prov_name) |> 
  mutate()

md$bc$rent_log <- att_gt("rent_log", tname = "year", idname = "id", 
                         gname = "treat", allow_unbalanced_panel = TRUE, 
                         data = dd$bc)

dd$bc <- mutate(dd$bc, id = str_remove(as.character(id), "^1111"))

md$bc$rent_log |> aggte("simple", alp = .001)

did_effects_bc <- 
  tibble(treat = md$bc$rent_log$group,
         year = md$bc$rent_log$t,
         att = md$bc$rent_log$att) |> 
  filter(year >= treat)

did_rent_dif_bc <-
  dd$bc |> 
  mutate(id = as.character(id)) |> 
  # Add effects to data
  inner_join(did_effects_bc, by = c("year", "treat")) |> 
  # Create counterfactual for rent_log in the absence of treatment
  mutate(rent_log_cf = rent_log - att) |> 
  # Un-standardize rent_log_cf
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  # Exponentiate rent_log_cf_raw to get counterfactual rent
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  # Exponentiate rent_log_raw to get actual rent
  mutate(rent_raw = exp(rent_log_raw)) |>
  # Get rent difference
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, treat, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

# Average rent decrease in first year
did_rent_dif_bc |> 
  filter(year == treat) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif))

# Average rent decrease in 2023
did_rent_dif_bc |> 
  filter(year == 2023) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif))

# Vancouver results
van_treat <- 
  did_rent_dif_bc |> 
  mutate(pct = rent_dif / rent_raw) |> 
  left_join(select(cmhc_nbhd, id:name_CMA)) |> 
  filter(year == 2023) |> 
  filter(name_CMA == "Vancouver") |> 
  st_as_sf()

van_treat |> 
  inner_join(select(reg, id, name_CSD)) |> 
  filter(name_CSD == "Vancouver") |> 
  st_drop_geometry() |> 
  summarize(total_rent_dif = sum(rent_dif * tenant_count),
            mean_rent_dif = total_rent_dif / sum(tenant_count))


# Figure 4.2. Graph of BC rent savings ------------------------------------

fig_2_2 <-
  did_rent_dif_van |> 
  left_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA)), by = "id") |> 
  filter(province == prov_name) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = year) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(year) |> 
  filter(year >= 2020) |> 
  mutate(label = scales::dollar(total_rent_dif, accuracy = 0.1, 
                                scale = 1 / 1000000, suffix = "M")) |> 
  ggplot(aes(year, total_rent_dif)) +
  geom_col(fill = col_palette[1]) + 
  geom_label(aes(y = total_rent_dif + 4000000, label = label), 
             colour = col_palette[1], family = "Futura") + 
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_x_continuous(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_2_2.png", fig_2_2, width = 8, height = 4, units = "in")


# Figure 4.3. Vancouver treatment map -------------------------------------

fig_2_3 <-
  van_treat |> 
  filter(id != "2270780") |> 
  ggplot(aes(fill = rent_dif)) +
  geom_sf(data = DA_union, fill = "grey80", colour = "transparent") +
  geom_sf(colour = "white") +
  geom_sf(data = water, colour = "transparent", fill = "white") +
  scale_fill_stepsn("Decrease in rent caused by STR regulation",
                    labels = scales::dollar, 
                    colours = col_palette[c(4, 1, 6, 2)]) +
  coord_sf(xlim = st_bbox(filter(van_treat, id != "2270780"))[c(1, 3)], 
           ylim = st_bbox(filter(van_treat, id != "2270780"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(40, "points"),
        text = element_text(family = "Futura"))

ggsave("output/figure_2_3.png", fig_2_3, width = 8, height = 5, units = "in")  


# Table 6. Regulation effects across BC -----------------------------------

did_rent_dif_van |> 
  left_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA))) |> 
  inner_join(select(reg, id, name_CSD)) |> 
  filter(year == 2023) |> 
  filter(province == prov_name) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  mutate(region = case_when(
    name_CSD %in% c("Vancouver", "Burnaby", "Victoria", "Nanaimo") ~ name_CSD,
    .default = "Other municipalities")) |>
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = region) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(-total_rent) |> 
  mutate(mean_rent_dif = round(mean_rent_dif, 0),
         pct = round(pct, 3))

did_rent_dif_bc |> 
  left_join(st_drop_geometry(select(cmhc_nbhd, id:name_CMA))) |> 
  inner_join(select(reg, id, name_CSD)) |> 
  filter(year == 2023) |> 
  filter(province == prov_name) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  mutate(region = case_when(
    name_CSD %in% c("Vancouver", "Burnaby", "Victoria", "Nanaimo") ~ name_CSD,
    .default = "Other municipalities")) |>
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = region) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(-total_rent) |> 
  mutate(mean_rent_dif = round(mean_rent_dif, 0),
         pct = round(pct, 3))


# What would be the likely results of a province-wide ---------------------

dyn <- aggte(md$van$rent_log, type = "dynamic")

did_rent_non_treated <-
  dr$main |> 
  filter(id %in% pr_table$id[pr_table$pr_after],
         !id %in% pr_table$id[pr_table$pr_before]) |> 
  filter(year == 2023) |> 
  mutate(att = dyn$att.egt[dyn$egt == 0]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_rent_non_treated_y2 <-
  dr$main |> 
  filter(id %in% pr_table$id[pr_table$pr_after],
         !id %in% pr_table$id[pr_table$pr_before]) |> 
  filter(year == 2023) |> 
  mutate(att = dyn$att.egt[dyn$egt == 1]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_rent_non_treated_y3 <-
  dr$main |> 
  filter(id %in% pr_table$id[pr_table$pr_after],
         !id %in% pr_table$id[pr_table$pr_before]) |> 
  filter(year == 2023) |> 
  mutate(att = dyn$att.egt[dyn$egt == 2]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

# Average rent growth
avg_rent_growth <- 
  cmhc |> 
  filter(id %in% cmhc_nbhd$id[cmhc_nbhd$province == prov_name]) |> 
  summarize(rent = sum(rent * universe, na.rm = TRUE) / 
              sum(universe, na.rm = TRUE),
            .by = year) |> 
  mutate(rent_pct = slide_dbl(rent, \(x) (x[2] - x[1]) / x[1], .before = 1)) |> 
  slice(8:9) |> 
  pull(rent_pct) |> 
  mean()

# Possible average rent decrease in 2025
did_rent_non_treated |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1)) |> 
  mutate(across(mean_rent:total_rent_dif, \(x) x * (1 + avg_rent_growth) ^ 2))

# Possible average rent decrease in 2026
did_rent_non_treated_y2 |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1)) |> 
  mutate(across(mean_rent:total_rent_dif, \(x) x * (1 + avg_rent_growth) ^ 3))

# Possible average rent decrease in 2027
did_rent_non_treated_y3 |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1)) |> 
  mutate(across(mean_rent:total_rent_dif, \(x) x * (1 + avg_rent_growth) ^ 4))

# Cost of repealing rules after 2024
((did_rent_non_treated_y2 |> 
    mutate(total_rent = rent_raw * tenant_count,
           total_rent_dif = rent_dif * tenant_count) |> 
    summarize(
      mean_rent = weighted.mean(rent_raw, tenant_count),
      mean_rent_dif = weighted.mean(rent_dif, tenant_count),
      total_rent = sum(total_rent),
      total_rent_dif = sum(total_rent_dif)) |> 
    mutate(pct = scales::percent(
      total_rent_dif / (total_rent + total_rent_dif), 0.1)) |> 
    mutate(across(mean_rent:total_rent_dif, \(x) x * 
                    (1 + avg_rent_growth) ^ 3)) |> 
    pull(total_rent_dif)) +
    (did_rent_non_treated_y3 |> 
       mutate(total_rent = rent_raw * tenant_count,
              total_rent_dif = rent_dif * tenant_count) |> 
       summarize(
         mean_rent = weighted.mean(rent_raw, tenant_count),
         mean_rent_dif = weighted.mean(rent_dif, tenant_count),
         total_rent = sum(total_rent),
         total_rent_dif = sum(total_rent_dif)) |> 
       mutate(pct = scales::percent(
         total_rent_dif / (total_rent + total_rent_dif), 0.1)) |> 
       mutate(across(mean_rent:total_rent_dif, \(x) x * 
                       (1 + avg_rent_growth) ^ 4)) |> 
       pull(total_rent_dif))) |> 
  (\(x) x * 12)() |> 
  scales::dollar()


# Figure 2.4: Possible per-nbhd average rent decrease in 2025 -------------

treat <- 
  did_rent_non_treated |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  inner_join(cmhc_nbhd, by = "id") |> 
  st_as_sf() |> 
  filter(name_CMA %in% c("Vancouver", "Abbotsford - Mission")) |> 
  mutate(rent_dif = rent_dif * (1 + avg_rent_growth) ^ 2)

bb <- st_bbox(treat)

fig_2_4 <-
  treat |> 
  ggplot(aes(fill = rent_dif)) +
  geom_sf(data = DA_union, fill = "grey80", colour = "transparent") +
  geom_sf(colour = "white") +
  geom_sf(data = water, colour = "transparent", fill = "white") +
  scale_fill_stepsn("Est. avg.\n2025 monthly\nrent decrease",
                    labels = scales::dollar, 
                    colours = col_palette[c(4, 6)], n.breaks = 4) +
  coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "right", 
        text = element_text(family = "Futura"))

ggsave("output/figure_2_4.png", fig_2_4, width = 8, height = 6, units = "in")  


# Table 7: Possible per-city average rent decrease in 2025 ----------------

tab_7 <-
  did_rent_non_treated |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = name_CSD) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(-total_rent) |> 
  slice(1:10) |> 
  arrange(name_CSD) |> 
  select(name_CSD, mean_2023 = mean_rent_dif, total_2023 = total_rent_dif) |> 
  mutate(across(mean_2023:total_2023, \(x) x * (1 + avg_rent_growth) ^ 2))

tab_7 <- 
  did_rent_non_treated_y2 |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = name_CSD) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(name_CSD) |> 
  select(name_CSD, mean_2024 = mean_rent_dif, total_2024 = total_rent_dif) |> 
  mutate(across(mean_2024:total_2024, \(x) x * (1 + avg_rent_growth) ^ 3)) |> 
  inner_join(tab_7, by = "name_CSD")

tab_7 <- 
  did_rent_non_treated_y3 |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = name_CSD) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  arrange(name_CSD) |> 
  select(name_CSD, mean_2025 = mean_rent_dif, total_2025 = total_rent_dif) |> 
  mutate(across(mean_2025:total_2025, \(x) x * (1 + avg_rent_growth) ^ 4)) |> 
  inner_join(tab_7, by = "name_CSD")

tab_7 |> 
  select(name_CSD, mean_2023, mean_2024, mean_2025, total_2023, total_2024,
         total_2025) |> 
  mutate(across(mean_2023:mean_2025, \(x) scales::dollar(x, 1)),
         across(total_2023:total_2025, \(x) scales::dollar(
           x, 0.1, scale = 1 / 1000000, suffix = "M"))) |> 
  gt::gt()
