#### 05 IMPUTATION #############################################################

# This should only be run from within another script that already has the 
# required packages and objects loaded

# nn <- nngeo::st_nn(cmhc_nbhd, cmhc_nbhd, k = 6, maxdist = 50000)
# qsave(nn, file = "output/nn.qs")
nn <- qread("output/nn.qs")

# Get list of nearest neighbours
nn_join <-
  cmhc_nbhd |> 
  st_drop_geometry() |> 
  select(id) |> 
  mutate(nn = !!nn) |> 
  mutate(nn = map(nn, \(x) cmhc_nbhd$id[x])) |> 
  mutate(nn = map2(nn, id, \(x, y) x[x != y]))

# Add nearest neighbours to monthly_sept
monthly_impute <- 
  monthly_sept |> 
  inner_join(nn_join, by = "id") |> 
  relocate(nn, .after = rent)

# Calculate rent/universe/vacancy values for nearest neighbours
monthly_impute <-
  monthly_impute |> 
  mutate(rent_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(rent) |> 
      mean(na.rm = TRUE)}), .before = nn) |> 
  mutate(univ_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(universe) |> 
      mean(na.rm = TRUE)}), .before = nn) |> 
  mutate(vac_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(vacancy) |> 
      mean(na.rm = TRUE)}), .before = nn)

# Special cases with no neighbours
special_cases <- c("3275001", "3920001", "5800001", "6090001", "6185001", 
                   "6298001", "7045001", "7095001", "7175001", "7185001",
                   "7240002", "7245001", "7253001", "7280001", "7300001",
                   "7360001", "7467001", "7480001", "7496001", "7579001",
                   "7581001", "7650001", "7700001", "7705001", "7730001",
                   "7734001", "7839001", "7845001", "7860001", "7865001")

monthly_impute <-
  monthly_impute |> 
  mutate(rent_nn = if_else(id %in% special_cases, rent, rent_nn),
         univ_nn = if_else(id %in% special_cases, universe, univ_nn),
         vac_nn = if_else(id %in% special_cases, vacancy, vac_nn))

# Calculate ratio of local values to nn values for each year
monthly_impute <-
  monthly_impute |> 
  mutate(rent_ratio = rent / rent_nn, .after = rent_nn) |> 
  mutate(univ_ratio = universe / univ_nn, .after = univ_nn) |> 
  mutate(vac_ratio = vacancy / vac_nn, .after = vac_nn)

# Fit per-neighbourhood linear models and predict missing values for rent
monthly_impute_rent <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(rent)) > 0, .by = id) |> 
  mutate(rent_lm = list(lm(rent_ratio ~ year, data = tibble(rent_ratio, year))),
         .after = rent_ratio, .by = id) |> 
  rowwise() |>
  mutate(new_ratio = predict(rent_lm, tibble(rent_ratio, year))) |> 
  ungroup() |> 
  mutate(rent_new = rent_nn * new_ratio)

# Use simple linear trend for any leftover neighbourhoods
leftovers <- 
  monthly_impute_rent |> 
  filter(sum(is.na(rent_new)) > 0, .by = id) |> 
  mutate(rent_lm = list(lm(rent ~ year, data = tibble(rent, year))),
         .after = rent_ratio, .by = id) |> 
  rowwise() |>
  mutate(rent_new = predict(rent_lm, tibble(rent, year))) |> 
  ungroup()

monthly_impute_rent <- 
  monthly_impute_rent |> 
  anti_join(leftovers, by = "id") |> 
  bind_rows(leftovers) |> 
  filter(is.na(rent)) |> 
  select(id, year, rent_new)

# Fit per-neighbourhood linear models and predict missing values for universe
monthly_impute_univ <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(universe)) > 0, .by = id) |> 
  mutate(univ_lm = list(lm(univ_ratio ~ year, data = tibble(univ_ratio, year))),
         .after = univ_ratio, .by = id) |> 
  rowwise() |>
  mutate(new_ratio = predict(univ_lm, tibble(univ_ratio, year))) |> 
  ungroup() |> 
  mutate(univ_new = univ_nn * new_ratio) |> 
  filter(is.na(universe)) |> 
  select(id, year, univ_new)

# Fit per-neighbourhood linear models and predict missing values for vacancy
monthly_impute_vac <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(vacancy)) > 0, .by = id) |> 
  mutate(vac_ratio = if_else(is.infinite(vac_ratio), NA, vac_ratio)) |> 
  mutate(vac_nn_group = mean(vac_nn, na.rm = TRUE), .by = id, 
         .after = vac_nn) |> 
  mutate(vac_lm = list(
    if (sum(!is.na(vac_ratio)) > 0) {
      lm(vac_ratio ~ year, data = tibble(vac_ratio, year))  
    } else NA), .after = vac_ratio, .by = id) |>
  # Try to predict with model
  rowwise() |>
  mutate(new_ratio = if (is.logical(vac_lm[[1]])) NA_real_ else 
    predict(vac_lm, tibble(vac_ratio, year)), .after = vac_ratio) |> 
  ungroup() |> 
  # Use vac_nn_group is vac_nn is NA, and use vac_nn or vac_nn_group directly 
  # if there is no model
  mutate(vac_new = coalesce(vac_nn * new_ratio, vac_nn_group * new_ratio,
                            vac_nn, vac_nn_group), .after = vacancy) |> 
  filter(is.na(vacancy)) |>
  select(id, year, vac_new) |>
  mutate(vac_new = pmax(vac_new, 0)) |> 
  # Suppress warnings about rank deficiency in models
  suppressWarnings()

# Coalesce new values
monthly_impute <-
  monthly_impute |> 
  left_join(monthly_impute_rent, by = c("id", "year")) |> 
  left_join(monthly_impute_univ, by = c("id", "year")) |> 
  left_join(monthly_impute_vac, by = c("id", "year")) |> 
  mutate(rent_new = coalesce(rent, rent_new)) |> 
  mutate(univ_new = coalesce(universe, univ_new)) |>
  mutate(vac_new = coalesce(vacancy, vac_new)) |> 
  select(id, year, rent, rent_new, universe, univ_new, vacancy, vac_new) |> 
  st_drop_geometry() 


# Clean up ----------------------------------------------------------------

rm(leftovers, nn, nn_join, special_cases, monthly_impute_rent, 
   monthly_impute_univ, monthly_impute_vac)
