#### Chapter 3: Enforcement analysis ###########################################

source("R/01_startup.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
qload("output/ia.qsm", nthreads = availableCores())
CSD_with_PR <- qread("output/CSD_with_PR.qs")
CSD <- qread("output/CSD.qs")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())


# Prepare data ------------------------------------------------------------

ar_365 <- 
  monthly |> 
  filter(month >= yearmonth("2022-07")) |> 
  summarize(R = sum(R), A = sum(A), B = sum(B), 
            FREH = FREH[month == max(month)], .by = property_ID)

freh_2023 <- 
  monthly |> 
  filter(month == max(month), FREH) |> 
  pull(property_ID)

non_freh_2023 <- 
  monthly |> 
  filter(month == max(month), !FREH) |> 
  pull(property_ID)

# Single table
freh_trend_table <- 
  monthly_ia |> 
  inner_join(CSD_with_PR) |> 
  filter(month >= yearmonth("2024-01")) |> 
  filter(listing_type == "Entire home/apt") |> 
  summarize(
    FREH_new = sum(!pr_before & pr_after & property_ID %in% freh_2023),
    FREH_none = sum(!pr_before & !pr_after & property_ID %in% freh_2023),
    FREH_steady = sum(pr_before & pr_after & property_ID %in% freh_2023),
    non_new = sum(!pr_before & pr_after & property_ID %in% non_freh_2023),
    non_none = sum(!pr_before & !pr_after & property_ID %in% non_freh_2023),
    non_steady = sum(pr_before & pr_after & property_ID %in% non_freh_2023),
    .by = month) |> 
  pivot_longer(-month) |> 
  mutate(month = month + 1,
         FREH = str_detect(name, "^FREH"),
         PR = case_when(
           str_detect(name, "_new") ~ "New PR restriction",
           str_detect(name, "_none") ~ "No PR restriction",
           str_detect(name, "_steady") ~ "Previous PR restriction")) |> 
  mutate(PR = factor(PR, levels = c("New PR restriction", 
                                    "Previous PR restriction",
                                    "No PR restriction"))) |> 
  mutate(FREH = if_else(FREH, "FREH", "non-FREH")) |> 
  mutate(index = value / value[month == yearmonth("2024-02")], .by = name)


# Listing trajectories before and after May 1, 2024 -----------------------

freh_trend_table |> 
  filter(name == "FREH_new")

freh_trend_table |> 
  filter(name == "non_new")

freh_trend_table |> 
  filter(name == "FREH_steady")

freh_trend_table |> 
  filter(name == "non_steady")


# Figure 3.1 Overall listing trajectories  --------------------------------

fig_3_1 <- 
  freh_trend_table |> 
  mutate(label = if_else(
    PR == "No PR restriction" & month == yearmonth("2024-06"), FREH, NA)) |> 
  ggplot(aes(month, index, colour = PR, linetype = FREH)) +
  geom_line(lwd = 0.9) + 
  facet_wrap(~PR, nrow = 3) +
  gghighlight(use_direct_label = FALSE) +
  geom_label(aes(label = label), size = 3, alpha = 0.8) +
  scale_y_continuous(name = "Percentage of listings still active",
                     label = scales::percent) +
  scale_x_yearmonth(name = NULL, minor_breaks = NULL) +
  scale_colour_manual(values = col_palette[c(1, 3, 5)]) +
  scale_linetype_manual(values = c("FREH" = 1, "non-FREH" = 2)) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_3_1.png", fig_3_1, width = 4, height = 8, units = "in")


# Highly active listings have been delisted more frequently ---------------

activity_trend_table <- 
  monthly_ia |> 
  inner_join(CSD_with_PR, by = c("city", "CSDUID")) |> 
  filter(month >= yearmonth("2024-01"), pr_after) |> 
  filter(listing_type == "Entire home/apt") |>
  left_join(ar_365, by = "property_ID") |> 
  mutate(R_cat = case_when(
    R == 0 ~ "0",
    R < 90 ~ "< 90",
    R < 120 ~ "< 120",
    R < 150 ~ "< 150",
    R < 180 ~ "< 180",
    R < 210 ~ "< 210",
    R < 240 ~ "< 240",
    R >= 240 ~ ">= 240")) |> 
  filter(!is.na(R_cat)) |> 
  count(month, R_cat) |> 
  mutate(month = month + 1) |> 
  mutate(index = n / n[month == yearmonth("2024-02")], .by = R_cat)

activity_trend_table |> 
  filter(R_cat == "< 120")

activity_trend_table |> 
  filter(R_cat == ">= 240")

activity_trend_table |> 
  filter(R_cat == "< 90")


# Figure 3.2 Listing trajectories by activity -----------------------------

fig_3_2 <-
  activity_trend_table |> 
  filter(!R_cat %in% c("0", "< 90")) |> 
  ggplot() +
  geom_line(aes(month, index, linetype = R_cat), 
            data = filter(activity_trend_table, R_cat %in% c("0", "< 90"))) +
  geom_line(aes(month, index, colour = R_cat), lwd = 0.9) +
  scale_y_continuous(name = "Percentage of listings still active",
                     label = scales::percent) +
  scale_x_yearmonth(name = NULL, minor_breaks = NULL) +
  scale_colour_manual(
    name = "FREH number\nof reserved nights",
    values = col_palette[c(6, 1, 4, 5, 3, 2)]) +
  scale_linetype_manual(name = "Non-FREH number\nof reserved nights",
                        values = c(2, 3)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

ggsave("output/figure_3_2.png", fig_3_2, width = 8, height = 4, units = "in")


# Regional patterns in early STR compliance -------------------------------

geography_trend_table <- 
  monthly_ia |> 
  inner_join(CSD_with_PR, by = c("city", "CSDUID")) |> 
  filter(month >= yearmonth("2024-01"), pr_after, property_ID %in% freh_2023) |> 
  count(month, tourism) |> 
  mutate(month = month + 1) |> 
  mutate(index = n / n[month == yearmonth("2024-02")], .by = tourism) |> 
  filter(tourism != "cariboo") |> 
  mutate_tourism()


# Figure 3.3 Listing trajectories by geography ----------------------------

fig_3_3 <- 
  monthly_ia |> 
  inner_join(CSD_with_PR, by = c("city", "CSDUID")) |> 
  filter(month >= yearmonth("2024-01"), pr_after, property_ID %in% freh_2023) |> 
  count(month, tourism) |> 
  mutate(month = month + 1) |> 
  mutate(index = n / n[month == yearmonth("2024-02")], .by = tourism) |> 
  filter(tourism != "cariboo") |> 
  mutate_tourism() |> 
  ggplot(aes(month, index, colour = tourism)) +
  geom_line(lwd = 0.9) +
  scale_y_continuous(name = "Percentage of listings still active",
                     label = scales::percent) +
  scale_x_yearmonth(name = NULL, minor_breaks = NULL) +
  scale_colour_manual(name = "Tourism\nregion",
                      values = col_palette[c(6, 1, 4, 5, 3)]) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

ggsave("output/figure_3_3.png", fig_3_3, width = 8, height = 4, units = "in")


# Figure 3.4 Metro Van map of listing trajectories ------------------------

van_listing_map <- 
  monthly_ia |> 
  inner_join(CSD_with_PR, by = c("city", "CSDUID")) |> 
  filter(month >= yearmonth("2024-01"), pr_after, property_ID %in% freh_2023) |> 
  filter(CMAUID == "59933") |> 
  count(month, city, CSDUID) |> 
  mutate(month = month + 1) |> 
  mutate(index = n / n[month == yearmonth("2024-02")], .by = c(city, CSDUID)) |> 
  # filter(month == max(month)) |> 
  inner_join(CSD) |> 
  st_as_sf()

fig_3_4 <- 
  van_listing_map |> 
  ggplot(aes(fill = index)) +
  geom_sf(data = DA_union, fill = "grey80", colour = "transparent") +
  geom_sf(colour = "white") +
  geom_sf(data = water, colour = "transparent", fill = "white") +
  facet_wrap(~month) + 
  scale_fill_stepsn(name = "Percentage of listings still active",
                    label = scales::percent, limits = c(0.5, 1.1), 
                    oob = scales::squish, colours = col_palette[c(1, 3)]) +
  coord_sf(xlim = st_bbox(van_listing_map)[c(1, 3)], 
           ylim = st_bbox(van_listing_map)[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"), 
        legend.key.width = unit(40, "pt"))

ggsave("output/figure_3_4.png", fig_3_4, width = 8, height = 6, units = "in")


# Early indications -------------------------------------------------------

freh_trend_table |> 
  filter(name %in% c("FREH_new", "FREH_steady")) |> 
  summarize(value = sum(value), .by = month) |> 
  mutate(index = value / value[month == yearmonth("2024-02")])

