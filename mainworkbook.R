## Setup

library(terra)
library(z22)
library(sf)
library(tidyverse)
library(ggplot2)
library(tigris)
library(stringr)
library(gerda)
library(fixest)
library(patchwork)
library(spdep)         
library(spatialreg)  
library(modelsummary)
library(biscale)
library(cowplot)
library(gridGraphics)
library(modelsummary)
library(pandoc)
library(geojsonsf)
library(jsonlite)
library(extrafont)
library(ggcorrplot)
library(ggpattern)

## Data Processing - update your wd()

nrw_lw <- read.csv(file = "~/Desktop/THESIS/landtagswahlen.csv") %>%
  mutate(ags = paste0("0", ags),
         election_date = dmy(election_date),
         election_year = as.character(year(election_date)),
         election_lvl = "state",
         ad_demokraten = NA) %>%
  rename(linke_pds = pds_linke) %>%
  filter(election_year != "2017")
lw_2017 <- read.csv(file = "~/Desktop/THESIS/NRW/2017_Landerwahl.csv") %>%
  mutate(ags = paste0("0", ags),
         election_date = ymd(election_date),
         election_year = as.character(year(election_date)),
         election_lvl = "state")
nrw_lw <- rbind(nrw_lw, lw_2017)
state_unharm <- load_gerda_web("state_unharm")
federal_unharm <- read.csv(file = "~/Desktop/THESIS/federal_muni_unharm.csv") %>%
  mutate(ags = paste0("0", ags))
kreise <- read_sf("~/Desktop/THESIS/VG250_KRS/VG250_KRS.shp")
gemeinde <- read_sf("~/Desktop/THESIS/VG250_GEM/VG250_GEM.shp")

dup_ids <- gemeinde %>%
  st_drop_geometry() %>%
  count(AGS) %>%
  filter(n > 1)

gemeinde_clean <- gemeinde %>%
  group_by(AGS) %>%
  summarise(across(where(~!inherits(., "sfc")), first),
            geometry = st_union(geometry),
            .groups = "drop") %>%
  select(AGS, GEN, BEZ, SN_L)

state <- load_gerda_web("state_harm") %>%
  add_gerda_census() %>%
  mutate(election_lvl = "state",
         election_year = as.character(election_year))
federal <- read.csv(file = "~/Desktop/THESIS/federal_muni_harm_25.csv") %>%
  mutate(ags = as.character(ags),
         election_lvl = "federal",
         election_year = as.character(election_year),
         ags = paste0("0", ags),
         state = paste0("0", state))

state_date_lookup <- state_unharm %>%
  select(ags, date) %>%
  mutate(election_year = as.character(year(date)),
         election_date = date) %>%
  distinct(ags, election_year, election_date)
federal_date_lookup <- federal_unharm %>%
  select(ags, election_date) %>%
  mutate(election_date = as.Date(election_date),
         election_year = as.character(year(election_date))) %>%
  distinct(ags, election_year, election_date)

state <- state %>%
  left_join(state_date_lookup,
            by = c("ags", "election_year"))
   
federal <- federal %>%
  left_join(federal_date_lookup,
            by = c("ags", "election_year"))

minor_parties <- federal %>%
  select(c(rep:zentrum, npd), -ad_demokraten) %>%
  colnames()
federal <- federal %>%
  mutate(
    other = rowSums(across(all_of(minor_parties), 
                                     ~ replace_na(.x, 0)))) %>%
  select(-all_of(minor_parties))

federal <- federal %>%
  select(everything()) %>%
  relocate(other, .after = afd) %>%
  relocate(cdu_csu, .after = csu) %>%
  select(-c(cdu, csu))
state <- state %>%
  relocate(cdu_csu, .after = csu) %>%
  relocate(election_date, .after = election_year) %>%
  select(-c(cdu, csu))

state <- state %>%
  left_join(gemeinde_clean,
            by = join_by(ags == AGS)) %>%
  filter(!(state == "05" & election_year == "2017"))

census_merge <- state %>%
  select(ags, state, population_census22:avg_rent_per_m2_census22,
         GEN:geometry) %>%
  unique()

nrw_lw <- nrw_lw %>%
  left_join(census_merge 
              %>% filter(state == "05"), 
            by = join_by(ags == ags))
federal <- federal %>%
  left_join(census_merge, 
            by = join_by(ags == ags,
                         state == state))

turkish <- read.csv(file = "~/Desktop/THESIS/turkey.csv", 
                    colClasses = c(ags = "character")) %>%
  mutate(ags = paste0(substr(ags, 1, 5), 
                      substr(ags, nchar(ags) - 2, nchar(ags))))

nrw_missing <- nrw_lw %>%
  anti_join(
    state,
    by = c("state", "election_date")
  ) %>%
  mutate(valid_votes = total_votes)

religion <- read.csv(file = "~/Desktop/THESIS/religion_breakdown.csv") %>%
  filter(AGS_STATE == 5) %>%
  mutate(ags = paste0("0", AGS_8)) %>%
  group_by(ags) %>%
  summarize(total = sum(total),
            protestant = sum(Protestant.Count) / total * 100,
            catholic = sum(Catholic.Count) / total * 100,
            other_none = sum(Other.Count) / total * 100)
  
all_elections <- bind_rows(federal, state, nrw_missing) %>%
  select(-c("state_name", "total_vote_share", "ags_name"))

party_levels <- c("cdu_csu", "spd", "fdp", "gruene", "linke_pds", 
                  "afd", "ad_demokraten", "other")

all_long <- all_elections %>%
  pivot_longer(
    cols = cdu_csu:ad_demokraten,
    names_to = "party",
    values_to = "vote_share"
  ) %>%
  filter(!is.na(vote_share)) %>%
  mutate(vote_share = 100*vote_share,
         party = factor(party, levels = party_levels))

all_long_processing <- all_long %>%
  select(ags, GEN, state, party, election_lvl, eligible_voters,
         valid_votes, turnout, election_date, vote_share, 
         population_census22:avg_rent_per_m2_census22)

vote_changes <- all_long_processing %>%
  mutate(election_date = as.Date(election_date),
         election_year = as.character(year(election_date))) %>%
  arrange(ags, party, election_date, election_lvl) %>%
  group_by(ags, party) %>%
  mutate(
    last_date        = lag(election_date),
    last_vote_share  = lag(vote_share),
    last_election    = lag(election_lvl),
    days_between     = as.numeric(election_date - last_date)
  ) %>%
  ungroup() %>%
  mutate(vote_diff = ifelse(is.na(last_vote_share), 
                            vote_share,
                            vote_share - last_vote_share),
         transition = paste(last_election, "→", election_lvl)) %>%
  filter(vote_diff < 100,
         vote_diff > -100) %>%
  mutate(vote_change_intensity = abs(vote_diff),
         is_2017 = ifelse(transition == "state → federal" &
                          year(election_date) == 2017 &
                          state == "05",
                       TRUE,
                       FALSE),
         years_between = days_between/365,
         years_bin = cut(years_between,
                        breaks = c(-1, .25, .5, 1, 2, Inf)))

nrw_changes <- vote_changes %>%
  filter(state == "05",
         election_lvl %in% c("state", "federal"))

plot_vote_diffs <- vote_changes %>%
  filter(transition == "state → federal",
         days_between < 1600 & !is.na(days_between))

## Plots

color_vector <- c(cdu_csu = "black", fdp = "gold", gruene = "forestgreen", 
                  linke_pds = "purple", spd = "red3", afd = "skyblue2",
                  other = "grey")
major_parties <- c("cdu_csu", "spd", "gruene", "linke_pds", "afd", "fdp")

party_plotlabels <- c(
  cdu_csu = "CDU",
  spd = "SPD", 
  linke_pds = "Die Linke",
  gruene = "The Greens",
  fdp = "FDP",
  afd = "AfD")

ggplot(data = plot_vote_diffs %>%
        filter(party %in% major_parties),
      mapping = aes(x = years_between, 
                    y = vote_change_intensity, 
                    color = party)) +
  scale_color_manual(values = color_vector) +
  facet_wrap(~party, 
             labeller = as_labeller(party_plotlabels)) + 
  geom_point(alpha = 0.01) +
  scale_y_continuous(limits = c(-.001, 50)) +
  geom_vline(xintercept = c(.25, .5, 1, 2),
             linetype = "dashed",
             color = "grey40",
             alpha = 0.6) +
  geom_smooth(method = "loess") +
  labs(title = "Figure 6: Fluctuation in Vote Share Between Subsequent Elections",
       subtitle = "Within Gemeinde from 2006-2019 (State → Federal transitions only)",
       x = "Years from Last Election",
       y = "Absolute Value of Difference in Vote Share (%)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic")) +
  guides(color = "none")

highlight_start <- as.Date("2017-05-01")
highlight_end   <- as.Date("2017-11-01")
highlight_df <- data.frame(
  xmin = highlight_start,
  xmax = highlight_end,
  ymin = -Inf,
  ymax = Inf,
  period = "2017"
)

lander_aggs <- vote_changes %>%
  filter(!is.na(vote_share),
         !is.na(valid_votes),
         election_date > "2009-07-01") %>%
  group_by(state, party, election_date) %>%
  summarise(vote_share = sum(vote_share*valid_votes)/sum(valid_votes)) %>%
  mutate(is_nrw = ifelse(state == "05", TRUE, FALSE))

federal_aggs <- vote_changes %>%
  filter(!is.na(vote_share),
         !is.na(valid_votes),
         election_date > "2009-07-01",
         election_date < "2022-01-01") %>%
  group_by(party, election_date) %>%
  summarise(vote_share = sum(vote_share*valid_votes)/sum(valid_votes)) %>%
  mutate(is_nrw = ifelse(election_date %in% c("2017-09-24", "2017-05-14"), 
                         TRUE, FALSE))

alpha_vec <- c("TRUE" = .8, "FALSE" = .2)
width_vec <- c("TRUE" = .8, "FALSE" = .3)

zoom_lander_timeseries <- ggplot(
  lander_aggs %>% 
    filter(party %in% major_parties,
           election_date <= "2022-01-01",
           election_date >= "2009-01-01"),
  aes(x = election_date, y = vote_share)) + 
  geom_line(
    aes(color = party,
        group = state,
        alpha = is_nrw,
        linewidth = is_nrw)) +
  facet_wrap(~ party,
             scales = "fixed",
             labeller = as_labeller(party_plotlabels)) +
  geom_smooth(
    aes(group = 1),
    method = "loess",
    se = TRUE,
    color = "gray30",
    fill = "gray70",
    linetype = 5) +
    geom_rect(
    data = highlight_df,
    aes(xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax),
    fill = "firebrick",
    alpha = 0.2,
    color = "black",
    inherit.aes = FALSE) +
    scale_linewidth_manual(
    values = c("TRUE" = 0.8,
               "FALSE" = 0.4),
    labels = c(
      "TRUE" = "North Rhine-Westphalia",
      "FALSE" = "other States")) +  
  scale_color_manual(values = color_vector) +
  scale_alpha_manual(
    values = c("TRUE" = 1,
               "FALSE" = 0.25)) +
  labs(
    title = "Figure 4: Voting Trends in Germany Across States (2009-2022)",
    subtitle = "With 2017 and North Rhine-Westphalia Highlighted",
    caption = "Source: GERDA Elections Database (Hedesheimmer et. al, 2025)",
    x = "",
    y = "Vote Share by Party",
    linewidth = "State") +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(face = "italic", size = 10),
    plot.caption = element_text(family = "Times New Roman", size = 8, hjust = 0,
                                color = "grey40", face = "italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9)) +
  guides(alpha = "none",
         color = "none")
zoom_lander_timeseries

party_cols <- c("spd", "cdu_csu", "gruene", "fdp", "linke_pds", "afd")
party_vote_share_cols <- paste0("vote_share_", party_cols)
party_vote_diff_cols <- paste0("vote_diff_", party_cols)

party_colors <- c(
  spd = "red3",
  cdu_csu = "gray28",
  gruene = "#1AA037",
  fdp = "#FFED00",
  linke = "magenta3",
  linke_pds = "magenta3",
  afd = "dodgerblue",
  other = "lightgrey"
)

party_colors2 <- party_colors[!names(party_colors) %in% c("other", "linke")]

nrw_mapdata_2017 <- vote_changes %>%
  filter(state == "05",
    transition == "state → federal",
    election_year == "2017") %>%
  left_join(gemeinde_clean, by = join_by(ags == AGS,
                                         GEN == GEN)) %>%
  pivot_wider(id_cols = c(ags:GEN, 
                          share_under18_census22:share_migration_bg_census22,
                          BEZ:geometry),
              names_from = party,
              names_sep = "_",
              values_from = c(vote_share, vote_diff)) %>%
  rowwise() %>%
  mutate(
    winner = party_vote_share_cols[which.max(
      c_across(all_of(party_vote_share_cols)))],
    winner_share = max(c_across(all_of(party_vote_share_cols))),
    biggest_winner = party_vote_diff_cols[which.max(
      c_across(all_of(party_vote_diff_cols)))],
    biggest_loser = party_vote_diff_cols[which.min(
      c_across(all_of(party_vote_diff_cols)))],
    winner_quant = max(c_across(all_of(party_vote_diff_cols))),
    loser_quant = min(c_across(all_of(party_vote_diff_cols)))
  ) %>%
  ungroup() %>%
  mutate(winner = str_remove(winner, "^vote_share_"),
         biggest_winner = str_remove(biggest_winner, "^vote_diff_"),
         biggest_loser = str_remove(biggest_loser, "^vote_diff_")) %>%
  left_join(religion, by = join_by(ags == ags))

turkish_nrw <- turkish %>%
  filter(substr(ags, 1, 2) == "05")

nrw_2017 <- nrw_mapdata_2017 %>%
  left_join(turkish,
            by = join_by(ags == ags)) %>%
  mutate(spd_diff_sq = vote_diff_spd^2,
         cdu_diff_sq = vote_diff_cdu_csu^2,
         greens_diff_sq = vote_diff_gruene^2,
         left_diff_sq = vote_diff_linke_pds^2,
         spd_fav = vote_share_spd - mean(nrw_mapdata_2017$vote_share_spd),
         cdu_fav = vote_share_cdu_csu - mean(nrw_mapdata_2017$vote_share_cdu_csu),
         greens_fav = vote_share_gruene - mean(nrw_mapdata_2017$vote_share_gruene),
         left_fav = vote_share_linke_pds - mean(nrw_mapdata_2017$vote_share_linke_pds))

top_turkish <- nrw_2017 %>%
  slice_max(turkish_born, n = 20)

nrw_2017 <- nrw_2017 %>%
  mutate(
    top_turkish_flag = ifelse(ags %in% top_turkish$ags, 
                              "Top Turkish", "Other"),
    gain_magnitude = winner_quant,
    loss_magnitude = abs(loser_quant),
    biggest_winner = factor(biggest_winner, levels = names(party_colors2)),
    biggest_loser  = factor(biggest_loser,  levels = names(party_colors2))
  )

nrw_outline <- nrw_2017 %>%
  select(geometry) %>%
  st_as_sf %>%
  st_union

vote_changes_diag <- vote_changes %>%
  filter(!is.na(last_election),
         !is.na(days_between),
         vote_diff != 0) %>%
  mutate(transition_party = paste0(election_year, last_election, "_to_", 
                                   election_lvl, "_", party)) %>%
  group_by(party, transition) %>%
  mutate(
    vote_change_dm = vote_diff - mean(vote_diff, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(state == "05") %>%
  left_join(gemeinde_clean %>% select(AGS, geometry),
            by = join_by(ags == AGS)) %>%
  left_join(turkish %>% select(-GEN),
            by = join_by(ags == ags)) %>%
  mutate(turkish_share_std = scale(turkish_born)[,1],
         turkish_born = turkish_born * 100,
         color_group = ifelse(is_2017, as.character(party), "other"))

ggplot(vote_changes_diag %>%
         filter(party %in% party_cols),
       aes(x = turkish_born,
           y = vote_change_dm,
           color = color_group,
           alpha = factor(is_2017))) +
  geom_point(size = .75) +
  scale_y_continuous(limits = c(-15, 15)) +
  scale_color_manual(values = color_vector) +
  scale_alpha_manual(
    values = c(
      "FALSE" = 0.03,
      "TRUE"  = 0.3),
    labels = c("FALSE" = "other elections",
               "TRUE" = "2017 State → Federal"),
    guide = guide_legend(
      override.aes = list(
        color = c("grey70", "red3"),   # <-- THIS is the key trick
        size = 2,
        linewidth = 1))) +
  geom_smooth(method = "lm", se = FALSE,
              linewidth = .8) +
  facet_wrap(~party,
             labeller = as_labeller(party_plotlabels)) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dashed",
             linewidth = .3) +
  labs(
    title = "Figure 7: Municipality-Level Deviations from National Swings by Turkish Population",
    subtitle = "Comparing 2017 to national election transitions among NRW Gemeinde",
    x = "Turkish Share in Gemeinde (%)",
    y = "Demeaned Deviation in Vote Share (%, Transition × Party)",
    alpha = "Transition",
    caption = "Source: GERDA Elections Database (Hedesheimmer et. al, 2025)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
  plot.title       = element_text(face = "bold", size = 10),
  plot.subtitle = element_text(face = "italic", size = 8),
  plot.caption = element_text(family = "Times New Roman", size = 8, hjust = 0,
                                              color = "grey40", face = "italic"),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 9))

## Unused Plots

religious_map <- ggplot(nrw_mapdata_2017) +
  geom_sf(aes(geometry = geometry,
              fill = other_none),
          color = NA) +
  scale_fill_gradient(name = "Proportion (%) Non-Christian",
                      low = "white",
                      high = "darkgreen") +
  theme_void()
religious_map

shared_alpha_scale <- scale_alpha_continuous(
  name = "Magnitude of Vote Shift",
  limits = c(0, 0.1),
  guide = guide_legend(
    ncol = 2,       # ← THIS does it
    byrow = TRUE    # fills across rows (looks cleaner)
  )
)

shared_fill_scale <- scale_fill_manual(
  values = party_colors2,
  labels = party_plotlabels,
  drop = FALSE,   
  name = "Party")

biggest_winner_2017 <- ggplot(nrw_2017) +
  geom_sf_pattern(
    aes(
      geometry = geometry,
      fill = biggest_winner,
      alpha = gain_magnitude,
      pattern = top_turkish_flag
    ),
    color = NA,
    pattern_fill = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02
  ) +
  geom_sf(data = top_turkish,
          mapping = aes(geometry = geometry),
          color = "black",
          linewidth = .5) +
  geom_sf(data = nrw_outline,
          mapping = aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .3) + shared_fill_scale +
  shared_alpha_scale +
  scale_pattern_manual(
    values = c("Other" = "none", "Top Turkish" = "stripe"),
    name = "High Turkish Population",
    labels = c("Other", "Top 20 Municipalities by % Turkish")) +
  labs(
    title = "Vote Gains") +
  theme_void() +
  guides(
    fill = guide_legend(
      order = 3,
      override.aes = list(pattern = "none")
    ),
    alpha = guide_legend(
      order = 1,
      override.aes = list(pattern = "none")
    ),
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        fill = "grey70",
        alpha = 1)))

biggest_loser_2017 <- ggplot(nrw_2017) +
  geom_sf_pattern(
    aes(
      geometry = geometry,
      fill = biggest_loser,
      alpha = loss_magnitude,
      pattern = top_turkish_flag
    ),
    color = NA,
    pattern_fill = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02
  ) +
  geom_sf(data = top_turkish,
          mapping = aes(geometry = geometry),
          color = "black",
          linewidth = .5) +
  geom_sf(data = nrw_outline,
          mapping = aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .3) +
  shared_fill_scale +
  shared_alpha_scale +
  scale_pattern_manual(
    values = c("Other" = "none", "Top Turkish" = "stripe"),
    name = "High Turkish Population",
    labels = c("Other", "Top 20 Municipalities by % Turkish")) +
  labs(
    title = "Vote Losses") +
  theme_void() +
  guides(fill = "none",
         alpha = guide_legend(
           order = 1,
           override.aes = list(pattern = "none")
         ),
         pattern = guide_legend(
           order = 2,
           override.aes = list(
             fill = "grey70",
             alpha = 1)))

combined_plot <- (biggest_winner_2017 | biggest_loser_2017) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Vote Shifts in NRW Municipalities (2017)",
    subtitle = "Largest Party Gains and Losses by Municipality",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal"))

combined_plot

spd_shift_2017 <- ggplot(nrw_2017) +
  geom_sf(aes(geometry = geometry,
              fill = vote_diff_spd), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    high = "red3",
    midpoint = 0,
    limits = c(-.1, .05),
    name = "Vote Shift"
  ) +
  geom_sf(data = top_turkish,
          aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .5) +
  labs(title = "SPD") +
  theme_void()
spd_shift_2017

cdu_shift_2017 <- ggplot(nrw_2017) +
  geom_sf(aes(geometry = geometry,
              fill = vote_diff_cdu_csu), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    high = "red3",
    midpoint = 0,
    limits = c(-.1, .05),
    guide = "none"
  ) +
  geom_sf(data = top_turkish,
          aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .5) +
  labs(title = "CDU") +
  theme_void()

greens_shift_2017 <- ggplot(nrw_2017) +
  geom_sf(aes(geometry = geometry,
              fill = vote_diff_gruene), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    high = "red3",
    midpoint = 0,
    limits = c(-.1, .05),
    guide = "none"
  ) +
  geom_sf(data = top_turkish,
          aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .5) +
  labs(title = "The Greens") +
  theme_void()

linke_shift_2017 <- ggplot(nrw_2017) +
  geom_sf(aes(geometry = geometry,
              fill = vote_diff_linke_pds), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    high = "red3",
    midpoint = 0,
    limits = c(-.1, .05),
    guide = "none"
  ) +
  geom_sf(data = top_turkish,
          aes(geometry = geometry),
          fill = NA,
          color = "black",
          linewidth = .5) +
  labs(title = "die Linke") +
  theme_void()

vote_shift_2017 <- (spd_shift_2017 + 
                      cdu_shift_2017 + 
                      greens_shift_2017 + 
                      linke_shift_2017) +
  plot_layout(ncol = 2,
              guides = "collect") +
  plot_annotation(
    title = "2017 Vote Shifts Among Major Parties",
    subtitle = "Vote Share Shift by Parties within Gemeinde (State → Federal)",
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text  = element_text(size = 7)))
vote_shift_2017
