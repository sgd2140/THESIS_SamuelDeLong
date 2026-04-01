stb_demos <- read.csv(file = "~/Desktop/Thesis/Köln/stimmbezirk_info.csv") # Adjust uploads for own wd
stt_demos <- read.csv(file = "~/Desktop/Thesis/Köln/stadtteile_info.csv")

stimmbezirk_17 <- read_sf("~/Desktop/THESIS/Stimmbezirk_201705/") %>%
  mutate(NUMMER = as.numeric(NUMMER),
         NR_STADTTE = as.numeric(NR_STADTTE),
         centroid = st_centroid(geometry)) %>%
  select(NUMMER, NR_STADTTE, centroid, geometry) %>%
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2])

stadtteil <- read_sf("~/Desktop/THESIS/Köln/Stadtteil_21/") %>%
  mutate(nummer = as.numeric(nummer)) %>%
  left_join(stt_demos, by = join_by("nummer" == "stadtteil")) %>%
  select(nummer, stadtteil_name:unemployment)

stimmbezirk <- read_sf("~/Desktop/THESIS/Köln/Stimmbezirk_20/") %>%
  mutate(nummer = as.numeric(nummer),
         nr_stteil = as.numeric(nr_stteil)) %>%
  left_join(stb_demos, by = join_by("nummer" == "stimmbezirk")) %>%
  select(nummer, nr_stteil:stteil, geometry:X65_plus)
  
count_cols <- stimmbezirk %>%
  select(total_pop:X65_plus) %>%
  st_drop_geometry() %>%
  colnames

rstimmbezirk_agg <- stimmbezirk %>%
  st_drop_geometry() %>%
  group_by(nr_stteil) %>%
  summarise(
    across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  select(-c(total_pop, migration_bg))

interculturals <- st_read("~/Desktop/THESIS/Köln/interculturals.json") %>%
  filter(!is.na(z_name)) %>%
  mutate(has_turkish = str_detect(z_sprachen, "Türkisch"))
    
stadtteil_full <- stadtteil %>%
  st_as_sf(crs = 25832) %>%
  st_transform(crs = 4326) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6,
         pop_density = total_pop / area_km2,
         log_pop_density = log(pop_density)) %>%
    left_join(rstimmbezirk_agg, by = join_by("nummer" == "nr_stteil")) %>%
  mutate(turkish_mhg_share = turkish_migration_bg / total_pop,
            mhg_share = migration_bg / total_pop,
            social_elig_share = social_eligible / total_pop)

stadtteil_join <- stadtteil_full %>%
  st_drop_geometry()

koln_outline <- stadtteil %>%
  st_transform(crs = st_crs(stimmbezirk_17)) %>%
  st_union() %>%
  st_as_sf()

## Population visualizations

ggplot(data = stadtteil_full) +
  geom_sf(mapping = aes(geometry = geometry,
                        fill = turkish_mhg_share,
                        color = turkish_mhg_share)) +
  geom_sf(data = interculturals %>% filter(has_turkish == TRUE),
          mapping = aes(geometry = geometry, shape = "Turkish-language\ncultural centre"),
          color = "black",
          size = 1.5, alpha = 0.5) +
  geom_sf(data = koln_outline,
          color = "black",
          fill = NA) +
  scale_fill_gradient(
    low    = "white",
    high   = "red2",
    name   = "Turkish Migration\nBackground (% of pop.)",
    guide  = guide_colorbar(
      barwidth  = 0.8,
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 7)
    )
  ) +
  scale_color_gradient(
    low   = "white",
    high  = "red2",
    guide = "none"         
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Turkish-language\ncultural centre" = 19),
    guide  = guide_legend(
      override.aes = list(size = 1.5, alpha = 0.5)
    )
  ) +
  labs(title = "Turkish Population in Köln") +
  theme_void() +
  theme(
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text     = element_text(size = 8),
    legend.title    = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.spacing.y = unit(0.3, "cm"),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.4),
    legend.margin     = margin(4, 6, 4, 6),
    legend.position   = c(0.2, 0.2)
  )

## Election analysis

landerwahl_koln <- read.csv(file = "~/Desktop/Thesis/Köln/2017Landerwahl.csv") %>%
  mutate(election_date = ymd("2017-05-14"),
         election_yr = "2017",
         election_lvl = "state")
kolnbundes_2017 <- read_csv(file = "~/Desktop/THESIS/Köln/2017Bundeswahl.csv") %>%
  mutate(election_date = ymd("2017-09-24"),
         election_yr = "2017",
         election_lvl = "federal")
kolnbundes_2013 <- read_csv(file = "~/Desktop/THESIS/Köln/2013Bundeswahl.csv") %>%
  mutate(election_date = ymd("2013-09-22"),
         election_yr = "2013",
         election_lvl = "federal")

koln_dataset <- rbind(landerwahl_koln, kolnbundes_2017, kolnbundes_2013) %>%
  pivot_longer(
    cols = spd:other,
    names_to = "party",
    values_to = "votes"
  ) %>%
  mutate(votes = votes * 100)

koln_analysis <- koln_dataset %>%
  arrange(stimmbezirk, party, election_date, election_lvl) %>%
  group_by(stimmbezirk, party) %>%
  mutate(last_date        = lag(election_date),
         last_vote  = lag(votes),
         last_election    = lag(election_lvl),
         days_between     = as.numeric(election_date - last_date)) %>%
  ungroup() %>%
  mutate(vote_diff = ifelse(is.na(last_vote), 
                            votes,
                            votes - last_vote)) %>%
  filter(election_yr != "2013") %>%
  pivot_wider(id_cols = c(stimmbezirk:election_lvl),
              names_from = party,
              values_from = c(votes, last_vote, vote_diff))

koln_analysis_geo <- koln_analysis %>%
  left_join(stimmbezirk_17, by = join_by("stimmbezirk" == "NUMMER")) %>%
  left_join(stadtteil_join, 
            by = join_by("NR_STADTTE" == "nummer")) %>%
  mutate(anti_endorsed_share = votes_spd + votes_cdu_csu + votes_gruene,
         last_aggregate = last_vote_cdu_csu + last_vote_gruene + last_vote_spd,
         non_turkish_foreign = mhg_share - turkish_mhg_share,
         youth_share = under_18 / total_pop,
         elder_share = X65_plus / total_pop,
         post = ifelse(election_lvl == "federal", 1, 0))

koln_analysis_std <- koln_analysis_geo %>%
  st_drop_geometry() %>%
  mutate(turkish_mhg_std = scale(turkish_migration_bg)[,1],
         youth_share_std = scale(youth_share)[,1],
         elder_share_std = scale(elder_share)[,1],
         nonturkish_mhg_std = scale(non_turkish_foreign)[,1],
         social_elig_std = scale(social_elig_share)[,1]) %>%
  filter(votes_ad_demokraten != 0)

loss_plot <- koln_analysis_geo %>%
  filter(election_lvl == "federal")

most_turkish <- stadtteil_full %>%
  arrange(turkish_mhg_share) %>%
  tail(10) %>%
  st_transform(crs = st_crs(stimmbezirk_17))

ggplot(loss_plot) +
  geom_sf(mapping = aes(geometry = geometry,
            fill = vote_diff_ad_demokraten),
          color = NA) +
  scale_fill_gradient2(low = "blue",
                      high = "red3") +
  geom_sf(data = koln_outline, fill = NA, color = "black") +
  geom_sf(data = most_turkish,
          color = "black", linewidth = .6, fill = NA) +
  theme_void()

## Models

koln_covariates <- c(
  "post:social_elig_std",
  "post:youth_share_std",
  "post:elder_share_std")

did_formula <- function(outcome) {
  as.formula(paste(
    outcome, "~",
    "post + post:turkish_mhg_std +",
    paste(koln_covariates, collapse = " + "),
    paste("| stimmbezirk")
  ))
}

### Aggregated Anti-Endorsements

m_agg_ols <- feols(
  did_formula("anti_endorsed_share"),
  data    = koln_analysis_std,
  cluster = ~NR_STADTTE
)

m_agg_ols

coef_labels <- c("post" = "Post/Vote Shift (Intercept",
                 "post:turkish_mhg_std" = "Post × Turkish Share",
                 "post:social_elig_std" = "Social Welfare Eligibility",
                 "post:youth_share_std" = "Young Adult (18-25) Share",
                 "post:elder_share_std" = "Older Adult (65+) Share")

modelsummary(
  m_agg_ols,
  coef_map = coef_labels,
  fmt = 2,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Anti-Endorsement Effect on Aggregated Party Vote Share: Cologne",
  notes = "Stimmbezirke fixed effects included in all models. Standard errors clustered at Stadtteile level.",
  output = "~/Desktop/THESIS/Köln/koln_agg.html"  # or "html", "docx", "kableExtra" etc.
)

## Party-specific analysis

parties <- c("votes_cdu_csu", "votes_spd", "votes_fdp",
             "votes_gruene", "votes_linke_pds", "votes_afd",
             "votes_ad_demokraten")

party_formula <- function(outcome) {
  
  as.formula(paste(
    outcome, "~",
    "post + post:turkish_mhg_std +",
    "+",                        
    paste(koln_covariates, collapse = " + "),
    "| stimmbezirk"
  ))
}

party_specific <- map(
  setNames(parties, parties),
  ~ feols(
    party_formula(.x),
    data = koln_analysis_std,
    cluster = ~NR_STADTTE
  )
)

party_specific

col_labels <- list(
  votes_cdu_csu        = "CDU/CSU",
  votes_spd            = "SPD",
  votes_gruene         = "the Greens",
  votes_fdp            = "FDP",
  votes_afd            = "AfD",
  votes_linke_pds      = "Die Linke",
  votes_ad_demokraten  = "ADD"
)


modelsummary(
  party_specific,
  coef_map = coef_labels,
  fmt = 2,
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  gof_map  = list(
    list(raw = "nobs",          clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R²",      fmt = 2),
    list(raw = "rmse",          clean = "RMSE",          fmt = 3)
  ),
  title  = "Table 5: Party-specific vote share models, Köln Stimmbezirke (2017)",
  notes  = "Standard errors clustered at the Stimmbezirk level. Continuous covariates standardized. Dep. vars. are Zweitestimmen vote shares.",
  output = "~/Desktop/THESIS/köln/party_specific_table.docx"
)
