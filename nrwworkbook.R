## Data Upload - change wd() (most data in this workbook is processed
## inititally in the mainworkbook.R file)

bw_kreise <- read_sf("~/Desktop/THESIS/NRW/de_constituency_bounds/")
gemeinde_clean <- gemeinde_clean %>%
  mutate(ARS = str_sub(AGS, end = 5))

## Data Processing
nrw_panel <- nrw_changes %>%
  group_by(ags, GEN, state, election_lvl, election_date, election_year, party) %>%
  slice_max(vote_share) %>%                    
  ungroup() %>%
  pivot_wider(id_cols = c(ags:eligible_voters, turnout, election_date,
                          population_census22:election_year),
              names_from = party,
              values_from = c(vote_share, last_vote_share, vote_diff),
              names_sep = "_") %>%
  left_join(turkish %>% select(-GEN), by = join_by(ags == ags)) %>%
  left_join(religion, by = join_by(ags == ags)) %>%
  left_join(gemeinde_clean %>% select(-GEN), 
            by = join_by(ags == AGS, state == SN_L)) %>%
  st_as_sf() %>%
  mutate(post = ifelse(election_year == 2017 & election_lvl == "federal",
                       1, 0),
         turkish_born = 100 * turkish_born,
         non_turkish_foreign = 1 - (german_born + turkish_born),
         area_km2       = as.numeric(st_area(geometry)) / 1e6,
         pop_density    = total / area_km2,
         log_pop_density = log(pop_density),
         anti_endorsed_share = vote_share_cdu_csu + vote_share_spd + 
           vote_share_gruene,
         non_endorsed_share = vote_share_fdp + vote_share_linke_pds + 
           ifelse(is.na(vote_share_afd), 0, vote_share_afd) + 
           ifelse(is.na(vote_share_other), 0, vote_share_other) +
           ifelse(is.na(vote_share_ad_demokraten), 
                  0, vote_share_ad_demokraten))

avg_votes <- nrw_panel %>%
  group_by(ags) %>%
  summarise(across(
    starts_with("vote_share_"),
    ~ mean(.x, na.rm = TRUE),
    .names = "avg_{.col}"
  ))

nrw_analysis <- nrw_panel %>%
  st_as_sf(crs = 25832) %>%
  st_transform(crs = 4326) %>%
  mutate(turkish_share_std = scale(turkish_born)[,1],
         italian_share_std = scale(italian_born)[,1],
         log_pop_std = scale(log(population_census22))[,1],
         youngadult_share_std = scale(share_18to29_census22)[,1],
         olderadult_share_std = scale(share_65plus_census22)[,1],
         nonturkishforeign_share_std = scale(non_turkish_foreign)[,1],
         avgrent_share_std = scale(avg_rent_per_m2_census22)[,1],
         post = ifelse(election_date == "2017-09-24", 
                       1, 0),
         centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2])

analysis_2017 <- nrw_analysis %>%
  filter(year(election_date) == "2017")

## Plots

add_mod <- feols(vote_share_ad_demokraten ~ turkish_born,
      data = analysis_2017,
      cluster = ~ ARS)

map_data <- nrw_panel %>%
  st_drop_geometry() %>%
  filter(post == 1) %>%           
  left_join(
   gemeinde_clean %>% filter(SN_L == "05") %>% select(AGS, geometry),
    by = join_by(ags == AGS
  )) %>%
  st_as_sf()

map_turkish <- ggplot(map_data) +
  geom_sf(aes(fill = turkish_born), color = NA, size = 0.2) +
  scale_fill_gradient(
    low = "white",
    high = "red3",
    name     = "Turkish-born\npopulation (%)"
  ) +
  labs(title = "") +
  geom_sf(data = nrw_outline, fill = NA, color = "black", linewidth = 0.2) +
  theme_void() +
  theme(text = element_text(family = "Times New Roman"))

map_add <- ggplot(map_data) +
  geom_sf(aes(fill = vote_share_ad_demokraten), color = NA, size = 0.2) +
  scale_fill_distiller(
    palette   = "Purples",
    direction = 1,
    name      = "ADD\nvote share (%)"
  ) +
  labs(title = "") +
  geom_sf(data = nrw_outline, fill = NA, color = "black", linewidth = 0.2) +
  theme_void() +
  theme(text = element_text(family = "Times New Roman"))

map_turkish + map_add +
  plot_annotation(
    title    = "Figure 8: Turkish origin and ADD Support (NRW, 2017)",
    caption  = "Sources: GERDA Elections Database (Hedesheimmer et. al, 2025); Statistik NRW, Zensus 22",
    theme    = theme(text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.caption = element_text(hjust = .15, face = "italic"))
  )

## Models

covariates <- c(
  "log_pop_density:post",
  "avgrent_share_std:post",
  "youngadult_share_std:post",
  "olderadult_share_std:post"
)

did_formula <- function(outcome) {
  as.formula(paste(
    outcome, "~",
    "post + post:turkish_share_std +",
    paste(covariates, collapse = " + "),
    "| ags"
  ))}

### Aggregated Anti-Endorsements

m_agg_ols <- feols(
  did_formula("anti_endorsed_share"),
  data = analysis_2017,
  weights = ~population_census22,
  cluster = ~ARS
)

m_agg_conley_25 <- feols(
  did_formula("anti_endorsed_share"),
  data = analysis_2017,
  weights = analysis_2017$population_census22,
  vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 25)
)

m_agg_conley_50 <- feols(
  did_formula("anti_endorsed_share"),
  data = analysis_2017,
  weights = analysis_2017$population_census22,
  vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 50)
)

agg_coefs_clustered <- coeftable(m_agg_ols)["post:turkish_share_std",]
agg_coefs_conley_25 <- coeftable(m_agg_conley_25)["post:turkish_share_std", ]
agg_coefs_conley_50 <-  coeftable(m_agg_conley_50)["post:turkish_share_std", ]

agg_coefs_table <- rbind(agg_coefs_clustered, agg_coefs_conley_25, 
                     agg_coefs_conley_50)

models <- list(
  "Clustered (ARS)" = m_agg_ols,
  "Conley (25km)"   = m_agg_conley_25,
  "Conley (50km)"   = m_agg_conley_50
)

coef_labels <- c("post" = "Post marker (Intercept)",
                 "post:turkish_share_std" = "Post × Turkish Share",
                 "post:log_pop_density" = "Log Pop. Density (interaction)",
                 "post:avgrent_share_std" = "Avg. Rent per Sq. Meter (interaction)",
                 "post:youngadult_share_std" = "Young Adult (18-25) Share (interaction)",
                 "post:olderadult_share_std" = "Older Adult (65+) Share (interaction)",
                 "post:nonturkishforeign_share_std" = "Non-Turkish Foreigner Share (interaction)")

modelsummary(
  models,
  coef_map = coef_labels,
  fmt = 2,
  gof_map = list(
    list(raw = "nobs",          clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R²",      fmt = 2),
    list(raw = "rmse",          clean = "RMSE",          fmt = 3)
  ),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Effect of Anti-Endorsement on Anti-Endorsed Party Vote Share",
  notes = "Gemeinde fixed effects included in all models. Standard errors clustered at Kreis (ARS) level in column 1; Conley spatial SEs with indicated cutoff in columns 2-3. Models weighted by 2022 census population.",
  output = "~/Desktop/THESIS/NRW/nrw_agg.docx"  # or "html", "docx", "kableExtra" etc.
)

model_vars <- nrw_analysis %>%
  filter(post == 1) %>%            # one row per unit — use federal election rows
  select(
    anti_endorsed_share,
    turkish_share_std,
    avgrent_share_std,
    youngadult_share_std,
    olderadult_share_std,
    log_pop_density
  ) %>%
  st_drop_geometry()

cor_matrix <- cor(model_vars, use = "pairwise.complete.obs")
rownames(cor_matrix) <- colnames(cor_matrix) <- c(
  "Anti-endorsed share",
  "Turkish MHG (std)",
  "Avg. rent per sq. meter (std)",
  "Young adult share (std)",
  "Elder share (std)",
  "Log pop. density"
)

cor_pmat <- cor_pmat(model_vars)
rownames(cor_pmat) <- colnames(cor_pmat) <- c(
  "Anti-endorsed share",
  "Turkish MHG (std)",
  "Avg. rent per sq. meter (std)",
  "Young adult share (std)",
  "Elder share (std)",
  "Log pop. density"
)

round(cor_matrix, 2)
ggcorrplot(
  cor_matrix,
  p.mat     = cor_pmat,
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3,
  insig     = "pch",        # show stars for significance
  sig.level = c(0.01, 0.05, 0.1), # thresholds for ***, **, *
  pch.cex   = 6,
  colors    = c("#2166ac", "white", "#d6604d"),
  outline.color = "white",
  ggtheme   = theme_bw()
) +
  labs(
    title   = "Correlation Matrix: NRW Model Variables",
    subtitle = "insignificant at p < .05 crossed out"
  ) +
  theme(
    text         = element_text(family = "Times New Roman"),
    plot.title   = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 9),
    legend.title = element_text(size = 9)
  )

## Party-specific analysis

parties <- c("vote_share_cdu_csu", "vote_share_spd",
             "vote_share_fdp", "vote_share_gruene", "vote_share_linke_pds", 
             "vote_share_afd", "vote_share_ad_demokraten")

party_covariates <- c(
  "log_pop_density:post",
  "avgrent_share_std:post",
  "youngadult_share_std:post",
  "olderadult_share_std:post")

party_formula <- function(outcome) {
  
  as.formula(paste(
    outcome, "~",
    "post + post:turkish_share_std +",                        
    paste(party_covariates, collapse = " + "),
    "| ags"
  ))
}

party_specific <- map(
  setNames(parties, parties),
  ~ feols(
    party_formula(.x),
    data = analysis_2017,
    weights = analysis_2017$population_census22,
    cluster = ~ARS
  )
)

party_specific

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
  title  = "Party-specific vote share models, NRW Gemeinde (2017)",
  notes  = "Standard errors clustered at Kreise level. Continuous covariates standardized. Dep. vars. are Zweitestimmen vote shares.",
  output = "~/Desktop/THESIS/NRW/party_specific_table.docx"
)

## Placebo analysis - Italians

placebo_covariates <- party_covariates <- c(
  "log_pop_density:post",
  "avgrent_share_std:post",
  "youngadult_share_std:post",
  "olderadult_share_std:post")

placebo_formula <- function(outcome) {
  
  as.formula(paste(
    outcome, "~",
    "post + post:italian_share_std +",                        
    paste(party_covariates, collapse = " + "),
    "| ags"
  ))
}

placebo_party_specific <- map(
  setNames(parties, parties),
  ~ feols(
    placebo_formula(.x),
    data = analysis_2017,
    weights = analysis_2017$population_census22,
    cluster = ~ARS
  )
)

placebo_labels <- c("post" = "Post marker (Intercept)",
                 "post:italian_share_std" = "Post × Italian Share",
                 "post:log_pop_density" = "Log Pop. Density",
                 "post:avgrent_share_std" = "Avg. Rent per Sq. Meter",
                 "post:youngadult_share_std" = "Young Adult (18-25) Share",
                 "post:olderadult_share_std" = "Older Adult (65+) Share")

modelsummary(
  placebo_party_specific,
  coef_map = placebo_labels,
  fmt = 2,
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  gof_map  = list(
    list(raw = "nobs",          clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R²",      fmt = 2),
    list(raw = "rmse",          clean = "RMSE",          fmt = 3)
  ),
  title  = "Placebo (Italian-share) vote share models across parties, NRW Gemeinde (2017)",
  notes  = "Standard errors clustered at Kreise level. Continuous covariates standardized. Dep. vars. are Zweitestimmen vote shares.",
  output = "~/Desktop/THESIS/NRW/placebo_table.docx"
)
