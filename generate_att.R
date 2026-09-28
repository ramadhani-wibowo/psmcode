library(dplyr)
library(MatchIt)
library(broom)
# -----------------------------
# Full PA list
# -----------------------------
pa_list <- c()
# -----------------------------
# Results table
# -----------------------------
results <- tibble(
  pa_id = character(),
  att_pre4 = numeric(),
  se_pre4 = numeric(),
  p_pre4 = numeric(),
  ci_lower_pre4 = numeric(),
  ci_upper_pre4 = numeric(),
  att_psm3 = numeric(),
  se_psm3 = numeric(),
  p_psm3 = numeric(),
  ci_lower_psm3 = numeric(),
  ci_upper_psm3 = numeric()
)
# -----------------------------
# Loop over PAs
# -----------------------------
for (pa_id in pa_list) {
  cat("Processing PA:", pa_id, "...\n")
  
  dat_all <- psm_df_bui3 %>%
    filter(mc == pa_id | PA == 0) %>%
    mutate(
      lulc = factor(lulc),
      elev_class = factor(elev_class),
      roadd  = ifelse(is.na(roadd), 0, roadd),
      ttcity = ifelse(is.na(ttcity), 0, ttcity)
    )
  
  dat_all$lulc <- droplevels(dat_all$lulc)
  dat_all$elev_class <- droplevels(dat_all$elev_class)
  
  treated <- dat_all %>% filter(PA == 1)
  controls <- dat_all %>% filter(PA == 0)
  
  # -----------------------------
  # Pre4 ATT
  # -----------------------------
  set.seed(123)
  controls_sample <- controls %>% slice_sample(n = min(4 * nrow(treated), nrow(controls)))
  
  att_pre4 <- mean(treated$bui2020, na.rm = TRUE) - mean(controls_sample$bui2020, na.rm = TRUE)
  se_pre4 <- sqrt(var(treated$bui2020)/nrow(treated) + var(controls_sample$bui2020)/nrow(controls_sample))
  t_pre4 <- att_pre4 / se_pre4
  p_pre4 <- 2 * (1 - pnorm(abs(t_pre4)))
  ci_lower_pre4 <- att_pre4 - 1.96 * se_pre4
  ci_upper_pre4 <- att_pre4 + 1.96 * se_pre4
  
  # -----------------------------
  # PSM3 ATT
  # -----------------------------
  rhs <- c("elev", "slope", "roadd", "ttcity", "factor(lulc)", "factor(elev_class)")
  f <- reformulate(rhs, response = "PA")
  
  psm_match <- try(matchit(f, data = dat_all, method = "nearest", distance = "logit",
                           ratio = 3, replace = TRUE, caliper = 0.25), silent = TRUE)
  
  if (inherits(psm_match, "try-error")) {
    att_psm3 <- se_psm3 <- p_psm3 <- ci_lower_psm3 <- ci_upper_psm3 <- NA
  } else {
    md <- match.data(psm_match)
    att_psm3 <- mean(md$bui2020[md$PA == 1], na.rm = TRUE) - mean(md$bui2020[md$PA == 0], na.rm = TRUE)
    se_psm3 <- sqrt(var(md$bui2020[md$PA == 1])/sum(md$PA == 1) + var(md$bui2020[md$PA == 0])/sum(md$PA == 0))
    t_psm3 <- att_psm3 / se_psm3
    p_psm3 <- 2 * (1 - pnorm(abs(t_psm3)))
    ci_lower_psm3 <- att_psm3 - 1.96 * se_psm3
    ci_upper_psm3 <- att_psm3 + 1.96 * se_psm3
  }
  
  # -----------------------------
  # Store results
  # -----------------------------
  results <- results %>% add_row(
    pa_id = pa_id,
    att_pre4 = att_pre4, se_pre4 = se_pre4, p_pre4 = p_pre4,
    ci_lower_pre4 = ci_lower_pre4, ci_upper_pre4 = ci_upper_pre4,
    att_psm3 = att_psm3, se_psm3 = se_psm3, p_psm3 = p_psm3,
    ci_lower_psm3 = ci_lower_psm3, ci_upper_psm3 = ci_upper_psm3
  )
}
# -----------------------------
# View results
# -----------------------------
print(results)
