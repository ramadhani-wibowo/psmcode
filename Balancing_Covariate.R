library(dplyr)
library(MatchIt)
library(tibble)
library(readr)
# ---------------------------
# Function: PSM per PA
# ---------------------------
run_psm_pa_fixed_caliper <- function(df = psm_df_pa , pa_id, pre_ratio = 4, caliper = 0.25, max_ratio = 3) {
  dat_all <- df %>%
    filter(mc == pa_id | PA == 0) %>%
    mutate(
      lulc = factor(lulc),
      elev_class = factor(elev_class),
      roadd = ifelse(is.na(roadd), 0, roadd),
      ttcity = ifelse(is.na(ttcity), 0, ttcity)
    )
  treated <- dat_all %>% filter(mc == pa_id & PA == 1)
  control <- dat_all %>% filter(PA == 0)
  n_t <- nrow(treated)
  n_c <- nrow(control)
  if (n_t == 0 || n_c == 0) return(NULL)
  # Pre-sample controls (4x treated, no cap)
  target_controls <- min(max(1, floor(pre_ratio * n_t)), n_c)
  set.seed(123)
  control_sampled <- control %>% slice_sample(n = target_controls)
  dat <- bind_rows(treated, control_sampled)
  # Handle categorical vars
  cat_vars <- c("lulc", "elev_class")
  keep_cat <- cat_vars[sapply(dat[cat_vars], function(x) nlevels(factor(x)) >= 2)]
  rhs <- c("elev", "slope", "roadd", "ttcity")
  if (length(keep_cat) > 0) {
    rhs <- c(rhs, paste0("factor(", keep_cat, ")"))
  }
  f <- reformulate(rhs, response = "PA")
  attempted <- list()
  for (ratio in 1:max_ratio) {
    attempt <- try(
      matchit(f,
              data = dat,
              method = "nearest",
              distance = "logit",
              ratio = ratio,
              replace = TRUE,
              caliper = caliper),
      silent = TRUE
    )
    if (inherits(attempt, "try-error")) next
    md <- match.data(attempt)
    total_controls <- as.numeric(nrow(dat %>% filter(PA == 0)))
    matched_controls <- as.numeric(nrow(md %>% filter(PA == 0)))
    matched_treated <- as.numeric(nrow(md %>% filter(PA == 1)))
    unmatched_pct <- round(100 * (total_controls - matched_controls) / total_controls, 2)
    matched_treated_pct <- round(100 * matched_treated / n_t, 2)
    stats <- tibble(
      pa_id = pa_id,
      total_treated = n_t,
      matched_treated = matched_treated,
      matched_treated_pct = matched_treated_pct,
      total_controls = total_controls,
      matched_controls = matched_controls,
      unmatched_pct = unmatched_pct,
      ratio = ratio,
      caliper = caliper,
      replace = TRUE,
      pre_ratio = pre_ratio
    )
    # Acceptable match: unmatched <= 20%
    if (unmatched_pct <= 20) return(stats)
    attempted[[length(attempted) + 1]] <- list(stats = stats, attempt = attempt)
  }
  if (length(attempted) == 0) return(NULL)
  # Pick best (lowest unmatched%)
  best <- attempted[[which.min(sapply(attempted, function(x) x$stats$unmatched_pct))]]
  return(best$stats)
}
# ---------------------------
# Run for all 600 PAs
# ---------------------------
pa_ids_all <- pa_ids
stats_file_all <- "F:/TRY NON Aggregated/PSM_results_perPA_all.csv"
# Write header with UTF-8 BOM
write_lines(
  paste0("\ufeff", paste(c(
    "pa_id","total_treated","matched_treated","matched_treated_pct",
    "total_controls","matched_controls","unmatched_pct","ratio","caliper","replace","pre_ratio"
  ), collapse = ",")),
  stats_file_all
)
# Loop through all PAs
for (p in pa_ids_all) {
  cat("Processing PA", p, "...\n")
  res_stats <- run_psm_pa_fixed_caliper(psm_df_pa, p, caliper = 0.25)
  
  if (is.null(res_stats)) {
    # Save a row with NAs if matching failed
    res_stats <- tibble(
      pa_id = p,
      total_treated = NA,
      matched_treated = NA,
      matched_treated_pct = NA,
      total_controls = NA,
      matched_controls = NA,
      unmatched_pct = NA,
      ratio = NA,
      caliper = 0.25,
      replace = TRUE,
      pre_ratio = 4
    )
    cat("⚠️ PA", p, "did not produce valid matches and was saved with NAs.\n")
  }
  
  # Append to CSV
  write_csv(res_stats, stats_file_all, append = TRUE)
}

