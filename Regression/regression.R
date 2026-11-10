# Regression analysis for the Anticonformity study.
#
# OUTCOMES (neighborhood level, per round)
# - r0: proportion of neutral responses -> mixed-effects Binomial model (glmer)
# - mu: polarization index               -> mixed-effects Gaussian model (lmer)
#
# WORKFLOW
# 1. Load the individual-level data (Clean_files/07_combined_classified.csv)
# 2. Aggregate to neighborhood x round: compute r0, rA, rF, and derive mu
#    (saved as dn_combined.csv)
# 3. Prepare the data: rename columns and relevel factors
# 4. For each outcome (r0, then mu):
#    - Fit candidate models: main effects only vs. interactions,
#      with different round splines
#    - Compare models (AIC/BIC tables and Delta AIC/BIC figures)
#    - Select the best model
#    - Compute marginal effects and predictions (marginaleffects)
# 5. Export results to Regression/ and the figure folder:
#    - .html / .tex tables: model comparison, best model, AME tables
#    - .png figures: fit comparison, AME and predicted-value plots
#    - .rds files: fitted models (models/)
#
# NOTE: To skip refitting, load the saved models from models/ instead
# (see "ONLY LOAD THE MODELS" below)
# IMPORTANT: If you load the models without refitting, you still need to load the dn_combined in order to run the marginaleffects functions.

# REPRODUCIBILITY (renv)
# Package versions are pinned in renv.lock. The .Rprofile in this folder
# activates renv automatically when R is started with this folder as the
# working directory (e.g. open the Regression folder in VS Code / RStudio).
# If you add or remove packages, run renv::snapshot() to update renv.lock.
# Set PATH below to the location of this repository on your machine.

# Load packages
library(lme4)
library(dplyr)
library(modelsummary)
library(tinytable)
library(splines)
library(ggplot2)
library(tibble)
library(marginaleffects)

# Do this once to restore the exact installed package versions to run the codes from `renv.lock`
renv::restore() 

# Load directory
PATH <- "/Users/Lasmi Marbun/Documents/Git/Anticonformity_main/Anticonformity-Analysis/2026/Publishable/"
REGRESSION_PATH <- paste0(PATH, "Regression/")
MODEL_PATH <- paste0(REGRESSION_PATH, "models/")


### ONLY LOAD THE MODELS ###
## r0 Binomial Models
r0_binom_models_load <- readRDS(paste0(MODEL_PATH, "/r0_binom_models.rds"))
## Example to call the model: either with their names or with the index
# names(r0_binom_models_load)
# r0_binom_models_load$"Main only, Round Spline Knot 5,10,15"
# r0_binom_models_load[1]
## Best model: "Interaction, No Round Slope, Round Spline Knot 5,10,15"
r0_binom_models_legend = r0_binom_models_load
# To see the interpretation, jump to the rows where r0_binom_models_legend is used


## mu Binomial Models
# mu_gaussian_models_load <- readRDS(paste0(MODEL_PATH, "/mu_gaussian_models.rds"))
# mu_gaussian_models_load$"Main only, Round Spline Knot 5,10,15"
# mu_gaussian_models_load[2]
# names(mu_gaussian_models_load)

## Load the prettified dn_combined
dn_combined <- readRDS(paste0(MODEL_PATH, "/dn_combined.rds"))


# Load data
df <- read.csv(paste0(PATH, "Clean_files/07_combined_classified.csv"))

# Check colnames
colnames(df)

# Check value counts in these columns
table(df$many_forced)
table(df$duplicated_participant)

# Check how many unique participant_codes in each nid for each round_no
participant_counts <- df %>%
  group_by(nid) %>%
  summarise(unique_participants = n_distinct(pid)) %>%
  ungroup()
participant_counts # still 18 (nid is not unique yet)

# Create a unique nid for each framing
df <- df %>%
  mutate(
    nid_unique = paste0(nid, "_", ifelse(framing == "1", "nonpol", "pol"))
  )
colnames(df)


# --------------------------------------------------------------------------------
# Step 2: Aggregate individual-level data to neighborhood-level
# --------------------------------------------------------------------------------
# Convert variables in df to numeric
df <- df %>%
  mutate(
    antic_actual = as.numeric(antic_actual),
    nudge_aligned = as.numeric(nudge_aligned)
  )


# --------------------------------------------------------------------------------
# Step 3 (merged): r0, rA, rF and mu are computed from the raw individual
# --------------------------------------------------------------------------------
# responses (response in {-1, 0, 1}) within each neighborhood and round, before averaging, then mu is derived from them.

dn_combined <- df %>%
  group_by(nid_unique, beta, p_anti, framing, round_no) %>%
  summarise(
    antic_actual = mean(antic_actual, na.rm = TRUE),
    nudge_aligned = mean(nudge_aligned, na.rm = TRUE),
    r0 = mean(response == 0, na.rm = TRUE),
    rA = mean(response == -1, na.rm = TRUE),
    rF = mean(response == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Polarization index (mu)
    mu = ((1 - abs(rA - rF)) / 2) * (
      ifelse(r0 + rA == 0, 0, rA / (r0 + rA)) +
      ifelse(r0 + rF == 0, 0, rF / (r0 + rF))
    )
  )

# Add the pid_counts
pid_counts <- df %>%
  group_by(nid_unique) %>%
  summarise(participants = n_distinct(pid), .groups = "drop")

dn_combined <- dn_combined %>% left_join(pid_counts, by = "nid_unique")
head(dn_combined)


# Count how many nid_unique per condition (framing, beta, p_anti)
dn_combined %>%
  group_by(framing, beta, p_anti) %>%
  summarise(unique_nid = n_distinct(nid_unique)) %>%
  ungroup()


# Save the aggregated neighborhood-level dataset
write.csv(dn_combined, paste0(REGRESSION_PATH, "/dn_combined.csv"), row.names = FALSE)



# --------------------------------------------------------------------------------
# Step 4: Prepare Data for Linear Mixed Model
# --------------------------------------------------------------------------------
# Convert variables in dn_combined to factors
dn_combined <- dn_combined %>%
  mutate(nid_unique = as.factor(nid_unique),
         beta = as.factor(beta),
         p_anti = as.factor(p_anti),
         framing = as.factor(framing)
  )

levels(dn_combined$p_anti)
contrasts(dn_combined$p_anti)

# Set reference level for p_anti to "99"
dn_combined <- dn_combined %>%
  mutate(
    p_anti = relevel(
      factor(p_anti),
      ref = "99"
    )
  )
levels(dn_combined$p_anti)
# Check number of observations per neighborhood (should all be 21 for 21 rounds)
table(dn_combined$nid_unique)

# Reconstruct the n_zero and n_total (important for the binomial model)
dn_combined <- dn_combined %>%
  mutate(
    n_total = participants,
    n_zero = r0 * n_total
  )

# Check if any of the column are non-integer
all(dn_combined$n_zero %% 1 == 0) # should be TRUE
all(dn_combined$r0 %% 1 == 0) # should be FALSE

is.integer(dn_combined$n_zero)
# Convert n_zero and n_total to integer
dn_combined <- dn_combined %>%
  mutate(
    n_zero = as.integer(n_zero),
    n_total = as.integer(n_total)
  )
all(dn_combined$n_zero %% 1 == 0) # should be TRUE
is.integer(dn_combined$n_zero)


table(dn_combined$beta)
table(dn_combined$p_anti)
table(dn_combined$framing)

# Calculate standard deviation of r0 from all neighborhoods
sd(dn_combined$r0)

# Calculate standard deviation of mu from all neighborhoods
sd(dn_combined$mu)


# Prettify labels
dn_combined$beta <- factor(
  dn_combined$beta,
  levels = c(0, 0.5),
  labels = c("β = 0.0", "β = 0.5")
)
dn_combined$p_anti <- factor(
  dn_combined$p_anti,
  levels = c(99, 0, 0.5, 1),
  labels = c("Anti = Control", "Anti = 0.0", "Anti = 0.5", "Anti = 1.0")
)

dn_combined$framing <- factor(
  dn_combined$framing,
  levels = c("1", "2"),
  labels = c("Nonpolitical framing", "Political framing")
)

# Save the prettified dn_combined in a RDS file
# RDS keeps factor levels/order (reference level) and column types; CSV is only a readable copy
saveRDS(dn_combined, paste0(MODEL_PATH, "/dn_combined.rds"))
# Reload with: dn_combined <- readRDS(paste0(MODEL_PATH, "/dn_combined.rds"))

# ===============================================================================
# FITTING THE MODELS
# ===============================================================================

# ------------------------------------------------------------------------------
# Predicting r0, assuming binomial distribution (1=choosing 0, 0=otherwise)
# ------------------------------------------------------------------------------
# 1) Model with main effects only, without interactions

# 1a) Model without interactions (but + round_no as fixed effect, different trends)
m_binom_main_only <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    round_no + # if round_no as fixed effect
    beta + p_anti + framing + 
    (1 | nid_unique), # if round_no as random effect
    family = binomial,
    data = dn_combined
)
isSingular(m_binom_main_only) # FALSE


# 1b) Model with main effects only, without interactions, round_no spline df=2
m_binom_main_only_roundspline_2 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, df = 2) + # round_no as spline with df=2
    beta + p_anti + framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_main_only_roundspline_2) # FALSE


# 1c) Model with main effects only, without interactions, round_no spline df=3
m_binom_main_only_roundspline_3 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, df = 3) + # round_no as spline with df=3
    beta + p_anti + framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_main_only_roundspline_3) # FALSE
# Result: Converged

# 1d) Model with main effects only, without interactions, round_no spline knots at 5,10,15
m_binom_main_only_roundspline_spec_5_10_15 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, knots = c(5, 10, 15)) + # round_no as spline with specified knots
    beta + p_anti + framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_main_only_roundspline_spec_5_10_15) # FALSE


# 2) Model with interactions
# 2a) Model with interactions, without rounds as slope
m_binom_interactions_noroundslope <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    round_no +
    beta * p_anti * framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_interactions_noroundslope) # FALSE

# 2b) Model with interactions, without rounds as slope, round_no as spline with df = 2
m_binom_interactions_noroundslope_roundspline_2 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, df = 2) + # round_no as spline with df=2
    beta * p_anti * framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_interactions_noroundslope_roundspline_2) # FALSE

# 2c) Model with interactions, without rounds as slope, round_no as spline with df = 3
m_binom_interactions_noroundslope_roundspline_3 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, df = 3) + # round_no as spline with df=3
    beta * p_anti * framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_interactions_noroundslope_roundspline_3) # FALSE

# 2d) Model with interactions, without rounds as slope, round_no as spline with knots at 5, 10, 15
m_binom_interactions_noroundslope_roundspline_spec_5_10_15 <- glmer(
    cbind(n_zero, n_total - n_zero) ~ 
    ns(round_no, knots = c(5, 10, 15)) + # round_no as spline with specified knots
    beta * p_anti * framing + 
    (1 | nid_unique),
    family = binomial,
    data = dn_combined,
    control = glmerControl(
      optimizer='bobyqa',
      optCtrl = list(maxfun = 2e5))
)
isSingular(m_binom_interactions_noroundslope_roundspline_spec_5_10_15) # FALSE

# Save the models
# Create a named list for model comparison with descriptive labels
r0_binom_models_legend <- list(
  "Main only" = m_binom_main_only,
  "Main only, Round Spline df=2" = m_binom_main_only_roundspline_2,
  "Main only, Round Spline df=3" = m_binom_main_only_roundspline_3,
  "Main only, Round Spline Knot 5,10,15" = m_binom_main_only_roundspline_spec_5_10_15,
  "Interaction, No Round Slope" = m_binom_interactions_noroundslope,
  "Interaction, No Round Slope, Round Spline df=2" = m_binom_interactions_noroundslope_roundspline_2,
  "Interaction, No Round Slope, Round Spline df=3" = m_binom_interactions_noroundslope_roundspline_3,
  "Interaction, No Round Slope, Round Spline Knot 5,10,15" = m_binom_interactions_noroundslope_roundspline_spec_5_10_15)

# # or if you loaded the models from the RDS file, you can access them like this:
# r0_binom_models_load <- readRDS(paste0(MODEL_PATH, "/r0_binom_models.rds"))
# r0_binom_models_legend = r0_binom_models_load

# Save to the path
saveRDS(r0_binom_models_legend, paste0(MODEL_PATH, "/r0_binom_models.rds"))

# Test load
# r0_models_loaded <- readRDS(paste0(MODEL_PATH, "/r0_binom_models.rds"))
# r0_models_loaded$"Main only, Round Spline Knot 5,10,15"

# ------------------------------------------------------------------------------
# Output tables for the models
# ------------------------------------------------------------------------------
today <- Sys.Date()
# remove dashes
today <- gsub("-", "", today)
output_file <- paste0(REGRESSION_PATH, "models_binom_compare_", today, ".html")

modelsummary(
  r0_binom_models_legend,
  output = output_file,
  exponentiate = TRUE, # to get the odds ratios instead of log-odds
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  notes = "<i>Note. </i>Reference categories: β = 0 (no interaction), Anti = no nudge (control), Nonpolitical framing. <br>Row-wise: Estimate; SE (standard error); CI [confidence interval].<br>*p < .05, **p < .001.",
  escape = FALSE,
  coef_rename = function(x) {
    x <- gsub("betaβ = 0.5", "β = 0.5", x)
    x <- gsub("p_antiAnti = 0.0", "Anti = 0.0", x)
    x <- gsub("p_antiAnti = 0.5", "Anti = 0.5", x)
    x <- gsub("p_antiAnti = 1.0", "Anti = 1.0", x)
    x <- gsub("framingPolitical framing", "Political framing", x)
    x <- gsub("round_no", "Round", x)
    x <- gsub(":", " × ", x)
    return(x)
  }
  
) |>
  group_tt(
    j = list(
      "Binomial (Converged)" = 2:9
    )
  )

# ------------------------------------------------------------------------------
# Figures to compare model fit using Delta AIC and Delta BIC
# ------------------------------------------------------------------------------

cmp <- tibble(

  model = c("Main only", 
  "Main only, Round Spline df=2", 
  "Main only, Round Spline df=3", 
  "Main only, Round Spline Knot 5,10,15",
  "Interaction, No Round Slope",
  "Interaction, No Round Slope, Round Spline df=2",
  "Interaction, No Round Slope, Round Spline df=3", 
  "Interaction, No Round Slope, Round Spline Knot 5,10,15"),
  AIC = c(
    AIC(m_binom_main_only), 
    AIC(m_binom_main_only_roundspline_2), 
    AIC(m_binom_main_only_roundspline_3), 
    AIC(m_binom_main_only_roundspline_spec_5_10_15), 
    AIC(m_binom_interactions_noroundslope), 
    AIC(m_binom_interactions_noroundslope_roundspline_2),
    AIC(m_binom_interactions_noroundslope_roundspline_3),
    AIC(m_binom_interactions_noroundslope_roundspline_spec_5_10_15)),
  BIC = c(
    BIC(m_binom_main_only), 
    BIC(m_binom_main_only_roundspline_2), 
    BIC(m_binom_main_only_roundspline_3), 
    BIC(m_binom_main_only_roundspline_spec_5_10_15), 
    BIC(m_binom_interactions_noroundslope), 
    BIC(m_binom_interactions_noroundslope_roundspline_2),
    BIC(m_binom_interactions_noroundslope_roundspline_3),
    BIC(m_binom_interactions_noroundslope_roundspline_spec_5_10_15))
) %>%
  mutate(dAIC = AIC - min(AIC), dBIC = BIC - min(BIC)) %>%
  tidyr::pivot_longer(c(dAIC, dBIC), names_to = "criterion", values_to = "delta")

deltafig <- ggplot(cmp, aes(delta, reorder(model, -delta), colour = criterion)) +
  geom_point(size = 3) +
  labs(x = "Δ from best model (lower is better)", y = NULL) +
  scale_colour_discrete(labels = c(dAIC = "Delta AIC", dBIC = "Delta BIC")) +
  theme_bw(base_size=13, base_family = "Arial") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank())


###  Export figure
FIGURE_PATH = paste0(REGRESSION_PATH, "figures/")
deltafig
fig_file <- paste0(FIGURE_PATH, "Delta_AICBIC_r0-binom-models.png")
png(filename = fig_file, width = 2500, height = 1200, res = 300)
print(deltafig)
dev.off()
message("Saved figure to: ", normalizePath(fig_file))


# ----------------------------------------------------------------------------------
# Average Marginal Effects (AME) Plots
# ----------------------------------------------------------------------------------
library(marginaleffects)


# AME on the probability scale, population-level (random effects set to 0)
ame <- lapply(names(r0_binom_models_legend), function(nm) {
  # average marginal effects using 'avg_slopes'
  avg_slopes(
    r0_binom_models_legend[[nm]],
    variables = c("beta", "p_anti", "framing"),   # add "round_no" if you want it
    type   = "response",
    re.form = NA
  ) %>% mutate(model = nm)
}) %>% bind_rows()


# One label per variable and contrast
ame <- ame %>%
  mutate(
    term_label = case_when(
      term == "p_anti"  ~ gsub("Anti = Control", "Control", paste0(contrast)),
      TRUE              ~ contrast
    ),
    term_label = gsub(" - ", " vs. ", term_label),
    term_label = gsub("Political framing vs. Nonpolitical framing",
                      "Political vs. Nonpolitical framing", term_label),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )


avgmargeff_plot <- ggplot(ame, aes(x = estimate, y = term_label, colour = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6)
  ) +
  labs(
    title = "Average marginal effects on probability of middle ground option (r0)",
    x = "Change in Pr(r0)",
    y = NULL
  ) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8, 7),
                     labels = scales::label_wrap(25)) +
  # wrap long labels
  scale_colour_discrete(labels = scales::label_wrap(25)) +
  scale_y_discrete(labels = scales::label_wrap(20)) +
  theme_bw(base_size=13, base_family = "Arial") +
  theme(legend.position = "bottom",
        legend.location = "panel",         # align legend to the panel, not the y-label
        legend.justification = "center") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank())

avgmargeff_plot <- avgmargeff_plot +
  guides(
    colour = guide_legend(ncol = 3, byrow = TRUE),
    shape  = guide_legend(ncol = 3, byrow = TRUE)
  ) +
  theme(
    legend.title = element_blank(),
    legend.text  = element_text(size = 11),
    legend.location = "panel",         # align to the panel, not the full plot
    legend.justification = "left",     # legend starts at the panel's left edge
    legend.box.just = "left",
    legend.text.align = 0
  )

### Export average marginal effects plot
avgmargeff_plot
fig_file_ame <- paste0(FIGURE_PATH, "AME_r0-binom-models.png")
png(filename = fig_file_ame, width = 2500, height = 2500, res = 300)
print(avgmargeff_plot)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_ame))




# Plotting the predicted probabilities for the best model
best_model <- m_binom_interactions_noroundslope_roundspline_spec_5_10_15
# or 
# best_model <- r0_models_loaded$Interaction, No Round Slope, Round Spline Knot 5,10,15
pred <- avg_predictions(
  best_model,
  by      = c("beta", "p_anti", "framing"),
  type    = "response",
  re.form = NA
)

pred_plot <- ggplot(pred, aes(x = factor(p_anti), y = estimate,
                              colour = factor(beta), group = factor(beta))) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4)) +
  geom_text(aes(y = conf.high, label = sprintf("%.2f", estimate)),
            position = position_dodge(width = 0.4),
            vjust = -0.8, size = 2.8, show.legend = FALSE) +
  scale_x_discrete(labels = scales::label_wrap(8)) +
  facet_wrap(~ framing) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Anticonformity Tip", y = "Predicted Pr(r0)", colour = "Between-faction Interaction",
       title = "Predicted probability of middle ground option (r0) by conditions") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 

## Export predicted probabilities plot
pred_plot
fig_file_pred <- paste0(FIGURE_PATH, "Pred_r0-binom-model-best.png")
png(filename = fig_file_pred, width = 2000, height = 1200, res = 300)
print(pred_plot)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_pred))

# Average predictions across models
pred_all <- lapply(names(r0_binom_models_legend), function(nm) {
  avg_predictions(r0_binom_models_legend[[nm]], by = c("beta", "p_anti", "framing"),
                  type = "response", re.form = NA) %>%
    mutate(model = nm)
}) %>% bind_rows()

pred_all_plot <- ggplot(pred_all, aes(x = factor(p_anti), y = estimate,
                                      colour = factor(beta), group = factor(beta))) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4), size = 0.3) +
  geom_line(position = position_dodge(width = 0.4)) +
  facet_grid(framing ~ model, labeller = label_wrap_gen(width = 18)) +
  scale_x_discrete(labels = scales::label_wrap(8)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "p_anti", y = "Predicted Pr(r0)", colour = "beta",
       title = "Predicted probability of r0 by condition across models") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

## Export predicted probabilities across models plot
pred_all_plot
fig_file_pred_all <- paste0(FIGURE_PATH, "Pred_r0-binom-models-comparison.png")
png(filename = fig_file_pred_all, width = 6000, height = 1600, res = 300)
print(pred_all_plot)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_pred_all))



# ------------------------------------------------------------------------------------------------------------
# Export the table of the best model to html
# ------------------------------------------------------------------------------------------------------------
best_binom <- m_binom_interactions_noroundslope_roundspline_spec_5_10_15
# or if you load the model directly from the RDS file:
# best_binom <- readRDS(paste0(MODEL_PATH, "/r0_binom_models.rds"))$`Interaction, No Round Slope, Round Spline Knot 5,10,15`

output_file <- paste0(REGRESSION_PATH, "best_r0-binom_model.html")

modelsummary(
  best_binom,
  output = output_file,
  exponentiate = TRUE, # to get the odds ratios instead of log-odds
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  notes = "<i>Note. </i>Reference categories: β = 0 (no interaction), Anti = no nudge (control), Nonpolitical framing. <br>Row-wise: Estimate; SE (standard error); CI [confidence interval].<br>*p < .05, **p < .01, ***p < .001.",
  escape = FALSE,
  coef_rename = function(x) {
    x <- gsub("betaβ = 0.5", "β = 0.5", x)
    x <- gsub("p_antiAnti = 0.0", "Anti = 0.0", x)
    x <- gsub("p_antiAnti = 0.5", "Anti = 0.5", x)
    x <- gsub("p_antiAnti = 1.0", "Anti = 1.0", x)
    x <- gsub("framingPolitical framing", "Political framing", x)
    x <- gsub("round_no", "Round", x)
    x <- gsub(":", " × ", x)
    return(x)
  }
  
) 


# --------------------------------------------------------------------------------------------
# Best r0 binom model AME
# --------------------------------------------------------------------------------------------
# H2: anti decreases polarization index and increases middle ground (r0)

# AME on the probability scale, population-level (random effects set to 0)

# (a) Pooled AME of each Anti level vs. control (averaged over beta, framing, round)
ame_anti_pooled <- avg_comparisons(
  best_binom,
  newdata = dn_combined,
  variables = list(p_anti = "reference"),
  type = "response", re.form = NA
)

# (b) AME of each Anti level vs. control within each beta x framing condition
ame_anti_cond <- avg_comparisons(
  best_binom,
  newdata = dn_combined,
  variables = list(p_anti = "reference"),
  by = c("beta", "framing"),
  type = "response", re.form = NA
)


# # (c) Pairwise contrasts among all Anti levels (e.g., 0.5 vs. 0.0, 1.0 vs. 0.5)
# ame_anti_pairwise <- avg_comparisons(
#   best_binom,
#   variables = list(p_anti = "pairwise"),
#   type = "response", re.form = NA
# )


# Tidy, rounded output for reporting (probability-scale differences, in percentage points)
fmt_ame <- function(x) {
  as.data.frame(x) %>%
    transmute(across(any_of(c("beta", "framing")), identity),
              contrast,
              AME_pp = round(100 * estimate, 2),
              SE_pp  = round(100 * std.error, 2),
              CI_low_pp  = round(100 * conf.low, 2),
              CI_high_pp = round(100 * conf.high, 2),
              z = round(statistic, 2),
              p = signif(p.value, 3))
}
fmt_ame(ame_anti_pooled)
fmt_ame(ame_anti_cond)
# fmt_ame(ame_anti_pairwise)


# Three-way interaction on the probability scale (delta method on the conditional AMEs). For each Anti level:
# the framing gap of the Anti effect (Political - Nonpolitical) at beta = 0 and at beta = 0.5, and their
# difference ((Pol - Nonpol | beta = 0.5) - (Pol - Nonpol | beta = 0)) = the three-way interaction
ame_cond_df <- as.data.frame(ame_anti_cond)
V_ame_cond <- vcov(ame_anti_cond)
ame_3way <- lapply(c("Anti = 0.5", "Anti = 1.0"), function(a) {
  idx <- function(b, f) {
    i <- which(grepl(a, ame_cond_df$contrast, fixed = TRUE) & ame_cond_df$beta == b & ame_cond_df$framing == f)
    stopifnot(length(i) == 1)
    i
  }
  k_gap <- function(b) {
    k <- rep(0, nrow(ame_cond_df))
    k[idx(b, "Political framing")] <- 1
    k[idx(b, "Nonpolitical framing")] <- -1
    k
  }
  k0 <- k_gap("β = 0.0")
  k5 <- k_gap("β = 0.5")
  ks <- list("Framing gap at $\\beta = 0$" = k0,
             "Framing gap at $\\beta = 0.5$" = k5,
             "Three-way interaction (gap at $\\beta = 0.5$ $-$ gap at $\\beta = 0$)" = k5 - k0)
  bind_rows(lapply(names(ks), function(nm) {
    est <- sum(ks[[nm]] * ame_cond_df$estimate)
    se  <- sqrt(drop(t(ks[[nm]]) %*% V_ame_cond %*% ks[[nm]]))
    tibble(grp = paste(a, "vs. Control"), contrast = nm,
           estimate = est, std.error = se,
           conf.low = est - qnorm(0.975) * se, conf.high = est + qnorm(0.975) * se,
           statistic = est / se, p.value = 2 * pnorm(-abs(est / se)))
  }))
}) %>% bind_rows()
# in percentage points
ame_3way %>% transmute(grp, contrast, pp = round(100 * estimate, 2),
                       CI_low_pp = round(100 * conf.low, 2), CI_high_pp = round(100 * conf.high, 2),
                       p = signif(p.value, 3))


# Export AME tables to LaTeX (same style as best_gauss.tex); values are taken directly from the model objects
tex_p <- function(p) {
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  txt <- ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
  ifelse(stars == "", txt, paste0("$", txt, "^{\\text{", stars, "}}$"))
}
tex_label <- function(x) {
  x <- gsub(" - ", " vs. ", x)
  x <- gsub("Anti = Control", "Control", x)
  gsub("β", "$\\beta$", x, fixed = TRUE)
}
ame_rows <- function(d) {
  sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
          tex_label(d$contrast), 100 * d$estimate, 100 * d$std.error,
          100 * d$conf.low, 100 * d$conf.high, tex_p(d$p.value))
}
write_ame_tex <- function(ame, file, caption, label, by_cond = FALSE, note_extra = NULL) {
  d <- as.data.frame(ame)
  if (by_cond) d$grp <- paste(d$beta, d$framing, sep = ", ")
  if (by_cond || "grp" %in% names(d)) {
    body <- unlist(lapply(unique(d$grp), function(g) {
      c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", tex_label(g)),
        ame_rows(d[d$grp == g, ]))
    }))
  } else body <- ame_rows(d)
  note <- paste0("\\textit{Note.} Average marginal effects (AME) on the probability scale, expressed in percentage points ",
                 "(population level, random effects set to zero). SE = standard error; CI = confidence interval. ",
                 note_extra,
                 "*$p < .05$, **$p < .01$, ***$p < .001$.")
  tex <- c(
    "\\begin{table}[H]", "\\centering",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    "\\begingroup", "\\footnotesize", "\\renewcommand{\\arraystretch}{0.90}", "",
    "\\begin{tabularx}{\\linewidth}{Xcccc}", "\\toprule",
    "\\textbf{Contrast} & \\textbf{AME (pp)} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    "\\midrule", body, "\\bottomrule", "\\end{tabularx}", "",
    "\\vspace{0.6em}", "{\\footnotesize", "\\setlength{\\baselineskip}{12pt}", note, "\\par}",
    "\\endgroup", "\\end{table}")
  writeLines(tex, file, useBytes = TRUE)
}


# Write the table for the pooled AME
write_ame_tex(ame_anti_pooled, 
    paste0(REGRESSION_PATH, "best_r0_ame_anti_pooled.tex"),
    "Average Marginal Effects of Anticonformity vs. Control on the Probability of Choosing the Middle Ground (Pooled Over $\\beta$, Framing, and Round)",
    "tab:ame_anti_pooled")

# Write the table for the conditional AME
write_ame_tex(ame_anti_cond, 
    paste0(REGRESSION_PATH, "best_r0_ame_anti_cond.tex"),
    "Average Marginal Effects of Anticonformity vs. Control on the Probability of Choosing the Middle Ground (Conditional on $\\beta$ and Framing)",
    "tab:ame_anti_cond",
    by_cond = TRUE)

# Write the table for the three-way interaction AME (difference in the framing gap of the Anti effect between beta = 0.5 and beta = 0)
write_ame_tex(ame_3way, 
    paste0(REGRESSION_PATH, "best_r0_ame_anti_3way.tex"),
    "Framing Gap in the Anticonformity Effect on the Probability of Choosing the Middle Ground, and the Three-Way Interaction (Anticonformity $\\times$ $\\beta$ $\\times$ Framing)",
    "tab:ame_anti_3way",
    note_extra = paste0("The framing gap is the difference in the AME of Anti vs. control between the political and the nonpolitical framing ",
                        "(Political $-$ Nonpolitical) at a given $\\beta$; the three-way interaction is the difference between the gaps at $\\beta = 0.5$ and $\\beta = 0$. ",
                        "Estimates are obtained from the conditional AMEs with the delta method. "))

# # Write the table for the pairwise AME
# write_ame_tex(ame_anti_pairwise, 
#     paste0(REGRESSION_PATH, "best_r0_ame_anti_pairwise.tex"),
#     "Average Marginal Effects of Anticonformity vs. Control on the Probability of Choosing the Middle Ground (Pairwise Comparisons)",
#     "tab:ame_anti_pairwise",
#     by_cond = TRUE)


# Hypothesis test: the effect of anticonformity (0.5 and 1.0) at beta = 0.5 is
# smaller in the political than in the nonpolitical framing (binomial model, Pr(r0))

# Slice only rows at beta = 0.5; the effect is averaged over the observed round_no / framing distribution within each framing (population level, random effects = 0)
dat_beta05 <- dn_combined %>% filter(beta == "β = 0.5")


# 1) Conditional effects of each anti level vs. control within each framing
cond_all <- avg_comparisons(
  best_model,
  variables = list(p_anti = "reference"),
  by        = "framing",
  newdata   = dat_beta05,
  type      = "response",
  re.form   = NA
)

cond_tab <- as.data.frame(cond_all) %>%
  mutate(
    anti_level = trimws(sub(" - .*$", "", contrast)),
    row_id     = row_number()
  )
cond_tab[, c("framing", "contrast", "estimate", "std.error")]  # check row order/labels

anti_targets <- c("Anti = 0.5", "Anti = 1.0")
fr_non <- "Nonpolitical framing"
fr_pol    <- "Political framing"

# 2) Difference between framings (political - nonpolitical) via the delta method,
# using the joint covariance matrix of the conditional effects
V_cond <- vcov(cond_all)

diff_tab <- lapply(anti_targets, function(a) {
  i_pol <- cond_tab$row_id[cond_tab$anti_level == a & cond_tab$framing == fr_pol]
  i_non <- cond_tab$row_id[cond_tab$anti_level == a & cond_tab$framing == fr_non]
  stopifnot(length(i_pol) == 1, length(i_non) == 1)
  
  k <- rep(0, nrow(cond_tab)); k[i_pol] <- 1; k[i_non] <- -1
  est <- sum(k * cond_tab$estimate)
  se  <- sqrt(drop(t(k) %*% V_cond %*% k))
  
  tibble(
    anti_level = a,
    estimate   = est,
    std.error  = se,
    conf.low   = est - qnorm(0.975) * se,  # two-sided 95% CI (for plotting)
    conf.high  = est + qnorm(0.975) * se,
    conf.low_90  = est - qnorm(0.95) * se,  # 90% two-sided CI = matches the one-sided test at alpha = .05
    conf.high_90 = est + qnorm(0.95) * se,  # (its upper bound is the one-sided 95% upper bound)
    upper_95_onesided = est + qnorm(0.95) * se, # < 0 <=> significant at one-sided .05
    z          = est / se,
    p_two_sided = 2 * pnorm(-abs(z)),
    p_one_sided = pnorm(z)                  # H1: difference < 0
  )
}) %>% bind_rows() %>%
  mutate(p_one_sided_holm = p.adjust(p_one_sided, method = "holm"))

diff_tab
# Note: the hypothesis is supported for an anti level if p_one_sided < .05
# (equivalently, upper_95_onesided < 0) and estimate < 0.


# 3) Data for the plot: conditional effects by framing + framing difference
plot_cond <- cond_tab %>%
  filter(anti_level %in% anti_targets) %>%
  transmute(
    panel     = "Effect vs. control, by framing",
    row_label = paste0(anti_level, " vs. Control"),
    group     = framing,
    estimate, conf.low, conf.high
  )

plot_diff <- diff_tab %>%
  transmute(
    panel     = "Framing difference (Political \u2212 Nonpolitical)",
    row_label = paste0(anti_level, " vs. Control"),
    group     = "Difference",
    estimate, conf.low, conf.high, conf.low_90, conf.high_90
  )

plot_contrast <- bind_rows(plot_cond, plot_diff) %>%
  mutate(
    panel     = factor(panel, levels = c("Effect vs. control, by framing",
                                         "Framing difference (Political \u2212 Nonpolitical)")),
    row_label = factor(row_label, levels = rev(paste0(anti_targets, " vs. Control"))),
    group     = factor(group, levels = c(fr_non, fr_pol, "Difference"))
  )

# Both p-values as annotation next to the difference rows
plot_diff_lab <- diff_tab %>%
  transmute(
    panel     = factor("Framing difference (Political \u2212 Nonpolitical)",
                       levels = levels(plot_contrast$panel)),
    row_label = factor(paste0(anti_level, " vs. Control"), levels = levels(plot_contrast$row_label)),
    x         = conf.high,
    label     = paste0("p (one-sided, H1: diff < 0) ",
                       ifelse(p_one_sided < .001, "< .001", sprintf("= %.3f", p_one_sided)),
                       "\np (two-sided) ",
                       ifelse(p_two_sided < .001, "< .001", sprintf("= %.3f", p_two_sided)))
  )

contrast_plot <- ggplot(plot_contrast,
                        aes(x = estimate, y = row_label, colour = group, shape = group)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.5)) +
  # Thick bar on the difference rows = 90% CI, which corresponds to the one-sided test at alpha = .05
  geom_linerange(aes(xmin = conf.low_90, xmax = conf.high_90),
                 position = position_dodge(width = 0.5), linewidth = 1.6, na.rm = TRUE) +
  geom_text(data = plot_diff_lab, aes(x = x, y = row_label, label = label),
            inherit.aes = FALSE, hjust = -0.1, vjust = 0.5, size = 3, lineheight = 0.9) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("Nonpolitical framing" = "#1b9e77",
                                 "Political framing"    = "#d95f02",
                                 "Difference"           = "black")) +
  scale_shape_manual(values = c("Nonpolitical framing" = 16,
                                "Political framing"    = 17,
                                "Difference"           = 15)) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.6))) +
  labs(
    title   = "Anticonformity effect on Pr(r0) at \u03b2 = 0.5, by framing",
    x       = "Change in Pr(r0) relative to control",
    y       = NULL,
    caption = paste0("Thin lines: 95% two-sided CI. Thick bar (difference rows): 90% two-sided CI, ",
                     "equivalent to the one-sided test at \u03b1 = .05.\n",
                     "One-sided p tests H1: Political \u2212 Nonpolitical < 0.")
  ) +
  theme_bw(base_size = 13, base_family = "Arial") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        strip.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## Export contrast plot
contrast_plot
fig_file_contrast <- paste0(FIGURE_PATH, "Contrast_framing-difference_beta05.png")
png(filename = fig_file_contrast, width = 2400, height = 1600, res = 300)
print(contrast_plot)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_contrast))





# ------------------------------------------------------------------------------
# Predicting mu (polarization index), assuming Gaussian distribution
# ------------------------------------------------------------------------------
# Model without interactions
# 1a) Main effects only
mu_gaussian_main_only <- lmer(
    mu ~ 
    round_no + 
    beta + p_anti + framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE # REML = FALSE for model comparison using likelihood ratio tests
)
isSingular(mu_gaussian_main_only) # FALSE
# Note: converged and not a singular fit

# 1b) Model with round_no as a spline (df = 2) without interactions
mu_gaussian_main_roundspline_2 <- lmer(
    mu ~ 
    ns(round_no, df = 2) +
    beta + p_anti + framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE
)
isSingular(mu_gaussian_main_roundspline_2) # FALSE
# Note: Converged and not a singular fit

# 1c) Model with round_no as a spline (df = 3) without interactions
mu_gaussian_main_roundspline_3 <- lmer(
    mu ~ 
    ns(round_no, df = 3) +
    beta + p_anti + framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE
)
isSingular(mu_gaussian_main_roundspline_3) # FALSE
# Note: Converged and not a singular fit

# 1d) Model with round_no as a spline (knots at 5, 10, 15) without interactions
mu_gaussian_main_roundspline_spec_5_10_15 <- lmer(
    mu ~ 
    ns(round_no, knots = c(5, 10, 15)) +
    beta + p_anti + framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE
)
isSingular(mu_gaussian_main_roundspline_spec_5_10_15) # FALSE
# Note: Converged and not a singular fit

# 2) Model with interactions without round_no as a random slope
mu_gaussian_interactions_noroundslope <- lmer(
    mu ~ 
    round_no +
    beta * p_anti * framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE # REML = FALSE for model comparison using likelihood ratio tests
)
isSingular(m_gaussian_interactions_noroundslope) # FALSE
# Note: Converged, not a singular fit

# 2b) Adding round_no as spline without round_no as a random slope
mu_gaussian_interactions_noroundslope_roundspline_2 <- lmer(
    mu ~ 
    ns(round_no, df = 2) +
    beta * p_anti * framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE # REML = FALSE for model comparison using likelihood ratio tests
)
isSingular(mu_gaussian_interactions_noroundslope_roundspline_2) # FALSE
# Note: Converged, not a singular fit

# 2c) changing the df to 3
mu_gaussian_interactions_noroundslope_roundspline_3 <- lmer(
    mu ~ 
    ns(round_no, df = 3) +
    beta * p_anti * framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE # REML = FALSE for model comparison using likelihood ratio tests
)
isSingular(mu_gaussian_interactions_noroundslope_roundspline_3) # FALSE
# Note: Converged, not a singular fit

# 2d) specifying the knot locations to be at round_no = 5, 10, 15
mu_gaussian_interactions_noroundslope_roundspline_spec_5_10_15 <- lmer(
    mu ~ 
    ns(round_no, knots = c(5, 10, 15)) +
    beta * p_anti * framing + 
    (1 | nid_unique),
    data = dn_combined,
    REML = FALSE # REML = FALSE for model comparison using likelihood ratio tests
)
isSingular(mu_gaussian_interactions_noroundslope_roundspline_spec_5_10_15) # FALSE
# Note: Converged, not a singular fit


# Save the models
mu_gaussian_models_legend <- list(
    "Main only" = mu_gaussian_main_only, 
    "Main only, Round Spline df=2" = mu_gaussian_main_roundspline_2, 
    "Main only, Round Spline df=3" = mu_gaussian_main_roundspline_3, 
    "Main only, Round Spline Knot 5,10,15" = mu_gaussian_main_roundspline_spec_5_10_15,
  "Interaction, No Round Slope" = mu_gaussian_interactions_noroundslope,
  "Interaction, No Round Slope, Round Spline df=2" = mu_gaussian_interactions_noroundslope_roundspline_2,
  "Interaction, No Round Slope, Round Spline df=3" = mu_gaussian_interactions_noroundslope_roundspline_3,
  "Interaction, No Round Slope, Round Spline Knot 5,10,15" = mu_gaussian_interactions_noroundslope_roundspline_spec_5_10_15
)
# # or if you loaded the models from the RDS file, you can access them like this:
# mu_gaussian_models_load <- readRDS(paste0(MODEL_PATH, "/mu_gaussian_models.rds"))
# mu_gaussian_models_legend = mu_gaussian_models_load

# Save to the path
saveRDS(mu_gaussian_models_legend, paste0(MODEL_PATH, "/mu_gaussian_models.rds"))

# Test load
# mu_gaussian_models_load <- readRDS(paste0(MODEL_PATH, "/mu_gaussian_models.rds"))
# mu_gaussian_models_load$"Main only, Round Spline Knot 5,10,15"
# mu_gaussian_models_load[2]
# names(mu_gaussian_models_load)


# ------------------------------------------------------------------------------
# Output tables for the models
# ------------------------------------------------------------------------------
today <- Sys.Date()
# remove dashes
today <- gsub("-", "", today)
output_file <- paste0(REGRESSION_PATH, "models_gaussian_compare_", today, ".html")

# Model comparison for the Gaussian (converged and not singular fit)
modelsummary(
  mu_gaussian_models_legend,
  output = output_file,
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  notes = "<i>Note. </i>Reference categories: β = 0 (no interaction), Anti = no nudge (control), Nonpolitical framing. <br>Row-wise: Estimate; SE (standard error); CI [confidence interval].<br>*p < .05, **p < .001.",
  escape = FALSE,
  coef_rename = function(x) {
    x <- gsub("betaβ = 0.5", "β = 0.5", x)
    x <- gsub("p_antiAnti = 0.0", "Anti = 0.0", x)
    x <- gsub("p_antiAnti = 0.5", "Anti = 0.5", x)
    x <- gsub("p_antiAnti = 1.0", "Anti = 1.0", x)
    x <- gsub("framingPolitical framing", "Political framing", x)
    x <- gsub("round_no", "Round", x)
    x <- gsub(":", " × ", x)
    return(x)
  }
) |>
  group_tt(
    j = list(
      "Polarization index | Gaussian (converged)" = 2:4,
      "Polarization index | Gaussian with spline (converged)" = 5:9
    )
  )

# ------------------------------------------------------------------------------
# Figures to compare model fit using Delta AIC and Delta BIC
# ------------------------------------------------------------------------------
cmp_mu <- tibble(
  model = names(mu_gaussian_models_legend),
  AIC = sapply(mu_gaussian_models_legend, AIC),
  BIC = sapply(mu_gaussian_models_legend, BIC)
) %>%
  mutate(dAIC = AIC - min(AIC), dBIC = BIC - min(BIC)) %>%
  tidyr::pivot_longer(c(dAIC, dBIC), names_to = "criterion", values_to = "delta")

deltafig_mu <- ggplot(cmp_mu, aes(delta, reorder(model, -delta), colour = criterion)) +
  geom_point(size = 3) +
  labs(x = "Δ from best model (lower is better)", y = NULL) +
  scale_colour_discrete(labels = c(dAIC = "Delta AIC", dBIC = "Delta BIC")) +
  theme_bw(base_size=13, base_family = "Arial") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


###  Export figure
deltafig_mu
fig_file <- paste0(FIGURE_PATH, "Delta_AICBIC_mu-gaussian-models.png")
png(filename = fig_file, width = 2500, height = 1200, res = 300)
print(deltafig_mu)
dev.off()
message("Saved figure to: ", normalizePath(fig_file))


# Plot the marginal effects
library(marginaleffects)

# AME on the probability scale, population-level (random effects set to 0)
ame_mu <- lapply(names(mu_gaussian_models_legend), function(nm) {
  # average marginal effects using 'avg_slopes'
  avg_slopes(
    mu_gaussian_models_legend[[nm]],
    variables = c("beta", "p_anti", "framing"),   # add "round_no" if you want it
    type   = "response",
    re.form = NA
  ) %>% mutate(model = nm)
}) %>% bind_rows()

# One label per variable and contrast
ame_mu <- ame_mu %>%
  mutate(
    term_label = case_when(
      term == "p_anti"  ~ gsub("Anti = Control", "Control", paste0(contrast)),
      TRUE              ~ contrast
    ),
    term_label = gsub(" - ", " vs. ", term_label),
    term_label = gsub("Political framing vs. Nonpolitical framing",
                      "Political vs. Nonpolitical framing", term_label),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

# ame_mu <- ame_mu %>%
#   mutate(term_label = paste0(term, ": ", contrast))

avgmargeff_plot_mu <- ggplot(ame_mu, aes(x = estimate, y = term_label, colour = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6)
  ) +
  labs(
    title = "Average marginal effects on Polarization Index (μ)",
    x = "Change in Polarization Index (μ)",
    y = NULL
  ) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8, 7),
                     labels = scales::label_wrap(25)) +
  # wrap long labels
  scale_colour_discrete(labels = scales::label_wrap(25)) +
  scale_y_discrete(labels = scales::label_wrap(20)) +
  theme_bw(base_size=13, base_family = "Arial") +
  theme(legend.position = "bottom",
        legend.location = "panel",         # align legend to the panel, not the y-label
        legend.justification = "center") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank())

avgmargeff_plot_mu <- avgmargeff_plot_mu +
  guides(
    colour = guide_legend(ncol = 3, byrow = TRUE),
    shape  = guide_legend(ncol = 3, byrow = TRUE)
  ) +
  theme(
    legend.title = element_blank(),
    legend.text  = element_text(size = 11),
    legend.location = "panel",         # align to the panel, not the full plot
    legend.justification = "left",     # legend starts at the panel's left edge
    legend.box.just = "left",
    legend.text.align = 0
  )

### Export average marginal effects plot
avgmargeff_plot_mu
fig_file_ame <- paste0(FIGURE_PATH, "AME_gauss-models.png")
png(filename = fig_file_ame, width = 2500, height = 2500, res = 300)
print(avgmargeff_plot_mu)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_ame))


# ------------------------------------------------------------------------------------------------------------
# Export the table of the best model to html
# ------------------------------------------------------------------------------------------------------------
best_gauss <- mu_gaussian_interactions_noroundslope_roundspline_spec_5_10_15
# or if you load the model directly from the RDS file:
# best_gauss <- readRDS(paste0(MODEL_PATH, "/mu_gaussian_models.rds"))$Interaction, No Round Slope, Round Spline Knot 5,10,15

output_file <- paste0(REGRESSION_PATH, "best_mu-gauss_model.html")

modelsummary(
  best_gauss,
  output = output_file,
  exponentiate = TRUE, # to get the odds ratios instead of log-odds
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  notes = "<i>Note. </i>Reference categories: β = 0 (no interaction), Anti = no nudge (control), Nonpolitical framing. <br>Row-wise: Estimate; SE (standard error); CI [confidence interval].<br>*p < .05, **p < .01, ***p < .001.",
  escape = FALSE,
  coef_rename = function(x) {
    x <- gsub("betaβ = 0.5", "β = 0.5", x)
    x <- gsub("p_antiAnti = 0.0", "Anti = 0.0", x)
    x <- gsub("p_antiAnti = 0.5", "Anti = 0.5", x)
    x <- gsub("p_antiAnti = 1.0", "Anti = 1.0", x)
    x <- gsub("framingPolitical framing", "Political framing", x)
    x <- gsub("round_no", "Round", x)
    x <- gsub(":", " × ", x)
    return(x)
  }
  
) 

# ------------------------------------------------------------------------------------------------------------
# Create the latex table directly from a fitted lmer models
# ------------------------------------------------------------------------------------------------------------
model_to_latex_table <- function(
    model,
    tex_file = NULL,
    caption = "Linear Mixed-Effects Model Predicting Polarization Index (Gaussian; Round Modeled With a Natural Spline, Knots at 5, 10, 15)",
    label = "tab:",
    note = paste0(
      "\\textit{Note.} Reference categories: $\\beta = 0$ (no interaction), Anti = no nudge (control), ",
      "Nonpolitical framing. SE = standard error; CI = confidence interval. ",
      "*$p < .05$, **$p < .01$, ***$p < .001$."
    ),
    digits = 3
) {
  tex_escape <- function(x) gsub("([&%#_$])", "\\\\\\1", x)

  # Same renaming as coef_rename in the modelsummary call above
  clean_term <- function(x) {
    x <- gsub("betaβ = 0.5", "β = 0.5", x)
    x <- gsub("p_antiAnti = ", "Anti = ", x)
    x <- gsub("framingPolitical framing", "Political framing", x)
    x <- gsub("round_no", "Round", x)
    x
  }

  term_to_tex <- function(term) {
    if (term == "(Intercept)") return("Intercept")
    parts <- strsplit(clean_term(term), ":", fixed = TRUE)[[1]]
    is_beta <- startsWith(parts, "β")
    parts_tex <- ifelse(
      is_beta,
      paste0("$\\beta", sub("^β", "", parts), "$"),
      vapply(parts, tex_escape, character(1))
    )
    out <- parts_tex[1]
    for (i in seq_along(parts)[-1]) {
      if (i == 2 && is_beta[1]) {
        # beta as first factor: the times sign goes inside the math block
        out <- paste(sub("\\$$", " \\\\times$", out), parts_tex[i])
      } else {
        out <- paste(out, "$\\times$", parts_tex[i])
      }
    }
    out
  }

  fmt_num <- function(x, d = digits) sprintf(paste0("%.", d, "f"), x)

  fmt_p <- function(p) {
    if (p < .001) return("$< .001^{\\text{***}}$")
    p_txt <- sub("^0", "", fmt_num(p))
    stars <- if (p < .01) "**" else if (p < .05) "*" else ""
    if (nzchar(stars)) sprintf("$%s^{\\text{%s}}$", p_txt, stars) else p_txt
  }

  # Fixed effects: Wald z-based SE, 95% CI and p-values
  coefs <- summary(model)$coefficients
  est <- coefs[, "Estimate"]
  se <- coefs[, "Std. Error"]
  zcrit <- qnorm(0.975)
  p <- 2 * pnorm(-abs(est / se))
  fe_lines <- vapply(seq_along(est), function(i) {
    sprintf(
      "%s & %s & %s & [%s, %s] & %s \\\\",
      term_to_tex(rownames(coefs)[i]), fmt_num(est[i]), fmt_num(se[i]),
      fmt_num(est[i] - zcrit * se[i]), fmt_num(est[i] + zcrit * se[i]), fmt_p(p[i])
    )
  }, character(1))

  # Random effects (variance and SD); correlation rows are skipped
  vc <- as.data.frame(lme4::VarCorr(model))
  vc <- vc[is.na(vc$var2), ]
  re_lines <- vapply(seq_len(nrow(vc)), function(i) {
    name <- if (vc$grp[i] == "Residual") {
      "Residual"
    } else {
      sprintf("%s (%s)", sub("^\\(Intercept\\)$", "Intercept", vc$var1[i]), tex_escape(vc$grp[i]))
    }
    sprintf("%s & %s & %s & & \\\\", name, fmt_num(vc$vcov[i]), fmt_num(vc$sdcor[i]))
  }, character(1))

  # Model fit; Nakagawa R2 and adjusted ICC computed from the variance components
  var_f <- stats::var(as.vector(lme4::getME(model, "X") %*% lme4::fixef(model)))
  var_r <- sum(vc$vcov[vc$grp != "Residual"])
  var_e <- stats::sigma(model)^2
  r2_marg <- var_f / (var_f + var_r + var_e)
  r2_cond <- (var_f + var_r) / (var_f + var_r + var_e)
  icc <- var_r / (var_r + var_e)
  rmse <- sqrt(mean(stats::residuals(model)^2))
  # Grouping factor (nid_unique) = neighborhoods; observations per group = rounds
  n_groups <- lme4::ngrps(model)[1]
  n_rounds <- stats::nobs(model) / n_groups
  n_rounds <- if (n_rounds == round(n_rounds)) sprintf("%d", as.integer(n_rounds)) else sprintf("%.1f", n_rounds)
  n_obs <- format(stats::nobs(model), big.mark = ",", trim = TRUE)

  fit <- list(
    c("Observations", sprintf("%s (%s neighborhoods $\\times$ %s rounds)", n_obs, n_groups, n_rounds)),
    c("Marginal $R^2$", fmt_num(r2_marg)),
    c("Conditional $R^2$", fmt_num(r2_cond)),
    c("AIC", sprintf("%.1f", stats::AIC(model))),
    c("BIC", sprintf("%.1f", stats::BIC(model))),
    c("ICC", fmt_num(icc)),
    c("RMSE", fmt_num(rmse, 2))
  )
  fit_lines <- vapply(fit, function(x) {
    sprintf("%s & \\multicolumn{4}{l}{%s} \\\\", x[1], x[2])
  }, character(1))

  tex <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\begingroup",
    "\\footnotesize",
    "\\renewcommand{\\arraystretch}{0.90} % default is 1.0",
    "",
    "\\begin{tabularx}{\\linewidth}{Xcccc}",
    "\\toprule",
    "\\textbf{Fixed Effects} & \\textbf{Estimate} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    "\\midrule",
    fe_lines,
    "",
    "\\midrule",
    "\\textbf{Random Effects} & \\textbf{Variance} & \\textbf{SD} & & \\\\",
    "\\midrule",
    re_lines,
    "",
    "\\midrule",
    "\\textbf{Model Fit} & & & & \\\\",
    "\\midrule",
    fit_lines,
    "",
    "\\bottomrule",
    "\\end{tabularx}",
    "",
    "\\vspace{0.6em}",
    "{\\footnotesize",
    "\\setlength{\\baselineskip}{12pt}",
    note,
    "\\par}",
    "\\endgroup",
    "\\end{table}"
  )

  if (!is.null(tex_file)) {
    con <- file(tex_file, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(tex, con)
    message("Saved LaTeX table to: ", normalizePath(tex_file))
  }
  invisible(tex)
}

# Example
model_to_latex_table(
    model = best_gauss, 
    caption = "Linear Mixed-Effects Model Predicting Polarization Index (Gaussian; Round Modeled With a Natural Spline, Knots at 5, 10, 15)", 
    label = "tab:best_gauss", 
    note = "\\textit{Note.} We assume a gaussian distribution of polarization index as the outcome. Time (Round) is modelled with a natural cubic spline with interior knots at rounds 5, 10 and 15 (four basis coefficients, not individually interpretable). Reference categories: $\\beta = 0$ (no interaction), Anti = no tip (control), Nonpolitical framing. SE = standard error; CI = confidence interval. *$p < .05$, **$p < .01$, ***$p < .001$. \\par",
    tex_file = paste0(REGRESSION_PATH, "best_mu-gauss_model.tex")
)


model_to_latex_table(
    model = best_binom,
    caption = "Linear Mixed-Effects Model Predicting Proportion of Middle Ground Option ($r_0$) (Binomial; Round Modeled With a Natural Spline, Knots at 5, 10, 15)",
    label = "tab:best_binom",
    note = "\\textit{Note.} We assume a binomial outcome. Odds ratios are reported. Time (Round) is modelled with a natural cubic spline with interior knots at rounds 5, 10 and 15 (four basis coefficients, not individually interpretable). Reference categories: $\\beta = 0$ (no interaction), Anti = no tip (control), Nonpolitical framing. SE = standard error; CI = confidence interval. *$p < .05$, **$p < .01$, ***$p < .001$. \\par}",
    tex_file = paste0(REGRESSION_PATH, "best_r0-binom_model.tex")
)

