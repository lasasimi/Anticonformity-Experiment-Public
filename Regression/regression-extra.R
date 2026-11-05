
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
FIGURE_PATH <- paste0(REGRESSION_PATH, "figures/")
# Load the individual data
df <- read.csv(paste0(PATH, "Clean_files/07_combined_classified.csv"))

# Check colnames
colnames(df)

# Create a unique nid for each framing
df <- df %>%
  mutate(
    nid_unique = paste0(nid, "_", ifelse(framing == "1", "nonpol", "pol"))
  )
colnames(df)

# Prepare the data, 
# Slice the df to only include the conditions with p_anti == 1
df_sliced <- df %>%
  filter(p_anti == 1)

# check how many unique participants
n_distinct(df_sliced$participant_code) # 376 



# ------------------------------------------------------------------------------------------------------------
# Descriptives
# ------------------------------------------------------------------------------------------------------------
# 1) Percentage of anticonforming responses per individual
# Create a perc_antic variable, response_is_majority
# perc_antic: proportion of antic_actual responses per individual across the 19 rounds (round 2-20)
# response_is_majority: whether the individual's response aligns with the prev_majority response in that round
table(df_sliced$antic_actual)

df_sliced <- df_sliced %>%
  mutate(
    antic_actual = suppressWarnings(as.numeric(antic_actual)),
    prev_majority = as.numeric(prev_majority),
    # compare the raw response (-1/0/1) with the previous majority (-1/1; NA/99 = tie)
    response_is_majority = ifelse(is.na(prev_majority) | prev_majority == 99,
                                  NA, response == prev_majority)) 
table(df_sliced$antic_actual)
# Removed the '' in antic_actual and 'None' (because they don't contribute to the percentage)

# Now calculate the percentage, but the denominator should be the number of rounds with usable antic_actual data (each participant might differ)

# Create column n_antic_total_round
df_sliced_antic <- df_sliced %>%
  group_by(participant_code) %>%
  mutate(
    n_antic_total_round = sum(!is.na(antic_actual)),        # rounds with usable data
    n_antic_round = sum(antic_actual == 1, na.rm = TRUE),   # rounds where antic_actual == 1
    perc_antic = n_antic_round / n_antic_total_round
  ) %>%
  ungroup()
table(df_sliced_antic$n_antic_total_round)
table(df_sliced_antic$perc_antic)
table(df_sliced_antic$n_antic_round)


# One row per participant (used for the histogram)
df_participant <- df_sliced_antic %>%
  distinct(participant_code, n_antic_total_round, n_antic_round, perc_antic)
nrow(df_participant) # should be 376

rounds_per_participant <- df_sliced %>%
  group_by(participant_code) %>%
  summarise(
    n_rows         = n(),                                   # all rows (rounds, incl. round 0/1 if present)
    n_antic_data   = sum(!is.na(antic_actual)),             # rounds with usable antic_actual (0/1)
    min_round      = min(round_no),
    max_round      = max(round_no),
    .groups = "drop"
  )

table(rounds_per_participant$n_rows) 
table(rounds_per_participant$n_antic_data)


# Which rounds have antic_actual data, overall
df_sliced %>%
  group_by(round_no) %>%
  summarise(n_participants = n_distinct(participant_code),
            n_with_antic   = sum(!is.na(antic_actual)))

# Participants with fewer than expected rounds
rounds_per_participant %>% filter(n_antic_data < 19)

# Plot the percentage of antic_actual responses per individual,
# with the % of participants per bin labelled on top of each bar
n_participants <- n_distinct(df_sliced$participant_code) # 376

hist_perc_antic <- ggplot(df_participant, aes(x = perc_antic)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 binwidth = 0.05, boundary = 0, fill = "grey60", colour = "black") +
  stat_bin(binwidth = 0.05, boundary = 0, geom = "text", vjust = -0.5, size = 3.5,
           aes(y = after_stat(count / sum(count)),
               label = ifelse(after_stat(count) > 0,
                              scales::percent(after_stat(count / sum(count)), accuracy = 1),
                              ""))) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Proportion of anticonforming responses",
       y = "Percentage of participants",
       title = paste0("N receiving anticonformity tip = ", n_participants)) +
  theme_bw(base_size = 13, base_family = "Arial") +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
hist_perc_antic

# Export histogram
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)
fig_file_hist <- paste0(FIGURE_PATH, "Hist_perc-antic.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_perc_antic)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# ------------------------------------------------------------------------------------------------------------
# Demographics
# ------------------------------------------------------------------------------------------------------------
# Get the demographics for the sliced data
# get all the colnames that start with "demog"
demog_cols <- colnames(df_sliced)[startsWith(colnames(df_sliced), "demog")]

# Convert the columns to the correct format
df_sliced <- df_sliced %>%
  mutate(demog_age = as.numeric(demog_age), # age
         demog_gender = as.factor(demog_gender), # gender
         demog_edu = factor(demog_edu, # education level, in the order of the levels below
                            levels = c("Less than high school",
                                       "High school diploma or equivalent",
                                       "University degree or more")),
         demog_neigh_type = factor(demog_neigh_type, 
         levels = c("Urban", "Suburban", "Countryside")), # neighborhood type
         demog_pol_aff = as.numeric(demog_pol_aff)) # political affiliation


# Relevel the demographic factors to set reference levels
df_sliced <- df_sliced %>%
  mutate(demog_gender = relevel(demog_gender, ref = "Man"),
         # demog_edu: reference level is the first level set above
         demog_neigh_type = relevel(demog_neigh_type, ref = "Urban"))

df_sliced %>% select(all_of(demog_cols)) %>% str()
sapply(df_sliced[demog_cols], class)

# One row per participant, so that the demographic plots count participants (not rounds)
df_participant_demog <- df_sliced %>%
  distinct(participant_code, demog_age, demog_gender, demog_edu,
           demog_neigh_type, demog_pol_aff)
n_distinct(df_participant_demog$participant_code) == nrow(df_participant_demog) # should be TRUE

# THEME FOR PLOTS
# Shared theme and helpers for the demographic plots
theme_demog <- function() {
  theme_bw(base_size = 13, base_family = "Arial") +
    theme(text = element_text(colour = "black"),
          axis.text = element_text(colour = "black"),
          axis.title = element_text(colour = "black"),
          legend.text = element_text(colour = "black"),
          legend.title = element_text(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
}

# Bar plot of the % of participants per category, in the order of the factor levels
plot_demog_bar <- function(data, var, xlab, y_max = 100, y_by = 25) {
  agg <- data %>%
    group_by(across(all_of(var))) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(proportion = n / sum(n))
  ggplot(agg, aes(x = .data[[var]])) +
    geom_bar(aes(y = proportion * 100), stat = "identity",
             linetype = 1, linewidth = 0.1, colour = "black", fill = "#3182bd") +
    geom_text(aes(y = proportion * 100, label = sprintf("%.1f", proportion * 100)),
              vjust = -0.3, size = 2.5) +
    labs(x = xlab, y = "Percent", title = paste0("N = ", sum(agg$n))) +
    scale_y_continuous(limits = c(0, y_max), breaks = seq(0, y_max, by = y_by),
                       labels = function(x) paste0(x, "%")) +
    theme_demog()
}


# ------------------------------------------------------------------------------
# Plot each of the demographic variables
# ------------------------------------------------------------------------------
# 2a) Demog: Education level - Ordered
# ------------------------------------------------------------------------------
### Calculate percentages per category
df_sliced_agg <- df_participant_demog %>%
# remove blank values
	filter(demog_edu != "") %>%
	group_by(demog_edu) %>%	
	summarise(n = n()) %>%
	mutate(proportion = n / sum(n)) %>%
    # sort the bars by proportion
    arrange(desc(proportion))

###  Bar graph
hist_demog_edu <- ggplot(df_sliced_agg, aes(x=factor(demog_edu))) +
	geom_bar(aes(y=proportion*100), stat="identity",
		linetype=1, linewidth=0.1, colour="black", fill="#3182bd") +

    geom_text(aes(y=proportion*100,
		label=(sprintf("%.1f",round(proportion*100, 1)))),
		vjust = -0.3, size = 2.5) +
	ylab("Percent") +
	xlab("Education Level") +
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25),
	                   labels = function(x) paste0(x, "%")) +
	theme_bw(base_size=13, base_family="Arial") +
    theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
	    panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank())
hist_demog_edu

# Export histogram
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)
fig_file_hist <- paste0(FIGURE_PATH, "Hist_demog_edu.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_demog_edu)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# 2b) Demog: Gender - Factors
# ------------------------------------------------------------------------------
hist_demog_gender <- df_participant_demog %>%
  filter(!is.na(demog_gender), demog_gender != "") %>% # remove blank values
  mutate(demog_gender = droplevels(demog_gender)) %>%
  plot_demog_bar("demog_gender", "Gender")
hist_demog_gender

# Export histogram
fig_file_hist <- paste0(FIGURE_PATH, "Hist_demog_gender.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_demog_gender)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# 2c) Demog: Age - Numeric
# ------------------------------------------------------------------------------
df_age <- df_participant_demog %>%
  filter(!is.na(demog_age)) # remove blank values
hist_demog_age <- ggplot(df_age, aes(x = demog_age)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                 binwidth = 5, boundary = 15, closed = "left",
                 linewidth = 0.1, colour = "black", fill = "#3182bd") +
  stat_bin(binwidth = 5, boundary = 15, closed = "left", geom = "text",
           vjust = -0.3, size = 2.5,
           aes(y = after_stat(count / sum(count) * 100),
               label = ifelse(after_stat(count) > 0,
                              sprintf("%.1f", after_stat(count / sum(count) * 100)), ""))) +
  labs(x = "Age", y = "Percent", title = paste0("N = ", nrow(df_age))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_demog()
hist_demog_age

# Export histogram
fig_file_hist <- paste0(FIGURE_PATH, "Hist_demog_age.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_demog_age)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# 2d) Demog: Political Affiliation - Numeric
# ------------------------------------------------------------------------------
# Exclude the 99 ("Don't know") and blank values
pol_labels <- c("1" = "1\nExtremely\nLiberal", "2" = "2", "3" = "3",
                "4" = "4\nModerate", "5" = "5", "6" = "6", "7" = "7\nExtremely\nConservative")
hist_demog_pol_aff <- df_participant_demog %>%
  filter(!is.na(demog_pol_aff), demog_pol_aff != 99) %>%
  mutate(demog_pol_aff = factor(demog_pol_aff, levels = 1:7)) %>%
  plot_demog_bar("demog_pol_aff", "Political affiliation", y_max = 30, y_by = 10) +
  scale_x_discrete(labels = pol_labels, drop = FALSE)
hist_demog_pol_aff

# Export histogram
fig_file_hist <- paste0(FIGURE_PATH, "Hist_demog_pol_aff.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_demog_pol_aff)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# 2e) Demog: Neighborhood Type - Factors
# ------------------------------------------------------------------------------
hist_demog_neigh <- df_participant_demog %>%
  filter(!is.na(demog_neigh_type), demog_neigh_type != "") %>% # remove blank values
  mutate(demog_neigh_type = droplevels(demog_neigh_type)) %>%
  plot_demog_bar("demog_neigh_type", "Neighborhood Type",  y_max = 70, by = 10)
hist_demog_neigh

# Export histogram
fig_file_hist <- paste0(FIGURE_PATH, "Hist_demog_neigh.png")
png(filename = fig_file_hist, width = 2000, height = 1200, res = 300)
print(hist_demog_neigh)
dev.off()
message("Saved figure to: ", normalizePath(fig_file_hist))

# ==============================================================================================================
# MODELS
# ==============================================================================================================
# 2 models: 1) demographic variables predicting how likely someone to anticonform (y/n)
# 2) whether a) moving (1) or stay (0) and 2) there is a majority in the previous round predicting how likely someone to anticonform (y/n)



# ------------------------------------------------------------------------------
# Demographic variables predicting anticonformity (y/n)
# ------------------------------------------------------------------------------
# Get one row per participant with demographic variables and anticonformity outcome
df_participant <- df_sliced_antic %>%
  group_by(participant_code) %>%
  summarise(
    nid_unique = first(nid_unique),
    demog_age = first(demog_age),
    demog_gender = first(demog_gender),
    demog_pol_aff = first(demog_pol_aff),
    demog_neigh_type = first(demog_neigh_type),
    demog_edu = first(demog_edu),
    n_antic_round = first(n_antic_round),
    n_antic_total_round = first(n_antic_total_round),
    perc_antic = first(perc_antic)
  ) %>%
  ungroup() %>%
  mutate(demog_pol_aff = ifelse(demog_pol_aff == 99, NA, demog_pol_aff), # 99 = "Don't know"
         # rescale for the models: 
         demog_age_c = (demog_age - mean(demog_age, na.rm = TRUE)) / 10, # age in decades (centered),
         demog_pol_aff_c = demog_pol_aff - 4) # political affiliation centered at the midpoint (4)
df_participant


# Assuming a binomial distribution for the outcome variable (perc_antic)
# Check number of observations per neighborhood (should all be 21 for 21 rounds)
df_sliced_antic

table(df_sliced_antic$n_antic_total_round)
table(df_sliced_antic$n_antic_round)
table(df_sliced_antic$perc_antic)

all(df_sliced_antic$n_antic_round %% 1 == 0)
all(df_sliced_antic$perc_antic %% 1 == 0)
is.integer(df_sliced_antic$n_antic_round)
is.integer(df_sliced_antic$n_antic_round)


# ------------------------------------------------------------------------------
# MODEL I
# Predicting the number of anticonforming rounds out of the usable rounds (when the classification of anticonforming/not can be made) per participant.
# One row per participant, outcome = cbind(successes, failures): this models the proportion
# but weighs each participant by how many rounds they contributed.
# ------------------------------------------------------------------------------
# Drop the categories with very low freq. of gender "Prefer not to say" and "Other"
df_participant_filt <- df_participant %>%
  filter(!demog_gender %in% c("Prefer not to say", "Other"))

# Drop the demog_edu with Less than high school
df_participant_filt <- df_participant_filt %>%
  filter(demog_edu != "Less than high school") %>%
  droplevels() # so that the reference level of demog_edu is "High school diploma or equivalent"

binom_antic_demog <- glm(
    cbind(n_antic_round, n_antic_total_round - n_antic_round) ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type,
    data = df_participant_filt,
    family = binomial(link = "probit")
)
summary(binom_antic_demog)

# Participants are nested in neighborhoods, so add a neighborhood random intercept
# (observation-level random intercept (1 | participant_code) can be added for overdispersion)
binom_antic_demog_re <- glmer(
    cbind(n_antic_round, n_antic_total_round - n_antic_round) ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type + 
    (1 | nid_unique),
    data = df_participant_filt,
    family = binomial(link = "probit")
)
summary(binom_antic_demog_re)

# Save the models
antic_demog_models <- list(
  "Main effects of demographic variables" = binom_antic_demog,
  "Main effects of demographic variables with random intercept nid" = binom_antic_demog_re
)

# Save to the path
saveRDS(antic_demog_models, paste0(MODEL_PATH, "/antic_demog_models.rds"))

# # Test load
# antic_demog_models_loaded <- readRDS(paste0(MODEL_PATH, "/antic_demog_models.rds"))
# antic_demog_models_loaded[1] 
# # or 
# antic_demog_models_loaded[["Main effects of demographic variables with random intercept nid"]]


# ----------------------------------------------------------------------------------
# Create a Regression Table for the demographic models
# ----------------------------------------------------------------------------------

output_file <- paste0(REGRESSION_PATH, "antic_demog.html")

modelsummary(
  list("Probit GLM" = binom_antic_demog,
       "Probit GLMM (neighborhood random intercept)" = binom_antic_demog_re),
  output = output_file,
  exponentiate = FALSE, # probit coefficients are on the z-score scale, so odds ratios do not apply
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "demog_age_c" = "Age (per 10 years, centered)",
    "demog_genderWoman" = "Gender: Woman",
    "demog_eduUniversity degree or more" = "Education: University degree or more",
    "demog_pol_aff_c" = "Political affiliation (1 = extremely liberal, 7 = extremely conservative; centered at 4)",
    "demog_neigh_typeSuburban" = "Neighborhood: Suburban",
    "demog_neigh_typeCountryside" = "Neighborhood: Countryside"
  ),
  gof_omit = "RMSE",
  notes = "<i>Note. </i>Probit regressions (coefficients on the probit scale) of the number of anticonforming rounds out of each participant's rounds with usable data (binomial outcome), one row per participant who received the anticonformity tip. The GLMM adds a random intercept for neighborhood. Reference categories: Gender = Man, Education = High school diploma or equivalent, Neighborhood = Urban. Participants with gender \"Other\" or \"Prefer not to say\", with less than high school education, or with political affiliation \"Don't know\" are excluded. <br>Row-wise: Estimate; SE (standard error); CI [confidence interval].<br>*p < .05, **p < .01, ***p < .001.",
  escape = FALSE
)


# ----------------------------------------------------------------------------------
# Average Marginal Effects (AME) Tables
# ----------------------------------------------------------------------------------
# AME on the probability scale (probability of an anticonforming round), population-level for the GLMM (random effects set to 0)
mfx_antic_demog <- avg_slopes(binom_antic_demog, type = "response")
mfx_antic_demog_re <- avg_slopes(binom_antic_demog_re, type = "response", re.form = NA)

# Tidy, rounded output for checking (percentage points)
fmt_ame_demog <- function(x) {
  as.data.frame(x) %>%
    transmute(term, contrast,
              AME_pp = round(100 * estimate, 2),
              SE_pp = round(100 * std.error, 2),
              CI_low_pp = round(100 * conf.low, 2),
              CI_high_pp = round(100 * conf.high, 2),
              p = signif(p.value, 3))
}
fmt_ame_demog(mfx_antic_demog)
fmt_ame_demog(mfx_antic_demog_re)

# Export AME tables to LaTeX (same style as best_r0_ame_*.tex in regression.R)
tex_p <- function(p) {
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  txt <- ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
  ifelse(stars == "", txt, paste0("$", txt, "^{\\text{", stars, "}}$"))
}

# Row labels: numeric predictors are per unit increase, factors are contrasts vs. the reference category
tex_label_demog <- function(term, contrast) {
  out <- gsub(" - ", " vs. ", contrast)
  out[term == "demog_age_c"] <- "Age (+10 years)"
  out[term == "demog_pol_aff_c"] <- "Political affiliation (+1 point)"
  prefix <- c(demog_gender = "Gender: ", demog_edu = "Education: ", demog_neigh_type = "Neighborhood: ")
  has_prefix <- term %in% names(prefix)
  out[has_prefix] <- paste0(prefix[term[has_prefix]], out[has_prefix])
  out
}

ame_demog_rows <- function(d) {
  sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
          tex_label_demog(d$term, d$contrast), 100 * d$estimate, 100 * d$std.error,
          100 * d$conf.low, 100 * d$conf.high, tex_p(d$p.value))
}

write_ame_demog_tex <- function(ame_list, file, caption, label) {
  body <- unlist(lapply(names(ame_list), function(nm) {
    c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", nm),
      ame_demog_rows(as.data.frame(ame_list[[nm]])))
  }))
  note <- paste0("\\textit{Note.} Average marginal effects (AME) on the probability scale, expressed in percentage points, ",
                 "of the probability of an anticonforming round among participants who received the anticonformity tip. ",
                 "Age is centered and measured in decades; political affiliation is centered at the scale midpoint (1 = extremely liberal, 7 = extremely conservative). ",
                 "Categorical predictors are contrasts against the reference categories (Gender = Man, Education = High school diploma or equivalent, Neighborhood = Urban). ",
                 "The GLMM effects are population-level (random effects set to zero). SE = standard error; CI = confidence interval. ",
                 "*$p < .05$, **$p < .01$, ***$p < .001$.")
  tex <- c(
    "\\begin{table}[H]", "\\centering",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    "\\begingroup", "\\footnotesize", "\\renewcommand{\\arraystretch}{0.90}", "",
    "\\begin{tabularx}{\\linewidth}{Xcccc}", "\\toprule",
    "\\textbf{Predictor} & \\textbf{AME (pp)} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    "\\midrule", body, "\\bottomrule", "\\end{tabularx}", "",
    "\\vspace{0.6em}", "{\\footnotesize", "\\setlength{\\baselineskip}{12pt}", note, "\\par}",
    "\\endgroup", "\\end{table}")
  writeLines(tex, file, useBytes = TRUE)
}

write_ame_demog_tex(
  list("Probit GLM" = mfx_antic_demog,
       "Probit GLMM (neighborhood random intercept)" = mfx_antic_demog_re),
  paste0(REGRESSION_PATH, "antic_demog_ame.tex"),
  "Average Marginal Effects of Demographic Variables on the Probability of an Anticonforming Response",
  "tab:ame_antic_demog")




# ------------------------------------------------------------------------------
# MODEL II
# Predicting the number of anticonforming rounds out of the usable rounds (when the classification of anticonforming/not can be made) per participant.
# One row per participant, outcome = cbind(successes, failures): this models the proportion
# but weighs each participant by how many rounds they contributed.
# Having the previous majority (what the participant saw before responding) as a predictor
# ------------------------------------------------------------------------------
# Predicting whether an individual anticonforms in a given round (one row per participant x round).
# Each row is a Bernoulli trial (antic_actual = 1/0), so no counting is needed: participant_code and
# nid_unique enter as random intercepts, which handles repeated rounds and neighborhood clustering.
# NOTE: in the rows where antic_actual is defined, response_is_majority is exactly 1 - antic_actual
# (antic_actual == 0 <=> response == prev_majority), so it cannot be a predictor of antic_actual
# (perfect separation, singular fit). Use a predictor that is not a function of the same response.
# ------------------------------------------------------------------------------
# Same sample restrictions and rescaling as the participant-level models
# Demographics are only recorded in the first row of each participant (round 0) and are blank in later rounds,
# so take them from the participant-level data (already restricted and rescaled) and join them onto the round rows.
df_round <- df_sliced_antic %>%
  select(participant_code, round_no, antic_actual, prev_majority, response_is_majority, round_no, move) %>%
  inner_join(
    df_participant_filt %>%
      select(participant_code, nid_unique, demog_age_c, demog_edu, demog_gender,
             demog_pol_aff_c, demog_neigh_type),
    by = "participant_code") %>%
  filter(!is.na(antic_actual), !is.na(prev_majority))
colnames(df_round)

# Check: antic_actual vs response_is_majority should be a perfect diagonal
table(df_round$antic_actual, df_round$response_is_majority, useNA = "ifany")
n_distinct(df_round$participant_code)

binom_antic_move_re <- glmer(
    antic_actual ~ move +
    # demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type +
    (1 | participant_code) + (1 | nid_unique),
    data = df_round,
    family = binomial(link = "probit"),
    control = glmerControl(optimizer = "bobyqa")
)
summary(binom_antic_majority_re)
isSingular(binom_antic_majority_re) # TRUE

# Save the model
saveRDS(binom_antic_move_re,  paste0(MODEL_PATH, "/binom_antic_move_re.rds"))

# Diagnostics: the grouping factors must exist and rows must remain after NA removal
model_vars <- c("antic_actual", "demog_age_c", "demog_edu", "demog_gender",
                "demog_pol_aff_c", "demog_neigh_type", "participant_code", "nid_unique")
nrow(df_round)
colSums(is.na(df_round[model_vars]))
sapply(df_round[c("participant_code", "nid_unique")], class)
nrow(na.omit(df_round[model_vars])) # rows actually used by glmer


binom_antic_demog_round_re <- glmer(
    antic_actual ~ move + demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type +
    (1 | participant_code ) + (1 | nid_unique),
    data = df_round,
    family = binomial(link = "probit"),
    control = glmerControl(optimizer = "bobyqa")
)
summary(binom_antic_demog_round_re)

# Save the model
saveRDS(binom_antic_demog_round_re,  paste0(MODEL_PATH, "/binom_antic_demog_round_re.rds"))
