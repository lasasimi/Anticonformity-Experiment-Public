
# Load packages
library(lme4)
library(dplyr)
library(modelsummary)
library(tinytable)
library(splines)
library(ggplot2)
library(tibble)
library(tidyr)
library(marginaleffects)

# Set path to the project directory that consists the `renv.lock` file, if you're opening from IDE, ensure the folder you open is at the level where the `renv.lock` file is located
setwd("/Users/Lasmi Marbun/Documents/Git/Anticonformity_main/Anticonformity-Analysis/2026/Publishable/Regression")

# Do this once to restore the exact installed package versions to run the codes from `renv.lock`
renv::restore() 

# Load directory
PATH <- "/Users/Lasmi Marbun/Documents/Git/Anticonformity_main/Anticonformity-Analysis/2026/Publishable/"
REGRESSION_PATH <- paste0(PATH, "Regression/")
MODEL_PATH <- paste0(REGRESSION_PATH, "models/")
FIGURE_PATH <- paste0(REGRESSION_PATH, "figures/")

### ONLY LOAD THE MODELS ###
## Model I (b): 
# binom_antic_demog_round <- readRDS(paste0(MODEL_PATH, "/binom_antic_demog_round.rds"))
## Model II:
# binom_antic_clearmaj_noneigh_re <- readRDS(paste0(MODEL_PATH, "/binom_antic_clearmaj_noneigh_re.rds"))
## Additionally, load the df_round.rds
# df_round <- readRDS(paste0(MODEL_PATH, "/df_round.rds"))

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
# Remove rows with NAs 
df_participant_demog <- df_participant_demog %>%
  drop_na()
n_distinct(df_participant_demog$participant_code) == nrow(df_participant_demog) # should be TRUE
head(df_participant_demog)

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

all(df_sliced_antic$n_antic_round %% 1 == 0) # TRUE
all(df_sliced_antic$perc_antic %% 1 == 0) # FALSE
is.integer(df_sliced_antic$n_antic_round) # TRUE


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
  filter(demog_edu != "Less than high school")

# df_sliced_antic was created before the factor conversion above, so these columns are still character:
# convert to factors, drop the unused levels, then set the reference levels
df_participant_filt <- df_participant_filt %>%
  mutate(across(c(demog_gender, demog_edu, demog_neigh_type), as.factor)) %>%
  droplevels() %>%
  mutate(demog_gender = relevel(demog_gender, ref = "Woman"),
         demog_edu = relevel(demog_edu, ref = "High school diploma or equivalent"),
         demog_neigh_type = relevel(demog_neigh_type, ref = "Urban"))

sapply(df_participant_filt[c("demog_gender", "demog_edu", "demog_neigh_type")], levels) # first level = reference

binom_antic_demog <- glm(
    cbind(n_antic_round, n_antic_total_round - n_antic_round) ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type,
    data = df_participant_filt,
    family = binomial(link = "logit")
)
summary(binom_antic_demog)

# Participants are nested in neighborhoods, so add a neighborhood random intercept
# (observation-level random intercept (1 | participant_code) can be added for overdispersion)
binom_antic_demog_re <- glmer(
    cbind(n_antic_round, n_antic_total_round - n_antic_round) ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type + 
    (1 | nid_unique),
    data = df_participant_filt,
    family = binomial(link = "logit")
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
# antic_demog_models <- antic_demog_models_loaded
output_file <- paste0(REGRESSION_PATH, "antic_demog.html")

n_participants <- nrow(df_participant_filt)


modelsummary(
  list("Logit GLM" = binom_antic_demog,
       "Logit GLMM (neighborhood random intercept)" = binom_antic_demog_re),
  output = output_file,
  exponentiate = TRUE, # to get the odds ratios instead of log-odds
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})<br>[{conf.low}, {conf.high}]",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "demog_age_c" = "Age (per 10 years, centered)",
    "demog_genderMan" = "Gender: Man",
    "demog_eduUniversity degree or more" = "Education: University degree or more",
    "demog_pol_aff_c" = "Political affiliation (1 = extremely liberal, 7 = extremely conservative; centered at 4)",
    "demog_neigh_typeSuburban" = "Neighborhood: Suburban",
    "demog_neigh_typeCountryside" = "Neighborhood: Countryside"
  ),
  gof_omit = "RMSE",
  notes = "<i>Note. </i>Logit regressions of the number of anticonforming rounds out of each participant's rounds who received anticonformity tip (binomial outcome) N participants = 363. The rounds included are only those with a majority and not the initial rounds where participants have not seen the neighbors. The GLMM adds a random intercept for neighborhood. Odds ratios are reported: OR > 1 indicates higher odds of anticonforming, OR < 1 lower odds. The intercept is the odds of anticonforming for the reference profile (Age = sample mean, Political affiliation = 4, Gender = Woman, Education = High school diploma or equivalent, Neighborhood = Urban). Participants with gender \"Other\" or \"Prefer not to say\", with less than high school education, or with political affiliation \"Don't know\" are excluded. <br>Row-wise: Odds ratio; SE (standard error of the odds ratio); CI [95% confidence interval of the odds ratio].<br>*p < .05, **p < .01, ***p < .001.",
  escape = FALSE
)

binom_antic_demog_re

# ------------------------------------------------------------------------------------------------------------
# LaTeX table: odds ratios (OR) of binom_antic_demog_re (logit GLMM with neighborhood random intercept).
# Values come straight from the fitted model (Wald z-based CI, exp() of the log-odds).
# ------------------------------------------------------------------------------------------------------------
tex_p <- function(p) {
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  txt <- ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
  ifelse(stars == "", txt, paste0("$", txt, "^{\\text{", stars, "}}$"))
}

write_table_tex <- function(file, caption, label, header, body, note, colspec = "Xcccc") {
  tex <- c(
    "\\begin{table}[H]", "\\centering",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    "\\begingroup", "\\footnotesize", "\\renewcommand{\\arraystretch}{0.90}", "",
    sprintf("\\begin{tabularx}{\\linewidth}{%s}", colspec), "\\toprule",
    header, "\\midrule", body, "\\bottomrule", "\\end{tabularx}", "",
    "\\vspace{0.6em}", "{\\footnotesize", "\\setlength{\\baselineskip}{12pt}", note, "\\par}",
    "\\endgroup", "\\end{table}")
  writeLines(tex, file, useBytes = TRUE)
  message("Saved LaTeX table to: ", normalizePath(file))
  invisible(tex)
}

# --- Odds ratio table (one row per coefficient; Wald z-based CI, exp() of the log-odds) ---
or_demog_labels <- c(
  "(Intercept)" = "Intercept",
  "demog_age_c" = "Age (per 10 years, centered)",
  "demog_genderMan" = "Gender: Man",
  "demog_eduUniversity degree or more" = "Education: University degree or more",
  "demog_pol_aff_c" = "Political affiliation (+1 point; centered at 4)",
  "demog_neigh_typeSuburban" = "Neighborhood: Suburban",
  "demog_neigh_typeCountryside" = "Neighborhood: Countryside"
)

or_demog_rows <- function(model) {
  cf <- summary(model)$coefficients
  terms <- intersect(names(or_demog_labels), rownames(cf))
  z <- qnorm(0.975)
  est <- cf[terms, "Estimate"]
  se <- cf[terms, "Std. Error"]
  p <- 2 * pnorm(-abs(est / se))
  sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
          or_demog_labels[terms], exp(est), se, exp(est - z * se), exp(est + z * se), tex_p(p))
}

write_or_demog_tex <- function(model, file, caption, label) {
  re_var <- as.numeric(VarCorr(model)[[1]])
  note <- paste0(
    "\\textit{Note.} Odds ratios (OR) from a logit generalized linear mixed model of the number of anticonforming rounds ",
    "out of each participant's usable rounds (binomial outcome), with a random intercept for neighborhood ",
    sprintf("(variance = %.3f; %d neighborhoods; N = %s participants). ", re_var, ngrps(model)[[1]],
            format(nobs(model), big.mark = ",", trim = TRUE)),
    "OR $> 1$ indicates higher odds of anticonforming, OR $< 1$ lower odds. ",
    "The intercept is the odds of anticonforming for the reference profile (mean age, political affiliation = 4, ",
    "Gender = Woman, Education = High school diploma or equivalent, Neighborhood = Urban) in a typical neighborhood. ",
    "Participants with gender ``Other'' or ``Prefer not to say'', with less than high school education, ",
    "or with political affiliation ``Don't know'' are excluded. ",
    "SE = standard error of the log-odds coefficient; CI = Wald 95\\% confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Predictor} & \\textbf{OR} & \\textbf{SE (log-odds)} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    body = or_demog_rows(model), note = note, colspec = "Xcccc"
  )
}

write_or_demog_tex(
  binom_antic_demog_re,
  paste0(REGRESSION_PATH, "antic_demog_or.tex"),
  "Odds Ratios of Demographic Variables Predicting Anticonforming Responses (Logit GLMM)",
  "tab:or_antic_demog"
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
# (tex_p() is defined above)

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

write_ame_demog_tex <- function(ame_list, file, caption, label, data = df_participant_filt) {
  body <- unlist(lapply(names(ame_list), function(nm) {
    c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", nm),
      ame_demog_rows(as.data.frame(ame_list[[nm]])))
  }))
  ref <- sapply(data[c("demog_gender", "demog_edu", "demog_neigh_type")], function(x) levels(x)[1]) # reference = first level
  note <- paste0("\\textit{Note.} Average marginal effects (AME) on the probability scale, expressed in percentage points, ",
                 "of the probability of an anticonforming round among participants who received the anticonformity tip. ",
                 "Age is centered and measured in decades; political affiliation is centered at the scale midpoint (1 = extremely liberal, 7 = extremely conservative). ",
                 "Categorical predictors are contrasts against the reference categories ",
                 sprintf("(Gender = %s, Education = %s, Neighborhood = %s). ", ref[["demog_gender"]], ref[["demog_edu"]], ref[["demog_neigh_type"]]),
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
  list("Logit GLM" = mfx_antic_demog,
       "Logit GLMM (neighborhood random intercept)" = mfx_antic_demog_re),
  paste0(REGRESSION_PATH, "antic_demog_ame.tex"),
  "Average Marginal Effects of Demographic Variables on the Probability of an Anticonforming Response",
  "tab:ame_antic_demog")


# ------------------------------------------------------------------------------
# MODEL I (b) -> rounds as observations instead of aggregated counts, predicted by demog
# Note: this is the one reported in the paper
# Predicting the number of anticonforming rounds out of the usable rounds (when the classification of anticonforming/not can be made) per participant.
# ------------------------------------------------------------------------------
# Prepare the round-level dataset with relevant variables for modeling anticonformity.
df_round <- df_sliced_antic %>%
  select(participant_code, round_no, antic_actual, prev_majority, response_is_majority, round_no, move) %>%
  inner_join(
    df_participant_filt %>%
      select(participant_code, nid_unique, demog_age_c, demog_edu, demog_gender,
             demog_pol_aff_c, demog_neigh_type),
    by = "participant_code") %>%
  filter(!is.na(antic_actual), !is.na(prev_majority))
colnames(df_round)

# Create a demog model but with bernouli trials instead of counts
binom_antic_demog_round <- glmer(
    antic_actual ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type +
    (1 | participant_code) + (1 | nid_unique),
    data = df_round,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
)
summary(binom_antic_demog_round)
isSingular(binom_antic_demog_round) # if TRUE, check VarCorr() and drop the term with ~0 variance (e.g. nid_unique)
VarCorr(binom_antic_demog_round) # nid_unique has 0.000 variance, drop it

binom_antic_demog_round <- glmer(
    antic_actual ~
    demog_age_c + demog_edu + demog_gender + demog_pol_aff_c + demog_neigh_type +
    (1 | participant_code),
    data = df_round,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
)
isSingular(binom_antic_demog_round) # FALSE

# Save the model
saveRDS(binom_antic_demog_round,  paste0(MODEL_PATH, "/binom_antic_demog_round.rds"))
## Test load
# binom_antic_demog_round <- readRDS(paste0(MODEL_PATH, "/binom_antic_demog_round.rds"))


# Report the OR table into latex (reuses or_demog_labels / or_demog_rows defined for the demographic model above)
write_or_demog_round_tex <- function(model, file, caption, label, data = df_round) {
  n_rounds_dropped <- nrow(data) - nobs(model)
  n_part_dropped <- n_distinct(data$participant_code) - ngrps(model)[["participant_code"]]
  note <- paste0(
    "\\textit{Note.} Odds ratios (OR) from a logit generalized linear mixed model of whether a participant anticonformed in a round ",
    "(Bernoulli outcome, one row per participant and round) with a random intercept for participant ",
    sprintf("(variance = %.3f; %s rounds from %d participants). ",
            as.numeric(VarCorr(model)[["participant_code"]]), format(nobs(model), big.mark = ",", trim = TRUE),
            ngrps(model)[["participant_code"]]),
    "A random intercept for neighborhood was dropped because its estimated variance was zero (singular fit). ",
    "Only rounds in which the previous round had a majority among the three other neighbors are included. ",
    sprintf("%d rounds (%d participants) were dropped from the model because political affiliation was missing (``Don't know''). ",
            n_rounds_dropped, n_part_dropped),
    "OR $> 1$ indicates higher odds of anticonforming, OR $< 1$ lower odds. ",
    "The intercept is the odds of anticonforming for the reference profile (mean age, political affiliation = 4, ",
    "Gender = Woman, Education = High school diploma or equivalent, Neighborhood = Urban) for a typical participant (random intercept = 0). ",
    "Participants with gender ``Other'' or ``Prefer not to say'' or with less than high school education are excluded. ",
    "SE = standard error of the log-odds coefficient; CI = Wald 95\\% confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Predictor} & \\textbf{OR} & \\textbf{SE (log-odds)} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    body = or_demog_rows(model), note = note, colspec = "Xcccc"
  )
}

write_or_demog_round_tex(
  binom_antic_demog_round,
  paste0(REGRESSION_PATH, "antic_demog_round_or.tex"),
  "Odds Ratios of Demographic Variables Predicting Anticonforming Responses per Round (Logit GLMM)",
  "tab:or_antic_demog_round"
)

# Report the AME table into latex (reuses write_ame_demog_tex; population-level, random intercept set to 0)
mfx_antic_demog_round <- avg_slopes(binom_antic_demog_round, type = "response", re.form = NA)
fmt_ame_demog(mfx_antic_demog_round)

write_ame_demog_tex(
  list("Logit GLMM (participant random intercept; rounds as Bernoulli trials)" = mfx_antic_demog_round),
  paste0(REGRESSION_PATH, "antic_demog_round_ame.tex"),
  "Average Marginal Effects of Demographic Variables on the Probability of an Anticonforming Response per Round",
  "tab:ame_antic_demog_round",
  data = df_round)
  



# ------------------------------------------------------------------------------
# MODEL II
# Predicting the number of anticonforming rounds out of the usable rounds (when the classification of anticonforming/not can be made) per participant.
# ------------------------------------------------------------------------------

# Create a column indicating if majority was by 3 or just 2 neighbors in the previous round using df_sliced_antic
# Clear majority (all 3 other neighbors gave the same response in the previous round) vs. narrow majority (2 vs. 1).
# dgroup_response_list is a string like "[1.0, 1.0, -1.0]"; rounds without a majority (ties) are already dropped via antic_actual.
n_before <- nrow(df_round)
df_round <- df_round %>%
  left_join(
    df_sliced_antic %>% select(participant_code, round_no, dgroup_response_list),
    by = c("participant_code", "round_no")) %>%
  mutate(clear_majority = as.integer(vapply(
    strsplit(gsub("[][ ]", "", dgroup_response_list), ","),
    function(v) length(unique(v)) == 1, logical(1))))
stopifnot(nrow(df_round) == n_before) # the join must not duplicate rows
table(df_round$dgroup_response_list, df_round$clear_majority, useNA = "ifany")
table(df_round$clear_majority, df_round$antic_actual, useNA = "ifany")

# Save df_round as a .rds
saveRDS(df_round, paste0(MODEL_PATH, "/df_round.rds"))

# Each row is a Bernoulli trial (antic_actual = 1/0), so the outcome is modeled directly without counting.
# Participants contribute different numbers of rounds (rounds without a majority are excluded); the
# participant random intercept handles that and the repeated measures, nid_unique the neighborhood clustering.
binom_antic_clearmaj_re <- glmer(
    antic_actual ~ clear_majority +
    (1 | participant_code) + (1 | nid_unique),
    data = df_round,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
)
isSingular(binom_antic_clearmaj_re) # singular fit, check variance components
VarCorr(binom_antic_clearmaj_re) # nid_unique variance is 0, no need to include in the model then
summary(binom_antic_clearmaj_re)

# Without nid_unique as a random effect since its variance is 0
binom_antic_clearmaj_noneigh_re <- glmer(
    antic_actual ~ clear_majority +
    (1 | participant_code),
    data = df_round,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
)
isSingular(binom_antic_clearmaj_noneigh_re) # FALSE
summary(binom_antic_clearmaj_noneigh_re) # Note: similar coefficients to the model with nid_unique included

# Save the model
saveRDS(binom_antic_clearmaj_noneigh_re, paste0(MODEL_PATH, "/binom_antic_clearmaj_noneigh_re.rds"))

## Test load
# binom_antic_clearmaj_noneigh_re <- readRDS(paste0(MODEL_PATH, "/binom_antic_clearmaj_noneigh_re.rds"))

# ------------------------------------------------------------------------------------------------------------
# LaTeX tables for binom_antic_clearmaj_noneigh_re (logit GLMM, participant random intercept):
# odds ratios (OR) and average marginal effects (AME) of a clear (3 of 3) vs. narrow (2 of 3) majority.
# Uses tex_p() and write_table_tex() defined above.
# ------------------------------------------------------------------------------------------------------------
clearmaj_labels <- c(
  "(Intercept)" = "Intercept",
  "clear_majority" = "Clear majority (3 of 3) vs. narrow majority (2 of 3)"
)

clearmaj_note_common <- function(model) {
  paste0(
    "\\textit{Note.} Logit generalized linear mixed model of whether a participant anticonformed in a round ",
    "(Bernoulli outcome, one row per participant and round) with a random intercept for participant ",
    sprintf("(variance = %.3f; %s rounds from %d participants). ",
            as.numeric(VarCorr(model)[["participant_code"]]), format(nobs(model), big.mark = ",", trim = TRUE),
            ngrps(model)[["participant_code"]]),
    "A random intercept for neighborhood was dropped because its estimated variance was zero (singular fit); ",
    "coefficients were similar with it included. ",
    "Only rounds in which the previous round had a majority among the three other neighbors are included. ",
    "A clear majority means all three neighbors gave the same response in the previous round; a narrow majority means two of three did. "
  )
}

# --- Odds ratio table (Wald z-based CI, exp() of the log-odds) ---
or_clearmaj_rows <- function(model) {
  cf <- summary(model)$coefficients
  terms <- intersect(names(clearmaj_labels), rownames(cf))
  z <- qnorm(0.975)
  est <- cf[terms, "Estimate"]
  se <- cf[terms, "Std. Error"]
  p <- 2 * pnorm(-abs(est / se))
  sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
          clearmaj_labels[terms], exp(est), se, exp(est - z * se), exp(est + z * se), tex_p(p))
}

write_or_clearmaj_tex <- function(model, file, caption, label) {
  note <- paste0(
    clearmaj_note_common(model),
    "Odds ratios (OR) are reported: OR $> 1$ indicates higher odds of anticonforming, OR $< 1$ lower odds. ",
    "The intercept is the odds of anticonforming after a narrow majority for a typical participant (random intercept = 0). ",
    "SE = standard error of the log-odds coefficient; CI = Wald 95\\% confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Predictor} & \\textbf{OR} & \\textbf{SE (log-odds)} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    body = or_clearmaj_rows(model), note = note, colspec = "Xcccc"
  )
}

write_or_clearmaj_tex(
  binom_antic_clearmaj_noneigh_re,
  paste0(REGRESSION_PATH, "antic_clearmaj_or.tex"),
  "Odds Ratios of a Clear Majority in the Previous Round Predicting Anticonforming Responses (Logit GLMM)",
  "tab:or_antic_clearmaj"
)

# --- Average marginal effect (AME), probability scale, population-level (random intercept set to 0) ---
# clear_majority is binary (0/1), so avg_slopes() returns the discrete contrast 1 - 0.
mfx_antic_clearmaj <- avg_slopes(binom_antic_clearmaj_noneigh_re, variables = "clear_majority",
                                 type = "response", re.form = NA)
as.data.frame(mfx_antic_clearmaj)[c("term", "contrast", "estimate", "std.error", "conf.low", "conf.high", "p.value")]

write_ame_clearmaj_tex <- function(ame, model, file, caption, label) {
  d <- as.data.frame(ame)
  body <- sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
                  clearmaj_labels[["clear_majority"]], 100 * d$estimate, 100 * d$std.error,
                  100 * d$conf.low, 100 * d$conf.high, tex_p(d$p.value))
  note <- paste0(
    clearmaj_note_common(model),
    "The average marginal effect (AME) is the difference in the probability of anticonforming, in percentage points, ",
    "between a clear and a narrow majority, averaged over the observed rounds on the probability scale. ",
    "It is population-level (random intercept set to zero). SE = standard error; CI = confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Predictor} & \\textbf{AME (pp)} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    body = body, note = note, colspec = "Xcccc"
  )
}

write_ame_clearmaj_tex(
  mfx_antic_clearmaj, binom_antic_clearmaj_noneigh_re,
  paste0(REGRESSION_PATH, "antic_clearmaj_ame.tex"),
  "Average Marginal Effect of a Clear Majority in the Previous Round on the Probability of an Anticonforming Response",
  "tab:ame_antic_clearmaj"
)