# REPRODUCIBILITY (renv)
# Package versions are pinned in renv.lock. The .Rprofile in this folder
# activates renv automatically when R is started with this folder as the
# working directory (e.g. open the Regression folder in VS Code / RStudio).
# If you add or remove packages, run renv::snapshot() to update renv.lock.
# Set PATH below to the location of this repository on your machine.

# Load packages
library(ordinal)
library(dplyr)
library(modelsummary)
library(tinytable)
library(ggplot2)
library(marginaleffects)

# Load directory
PATH <- "/Users/Lasmi Marbun/Documents/Git/Anticonformity_main/Anticonformity-Analysis/2026/Publishable/"
REGRESSION_PATH <- paste0(PATH, "Regression/")
MODEL_PATH <- paste0(REGRESSION_PATH, "models/")
FIGURE_PATH <- paste0(REGRESSION_PATH, "figures/")

# Load data
df <- read.csv(paste0(PATH, "Clean_files/07_combined_classified.csv"))

# Check colnames
colnames(df)

# Check pol_charge and emo_charge columns
table(df$pol_charge)
table(df$emo_charge)

# Check the Mean and SD for both pol_charge and emo_charge for different framings and the n_participants
df %>%
  group_by(framing) %>%
  summarise(
    mean_pol = mean(pol_charge, na.rm = TRUE),
    sd_pol = sd(pol_charge, na.rm = TRUE),
    mean_emo = mean(emo_charge, na.rm = TRUE),
    sd_emo = sd(emo_charge, na.rm = TRUE),
    n_participants = n_distinct(participant_code)
  )


# ------------------------------------------------------------------------------
# Manipulation check: political charge and emotional charge by framing
# Predicting the ratings by framing type (political vs. nonpolitical as the ref. point)
# ------------------------------------------------------------------------------
nrow(df)/21
colnames(df)

# For this purpose, only take non-NAs row (pol_charge and emo_charge)
# and keep one row per participant (df has one row per participant per round)
df_manip <- df %>%
  filter(!is.na(pol_charge) & !is.na(emo_charge)) %>%
  distinct(participant_code, .keep_all = TRUE)

nrow(df_manip)  # should match the number of participants

df_manip$pol_charge <- ordered(
  df_manip$pol_charge,
  levels = 1:5
)

df_manip$emo_charge <- ordered(
  df_manip$emo_charge,
  levels = 1:5
)
# check column type of pol_charge
str(df_manip$pol_charge)

# Convert framing to a factor
df_manip$framing <- factor(as.character(df_manip$framing),
                     levels = c("1", "2"),
                     labels = c("Nonpolitical framing", "Political framing"))
levels(df_manip$framing)
table(df_manip$pol_charge)

# Save the df_manip to rds to contain the levels
saveRDS(df_manip, file.path(MODEL_PATH, "df_manip.rds"))
## Load the df_manip from rds
# df_manip <- readRDS(file.path(MODEL_PATH, "df_manip.rds"))

logit_pol  <- clm(pol_charge ~ framing, data = df_manip, link = "logit")
logit_emo  <- clm(emo_charge ~ framing, data = df_manip, link = "logit")

# Save the models into rds
manip_check_framing_models <- list(logit_pol = logit_pol, logit_emo = logit_emo)
saveRDS(manip_check_framing_models, file.path(MODEL_PATH, "manip_check_framing.rds"))

# Test load the models
# manip_check_framing_models <- readRDS(file.path(MODEL_PATH, "manip_check_framing.rds"))

# Check robustness with probit link
probit_pol <- update(logit_pol, link = "probit")   # robustness only
probit_emo <- update(logit_emo, link = "probit")   # robustness only

nominal_test(logit_pol) # significant
# Note: framing's effect on pol_charge is not constant across threshold. E.g. framing may shift people strongly from low to mid but not from mid to high.
nominal_test(logit_emo) # n.s.

nominal_test(probit_pol) # significant
nominal_test(probit_emo) # n.s.
m_mixed  <- clmm(pol_charge ~ framing + (1 | nid_unique), data = df_manip, link = "logit")

# change in the probability of each response category (political vs. nonpolitical)
ame_pol <- avg_comparisons(logit_pol, variables = "framing", type = "prob")
ame_emo <- avg_comparisons(logit_emo, variables = "framing", type = "prob")

# ------------------------------------------------------------------------------------------------------------------------
# Plot the pol_charge distribution
# ------------------------------------------------------------------------------------------------------------------------
# THEME FOR PLOTS
# Shared theme and helpers for the demographic plots
theme_plot <- function() {
  theme_bw(base_size = 13, base_family = "Arial") +
  theme(legend.position = "bottom",
        legend.location = "panel", # align legend to the panel, not the y-label
        legend.justification = "center") +
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


# Create a plot showing the distribution of political charge by framing
# Set Path for output files
output_file <- file.path(FIGURE_PATH, "Dist_pol_charge_pct.png")

pol_charge_pct <- df_manip %>%
  filter(!is.na(pol_charge), !is.na(framing)) %>%
  count(framing, pol_charge = factor(pol_charge)) %>%
  group_by(framing) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

framing_n <- tapply(pol_charge_pct$n, as.character(pol_charge_pct$framing), sum)

p <- ggplot(pol_charge_pct, aes(x = pol_charge, y = pct, fill = factor(framing))) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Perceived political charge",
    y = "Percentage of participants in condition (%)",
    fill = "Framing",
    title = "Distribution of perceived political charge by framing conditions"
  ) +
  scale_x_discrete(
    labels = function(x) {
      lbl <- c(
        "1" = "Strongly disagree", "2" = "Disagree", "3" = "Neutral",
        "4" = "Agree", "5" = "Strongly agree"
      )
      stringr::str_wrap(lbl[as.character(x)], width = 12)
    }
  ) +
  scale_fill_manual(
    values = c("Nonpolitical framing" = "#FF7F0E", "Political framing" = "#1F77B4"),
    labels = c(
      "Nonpolitical framing" = paste0("Nonpolitical framing (n = ", framing_n[["Nonpolitical framing"]], ")"),
      "Political framing" = paste0("Political framing (n = ", framing_n[["Political framing"]], ")")
    )
  ) +
  theme_plot()

ggsave(output_file, plot = p, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------------------------------------------------
# Do the same but with emo_charge instead of pol_charge
# ------------------------------------------------------------------------------------------------------------------------

emo_charge_pct <- df_manip %>%
  filter(!is.na(emo_charge), !is.na(framing)) %>%
  count(framing, emo_charge = factor(emo_charge)) %>%
  group_by(framing) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

framing_n <- tapply(emo_charge_pct$n, as.character(emo_charge_pct$framing), sum)

output_file <- file.path(FIGURE_PATH, "Dist_emo_charge_pct.png")
e <- ggplot(emo_charge_pct, aes(x = emo_charge, y = pct, fill = factor(framing))) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Perceived emotional charge",
    y = "Percentage of participants in condition (%)",
    fill = "Framing",
    title = "Distribution of perceived emotional charge by framing conditions"
  ) +
  scale_x_discrete(
    labels = function(x) {
      lbl <- c(
        "1" = "Strongly disagree", "2" = "Disagree", "3" = "Neutral",
        "4" = "Agree", "5" = "Strongly agree"
      )
      stringr::str_wrap(lbl[as.character(x)], width = 12)
    }
  ) +
  scale_fill_manual(
    values = c("Nonpolitical framing" = "#FF7F0E", "Political framing" = "#1F77B4"),
    labels = c(
      "Nonpolitical framing" = paste0("Nonpolitical framing (n = ", framing_n[["Nonpolitical framing"]], ")"),
      "Political framing" = paste0("Political framing (n = ", framing_n[["Political framing"]], ")")
    )
  ) +
  theme_plot()

ggsave(output_file, plot = e, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------------------------------------------------
# Combined plot: pol_charge and emo_charge side by side (facets), full 1-5 distribution by framing
# ------------------------------------------------------------------------------------------------------------------------
charge_pct <- df_manip %>%
  filter(!is.na(pol_charge), !is.na(emo_charge), !is.na(framing)) %>%
  select(participant_code, framing, pol_charge, emo_charge) %>%
  tidyr::pivot_longer(c(pol_charge, emo_charge), names_to = "construct", values_to = "rating") %>%
  mutate(
    construct = factor(construct,
                       levels = c("pol_charge", "emo_charge"),
                       labels = c("Perceived political charge", "Perceived emotional charge")),
    rating = factor(as.character(rating), levels = as.character(1:5))
  ) %>%
  count(construct, framing, rating, .drop = FALSE) %>%
  group_by(construct, framing) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# Same participants rate both constructs, so n per framing is identical across facets
framing_n_both <- charge_pct %>%
  filter(construct == "Perceived political charge") %>%
  group_by(framing) %>%
  summarise(n = sum(n))
framing_n_both <- setNames(framing_n_both$n, as.character(framing_n_both$framing))

output_file <- file.path(FIGURE_PATH, "Dist_pol_emo_charge_pct.png")
pe <- ggplot(charge_pct, aes(x = rating, y = pct, fill = framing)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3) +
  facet_wrap(~ construct, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(
    labels = function(x) {
      lbl <- c(
        "1" = "Strongly disagree", "2" = "Disagree", "3" = "Neutral",
        "4" = "Agree", "5" = "Strongly agree"
      )
      stringr::str_wrap(lbl[as.character(x)], width = 10)
    }
  ) +
  scale_fill_manual(
    values = c("Nonpolitical framing" = "#FF7F0E", "Political framing" = "#1F77B4"),
    labels = c(
      "Nonpolitical framing" = paste0("Nonpolitical framing (n = ", framing_n_both[["Nonpolitical framing"]], ")"),
      "Political framing" = paste0("Political framing (n = ", framing_n_both[["Political framing"]], ")")
    )
  ) +
  labs(
    x = "Rating",
    y = "Percentage of participants in condition (%)",
    fill = "Framing",
    title = "Distribution of perceived political and emotional charge by framing conditions"
  ) +
  theme_plot()

ggsave(output_file, plot = pe, width = 12, height = 6, dpi = 300)

# ------------------------------------------------------------------------------------------------------------
# LaTeX tables for the manipulation check: odds ratios (OR) and average marginal effects (AME)
# for pol_charge and emo_charge (framing: political vs. nonpolitical). Values come straight from the fitted models.
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

# --- Odds ratio table (one row per outcome; Wald z-based CI, exp() of the log-odds) ---
or_row <- function(model, outcome, term = "framingPolitical framing") {
  cf <- summary(model)$coefficients
  est <- cf[term, "Estimate"]
  se <- cf[term, "Std. Error"]
  z <- qnorm(0.975)
  p <- 2 * pnorm(-abs(est / se))
  sprintf("%s & %.3f & %.3f & [%.3f, %.3f] & %s & %s \\\\",
          outcome, exp(est), se, exp(est - z * se), exp(est + z * se), tex_p(p),
          format(nobs(model), big.mark = ",", trim = TRUE))
}

write_or_tex <- function(models, file, caption, label) {
  body <- vapply(names(models), function(nm) or_row(models[[nm]], nm), character(1))
  note <- paste0(
    "\\textit{Note.} Odds ratios (OR) from cumulative link (proportional odds) models with a logit link, ",
    "predicting the five-point rating by framing. Reference category: Nonpolitical framing. ",
    "OR $> 1$ indicates higher odds of a higher rating under Political framing. ",
    "SE = standard error of the log-odds coefficient; CI = Wald 95\\% confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Outcome} & \\textbf{OR} & \\textbf{SE (log-odds)} & \\textbf{95\\% CI} & \\textbf{p-value} & \\textbf{N} \\\\",
    body = body, note = note, colspec = "Xccccc"
  )
}

# --- AME table (change in the probability of each rating category, in percentage points) ---
write_ame_tex <- function(ames, file, caption, label) {
  body <- unlist(lapply(names(ames), function(nm) {
    d <- as.data.frame(ames[[nm]])
    c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", nm),
      sprintf("Rating %s & %.3f & %.3f & [%.3f, %.3f] & %s \\\\",
              d$group, 100 * d$estimate, 100 * d$std.error,
              100 * d$conf.low, 100 * d$conf.high, tex_p(d$p.value)))
  }))
  note <- paste0(
    "\\textit{Note.} Average marginal effects (AME) of Political vs. Nonpolitical framing on the probability of ",
    "each rating category, expressed in percentage points (pp), from cumulative link models with a logit link. ",
    "SE = standard error; CI = confidence interval. ",
    "*$p < .05$, **$p < .01$, ***$p < .001$."
  )
  write_table_tex(
    file, caption, label,
    header = "\\textbf{Rating category} & \\textbf{AME (pp)} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\",
    body = body, note = note
  )
}

write_or_tex(
  models = list("Political charge" = logit_pol, "Emotional charge" = logit_emo),
  file = paste0(REGRESSION_PATH, "manip_check_framing_or.tex"),
  caption = "Manipulation Check: Odds Ratios for Political vs. Nonpolitical Framing",
  label = "tab:manip_check_or"
)

write_ame_tex(
  ames = list("Political charge" = ame_pol, "Emotional charge" = ame_emo),
  file = paste0(REGRESSION_PATH, "manip_check_framing_ame.tex"),
  caption = "Manipulation Check: Average Marginal Effects of Political vs. Nonpolitical Framing on Rating Probabilities",
  label = "tab:manip_check_ame"
)
