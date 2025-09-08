#!/usr/bin/env Rscript

# Ward-level austerity analysis in R
# ----------------------------------
# This script replicates the analysis from the Stata file
# `austerity.leave.ward.level.do` using R.  It relies on
# data stored in `WARD17_SAVE2.rdata` which contains the
# ward level dataset created by `ward-austerity-modern.R`.
#
# The code follows the style requested by the user and
# makes use of the packages fixest, data.table, plyr,
# janitor and ggplot2.  Each major step is documented so
# that the workflow is transparent and reproducible.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(plyr)
  library(janitor)
  library(ggplot2)
})

# -------------------------------------------------------
# 1. Load and prepare data
# -------------------------------------------------------

load("WARD17_SAVE2.rdata")
# The object name inside the RData file is expected to be
# `WARD17_SAVE2`.  If not, the first object loaded will be
# used as the dataset.

if(!exists("WARD17_SAVE2")){
  obj_name <- ls()[1]
  WARD17_SAVE2 <- get(obj_name)
}

# Convert to data.table and clean variable names
dt <- as.data.table(WARD17_SAVE2)
dt <- clean_names(dt)

# Create a numeric identifier for fixed effects.  The Stata
# code encodes `LAD17CD`; if this variable is missing we fall
# back to the ward code `wd17cd`.
if("lad17cd" %in% names(dt)){
  dt[, id := as.integer(factor(lad17cd))]
} else if("wd17cd" %in% names(dt)){
  dt[, id := as.integer(factor(substr(wd17cd, 1, 3)))]
} else {
  dt[, id := .I]
}

# -------------------------------------------------------
# 2. Construct demographic controls
# -------------------------------------------------------

# Many census variables were labelled in the original Stata
# scripts as AGESH_0 ... AGESH_70.  If these variables are
# present we aggregate them into broader age buckets.  If
# they are absent the code skips these steps gracefully.
age_vars <- paste0("agesh_", 0:70)
if(all(age_vars %in% names(dt))){
  dt[, age_0_20 := rowSums(.SD), .SDcols = paste0("agesh_", 0:20)]
  dt[, age_20_30 := rowSums(.SD), .SDcols = paste0("agesh_", 21:30)]
  dt[, age_30_40 := rowSums(.SD), .SDcols = paste0("agesh_", 31:40)]
  dt[, age_40_50 := rowSums(.SD), .SDcols = paste0("agesh_", 41:50)]
  dt[, age_50_60 := rowSums(.SD), .SDcols = paste0("agesh_", 51:60)]
  dt[, age_60_70 := rowSums(.SD), .SDcols = paste0("agesh_", 61:70)]
  dt[, age_70p   := 1 - (age_0_20 + age_20_30 + age_30_40 +
                         age_40_50 + age_50_60 + age_60_70)]
}

# Drop variables containing "_total" or "_unknown" to mimic
# the data cleaning performed in Stata
drop_cols <- grep("(_total|_unknown)", names(dt), value = TRUE)
if(length(drop_cols)) dt[, (drop_cols) := NULL]

# Define groups of control variables
def_ctrl <- function(v) v[v %in% names(dt)]
controls_imig   <- def_ctrl(c("ks204ew0017", "ks204ew0018", "ks204ew0019"))
controls_demo   <- def_ctrl(c("age_0_20", "age_20_30", "age_30_40",
                              "age_50_60", "age_60_70", "age_70p"))
controls_edu    <- def_ctrl(c("ks501ew0014", "ks501ew0015", "ks501ew0016",
                              "ks501ew0018", "ks501ew0019"))
controls_tenure <- def_ctrl(c("ks402ew0010", "ks402ew0011",
                              "ks402ew0013", "ks402ew0015"))
controls_nssec  <- def_ctrl(c("ks611ew0017", "ks611ew0018", "ks611ew0019",
                              "ks611ew0020", "ks611ew0021", "ks611ew0022",
                              "ks611ew0023", "ks611ew0024", "ks611ew0025",
                              "ks611ew0027", "ks611ew0028", "ks611ew0030"))
controls_sector <- def_ctrl(c("ks605ew0020", "ks605ew0021", "ks605ew0022",
                              "ks605ew0023", "ks605ew0024", "ks605ew0025",
                              "ks605ew0026", "ks605ew0027", "ks605ew0028",
                              "ks605ew0029", "ks605ew0030", "ks605ew0031",
                              "ks605ew0032", "ks605ew0033", "ks605ew0034",
                              "ks605ew0035", "ks605ew0036", "ks605ew0037"))
controls_all <- c(controls_imig, controls_demo, controls_edu)

# -------------------------------------------------------
# 3. Scale benefit variables by population
# -------------------------------------------------------

# Helper function to scale variables by population size
scale_by_pop <- function(vars){
  for(v in vars){
    if(v %in% names(dt)){
      dt[, (v) := (get(v)/pop2015) * 100]
    }
  }
}

scale_by_pop(grep("^claim_totalclaimants", names(dt), value = TRUE))
scale_by_pop(grep("^claim_jobseekers", names(dt), value = TRUE))
scale_by_pop(grep("^claim_disabled", names(dt), value = TRUE))
scale_by_pop(grep("^jsa_sanction", names(dt), value = TRUE))
scale_by_pop(grep("^wp_mandatory", names(dt), value = TRUE))
scale_by_pop(grep("^pip_reassessment", names(dt), value = TRUE))

# Standardise Work Programme referrals by their standard
# deviation as in the Stata code
wp_vars <- grep("^wp_mandatory", names(dt), value = TRUE)
for(v in wp_vars){
  sdv <- sd(dt[[v]], na.rm = TRUE)
  if(!is.na(sdv) && sdv > 0) dt[, (v) := get(v)/sdv]
}

# -------------------------------------------------------
# 4. Main regression and simulations
# -------------------------------------------------------

# Main model: Ward Leave percentage on 2016q2 JSA sanctions
# controlling for contemporaneous claimant stock and basic
# demographic controls
main_model <- feols(
  ward_leave_pct ~ jsa_sanction_2016q2 + claim_jobseekers_2016q2 +
    .[, ..controls_imig] + .[, ..controls_demo] | id,
  cluster = ~id, data = dt
)
main_estimate <- coef(main_model)["jsa_sanction_2016q2"]

# Function to collect coefficients from a sequence of
# quarterly variables
time_simulation <- function(prefix, quarters, add_claim = FALSE){
  ldply(quarters, function(q){
    var <- paste0(prefix, q)
    if(!var %in% names(dt)) return(NULL)
    rhs <- var
    if(add_claim){
      claim_var <- paste0("claim_jobseekers", sub(prefix, "", var))
      if(claim_var %in% names(dt)) rhs <- paste(rhs, "+", claim_var)
    }
    rhs <- paste(rhs, "+", paste(c(controls_imig, controls_demo), collapse = "+"))
    fml <- as.formula(paste("ward_leave_pct ~", rhs, "| id"))
    mod <- feols(fml, cluster = ~id, data = dt)
    ci <- confint(mod, var)
    data.frame(var = var,
               estimate = coef(mod)[var],
               conf_low = ci[1],
               conf_high = ci[2])
  })
}

# JSA sanctions simulation
jsa_quarters <- grep("^jsa_sanction_20", names(dt), value = TRUE)
jsa_quarters <- sort(jsa_quarters)
jsa_sim <- time_simulation("jsa_sanction_", gsub("jsa_sanction_", "", jsa_quarters), add_claim = TRUE)
jsa_pval <- mean(jsa_sim$estimate < main_estimate)

p_jsa <- ggplot(jsa_sim, aes(x = var, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high)) +
  geom_hline(yintercept = main_estimate, colour = "red") +
  coord_flip() +
  labs(x = "Quarter", y = "Coefficient",
       title = "JSA sanction coefficients") +
  theme_minimal()

ggsave("sim_jsa_sanction_coefs.pdf", p_jsa, width = 7, height = 5)

# PIP Reassessment simulation
pip_quarters <- grep("^pip_reassessment_20", names(dt), value = TRUE)
pip_quarters <- sort(pip_quarters)
pip_sim <- time_simulation("pip_reassessment_", gsub("pip_reassessment_", "", pip_quarters), add_claim = FALSE)
pip_model <- feols(ward_leave_pct ~ pip_reassessment_2016q2 | id,
                   cluster = ~id, data = dt)
pip_main <- coef(pip_model)["pip_reassessment_2016q2"]
pip_pval <- mean(pip_sim$estimate < pip_main)

p_pip <- ggplot(pip_sim, aes(x = var, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high)) +
  geom_hline(yintercept = pip_main, colour = "red") +
  coord_flip() +
  labs(x = "Quarter", y = "Coefficient",
       title = "PIP reassessment coefficients") +
  theme_minimal()

ggsave("sim_pip_reassessment_coefs.pdf", p_pip, width = 7, height = 5)

# Work Programme mandatory referrals simulation
wp_quarters <- grep("^wp_mandatory_20", names(dt), value = TRUE)
wp_quarters <- sort(wp_quarters)
wp_sim <- time_simulation("wp_mandatory_", gsub("wp_mandatory_", "", wp_quarters), add_claim = FALSE)
wp_model <- feols(ward_leave_pct ~ wp_mandatory_2016q1 | id,
                  cluster = ~id, data = dt)
wp_main <- coef(wp_model)["wp_mandatory_2016q1"]
wp_pval <- mean(wp_sim$estimate < wp_main)

p_wp <- ggplot(wp_sim, aes(x = var, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high)) +
  geom_hline(yintercept = wp_main, colour = "red") +
  coord_flip() +
  labs(x = "Quarter", y = "Coefficient",
       title = "Work Programme referral coefficients") +
  theme_minimal()

ggsave("sim_work_programme_coefs.pdf", p_wp, width = 7, height = 5)

# -------------------------------------------------------
# 5. Recent quarter regressions with coefficient plots
# -------------------------------------------------------

make_coef_plot <- function(model, vars, title, file){
  cf <- data.table(var = vars,
                   estimate = coef(model)[vars])
  ci <- confint(model)[vars,]
  cf[, `:=`(conf_low = ci[,1], conf_high = ci[,2])]
  p <- ggplot(cf, aes(x = var, y = estimate)) +
    geom_point(colour = "maroon") +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                  colour = "maroon") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(x = "", y = "Coefficient", title = title) +
    theme_minimal()
  ggsave(file, p, width = 7, height = 5)
}

jsa_recent <- c("jsa_sanction_2015q3", "jsa_sanction_2015q4",
                "jsa_sanction_2016q1", "jsa_sanction_2016q2",
                "jsa_sanction_2016q3", "jsa_sanction_2016q4",
                "jsa_sanction_2017q1")
model_jsa_recent <- feols(
  as.formula(paste("ward_leave_pct ~", paste(jsa_recent, collapse = "+"), "| id")),
  cluster = ~id, data = dt
)
make_coef_plot(model_jsa_recent, jsa_recent,
               "JSA Benefit Sanctions",
               "ward_benefit_sanctions_jsa.png")

pip_recent <- c("pip_reassessment_2015q2", "pip_reassessment_2015q3",
                "pip_reassessment_2015q4", "pip_reassessment_2016q1",
                "pip_reassessment_2016q2", "pip_reassessment_2016q3",
                "pip_reassessment_2016q4")
model_pip_recent <- feols(
  as.formula(paste("ward_leave_pct ~", paste(pip_recent, collapse = "+"), "| id")),
  cluster = ~id, data = dt
)
make_coef_plot(model_pip_recent, pip_recent,
               "PIP Reassessment notifications",
               "ward_benefit_sanctions_pip.png")

wp_recent <- c("wp_mandatory_2015q2", "wp_mandatory_2015q3",
               "wp_mandatory_2015q4", "wp_mandatory_2016q1",
               "wp_mandatory_2016q2", "wp_mandatory_2016q3",
               "wp_mandatory_2016q4", "wp_mandatory_2017q1")
model_wp_recent <- feols(
  as.formula(paste("ward_leave_pct ~", paste(wp_recent, collapse = "+"), "| id")),
  cluster = ~id, data = dt
)
make_coef_plot(model_wp_recent, wp_recent,
               "Mandatory Work Programme referrals",
               "ward_benefit_sanctions_wp.png")

# -------------------------------------------------------
# 6. Summary table of key estimates
# -------------------------------------------------------

etable(main_model, pip_model, wp_model,
       file = "benefit_sanctions_ward.tex", replace = TRUE)

cat("Analysis complete.  Key figures and table written to disk.\n")
