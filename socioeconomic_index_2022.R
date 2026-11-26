##############################################################################
# SOCIOECONOMIC INDEX BY CENSUS TRACT - PRINCIPAL COMPONENT ANALYSIS - 2022
#
# Description:
#   Builds a socioeconomic index for each census tract from 2022 Brazilian
#   Census variables. The pipeline
#     1. checks the data (impossible values, extreme values, empty tracts),
#     2. removes one tract with insufficient data,
#     3. log-transforms the income variance and checks PCA suitability,
#     4. imputes missing values with PCA-based imputation (percentages on the
#        logit scale so that imputed values stay within 0-100%),
#     5. runs an unrotated PCA (diagnostics) and a varimax-rotated PCA,
#     6. builds an income index, an infrastructure index and a combined
#        socioeconomic index, and
#     7. classifies tracts into 4 levels (Jenks natural breaks and quartiles).
#
# Input : "- Atualizado _PCA 2022_ - Oficial.xlsx" (one row per census tract)
# Output: CSV files with the indices, varimax loadings and descriptive tables
##############################################################################
# Variables:
#  setor_censi        census tract code (identifier, not used in the PCA)
#  vari_renda         income variance (unit: BRL squared)
#  renda_med          mean income (dropped: same information as salario_min)
#  salario_min        mean income in minimum wages (BRL 1,212.00 in 2022)
#  perc_m_naoalf      % of women aged 15+ who are illiterate
#  prop_chef_mulher   % of households headed by a woman
#  prop_0ou1_banheiro % of households with 0 or 1 bathroom
#  prop_morador       % of households with 1 or 2 residents
##############################################################################

# install.packages(c("readxl", "missMDA", "FactoMineR", "psych",
#                    "classInt", "car", "RcmdrMisc"))

library(readxl)      # read the Excel file
library(missMDA)     # PCA-based imputation of missing values
library(FactoMineR)  # PCA (plots and diagnostics)
library(psych)       # KMO, Bartlett's test and varimax-rotated PCA
library(classInt)    # Jenks natural breaks
library(car)         # scatterplot matrix
library(RcmdrMisc)   # correlation matrix with adjusted p-values (rcorr.adjust)


# ---- 1. Load the data --------------------------------------------------------

data_2022 <- read_excel("- Atualizado _PCA 2022_ - Oficial.xlsx")
data_2022 <- as.data.frame(data_2022)

# Store the tract code as TEXT. If Excel stores it as a number, R may print
# it in scientific notation and the comparison with the outlier code
# (step 3) would fail silently.
if (is.numeric(data_2022$setor_censi)) {
  data_2022$setor_censi <- format(data_2022$setor_censi, scientific = FALSE, trim = TRUE)
} else {
  data_2022$setor_censi <- trimws(as.character(data_2022$setor_censi))
}
table(nchar(data_2022$setor_censi))   # all codes should have the same length

# Variables used in the PCA
vars <- c("vari_renda", "salario_min", "prop_morador", "prop_chef_mulher",
          "perc_m_naoalf", "prop_0ou1_banheiro")

# Subset of variables that are percentages
pct_vars <- c("prop_morador", "prop_chef_mulher",
              "perc_m_naoalf", "prop_0ou1_banheiro")

# renda_med is left out (duplicates salario_min)
dat <- data_2022[, c("setor_censi", vars)]


# ---- 2. Check the data BEFORE any analysis -----------------------------------

summary(dat[, vars])

# 2a. Scale of the percentages: 0-1 or 0-100? Variable names mix "prop_" and
#     "perc_". Check the ranges and set the limit below accordingly.
sapply(dat[, pct_vars], range, na.rm = TRUE)
pct_limit <- 100   # change to 1 if the variables are proportions (0 to 1)

# 2b. Impossible values (below 0 or above the limit)
pct_error <- apply(dat[, pct_vars], 1, function(row) {
  any(row < 0 | row > pct_limit, na.rm = TRUE)
})
sum(pct_error)

# 2c. Extreme values (beyond 3 x IQR). vari_renda is highly skewed, so the
#     rule is applied on the log scale; otherwise dozens of ordinary tracts
#     would be flagged.
dat_check <- dat[, vars]
dat_check$vari_renda <- log1p(dat_check$vari_renda)
extreme <- sapply(dat_check, function(x) {
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  x < q[1] - 3 * iqr | x > q[2] + 3 * iqr
})
colSums(extreme, na.rm = TRUE)   # number of extreme values per variable

# Tracts with at least one extreme or impossible value, for manual review
error_report <- dat[rowSums(extreme, na.rm = TRUE) > 0 | pct_error, ]
if (interactive()) View(error_report)

# 2d. Tracts with no information at all (all 6 variables missing) are removed
all_missing <- rowSums(is.na(dat[, vars])) == length(vars)
sum(all_missing)   # expected: 0
dat <- dat[!all_missing, ]


# ---- 3. Income-variance outlier (tract 354980520000015) ----------------------
# The variance is in BRL squared: 53.4 billion BRL^2 = a standard deviation of
# about BRL 231,000. The tract has a mean income of 45 minimum wages, missing
# prop_morador and prop_chef_mulher, and prop_0ou1_banheiro = 33.33%
# (suggesting very few households). Deleting only vari_renda and imputing it
# was tested: with 3 of 6 variables missing, imputation produced
# prop_morador = 236%. Decision: EXCLUDE the tract for insufficient data
# (reported in the paper).

outlier_tract <- "354980520000015"
stopifnot(outlier_tract %in% dat$setor_censi)   # make sure the code matched

dat <- dat[dat$setor_censi != outlier_tract, ]

nrow(dat)   # expected: 1088

# Number of missing variables per tract (reported in the paper)
table(n_missing = rowSums(is.na(dat[, vars])))


# ---- 4. Log transformation and correlations ----------------------------------

dat_log <- dat[, vars]
dat_log$vari_renda <- log1p(dat_log$vari_renda)   # log(1 + x) handles zeros

# Correlations and scatterplots on the log scale (on the raw scale the
# skewness of vari_renda distorts Pearson's correlation)
rcorr.adjust(dat_log, type = "pearson", use = "complete")

scatterplotMatrix(
  ~ vari_renda + salario_min + prop_morador + prop_chef_mulher +
    perc_m_naoalf + prop_0ou1_banheiro,
  data = dat_log, regLine = TRUE, smooth = FALSE,
  diagonal = list(method = "none")
)

# Suitability of the data for PCA (usually reported in the paper)
cor_matrix <- cor(dat_log, use = "complete.obs")
KMO(cor_matrix)                                                  # overall KMO >= 0.6
cortest.bartlett(cor_matrix, n = sum(complete.cases(dat_log)))   # p < 0.05

# Sanity checks for a common spreadsheet error: columns sorted independently
# of each other (which breaks the link between variables of the same tract)

# 1. Spearman: independently sorted columns give rho close to 1.000
round(cor(dat[, vars], method = "spearman", use = "pairwise.complete.obs"), 3)

# 2. Is any column sorted by row position? (values near +/-1 = sorted)
sapply(dat[, vars], function(x)
  round(cor(seq_along(x), x, method = "spearman", use = "complete.obs"), 3))

# 3. Plot: real data form a cloud; sorted columns form a thin curve
plot(dat$prop_morador, log1p(dat$vari_renda),
     xlab = "prop_morador", ylab = "log(vari_renda)")

# 4. First rows, to compare with the raw IBGE data
head(data_2022, 10)


# ---- 5. Imputation of missing values -----------------------------------------
# PCA-based imputation is linear and does not respect the bounds of the
# percentages (it produced values from -1% to 144%). Solution: impute the
# percentages on the LOGIT scale, which ranges from -infinity to +infinity,
# and transform them back to 0-100%. This forces every imputed value to lie
# within 0-100%. Because logit(0) and logit(100) are infinite, 0% and 100%
# are set to 0.5% and 99.5% ONLY during imputation; observed values are
# restored exactly at the end.

eps <- 0.005 * pct_limit   # 0.5 percentage point
logit_pct     <- function(p) qlogis(pmin(pmax(p, eps), pct_limit - eps) / pct_limit)
inv_logit_pct <- function(z) plogis(z) * pct_limit

dat_trans <- dat_log
dat_trans[pct_vars] <- lapply(dat_trans[pct_vars], logit_pct)

# Number of components for imputation: estim_ncpPCA chose 4 (the maximum
# allowed with 6 variables, i.e. no clear optimum). We use 2, the number of
# components retained by the PCA (Kaiser criterion).
estim <- estim_ncpPCA(dat_trans, ncp.min = 1, ncp.max = 4, scale = TRUE)
estim$ncp   # for the record only
ncp_imputation <- 2

imp     <- imputePCA(dat_trans, ncp = ncp_imputation, scale = TRUE)
dat_imp <- as.data.frame(imp$completeObs)

# Back-transform the percentages to 0-100% and restore the observed values
for (v in pct_vars) {
  observed <- !is.na(dat_log[[v]])
  dat_imp[[v]] <- inv_logit_pct(dat_imp[[v]])
  dat_imp[[v]][observed] <- dat_log[[v]][observed]
}
sum(is.na(dat_imp))                    # expected: 0
sapply(dat_imp[, pct_vars], range)     # everything within 0 and the limit

# Sensitivity analysis: do the imputed values change much with ncp = 1?
# Correlation between the two imputations, on the missing cells only.
imp_ncp1 <- imputePCA(dat_trans, ncp = 1, scale = TRUE)$completeObs
missing_cells <- is.na(dat_trans)
cor(imp$completeObs[missing_cells], imp_ncp1[missing_cells])   # close to 1 = stable

# Which tracts had missing data and how much (for the paper, and to judge
# whether any tract has too much missing data to be imputed)
n_miss <- rowSums(is.na(dat[, vars]))
table(n_miss)
dat[n_miss >= 3, ]   # tracts with half or more of the variables missing

# Copy on the raw scale (BRL squared), for descriptive tables only
dat_imp_raw <- dat_imp
dat_imp_raw$vari_renda <- expm1(dat_imp_raw$vari_renda)


# ---- 6. Unrotated PCA (diagnostics) ------------------------------------------

pca_2022 <- PCA(dat_imp, scale.unit = TRUE, ncp = 6, graph = TRUE)

# $eig always contains all 6 dimensions; ncp only limits how many appear
# in $ind and $var
pca_2022$eig

# Kaiser criterion: retain dimensions with eigenvalue > 1
n_comp <- sum(pca_2022$eig[, "eigenvalue"] > 1)
n_comp   # expected: 2
if (n_comp != 2) warning("Kaiser criterion suggests ", n_comp, " components, not 2.")

round(pca_2022$var$coord[, 1:2], 2)   # unrotated loadings


# ---- 7. Varimax-rotated PCA: the indices -------------------------------------
# Without rotation, Dim.2 is by construction a contrast orthogonal to Dim.1,
# and variables tend to load on both, which makes labelling Dim.2 as
# "infrastructure" fragile. Varimax rotation redistributes the loadings so
# that each variable loads strongly on ONE component, so each component
# becomes an interpretable construct. psych::principal returns standardized
# scores (mean 0, SD 1), which puts both indices on the same scale before
# they are combined.

pc_rot <- principal(dat_imp, nfactors = 2, rotate = "varimax", scores = TRUE)
print(pc_rot$loadings, cutoff = 0.4)
loadings_rot <- unclass(pc_rot$loadings)

# Which component is "income" and which is "infrastructure"? Identified by the
# loadings, not by position (the order may change in 2000/2010 data).
comp_income <- which.max(abs(loadings_rot["salario_min", ]))
comp_infra  <- setdiff(1:2, comp_income)
if (which.max(abs(loadings_rot["perc_m_naoalf", ])) != comp_infra)
  warning("perc_m_naoalf did not load on the infrastructure component: review the interpretation.")

scores <- pc_rot$scores
income_index         <- scores[, comp_income]
infrastructure_index <- scores[, comp_infra]

# Direction: "higher = better" for both indices, checked against the data
# (the sign of a principal component is arbitrary and may flip between years)
if (cor(income_index, dat_imp$salario_min) < 0)
  income_index <- -income_index
if (cor(infrastructure_index, dat_imp$prop_0ou1_banheiro) > 0)
  infrastructure_index <- -infrastructure_index

# Combined index: mean of the two indices, weighted by the proportion of
# variance explained by each rotated component
var_prop      <- pc_rot$Vaccounted["Proportion Var", ]
weight_income <- unname(var_prop[comp_income])
weight_infra  <- unname(var_prop[comp_infra])

socioeconomic_index <-
  (income_index * weight_income + infrastructure_index * weight_infra) /
  (weight_income + weight_infra)

# Check: should be strongly correlated with the unrotated Dim.1
cor(socioeconomic_index, pca_2022$ind$coord[, 1])


# ---- 8. Classification into 4 levels -----------------------------------------
# Level 1 = best, Level 4 = worst

final_table <- data.frame(
  setor_censi          = dat$setor_censi,
  income_index         = income_index,
  infrastructure_index = infrastructure_index,
  socioeconomic_index  = socioeconomic_index
)

# Jenks natural breaks (minimise variance within groups)
jenks_breaks <- classIntervals(final_table$socioeconomic_index,
                               n = 4, style = "jenks")
final_table$ses_level <- cut(
  final_table$socioeconomic_index,
  breaks = jenks_breaks$brks,
  labels = paste("Level", 4:1),   # low values = worst = Level 4
  include.lowest = TRUE
)
final_table$ses_level <- factor(final_table$ses_level,
                                levels = paste("Level", 1:4))

# Alternative: quartiles (4 groups of equal size)
final_table$ses_level_quartile <- cut(
  final_table$socioeconomic_index,
  breaks = quantile(final_table$socioeconomic_index, probs = seq(0, 1, 0.25)),
  labels = paste("Level", 4:1),
  include.lowest = TRUE
)
final_table$ses_level_quartile <- factor(final_table$ses_level_quartile,
                                         levels = paste("Level", 1:4))

# Number of tracts per level, and cross-tabulation of the two methods
count_table <- data.frame(
  Level     = levels(final_table$ses_level),
  N_tracts  = as.numeric(table(final_table$ses_level)),
  Percent   = round(100 * as.numeric(prop.table(table(final_table$ses_level))), 1)
)
print(count_table)
table(Jenks = final_table$ses_level, Quartile = final_table$ses_level_quartile)


# ---- 9. Table 1: characteristics of each level -------------------------------
# Mean AND median: vari_renda is highly skewed, so the mean alone is misleading.

dat_with_level <- data.frame(dat_imp_raw, level = final_table$ses_level)

# Transpose an aggregate() result: variables in rows, levels in columns
format_table <- function(tab) {
  m <- t(round(as.matrix(tab[, -1]), 2))
  colnames(m) <- as.character(tab$level)
  m
}

table1_mean   <- format_table(aggregate(. ~ level, data = dat_with_level, FUN = mean))
table1_median <- format_table(aggregate(. ~ level, data = dat_with_level, FUN = median))
table1_mean
table1_median

# Tract with the highest index (sanity check)
final_table[which.max(final_table$socioeconomic_index), ]


# ---- 10. Export --------------------------------------------------------------
# setor_censi is exported as text, ready to be joined with the shapefile.
# write.csv2 uses ";" as separator and "," as decimal mark (Brazilian Excel).

write.csv2(final_table, "socioeconomic_index_2022.csv", row.names = FALSE)
write.csv2(round(loadings_rot, 3), "varimax_loadings_2022.csv")
write.csv2(table1_mean, "table1_mean_2022.csv")
write.csv2(table1_median, "table1_median_2022.csv")
