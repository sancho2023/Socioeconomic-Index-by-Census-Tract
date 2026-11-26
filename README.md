# Socioeconomic Index by Census Tract

R code to build a census-tract-level socioeconomic index, using variables from the 2022 Brazilian Demographic Census and Principal Component Analysis (PCA).

The index summarises income, education and household characteristics into two interpretable components (**income** and **infrastructure**) and one combined **socioeconomic index**. Tracts are then classified into four socioeconomic levels for mapping and for spatial epidemiological analyses.

## Repository contents

| File | Description |
|---|---|
| `socioeconomic_index_2022.R` | Full analysis script, commented in English |
| `indicador_socioeconomico_2022_comentado.R` | Same analysis, with detailed explanations in Portuguese |

## Variables

| Variable | Description |
|---|---|
| `setor_censi` | Census tract code (identifier, not used in the PCA) |
| `vari_renda` | Income variance (BRL²), log-transformed before the analysis |
| `salario_min` | Mean income in minimum wages (BRL 1,212.00 in 2022) |
| `perc_m_naoalf` | % of women aged 15+ who are illiterate |
| `prop_chef_mulher` | % of households headed by a woman |
| `prop_0ou1_banheiro` | % of households with 0 or 1 bathroom |
| `prop_morador` | % of households with 1 or 2 residents |

## Methods

1. **Data checks:** impossible percentages, extreme values (3 × IQR rule, log scale for income variance) and tracts with no information.
2. **Exclusion:** one tract (354980520000015) was excluded because its data were insufficient: an extreme income variance plus 2 of 6 variables missing.
3. **Transformation and suitability:** `log1p` of the income variance, Pearson correlations, Kaiser–Meyer–Olkin (KMO) measure and Bartlett's test of sphericity.
4. **Missing data:** PCA-based imputation (`missMDA::imputePCA`, 2 components). Percentages are imputed on the logit scale so that every imputed value stays within 0–100%. Observed values are restored unchanged. A sensitivity analysis compares the imputations with 1 and 2 components.
5. **Unrotated PCA:** used to decide how many components to keep (Kaiser criterion, eigenvalue > 1).
6. **Varimax-rotated PCA** (`psych::principal`, 2 components):
   - The *income* and *infrastructure* components are identified by their loadings rather than by their position, and their sign is set so that higher values mean better conditions.
   - The combined index is the mean of the two standardized scores, weighted by the proportion of variance each component explains.
7. **Classification:** 4 levels (Level 1 = best, Level 4 = worst) using Jenks natural breaks, with quartiles as a sensitivity analysis.
8. **Descriptive tables:** mean and median of each variable by level.

## Requirements

R (≥ 4.1) and the packages:

```r
install.packages(c("readxl", "missMDA", "FactoMineR", "psych",
                   "classInt", "car", "RcmdrMisc"))
```

## How to run

1. Place the input file `- Atualizado _PCA 2022_ - Oficial.xlsx` (one row per census tract) in the project folder.
2. Set the working directory to that folder.
3. Run `socioeconomic_index_2022.R`.

The script writes four CSV files (`;` as separator, `,` as decimal mark):

- `socioeconomic_index_2022.csv`: indices and levels for each tract, ready to join with the census-tract shapefile by `setor_censi`
- `varimax_loadings_2022.csv`: rotated loadings
- `table1_mean_2022.csv` and `table1_median_2022.csv`: characteristics of each level

## Data

Census variables come from the 2022 Brazilian Demographic Census (IBGE). The processed input spreadsheet is not included in this repository.
