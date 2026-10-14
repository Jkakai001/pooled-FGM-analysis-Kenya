# Mali V2 alignment with Burkina Faso

Reference: `../Bukina Fasso/01_Notebooks/` V2 notebooks. Mali's 2023–24 paths,
MLI identifiers, nine survey regions, accent normalization and local estimates
are retained. No Burkina Faso coefficients or country results were transferred.

## Changes

- Risk: adopted the reference's supported-follow-up checks and descriptive
  comparison with shared y scales. Removed the duplicate raw-data reload that
  restored an invalid daughter history and redundant KM plot definitions.
  Preserved the refined Mali KM plots without endpoint circles or labels.
  Explicitly restrict self records to ages 15–49. Corrected survey-year wording
  and the erroneous attitude introduction. Explained that the spatial risk
  model smooths direct risks and does not use ethnicity/residence covariates.
- Attitude: confirmed the modelling code already matches the reference's v3
  methodology (despite the V2 filename): normalized survey-weighted joint
  covariate/BYM2 model, age smoothing and posterior aggregation. Clarified
  weights versus coefficients and regional terminology; retained Mali inputs.
- Prevalence: confirmed the same covariate-adjusted spatial model is already
  present. Adapted the reference's larger chart labels and panel presentation
  to Mali's 0–100% range, moved annotations to avoid overlap, and avoided the
  reference's inline package installation and country-specific 0–20% limit.
  The cohort ribbon averages regional interval endpoints; its description now
  identifies this as a descriptive envelope, not a national posterior interval.
- Comparison: retained the reference's final overlays with sample-size symbols,
  eliminating competing plot cells that overwrote the same output files.
  Preserved Mali geography and coordinate transformation.
- Risk and prevalence paths now work from the project root or notebook folder.
- Cleared stored cell outputs/execution counts to prevent old results from
  appearing to come from the revised code. Original notebooks and outputs are
  preserved in `backups/before_bfa_v2_alignment_20260923_112746`.

## Validation

- All R code cells parsed successfully across all four notebooks.
- Risk filtering: 34,507 histories, 24,284 events, 10,223 censored records;
  16,995 self histories all aged 15–49; impossible daughter history excluded.
- Person-time expansion: 216,378 annual records; event totals preserved.
- Survey-weighted discrete-time hazard regression rerun and converged.
- Descriptive model comparison rerun using the current filtered histories and
  existing regional estimates: unsupported daughter age bands remain missing.
- Attitude respondent joins and prevalence/attitude keys checked against all
  nine boundary records.
- Revised prevalence plot rendered from existing regional estimates and visually
  inspected. Preview and survey validation log: `../03_Outputs/MLI/V2_Alignment_Checks/`.

## Execution limits and rerun order

The available R installation lacks sf, spdep, INLA, randomForest, car,
survminer, changepoint and randomForestSRC. Full spatial fits and map rendering
were therefore not rerun. The exploratory Cox frailty fit was not revalidated.
Existing production CSVs and figures were not replaced by these checks.

With the notebook's required R packages available, restart each kernel and run
all cells in order. The risk and attitude workflows can run independently;
run prevalence before comparison, which reads its adjusted prevalence CSVs.
The existing upstream preparation notebooks/data were not modified.
