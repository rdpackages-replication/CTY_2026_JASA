# Cattaneo, Titiunik and Yu (2026, JASA)

Replication files for Cattaneo, Titiunik and Yu (2026), "Estimation and
Inference in Boundary Discontinuity Designs: Location-Based Methods."

This repository contains the empirical application and simulation scripts for
the location-based boundary discontinuity design analysis. The computations use
the R package [`rd2d`](https://cran.r-project.org/package=rd2d).

## Website

https://rdpackages.github.io/replication

## Data

The empirical application uses the Ser Pilo Paga data from:

- Londono-Velez, J., C. Rodriguez, and F. Sanchez (2020):
  [Upstream and Downstream Impacts of College Merit-Based Financial Aid for
  Low-Income Students: Ser Pilo Paga in Colombia](https://doi.org/10.1257/pol.20180131),
  _American Economic Journal: Economic Policy_ 12(2): 193-227.

The analysis dataset is [`spp.csv`](spp.csv). The scripts expect these columns:

- `running_saber11`
- `running_sisben`
- `eligible_spp`
- `beneficiary_spp`
- `spadies_any`
- `icfes_educm1`

## Repository Layout

- [`CTY_2026_JASA--empapp.R`](CTY_2026_JASA--empapp.R): runs the empirical
  application and writes CSV outputs to `output/`.
- [`CTY_2026_JASA--empapp_tables.R`](CTY_2026_JASA--empapp_tables.R): converts
  empirical CSV outputs into LaTeX table fragments in `tables/`.
- [`CTY_2026_JASA--empapp_figures.R`](CTY_2026_JASA--empapp_figures.R):
  converts empirical CSV outputs into figures in `figures/`.
- [`CTY_2026_JASA--simuls.R`](CTY_2026_JASA--simuls.R): runs the simulation
  study and writes raw simulation outputs to `output/`.
- [`CTY_2026_JASA--simuls_tables.R`](CTY_2026_JASA--simuls_tables.R): converts
  simulation outputs into LaTeX table fragments in `tables/`.
- [`spp.csv`](spp.csv): SPP data used by the empirical application and
  simulation design.

Generated artifacts are reproducible from the scripts. Three local folders are
required:

- `figures/`
- `tables/`
- `output/`

These generated folders are intentionally ignored by Git.

## Requirements

The scripts are written for R and require `rd2d` version 0.1.0 or newer, with
support for `params.other`, `params.cov`, and `summary(..., cbands = ...)`.
Install the released package from CRAN before running the replication scripts:

```r
install.packages("rd2d")
library(rd2d)
```

## Replication

To replicate the empirical application:

```sh
Rscript CTY_2026_JASA--empapp.R
Rscript CTY_2026_JASA--empapp_tables.R
Rscript CTY_2026_JASA--empapp_figures.R
```

To replicate the simulation study:

```sh
Rscript CTY_2026_JASA--simuls.R
Rscript CTY_2026_JASA--simuls_tables.R
```

The simulation design considers three calibrated polynomial DGPs, indexed by
`s = 1`, `s = 2`, and `s = 3`, for sharp and fuzzy designs.

The full simulation is computationally expensive. These environment variables
can be used to adjust local runs:

- `RD2D_M`: number of simulation replications, default `5000`
- `RD2D_N`: sample size per replication, default `10000`
- `RD2D_REPP`: simulation repetitions for critical values, default `2000`
- `RD2D_SEED`: simulation seed, default `20260510`
- `RD2D_WORKERS`: parallel workers, default based on available cores
- `RD2D_EMP_REPP`: empirical critical-value repetitions, default `2000`

The local polynomial choices and bandwidth-check count are left to the `rd2d`
package defaults.

Example quick simulation smoke run:

```sh
RD2D_M=2 RD2D_N=1000 RD2D_REPP=99 Rscript CTY_2026_JASA--simuls.R
```

## Outputs

- `output/empapp_*.csv`: empirical application data and summary tables.
- `output/simuls_*.csv`: simulation metadata, DGP calibration, targets, and run
  summaries.
- `output/simuls_raw_rep*.csv`: raw replication-level simulation files.
- `tables/empapp_*.tex`: empirical application LaTeX table fragments.
- `tables/simuls_*.tex`: simulation LaTeX table fragments.
- `figures/*.png`: generated empirical application figures.

By default, outputs are written to `output/`, `tables/`, and `figures/`. The
simulation scripts also support `RD2D_OUTPUT_DIR` and `RD2D_TABLES_DIR` for
local output-directory overrides.

## Reference

- Cattaneo, M. D., R. Titiunik, and R. R. Yu (2026):
  [Estimation and Inference in Boundary Discontinuity Designs: Location-Based
  Methods](https://rdpackages.github.io/references/Cattaneo-Titiunik-Yu_2026_JASA.pdf).<br>
  _Journal of the American Statistical Association_, revise and resubmit.<br>
  [Supplemental Appendix](https://rdpackages.github.io/references/Cattaneo-Titiunik-Yu_2026_JASA--Supplement.pdf).

## Acknowledgment

This work was supported in part by the National Science Foundation through
grants [SES-2019432](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2019432),
[DMS-2210561](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2210561),
[SES-2241575](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2241575), and
[SES-2342226](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2342226), and by
the National Institute for Food and Agriculture through grant
[2024-67023-42704](https://www.nifa.usda.gov/data).
