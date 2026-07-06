# SWaNs

**SWaNs** (*Sequential Wastewater Sampling for Source Localization in Sewer Networks*) is an R/Shiny application for deterministic source-tracking analyses in rooted sewer networks. The app supports data validation, single-source simulation, guided sequential sampling, and exhaustive comparison of four sampling strategies:

- `kGBS` - k-Batch Generalized Binary Search
- `MRP` - Maximum Relative Population
- `MCRP` - Maximum Cumulative Relative Population
- `SMCRP` - Skipping Maximum Cumulative Relative Population

The application is designed for transparent and reproducible decision support in wastewater source tracking.

## Repository scope

This public repository contains the SWaNs application code, documentation, a reproducibility snapshot of the software environment, a built-in example input, and synthetic benchmark inputs. It does **not** contain detailed real-world sewer-network data.

The live application is available at:

- <https://shiny.statistik.tu-dortmund.de/app/swans>

The public code repository is intended for release as:

- <https://github.com/YassineTalleb/SWaNs>

## Core functionality

- Upload and validate a sewer network as an adjacency matrix.
- Upload non-negative node weights and convert them into prior source probabilities.
- Detect reversed flow orientation and offer matrix transposition during validation.
- Simulate deterministic source localisation for a selected source node, strategy, and number of samplers.
- Run guided sequential sampling with user-supplied positive sample results.
- Compare strategies across sampler counts using prior-weighted expected values and unweighted distributions.
- Export publication-ready figures and summary tables.

## Public repository structure

- [app.R](app.R): Shiny app entry point
- [R/](R): application logic, algorithms, validation, plotting, and exports
- [www/styles.css](www/styles.css): app styling
- [data/example/](data/example): public example input files
- [data/synthetic/](data/synthetic): synthetic benchmark inputs
- [scripts/reproduce_results.R](scripts/reproduce_results.R): minimal reproduction script
- [HOSTING_GUIDE.md](HOSTING_GUIDE.md): deployment guide
- [TECHNICAL_BRIEFING_SWaNs.md](TECHNICAL_BRIEFING_SWaNs.md): implementation and operations briefing
- [swans_references.bib](swans_references.bib): BibTeX references for software and manuscript
- [renv.lock](renv.lock): dependency snapshot

## Requirements

- R 4.1 or newer
- Required R packages:
  - `shiny`
  - `bslib`
  - `readxl`
  - `htmltools`

The app uses base R graphics and a custom internal XLSX writer for summary exports.

The included `renv.lock` file is a dependency snapshot for reproducibility. It is not required to launch the app, but it helps recreate the tested package versions more consistently.

## Run locally

1. Install the required R packages.
2. Open the repository root in R or RStudio.
3. Start the app with:

```r
source("app.R")
```

or

```r
shiny::runApp()
```

## Reproducibility and data availability

The repository contains:

- a built-in 100-node example sewer network,
- public synthetic network and weight inputs for testing and demonstration,
- a minimal reproduction script, and
- a dependency snapshot in `renv.lock`.

Detailed real-world sewer-network data from the metropolitan case study are **not** included. These files are not publicly shared because they contain security-sensitive infrastructure information and were supplied under access restrictions.

The synthetic input files are included for transparency and reproducibility. They are provided **without a separate data license**.

## Citation

If you use SWaNs, please cite both the software and the companion manuscript.

Software citation metadata are provided in [CITATION.cff](CITATION.cff).

Companion manuscript:

Talleb Y, Pape L, Schmidt T, Nafo I, Moebus S, Ickstadt K, Schmiege D. *Source-tracking algorithms for wastewater-based epidemiology in urban sewer networks: Design and comparative evaluation in a German metropolitan area.* Submitted manuscript. Further bibliographic details will be added once available.

## License

The SWaNs source code in this repository is released under the GNU Affero General Public License, version 3 or later (`AGPL-3.0-or-later`). See [LICENSE](LICENSE).

Copyright (c) 2026 Yassine Talleb

## Contact

Yassine Talleb  
Corresponding contact for SWaNs  
<yassine.talleb@tu-dortmund.de>
