# Teachers' Media Competence and Its Classroom Integration: A Transposition Gap Shared by Women and Men in Basic Education in the Dominican Republic

Data and analysis code for the article of the same title.

The script `analysis.R` reproduces Tables I–IV, Fig. 1 and all statistics reported in the text from the anonymized dataset in `data/`.

## Repository structure

```
.
├── analysis.R                          Analysis script
├── data/
│   └── ami_alfamed_dr_teachers.csv     Anonymized dataset (N = 826; 705 retained)
└── output/                             Created when the script is run
    ├── tables/                         Tables I–IV (.docx)
    ├── figures/                        Fig. 1 (.png, .tiff; 600 dpi)
    └── results/                        Unrounded results (.csv), reported statistics (.txt), session information
```

## How to run

Requirements: R (≥ 4.1). The script installs any missing packages: `ggplot2`, `flextable`, `officer`, `car`, `MASS`, `psych`, `GPArotation` and `pwr`.

From R or RStudio:

```r
source("analysis.R")
```

From a terminal:

```
Rscript analysis.R
```

The script sets the working directory to its own location, so it can be run from any folder.

## Data

The data come from an online administration of the AMI-ALFAMED questionnaire for teachers to in-service basic education (primary) teachers in the Dominican Republic. All participants gave informed consent. The study was approved by the Research Ethics Committee of the University of Córdoba, Spain (ref. CEIH-24-06).

The dataset contains no names, institution names, contact details or open-ended responses.

### Codebook

| Variable | Description | Values |
|---|---|---|
| `id` | Sequential identifier | 1–826 |
| `excluded` | Excluded for low response quality | 0 = retained, 1 = excluded |
| `exclusion_reason` | Reason for exclusion (empty if retained) | `full_straightlining`, `block_straightlining`, `extremely_low_variance`, `excessive_missing`, `inconsistent_demographics` |
| `gender` | Gender | `female`, `male` |
| `age_group` | Age group (years) | 1 = <25, 2 = 26–34, 3 = 35–44, 4 = 45–54, 5 = >54 |
| `experience` | Teaching experience (years) | 1 = <5, 2 = 5–15, 3 = 16–25, 4 = >25 |
| `degree` | Highest academic degree | 1 = bachelor's, 2 = postgraduate, 3 = other |
| `institution_type` | Type of institution | 1 = public, 2 = private, 3 = other |
| `university_teacher` | Also teaches at university | 0 = no, 1 = yes |
| `mil_courses` | Media and information literacy courses received | 0 = none, 1, 2, 3, 4 = more than 3 |
| `home_*`, `work_*` | Devices available at home / at work: `tablet`, `laptop`, `desktop`, `ebook`, `smartphone`, `other` | 0 = no, 1 = yes |
| `Bx_Dy_Pk` | Personal competence item *k* of block *x*, subdimension *y* | 1–4 |
| `Bx_Dy_INT` | Classroom integration item of block *x*, subdimension *y* | 1–4; empty = no response |

Items are rated on a four-point scale: 1 = never / not at all; 4 = always / completely.

### Instrument structure

| Code | Block | Subdimension | Personal items | Integration item |
|---|---|---|---|---|
| B1-D1 | Knowledge | Access to and retrieval of information | 4 | 1 |
| B1-D2 | Knowledge | Language | 5 | 1 |
| B1-D3 | Knowledge | Technology | 4 | 1 |
| B1-D4 | Knowledge | Production and dissemination processes | 3 | 1 |
| B1-D5 | Knowledge | Politics and the media industry | 5 | 1 |
| B2-D1 | Understanding | Ideology and values | 5 | 1 |
| B2-D2 | Understanding | Reception and understanding | 5 | 1 |
| B3-D1 | Expression | Civic participation | 4 | 1 |
| B3-D2 | Expression | Creation | 3 | 1 |
| B3-D3 | Expression | Communication | 3 | 1 |

## Analyses

- **Scores.** Personal competence is the mean of the personal items of each subdimension or block. Classroom integration is the integration item of each subdimension. Gap = integration − personal competence, computed for each participant and subdimension.
- **Gender comparisons.** Student's *t* tests with pooled variance, Cohen's *d* and 95% confidence intervals. Benjamini–Hochberg FDR correction is applied separately to subdimensions (k = 10), blocks (k = 3) and integration items (k = 10). Mann–Whitney *U* tests are run as a robustness check.
- **Covariates.** ANCOVA on block scores with age group and teaching experience as ordinal covariates (type III sums of squares), including the homogeneity-of-slopes test, Levene's test and Shapiro–Wilk tests on the model residuals. The models are re-estimated with both covariates entered as categorical factors.
- **Integration of communication (B3-D3).** Ordinal logistic regression on gender, age group and teaching experience.
- **Gap.** Between-gender comparison of the gap (ΔGap = women − men), one-sample *t* tests against zero within each gender, and robustness checks with Mann–Whitney *U* and ANCOVA.
- **Moderation.** For each subdimension, a regression of the integration item on mean-centered personal competence, gender and their interaction (BH-FDR, k = 10).
- **Reliability.** Cronbach's α and McDonald's ω for each block, with listwise deletion.

Missing integration responses (B2-D2, B3-D2 and B3-D3) are handled by listwise deletion in each analysis.
