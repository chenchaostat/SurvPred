# SurvPred — Enrollment & Survival Event Prediction for Clinical Trials

**Version**: v1.0.0\
**Live App**: <https://chenchao.shinyapps.io/SurvPred/>\
**Platform**: shinyapps.io

------------------------------------------------------------------------

## Disclaimer

SurvPred is provided for research and educational purposes only. It is not intended to provide medical advice, clinical recommendations, regulatory guidance, or commercial decision-making support.

Users are responsible for independently validating all simulation assumptions, model outputs, and statistical conclusions before using them in any scientific, regulatory, or business context.

------------------------------------------------------------------------

## Overview

SurvPred is an interactive R Shiny application for forecasting **subject enrollment** and **survival events** in clinical trials. It supports the full workflow from study design through interim analysis, helping researchers make data-driven decisions with confidence.

### Key Capabilities

| Module | Description |
|----|----|
| **Enrollment Fitting** | Poisson, time-decay, B-spline, and piecewise Poisson models for accurate enrollment forecasting |
| **Survival Event Prediction** | Eight time-to-event models including exponential, Weibull, log-logistic, log-normal, piecewise exponential, Cox, spline, and model averaging |
| **Dropout Fitting** | Flexible censoring models with user-specified rates or data-driven fitting for realistic dropout simulation |
| **Prediction & Visualization** | Interactive Plotly charts with prediction intervals for enrollment completion and target event attainment |
| **Sensitivity Analysis** | Tornado plots showing the impact of parameter perturbations on prediction outcomes |
| **Report Download** | One-click HTML report export |

------------------------------------------------------------------------

## Project Structure

```         
SurvPred_shiny/
├── app.R                  # Main application (UI + server logic)
├── R/
│   ├── utilities.R        # Utility functions (piecewise exponential distributions, etc.)
│   ├── data.R             # Built-in dataset documentation
│   ├── summarizeObserved.R # Observed data summary statistics
│   ├── fitEnrollment.R    # Enrollment model fitting
│   ├── fitEvent.R         # Time-to-event model fitting
│   ├── fitDropout.R       # Time-to-dropout model fitting
│   ├── predictEnrollment.R # Enrollment prediction
│   ├── predictEvent.R     # Event prediction
│   └── getPrediction.R    # Master prediction dispatch
├── data/
│   ├── interimData1.rda   # Example data: enrollment ongoing
│   ├── interimData2.rda   # Example data: enrollment complete
│   └── finalData.rda      # Example data: target events reached
├── www/
│   ├── css/main.css       # Custom styling
│   ├── js/auth.js         # Authentication helper
│   └── images/logo.svg    # Application logo
└── rsconnect/             # shinyapps.io deployment config
```

------------------------------------------------------------------------

## Quick Start

### Use the hosted version

Visit <https://chenchao.shinyapps.io/SurvPred/> — no installation required.

### Run locally

``` r
# Install dependencies
install.packages(c(
  "shiny", "shinyMatrix", "shinyFeedback", "shinyjs", "shinybusy",
  "readxl", "writexl", "data.table", "DT", "purrr", "prompter",
  "ggplot2", "plotly", "bslib", "lrstat"
))

# Launch the app
shiny::runApp("path/to/SurvPred_shiny")
```

------------------------------------------------------------------------

## Data Format

Uploaded data must be an **.xlsx** file with the following columns:

| Column | Type | Description | Required |
|----|----|----|----|
| `trialsdt` | Date | Trial start date | Yes |
| `usubjid` | character | Unique subject identifier | Yes |
| `randdt` | Date | Randomization date | Yes |
| `cutoffdt` | Date | Data cutoff date | Yes |
| `time` | numeric | Days from randomization to event or censoring | For event prediction |
| `event` | 0/1 | Event indicator (1 = event, 0 = no event) | For event prediction |
| `dropout` | 0/1 | Dropout indicator (1 = dropout, 0 = no dropout) | For event prediction |
| `treatment` | numeric | Treatment arm number (1, 2, ...) | For stratified analysis |
| `treatment_description` | character | Human-readable treatment label | Optional |

> **Note**: If no file is uploaded in non-design stages, the app automatically loads the built-in `interimData1` dataset.

------------------------------------------------------------------------

## Application Panels

### Sidebar — Configuration & Parameters

The sidebar is organized into four sections:

#### Study Configuration

-   **Study Stage** — Choose the current trial phase:
    -   `Design stage` — No data required; predictions based on prior parameters
    -   `Real-time before enrollment completion` — Enrollment is ongoing; fit models to observed data
    -   `Real-time after enrollment completion` — Enrollment is finished; predict events and dropouts only
-   **Prediction Target** — What to predict:
    -   Before enrollment completion: `Enrollment only` or `Enrollment and event`
    -   After enrollment completion: `Event only` (fixed)
-   **Upload Data (.xlsx)** — Upload trial data (non-design stages only)

#### Prediction Targets

| Parameter | Description | Default |
|----|----|----|
| Target Enrollment | Total subjects to enroll (positive integer) | 300 |
| Target Events | Total events to reach (positive integer, ≤ target enrollment) | 200 |
| Prediction Interval | Confidence level for prediction bands | 95% |
| Years after cutoff | Number of years beyond the cutoff date to forecast | 4 |
| Event prediction at specific day? | Predict cumulative events at a chosen day | Off |

#### Display

Select which curves to show on the prediction plot: **Enrollment**, **Event**, **Dropout**, and/or **Ongoing** subjects.

#### Advanced Settings

| Setting | Description | Default |
|----|----|----|
| Stratify by Treatment | Fit separate models per treatment arm | Off |
| Number of Arms | Number of treatment arms (1–6) | 2 |
| Treatment allocation | Block randomization sizes per arm | Equal allocation |
| Fix Model Parameters | Use MLE point estimates instead of posterior draws | Off |
| Simulations | Number of Monte Carlo replications (100–10000) | 200 |
| Seed | Random seed for reproducibility | 2026 |

------------------------------------------------------------------------

### Data Summary

Available in non-design stages. Provides a comprehensive overview of the observed data:

-   **Key statistics**: trial start date, cutoff date, enrollment duration, subjects enrolled, events observed, dropouts, subjects at risk
-   **Available views**:
    -   `Pie chart of subject status` — Breakdown of subjects by status (event / dropout / ongoing)
    -   `Enrollment and event plot` — Cumulative enrollment and event curves over calendar time
    -   `Gantt chart for enrollment timeline` — Subject-level enrollment timeline
    -   `Kaplan-Meier plot for time to event` — KM survival curves
    -   `Nelson-Aalen cumulative hazard` — Non-parametric cumulative hazard estimate
    -   `Schoenfeld residual plot` — Proportional hazards diagnostic (requires stratification by treatment)
    -   `Kaplan-Meier plot for time to dropout` — KM curves for time to dropout

------------------------------------------------------------------------

### Enrollment Fit

#### Design Stage

Specify enrollment model priors directly:

| Model                 | Parameters                                          |
|-----------------------|-----------------------------------------------------|
| **Poisson**           | Monthly enrollment rate (subjects/month)            |
| **Time-decay**        | Base rate μ and decay rate δ                        |
| **Piecewise Poisson** | Starting time and enrollment rate for each interval |

#### Non-Design Stage

Choose between **model-based** (fit from data) or **user-specified** future enrollment rates.

**Model-based options**: - **Poisson** — Constant enrollment rate - **Time-decay** — Time-varying decay rate: $\lambda(t) = \frac{\mu}{\delta}(1 - e^{-\delta t})$ - **B-spline** — Flexible smoothed rate via B-spline basis functions (tuneable knots and lag for forward projection) - **Piecewise Poisson** — Piecewise constant rate with user-defined changepoints

**User-specified options**: - Constant rate - Piecewise constant rate (time axis in days from cutoff)

**Diagnostic views**: - `Fitted enrollment curve` — Model fit overlaid on observed cumulative enrollment - `Enrollment residuals` — Residual diagnostic plot

------------------------------------------------------------------------

### Event Fit

#### Design Stage

| Model | Required Parameters |
|----|----|
| **Exponential** | Hazard rate per treatment |
| **Weibull** | Shape and scale parameters per treatment |
| **Log-logistic** | Location (log scale) and scale (log scale) per treatment |
| **Log-normal** | Mean (log scale) and SD (log scale) per treatment |
| **Piecewise exponential** | Starting times and hazard rates per interval per treatment |

#### Non-Design Stage

Eight time-to-event models are available:

| Model | Description |
|----|----|
| Exponential | Constant hazard |
| Weibull | Monotonic hazard (increasing or decreasing) |
| Log-logistic | Non-monotonic hazard |
| Log-normal | Non-monotonic hazard |
| Piecewise exponential | Stepwise constant hazard |
| **Model averaging** | BIC-weighted combination of Weibull and log-normal — **recommended default** |
| Spline | Royston–Parmar flexible parametric model (tuneable knots and scale: hazard / odds / normal) |
| Cox | Semi-parametric Cox proportional hazards model |

**Diagnostic views**: - `Fitted survival curve` — Model-based survival curves vs. Kaplan-Meier - `Cox-Snell residuals` — Goodness-of-fit diagnostic - `Model comparison table` — AIC / BIC comparison across all fitted models - `Model comparison plot` — Side-by-side fitted curves for visual comparison

> **Model averaging** is the default because it synthesizes multiple parametric forms via information-criterion weighting, often yielding more robust predictions than any single model.

------------------------------------------------------------------------

### Dropout Fit

Mirrors the event model panel. Supported models:

-   **None** — No dropout (all censoring is administrative)
-   Exponential, Weibull, Log-logistic, Log-normal
-   Piecewise exponential
-   Model averaging
-   Spline, Cox

Both model-based (fitted from data) and user-specified dropout rates are supported, with the same diagnostic views as the event panel.

------------------------------------------------------------------------

### Prediction

After clicking **"Run Prediction"**, this panel displays:

-   **Prediction date summary**:
    -   Expected enrollment completion date with prediction interval
    -   Expected date to reach the target number of events with prediction interval
    -   If "Event prediction at specific day" is enabled, the expected cumulative event count at that day
-   **Prediction plot**: Interactive Plotly chart with calendar time on the x-axis showing:
    -   Cumulative enrollment (blue)
    -   Cumulative events (red)
    -   Cumulative dropouts (orange)
    -   Ongoing subjects (green)
    -   Semi-transparent prediction bands around each curve
-   **Download Report (HTML)** — Export the complete prediction report

------------------------------------------------------------------------

### Sensitivity

> **Prerequisite**: Run Prediction first before using this panel.

**Model assumptions**: The sensitivity module always uses Poisson (enrollment), Exponential (event), and Exponential (dropout) regardless of the model choices made in the fitting tabs. This ensures perturbed parameters have straightforward interpretations.

**Controls**: - **Perturbation range (%)** — e.g., ±20% - **Step size (%)** — Increment between scenarios - **Parameters to vary** — Enrollment rate and/or dropout rate

**Outputs**: - **Tornado plot** — Visualizes how perturbations to each parameter affect the predicted time to reach the target number of events - **Scenario details table** — Lists all perturbation combinations and their corresponding predictions

------------------------------------------------------------------------

### Reference

Lists the methodological references that underpin the application (see [References](#references) below).

------------------------------------------------------------------------

## Methodological Background

### Enrollment Models

-   **Poisson**: Homogeneous Poisson process with constant enrollment intensity.
-   **Time-decay**: Enrollment rate decays over time — $\lambda(t) = \frac{\mu}{\delta}(1 - e^{-\delta t})$ — capturing the typical slowdown in recruitment as a trial progresses.
-   **B-spline**: The log enrollment rate is modeled as a B-spline function of time, providing flexible, data-driven shape estimation.
-   **Piecewise Poisson**: Stepwise constant rates, useful when enrollment capacity changes at known calendar dates (e.g., new sites opening).

### Event-Time Models

-   **Parametric**: Exponential, Weibull, log-logistic, and log-normal distributions.
-   **Semi-parametric**: Cox proportional hazards model.
-   **Flexible parametric**: Royston–Parmar spline models (Royston & Parmar, 2002), where a transformation of the survival function (log cumulative hazard, log cumulative odds, or inverse normal) is modeled as a natural cubic spline of log time.
-   **Model averaging**: BIC-weighted average of Weibull and log-normal models. Weights are proportional to $\exp(-\text{BIC}/2)$.

### Prediction Algorithm

1.  Fit enrollment, event, and dropout models to observed data (or use user-specified priors at the design stage).
2.  Draw model parameters from the asymptotic posterior (or use MLE point estimates if "Fix Model Parameters" is enabled).
3.  Simulate arrival times for future subjects, event times for both ongoing and future subjects, and dropout times.
4.  Aggregate results across Monte Carlo replications to produce median predictions and prediction intervals.

------------------------------------------------------------------------

## Built-in Example Datasets

Three datasets are bundled with the app for demonstration. When no file is uploaded, `interimData1` is loaded by default.

| Dataset | Scenario | Sample Size |
|----|----|----|
| `interimData1` | Enrollment is still ongoing (mid-enrollment interim analysis) | 225 subjects |
| `interimData2` | Enrollment has completed (post-enrollment interim analysis) | 300 subjects |
| `finalData` | Target number of events has been reached (final analysis) | 300 subjects |

Each dataset contains the columns: `trialsdt`, `usubjid`, `randdt`, `treatment`, `treatment_description`, `time`, `event`, `dropout`, `cutoffdt`.

------------------------------------------------------------------------

## R Package Dependencies

| Package              | Purpose                          |
|----------------------|----------------------------------|
| `shiny`              | Web application framework        |
| `bslib`              | Bootstrap 5 theming              |
| `shinyMatrix`        | Matrix input widgets             |
| `shinyFeedback`      | Input validation feedback        |
| `shinyjs`            | JavaScript extensions            |
| `shinybusy`          | Loading spinners                 |
| `readxl` / `writexl` | Excel file I/O                   |
| `data.table`         | Fast data manipulation           |
| `DT`                 | Interactive data tables          |
| `ggplot2` / `plotly` | Static and interactive graphics  |
| `purrr`              | Functional programming utilities |
| `prompter`           | Tooltip helpers                  |
| `lrstat`             | Clinical trial statistics        |

------------------------------------------------------------------------

## Recommended Workflow

1.  **Select study stage** — Design, enrollment ongoing, or enrollment complete.
2.  **Upload data** (non-design stages) — Use your own .xlsx file or the built-in example data.
3.  **Set prediction targets** — Target enrollment, target events, prediction interval level, and forecast horizon.
4.  **Review the Data Summary tab** — Verify that key statistics and plots match your expectations.
5.  **Choose an enrollment model** — Inspect model fit and residuals in the Enrollment Fit tab.
6.  **Choose an event model** — Compare models via AIC/BIC and residual diagnostics; model averaging is recommended by default.
7.  **Choose a dropout model** — Select an appropriate dropout/censoring model.
8.  **Run prediction** — Click "Run Prediction" and review the results.
9.  **Sensitivity analysis** (optional) — Assess how parameter uncertainty affects predictions.
10. **Download the report** — Export results as an HTML file.

------------------------------------------------------------------------

## Important Notes

1.  **Cutoff date**: The `cutoffdt` column is the boundary between "observed" and "future." Make sure it reflects the actual data lock point.
2.  **Minimum event requirements**: Each event model has a minimum number of observed events needed for fitting:
    -   Exponential: ≥ 1 event
    -   Weibull, log-logistic, log-normal, model averaging: ≥ 2 events
    -   Spline: ≥ `k + 2` events (where `k` is the number of inner knots)
    -   If stratifying by treatment, each arm must independently satisfy these requirements.
3.  **Fix Model Parameters**: Enable this in Advanced Settings to align the Sensitivity baseline with the Prediction result (sensitivity always uses MLE point estimates).
4.  **Simulation count**: Higher values yield more precise prediction intervals at the cost of longer computation. Use 200 for exploration; 500–1000 for final reporting.
5.  **Random seed**: A fixed seed ensures reproducibility across sessions.
6.  **Browser**: Use the latest version of Chrome, Firefox, or Edge for the best experience.

------------------------------------------------------------------------

## References {#references}

1.  Bagiella E, Heitjan DF. Predicting analysis times in randomized clinical trials. *Statistics in Medicine*. 2001; 20:2055–2063.
2.  Ying GS, Heitjan DF. Weibull prediction of event times in clinical trials. *Pharmaceutical Statistics*. 2008; 7:107–120.
3.  Zhang X, Long Q. Stochastic modeling and prediction for accrual in clinical trials. *Statistics in Medicine*. 2010; 29:649–658.
4.  Royston P, Parmar MKB. Flexible parametric proportional-hazards and proportional odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. *Statistics in Medicine*. 2002; 21:2175–2197.
5.  Kaifeng Lu. *Event Prediciton*. 2026.

------------------------------------------------------------------------

## Deployment Info

-   **Host**: shinyapps.io
-   **Account**: chenchao
-   **App ID**: 17566866
-   **URL**: <https://chenchao.shinyapps.io/SurvPred/>
