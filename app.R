library(shiny)
library(shinyMatrix)
library(shinyFeedback)
library(shinyjs, warn.conflicts = FALSE)
library(shinybusy)
library(readxl)
library(writexl)
library(data.table)
library(DT)
library(purrr)
library(prompter)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
library(bslib)
library(lrstat)

# ============================================================
# Source all SurvPred package functions (standalone deployment)
# ============================================================
source("R/utilities.R", local = FALSE)
source("R/data.R", local = FALSE)
source("R/summarizeObserved.R", local = FALSE)
source("R/fitEnrollment.R", local = FALSE)
source("R/fitEvent.R", local = FALSE)
source("R/fitDropout.R", local = FALSE)
source("R/predictEnrollment.R", local = FALSE)
source("R/predictEvent.R", local = FALSE)
source("R/getPrediction.R", local = FALSE)

# Load package datasets
load("data/interimData1.rda")
load("data/interimData2.rda")
load("data/finalData.rda")

# ============================================================
# Landing Page UI
# ============================================================

landing_page <- function() {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "images/logo.svg")
    ),

    # Hero Section
    tags$div(class = "hero-section",
      tags$div(class = "hero-badge", "v1.0.0  |  Clinical Trial Analytics"),
      tags$h1(class = "hero-title", "SurvPred"),
      tags$p(class = "hero-subtitle",
             "Advanced enrollment and survival event prediction for clinical trials. ",
             "From study design to interim analysis — make data-driven decisions with confidence."),
      tags$div(class = "hero-cta",
        actionButton("start_btn", "Get Started", icon = icon("rocket"),
                    class = "btn btn-hero-primary btn-lg"),
        tags$a(href = "#features", class = "btn btn-hero-outline btn-lg",
               icon("info-circle"), " Learn More")
      )
    ),

    # Features Section
    tags$div(class = "features-section", id = "features",
      tags$h2("Comprehensive Prediction Toolkit"),
      tags$div(class = "row",
        tags$div(class = "col-md-4",
          tags$div(class = "feature-card",
            tags$div(class = "feature-icon", icon("users")),
            tags$h3("Enrollment Fitting"),
            tags$p("Poisson, time-decay, B-spline, and piecewise Poisson models for accurate enrollment forecasting.")
          )
        ),
        tags$div(class = "col-md-4",
          tags$div(class = "feature-card",
            tags$div(class = "feature-icon", icon("heart-pulse")),
            tags$h3("Survival Event Prediction"),
            tags$p("Exponential, Weibull, log-logistic, log-normal, Cox, spline, and model averaging for robust time-to-event analysis.")
          )
        ),
        tags$div(class = "col-md-4",
          tags$div(class = "feature-card",
            tags$div(class = "feature-icon", icon("user-xmark")),
            tags$h3("Dropout Fitting"),
            tags$p("Flexible censoring models with user-specified rates or data-driven fitting for realistic dropout simulation.")
          )
        )
      )
    ),

    # Footer
    tags$div(class = "footer",
      tags$p("SurvPred — Survival Prediction for Clinical Trials")
    )
  )
}

# ============================================================
# Main Application UI
# ============================================================

# conditional panels for treatment allocation
f_treatment_allocation <- function(i) {
  conditionalPanel(
    condition = paste("input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("treatment_allocation_", i),
      label = tags$span(
        "Treatment allocation",
        tags$span(icon(name = "question-circle")) %>%
          add_prompt(message = "in a randomization block",
                     position = "right")),

      value = matrix(rep(1,i), ncol = 1,
                     dimnames = list(paste("Treatment", 1:i), "Size")),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE, editableNames=TRUE),
      cols = list(names=TRUE, extend=FALSE))
  )
}


f_exponential_survival <- function(i) {
  conditionalPanel(
    condition = paste("input.event_prior == 'Exponential' && input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("exponential_survival_", i),
      label = "Hazard rate for each treatment",
      value = matrix(rep(0.0030, i), nrow = 1,
                     dimnames = list(NULL, paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=FALSE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_weibull_survival <- function(i) {
  conditionalPanel(
    condition = paste("input.event_prior == 'Weibull' && input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("weibull_survival_", i),
      label = "Weibull parameters",
      value = matrix(rep(c(1.42, 392), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Shape", "Scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_llogis_survival <- function(i) {
  conditionalPanel(
    condition = paste("input.event_prior == 'Log-logistic' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("llogis_survival_", i),
      label = "Log-logistic parameters",
      value = matrix(rep(c(5.4, 1), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Location on log scale",
                                       "Scale on log scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_lnorm_survival <- function(i) {
  conditionalPanel(
    condition = paste("input.event_prior == 'Log-normal' && input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("lnorm_survival_", i),
      label = "Log-normal parameters",
      value = matrix(rep(c(5.4, 1), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Mean on log scale",
                                       "SD on log scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_piecewise_exponential_survival <- function(i) {
  conditionalPanel(
    condition = paste("input.event_prior == 'Piecewise exponential' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("piecewise_exponential_survival_", i),
      label = "Hazard rate by time interval for each treatment",
      value = matrix(c(0, rep(0.0030, i)), nrow = 1,
                     dimnames = list(
                       "Interval 1",
                       c("Starting time", paste("Treatment", 1:i)))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    ),

    actionButton(paste0("add_piecewise_exponential_survival_", i),
                 label=NULL, icon=icon("plus")),
    actionButton(paste0("del_piecewise_exponential_survival_", i),
                 label=NULL, icon=icon("minus"))
  )
}


f_exponential_dropout <- function(i) {
  conditionalPanel(
    condition = paste("input.dropout_prior == 'Exponential' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("exponential_dropout_", i),
      label = "Hazard rate for each treatment",
      value = matrix(rep(0.0003, i), nrow = 1,
                     dimnames = list(NULL, paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=FALSE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_weibull_dropout <- function(i) {
  conditionalPanel(
    condition = paste("input.dropout_prior == 'Weibull' && input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("weibull_dropout_", i),
      label = "Weibull parameters",
      value = matrix(rep(c(1.25, 1000), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Shape", "Scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_llogis_dropout <- function(i) {
  conditionalPanel(
    condition = paste("input.dropout_prior == 'Log-logistic' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("llogis_dropout_", i),
      label = "Log-logistic parameters",
      value = matrix(rep(c(8, 2.64), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Location on log scale",
                                       "Scale on log scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_lnorm_dropout <- function(i) {
  conditionalPanel(
    condition = paste("input.dropout_prior == 'Log-normal' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("lnorm_dropout_", i),
      label = "Log-normal parameters",
      value = matrix(rep(c(8, 2.64), i), nrow = 2, byrow = FALSE,
                     dimnames = list(c("Mean on log scale",
                                       "SD on log scale"),
                                     paste("Treatment", 1:i))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    )
  )
}


f_piecewise_exponential_dropout <- function(i) {
  conditionalPanel(
    condition = paste("input.dropout_prior == 'Piecewise exponential' &&
                      input.k ==", i),

    shinyMatrix::matrixInput(
      paste0("piecewise_exponential_dropout_", i),
      label = "Hazard rate by time interval for each treatment",
      value = matrix(c(0, rep(0.0003, i)), nrow = 1,
                     dimnames = list(
                       "Interval 1",
                       c("Starting time", paste("Treatment", 1:i)))),
      inputClass = "numeric",
      rows = list(names=TRUE, extend=FALSE),
      cols = list(names=TRUE, extend=FALSE)
    ),

    actionButton(paste0("add_piecewise_exponential_dropout_", i),
                 label=NULL, icon=icon("plus")),
    actionButton(paste0("del_piecewise_exponential_dropout_", i),
                 label=NULL, icon=icon("minus"))
  )
}


observedPanel <- tabPanel(
  title = "Data Summary",
  value = "observed_data_panel",

  htmlOutput("dates"),
  tags$br(),
  htmlOutput("statistics"),

  div(style = "margin-top: 1rem;",
    selectInput("observed_view", tags$b("Select view"),
      choices = c("Pie chart of subject status",
                  "Enrollment and event plot",
                  "Gantt chart for enrollment timeline",
                  "Kaplan-Meier plot for time to event",
                  "Nelson-Aalen cumulative hazard",
                  "Schoenfeld residual plot",
                  "Kaplan-Meier plot for time to dropout"),
                  # "Subject-level data"),  # BACKUP: kept for future use
      selected = "Enrollment and event plot",
      width = "100%")
  ),

  conditionalPanel(
    condition = "input.observed_view == 'Enrollment and event plot'",
    div(style = "margin-top: 1.5rem;", plotlyOutput("cum_accrual_plot", height = "520px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Gantt chart for enrollment timeline' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion')",
    div(style = "margin-top: 1.5rem;", plotlyOutput("gantt_plot", height = "530px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Kaplan-Meier plot for time to event' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion')",
    div(style = "margin-top: 0rem;", plotlyOutput("event_km_plot", height = "530px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Nelson-Aalen cumulative hazard' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion')",
    div(style = "margin-top: 0rem;", plotlyOutput("na_plot", height = "530px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Schoenfeld residual plot' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion') &&
                 input.by_treatment",
    div(style = "margin-top: 0rem;", plotlyOutput("schoenfeld_plot", height = "530px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Schoenfeld residual plot' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion') &&
                 !input.by_treatment",
    div(style = "margin-top: 5rem; text-align: center; color: #888; font-size: 16px;",
        "Schoenfeld residuals are used to test the proportional hazards assumption ",
        "for treatment effect. Please check ",
        tags$b("Stratify by Treatment"), " to enable this plot.")),

  conditionalPanel(
    condition = "input.observed_view == 'Kaplan-Meier plot for time to dropout' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion')",
    div(style = "margin-top: 0rem;", plotlyOutput("dropout_km_plot", height = "530px"))),

  conditionalPanel(
    condition = "input.observed_view == 'Pie chart of subject status' &&
                 (input.to_predict == 'Enrollment and event' ||
                  input.stage == 'Real-time after enrollment completion')",
    div(style = "margin-top: 1.5rem;", plotlyOutput("status_pie_plot", height = "530px"))),

  # BACKUP: Subject-level data panel — commented out, kept for future use
  # conditionalPanel(
  #   condition = "input.observed_view == 'Subject-level data' &&
  #                input.stage != 'Design stage'",
  #   div(style = "margin-top: 1.5rem;", DT::DTOutput("input_df")))
)


enrollmentPanel <- tabPanel(
  title = "Enrollment Fit",
  value = "enroll_model_panel",

  conditionalPanel(
    condition = "input.stage == 'Design stage'",

    fluidRow(
      column(6, radioButtons(
        "enroll_prior",
        label = "Which enrollment model to use?",
        choices = c("Poisson",
                    "Time-decay",
                    "Piecewise Poisson"),
        selected = "Poisson",
        inline = FALSE)
      ),

      column(6,
             conditionalPanel(
               condition = "input.enroll_prior == 'Poisson'",

               numericInput(
                 "poisson_rate",
                 label = "Monthly enrollment rate (subjects per month)",
                 value = 1,
                 min = 0, max = 100, step = 1)
             ),

             conditionalPanel(
               condition = "input.enroll_prior == 'Time-decay'",

               fluidRow(
                 column(6, numericInput(
                   "mu",
                   label = "Base rate, mu",
                   value = 1.5,
                   min = 0, max = 100, step = 1)
                 ),

                 column(6, numericInput(
                   "delta",
                   label = "Decay rate, delta",
                   value = 2,
                   min = 0, max = 100, step = 1)
                 )
               )
             ),

             conditionalPanel(
               condition = "input.enroll_prior == 'Piecewise Poisson'",

               shinyMatrix::matrixInput(
                 "piecewise_poisson_rate",
                 label = "Monthly enrollment rate by time interval (subjects per month)",
                 value = matrix(c(0,30), ncol = 2,
                                dimnames = list("Interval 1",
                                                c("Starting time",
                                                  "Enrollment rate"))),
                 inputClass = "numeric",
                 rows = list(names=TRUE, extend=FALSE),
                 cols = list(names=TRUE, extend=FALSE)),

               actionButton("add_piecewise_poisson_rate",
                            label=NULL, icon=icon("plus")),
               actionButton("del_piecewise_poisson_rate",
                            label=NULL, icon=icon("minus"))
             )
      )
    )
  ),

  conditionalPanel(
    condition = "input.stage != 'Design stage'",

    fluidRow(
      column(12, div(class = "section-label",
        radioButtons(
          "enroll_rate_method",
          label = "How to determine future enrollment rate?",
          choices = c("Model-based (fitted from data)" = "model_based",
                      "User-specified" = "user_specified"),
          selected = "model_based",
          inline = TRUE)
      ))
    ),

    fluidRow(
      column(12, htmlOutput("current_enroll_rate"))
    ),

    conditionalPanel(
      condition = "input.enroll_rate_method == 'model_based'",

      fluidRow(
        column(4, div(class = "section-label section-label-spaced",
          radioButtons(
            "enroll_model",
            label = "Which enrollment model to use?",
            choices = c("Poisson",
                        "Time-decay",
                        "B-spline",
                        "Piecewise Poisson"),
            selected = "Poisson",
            inline = FALSE)
        )),

        column(4, div(style = "margin-top: 1.5rem;",
               conditionalPanel(
                 condition = "input.enroll_model == 'B-spline'",
                 numericInput(
                   "nknots",
                   label = "How many inner knots to use?",
                   value = 0,
                   min = 0, max = 10, step = 1)
               )
        )),

        column(4, div(style = "margin-top: 1.5rem;",
               conditionalPanel(
                 condition = "input.enroll_model == 'B-spline'",
                 numericInput(
                   "lags",
                   label = paste("How many days before the last enrollment",
                                 "date to average",
                                 "the enrollment rate over for prediction?"),
                   value = 30,
                   min = 0, max = 365, step = 1)
               )
        ))
      ),

      conditionalPanel(
        condition = "input.enroll_model == 'Piecewise Poisson'",

        shinyMatrix::matrixInput(
          "accrualTime",
          label = "What is the starting time of each time interval?",
          value = matrix(0, ncol = 1,
                         dimnames = list("Interval 1",
                                         "Starting time")),
          inputClass = "numeric",
          rows = list(names=TRUE, extend=FALSE),
          cols = list(names=TRUE, extend=FALSE)),

        actionButton("add_accrualTime",
                     label=NULL, icon=icon("plus")),
        actionButton("del_accrualTime",
                     label=NULL, icon=icon("minus"))
      )
    ),

    conditionalPanel(
      condition = "input.enroll_rate_method == 'user_specified'",

      fluidRow(
        column(6, radioButtons(
          "user_rate_type",
          label = "What type of enrollment rate?",
          choices = c("Constant rate" = "constant",
                      "Piecewise constant" = "piecewise"),
          selected = "constant",
          inline = TRUE)
        )
      ),

      conditionalPanel(
        condition = "input.user_rate_type == 'constant'",

        fluidRow(
          column(6, numericInput(
            "user_constant_rate",
            label = paste("Future monthly enrollment rate",
                          "(subjects per month)"),
            value = 30,
            min = 0.01, max = 30000, step = 0.1)
          )
        )
      ),

      conditionalPanel(
        condition = "input.user_rate_type == 'piecewise'",

        shinyMatrix::matrixInput(
          "user_piecewise_rate",
          label = paste("Monthly enrollment rate by time interval",
                        "(days from cutoff)"),
          value = matrix(c(0, 30), ncol = 2,
                         dimnames = list(
                           "Interval 1",
                           c("Starting time (days from cutoff)",
                             "Enrollment rate"))),
          inputClass = "numeric",
          rows = list(names=TRUE, extend=FALSE),
          cols = list(names=TRUE, extend=FALSE)),

        actionButton("add_user_piecewise_rate",
                     label=NULL, icon=icon("plus")),
        actionButton("del_user_piecewise_rate",
                     label=NULL, icon=icon("minus"))
      )
    ),

    div(style = "margin-top: 1.2rem;",
      selectInput("enroll_view", tags$b("Select view"),
        choices = c("Fitted enrollment curve",
                    "Enrollment residuals"),
        selected = "Enrollment residuals",
        width = "100%")
    ),

    conditionalPanel(
      condition = "input.enroll_view == 'Fitted enrollment curve'",
      div(style = "margin-top: 0.5rem;", plotlyOutput("enroll_fit", height = "460px"))),

    conditionalPanel(
      condition = "input.enroll_view == 'Enrollment residuals' &&
                   input.enroll_rate_method == 'model_based'",
      div(style = "margin-top: 0.5rem;", plotlyOutput("enroll_resid", height = "460px")))
  )
)


eventPanel <- tabPanel(
  title = "Event Fit",
  value = "event_model_panel",

  conditionalPanel(
    condition = "input.stage == 'Design stage'",

    fluidRow(
      column(4, div(class = "section-label",
        radioButtons(
          "event_prior",
          label = "Which time-to-event model to use?",
          choices = c("Exponential",
                      "Weibull",
                      "Log-logistic",
                      "Log-normal",
                      "Piecewise exponential"),
          selected = "Piecewise exponential",
          inline = FALSE)
      )),


      column(8,
             lapply(1:6, f_exponential_survival),
             lapply(1:6, f_weibull_survival),
             lapply(1:6, f_llogis_survival),
             lapply(1:6, f_lnorm_survival),
             lapply(1:6, f_piecewise_exponential_survival)
      )
    )
  ),


  conditionalPanel(
    condition = "input.stage != 'Design stage'",

    fluidRow(
      column(12, div(class = "section-label",
        radioButtons(
          "event_rate_method",
          label = "How to determine future event rate?",
          choices = c("Model-based (fitted from data)" = "model_based",
                      "User-specified" = "user_specified"),
          selected = "model_based",
          inline = TRUE)
      ))
    ),

    fluidRow(
      column(12, htmlOutput("current_event_hazard"))
    ),

    conditionalPanel(
      condition = "input.event_rate_method == 'model_based'",

      fluidRow(
        column(8, div(class = "section-label section-label-spaced",
          div(class = "radio-grid-2",
            radioButtons(
              "event_model",
              label = "Which time-to-event model to use?",
              choices = c("Exponential",
                          "Weibull",
                          "Log-logistic",
                          "Log-normal",
                          "Piecewise exponential",
                          "Model averaging",
                          "Spline",
                          "Cox"),
              selected = "Model averaging",
              inline = FALSE)
          )
        )),

        column(4,
               conditionalPanel(
                 condition = "input.event_model == 'Piecewise exponential'",

                 shinyMatrix::matrixInput(
                   "piecewiseSurvivalTime",
                   label = "What is the starting time of each time interval?",
                   value = matrix(0, ncol = 1,
                                  dimnames = list("Interval 1",
                                                  "Starting time")),
                   inputClass = "numeric",
                   rows = list(names=TRUE, extend=FALSE),
                   cols = list(names=TRUE, extend=FALSE)),

                 actionButton("add_piecewiseSurvivalTime",
                              label=NULL, icon=icon("plus")),
                 actionButton("del_piecewiseSurvivalTime",
                              label=NULL, icon=icon("minus"))
               ),

               conditionalPanel(
                 condition = "input.event_model == 'Spline'",

                 numericInput(
                   "spline_k",
                   label = "How many inner knots to use?",
                   value = 1,
                   min = 0, max = 10, step = 1),

                 radioButtons(
                   "spline_scale",
                   label = "Which scale to model as a spline function?",
                   choices = c("hazard", "odds", "normal"),
                   selected = "hazard",
                   inline = TRUE)
               ),

               conditionalPanel(
                 condition = "input.event_model == 'Cox'",

                 numericInput(
                   "m_event",
                   label = paste("How many event time intervals to",
                                 "extrapolate the hazard function",
                                 "beyond the last observed event time?"),
                   value = 5,
                   min = 1, max = 10, step = 1)
               )
        )
      ),

      uiOutput("event_fit_ic"),
      div(style = "margin-top: 0.5rem;",
        selectInput("event_view", tags$b("Select view"),
          choices = c("Fitted survival curve",
                      "Cox-Snell residuals",
                      "Model comparison table",
                      "Model comparison plot"),
          selected = "Cox-Snell residuals",
          width = "100%")
      ),
      conditionalPanel(
        condition = "input.event_view == 'Fitted survival curve'",
        uiOutput("event_fit")),
      conditionalPanel(
        condition = "input.event_view == 'Cox-Snell residuals'",
        div(style = "margin-top: 0.5rem;", uiOutput("event_cs_resid"))),
      conditionalPanel(
        condition = "input.event_view == 'Model comparison table'",
        div(style = "margin-top: 0.5rem;", DT::DTOutput("event_compare_table"))),
      conditionalPanel(
        condition = "input.event_view == 'Model comparison plot'",
        div(style = "margin-top: 0.5rem;", uiOutput("event_compare_plots")))
    ),

    conditionalPanel(
      condition = "input.event_rate_method == 'user_specified'",

      fluidRow(
        column(6, numericInput(
          "user_event_hazard_rate",
          label = paste("Future event hazard rate",
                        "(events per subject per month)"),
          value = 0.05,
          min = 0.0001, max = 1, step = 0.001)
        )
      )
    )
  )
)


dropoutPanel <- tabPanel(
  title = "Dropout Fit",
  value = "dropout_model_panel",

  conditionalPanel(
    condition = "input.stage == 'Design stage'",

    fluidRow(
      column(4, radioButtons(
        "dropout_prior",
        label = "Which time-to-dropout model to use?",
        choices = c("None",
                    "Exponential",
                    "Weibull",
                    "Log-logistic",
                    "Log-normal",
                    "Piecewise exponential"),
        selected = "Exponential",
        inline = FALSE)
      ),

      column(8,
             lapply(1:6, f_exponential_dropout),
             lapply(1:6, f_weibull_dropout),
             lapply(1:6, f_llogis_dropout),
             lapply(1:6, f_lnorm_dropout),
             lapply(1:6, f_piecewise_exponential_dropout)
      )
    )
  ),

  conditionalPanel(
    condition = "input.stage != 'Design stage'",

    fluidRow(
      column(12, div(class = "section-label",
        radioButtons(
          "dropout_rate_method",
          label = "How to determine future dropout rate?",
          choices = c("Model-based (fitted from data)" = "model_based",
                      "User-specified" = "user_specified"),
          selected = "model_based",
          inline = TRUE)
      ))
    ),

    fluidRow(
      column(12, htmlOutput("current_dropout_hazard"))
    ),

    conditionalPanel(
      condition = "input.dropout_rate_method == 'model_based'",

      fluidRow(
        column(8, div(class = "section-label section-label-spaced",
          div(class = "radio-grid-3",
            radioButtons(
              "dropout_model",
              label = "Which time-to-dropout model to use?",
              choices = c("None",
                          "Exponential",
                          "Weibull",
                          "Log-logistic",
                          "Log-normal",
                          "Piecewise exponential",
                          "Model averaging",
                          "Spline",
                          "Cox"),
              selected = "Exponential",
              inline = FALSE)
          )
        )),


        column(4,
               conditionalPanel(
                 condition = "input.dropout_model == 'Piecewise exponential'",

                 shinyMatrix::matrixInput(
                   "piecewiseDropoutTime",
                   label = "What is the starting time of each time interval?",
                   value = matrix(0, ncol = 1,
                                  dimnames = list("Interval 1",
                                                  "Starting time")),
                   inputClass = "numeric",
                   rows = list(names=TRUE, extend=FALSE),
                   cols = list(names=TRUE, extend=FALSE)),

                 actionButton("add_piecewiseDropoutTime",
                              label=NULL, icon=icon("plus")),
                 actionButton("del_piecewiseDropoutTime",
                              label=NULL, icon=icon("minus"))
               ),

               conditionalPanel(
                 condition = "input.dropout_model == 'Spline'",

                 numericInput(
                   "spline_k_dropout",
                   label = "How many inner knots to use?",
                   value = 1,
                   min = 0, max = 10, step = 1),

                 radioButtons(
                   "spline_scale_dropout",
                   label = "Which scale to model as a spline function?",
                   choices = c("hazard", "odds", "normal"),
                   selected = "hazard",
                   inline = TRUE)
               ),

               conditionalPanel(
                 condition = "input.dropout_model == 'Cox'",

                 numericInput(
                   "m_dropout",
                   label = paste("How many dropout time intervals to",
                                 "extrapolate the hazard function",
                                 "beyond the last observed dropout time?"),
                   value = 5,
                   min = 1, max = 10, step = 1)
               )
        )
      ),

      uiOutput("dropout_fit_ic"),
      div(style = "margin-top: 0.5rem;",
        selectInput("dropout_view", tags$b("Select view"),
          choices = c("Fitted survival curve",
                      "Cox-Snell residuals",
                      "Model comparison table",
                      "Model comparison plot"),
          selected = "Cox-Snell residuals",
          width = "100%")
      ),
      conditionalPanel(
        condition = "input.dropout_view == 'Fitted survival curve'",
        uiOutput("dropout_fit")),
      conditionalPanel(
        condition = "input.dropout_view == 'Cox-Snell residuals'",
        div(style = "margin-top: 0.5rem;", uiOutput("dropout_cs_resid"))),
      conditionalPanel(
        condition = "input.dropout_view == 'Model comparison table'",
        div(style = "margin-top: 0.5rem;", DT::DTOutput("dropout_compare_table"))),
      conditionalPanel(
        condition = "input.dropout_view == 'Model comparison plot'",
        div(style = "margin-top: 0.5rem;", uiOutput("dropout_compare_plots")))
    ),

    conditionalPanel(
      condition = "input.dropout_rate_method == 'user_specified'",

      fluidRow(
        column(6, numericInput(
          "user_dropout_hazard_rate",
          label = paste("Future dropout hazard rate",
                        "(events per subject per month)"),
          value = 0.01,
          min = 0.0001, max = 1, step = 0.001)
        )
      )
    )
  )
)


predictPanel <- tabPanel(
  title = "Prediction",
  value = "prediction_results_panel",

  div(style = "margin-top: 1.2rem;", uiOutput("pred_date")),
  div(style = "margin-top: 1.5rem;", uiOutput("pred_plot")),
  div(style = "margin-top: 1.5rem; text-align: right;",
    downloadButton("download_report", "Download Report (HTML)",
                  class = "btn-download-report"))
)


sensitivityPanel <- tabPanel(
  title = "Sensitivity",
  value = "sensitivity_panel",

  # ── Prerequisite reminder ─────────────────────────────
  div(style = "margin-bottom: 1.2rem; padding: 12px 16px;
              background: #FFF3CD; border-left: 4px solid #FFC107;
              border-radius: 6px; font-size: 13px; color: #856404;",
    tags$b(icon("exclamation-triangle"), " Prerequisite:"),
    "Please run ", tags$b("Prediction"), " first before running Sensitivity Analysis. ",
    "For the Sensitivity baseline to match the Prediction result, check ",
    tags$b("\"Fix Model Parameters\""), " in the Advanced Settings panel. ",
    "(Sensitivity always fixes parameters at MLE; Prediction draws from the ",
    "posterior by default.)"
  ),

  # ── Model assumption notice ───────────────────────────
  div(style = "margin-bottom: 1.2rem; padding: 12px 16px;
              background: #E8F4FD; border-left: 4px solid #2E86AB;
              border-radius: 6px; font-size: 13px; color: #1B3A5C;",
    tags$b(icon("info-circle"), " Model assumptions:"),
    "The Sensitivity module always uses ", tags$b("Poisson"), " for enrollment, ",
    tags$b("Exponential"), " for time-to-event, and ",
    tags$b("Exponential"), " for dropout, regardless of the model choices made in the ",
    "Enrollment Fit / Event Fit / Dropout Fit tabs. ",
    "This ensures that the perturbed parameters have a straightforward interpretation ",
    "and that the sensitivity results are not confounded by complex model structures."
  ),

  # ── Controls ──────────────────────────────────────────
  div(class = "sidebar-section",
    div(class = "sidebar-section-header", "Sensitivity Analysis Settings"),
    div(class = "sidebar-section-body",
      fluidRow(
        column(4,
          sliderInput("sens_range", "Perturbation range (%)",
            min = 5, max = 50, value = c(-20, 20), step = 5, ticks = FALSE)),
        column(3,
          numericInput("sens_step", "Step size (%)",
            value = 10, min = 5, max = 25, step = 5)),
        column(5,
          checkboxGroupInput("sens_factors", "Parameters to vary",
            choices = c("Enrollment rate" = "enroll",
                        "Dropout rate" = "dropout"),
            selected = c("enroll", "dropout"),
            inline = TRUE))
      ),
      div(style = "margin-top: 0.8rem; text-align: center;",
        actionButton("run_sensitivity", "Run Sensitivity Analysis",
          class = "btn-predict-sidebar",
          style = "width: 70%; padding: 10px 0; font-size: 14px; font-weight: 700;"))
    )
  ),

  # ── Tornado Plot ──────────────────────────────────────
  div(style = "margin-top: 1.5rem;",
    htmlOutput("sensitivity_baseline")),
  div(style = "margin-top: 1rem;",
    plotlyOutput("tornado_plot", height = "450px")),

  # ── Results Table ─────────────────────────────────────
  div(style = "margin-top: 1.5rem;",
    tags$h4("Scenario Details"),
    DT::DTOutput("sensitivity_table"))
)


referencePanel <- tabPanel(
  title = "Reference",
  value = "reference_panel",

  div(style = "margin-top: 1.2rem;",
    tags$h4("References"),
    tags$hr(),
    tags$div(class = "reference-list",
      tags$p(
        tags$b("1. "),
        "Emilia Bagiella and Daniel F. Heitjan. Predicting analysis times in randomized clinical trials. ",
        tags$i("Statistics in Medicine"), ", 2001; 20:2055-2063."
      ),
      tags$p(
        tags$b("2. "),
        "Gui-shuang Ying and Daniel F. Heitjan. Weibull prediction of event times in clinical trials. ",
        tags$i("Pharmaceutical Statistics"), ", 2008; 7:107-120."
      ),
      tags$p(
        tags$b("3. "),
        "Xiaoxi Zhang and Qi Long. Stochastic modeling and prediction for accrual in clinical trials. ",
        tags$i("Statistics in Medicine"), ", 2010; 29:649-658."
      ),
      tags$p(
        tags$b("4. "),
        "Patrick Royston and Mahesh K. B. Parmar. Flexible parametric proportional-hazards and proportional odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. ",
        tags$i("Statistics in Medicine"), ", 2002; 21:2175-2197."
      ),
      tags$p(
        tags$b("5. "),
        "Lu K. ",
        tags$i("Event Prediction"), ". 2026."
      )
    )
  )
)


# reduced style fileInput
fileInputNoExtra<-function(inputId, label, multiple = FALSE, accept = NULL,
                           width = NULL, buttonLabel = "Browse...",
                           placeholder = "No file selected"){

  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file",
                         style = "display: none;",
                         `data-restore` = restoredValue)
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")

  tags$label(
    class = "input-group-btn",
    type="button",
    style=if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";",
             "padding-right: 5px; padding-bottom: 0px;
             display:inline-block;"),

    span(class = "btn btn-default btn-file",type="button",
         buttonLabel, inputTag,
         style=if (!is.null(width))
           paste0("width: ", validateCssUnit(width), ";",
                  "border-radius: 4px; padding-bottom:5px;"))
  )
}


# user interface ----------------
ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "images/logo.svg")
    ),
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(),
    # Landing page (shown first)
    div(id = "landing-wrapper", landing_page()),
    # Main app (hidden until user clicks Get Started)
    shinyjs::hidden(div(id = "main-wrapper", main_app_ui()))
  )
}

# main prediction app UI
main_app_ui <- function() {
  bslib::page(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#1B3A5C",
      secondary = "#2E86AB",
      "card-border-radius" = "0.75rem",
      "enable-shadows" = TRUE
    ),
    title = "SurvPred — Clinical Trial Survival Prediction",
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    prompter::use_prompt(),
    shinybusy::add_busy_spinner(),

    # Modern header bar
    div(class = "main-header",
      div(class = "header-brand",
        div(class = "header-logo", "SP"),
        div(class = "header-title", "SurvPred")
      ),
      div(class = "header-actions",
        actionButton("logout_btn", NULL, icon = icon("sign-out-alt"),
                    class = "btn-logout", title = "Logout")
      )
    ),

  sidebarLayout(
    sidebarPanel(class = "sidebar-modern",

      # ── Section 1: Study Configuration ──────────────────────
      div(class = "sidebar-section",
        div(class = "sidebar-section-header", "Study Configuration"),
        div(class = "sidebar-section-body",
          fluidRow(
            column(6,
              radioButtons("stage", label = "Study Stage",
                choices = c("Design stage",
                            "Enrollment Ongoing" = "Real-time before enrollment completion",
                            "Enrollment Complete" = "Real-time after enrollment completion"),
                selected = "Real-time before enrollment completion")),
            column(6,
              conditionalPanel(
                condition = "input.stage == 'Design stage' ||
                             input.stage == 'Real-time before enrollment completion'",
                radioButtons("to_predict", label = "Prediction Target",
                  choices = c("Enrollment only", "Enrollment and event"),
                  selected = "Enrollment and event")),
              conditionalPanel(
                condition = "input.stage == 'Real-time after enrollment completion'",
                radioButtons("to_predict2", label = "Prediction Target",
                  choices = c("Event only"), selected = "Event only"))
            )
          ),
          conditionalPanel(
            condition = "input.stage != 'Design stage'",
            fileInput("file1", label = "Upload Data (.xlsx)", accept = ".xlsx")
          )
        )
      ),

      # ── Section 2: Prediction Targets ──────────────────────
      div(class = "sidebar-section",
        div(class = "sidebar-section-header", "Prediction Targets"),
        div(class = "sidebar-section-body",
          fluidRow(
            column(6,
              conditionalPanel(
                condition = "input.stage == 'Design stage' ||
                             input.stage == 'Real-time before enrollment completion'",
                numericInput("target_n", label = "Target Enrollment",
                  value = 300, min = 1, max = 20000, step = 1))
            ),
            column(6,
              conditionalPanel(
                condition = "input.to_predict == 'Enrollment and event' ||
                             input.stage == 'Real-time after enrollment completion'",
                numericInput("target_d", label = "Target Events",
                  value = 200, min = 1, max = 10000, step = 1))
            )
          ),
          fluidRow(
            column(6, radioButtons("pilevel", label = "Prediction Interval",
              choices = c("95%" = "0.95", "90%" = "0.90", "80%" = "0.80"),
              selected = "0.95", inline = TRUE)),
            column(6, numericInput("nyears", label = "Years after cutoff",
              value = 4, min = 1, max = 10, step = 1))
          ),
          conditionalPanel(
            condition = "input.to_predict == 'Enrollment and event' ||
                         input.stage == 'Real-time after enrollment completion'",
            fluidRow(
              column(6,
                checkboxInput("pred_at_t",
                  label = "Event prediction at specific day?", value = FALSE)),
              column(6,
                conditionalPanel(
                  condition = "input.pred_at_t",
                  numericInput("target_t", label = "Target Day",
                    value = 180, min = 1, max = 2000, step = 1)))
            )
          )
        )
      ),

      # ── Section 3: Display ─────────────────────────────────
      conditionalPanel(
        condition = "input.to_predict == 'Enrollment and event' ||
                     input.stage == 'Real-time after enrollment completion'",
        div(class = "sidebar-section",
          div(class = "sidebar-section-header", "Display"),
          div(class = "sidebar-section-body",
            checkboxGroupInput("to_show", label = "Display Curves",
              choices = c("Enrollment", "Event", "Dropout", "Ongoing"),
              selected = c("Enrollment", "Event", "Dropout", "Ongoing"),
              inline = TRUE)
          )
        )
      ),

      # ── Section 4: Advanced Settings ───────────────────────
      div(class = "sidebar-section",
        div(class = "sidebar-section-header sidebar-section-collapsible",
          "Advanced Settings"),
        div(class = "sidebar-section-body",
          fluidRow(
            column(6, checkboxInput("by_treatment",
              label = "Stratify by Treatment", value = FALSE)),
            column(6, conditionalPanel(
              condition = "input.stage == 'Design stage' || input.by_treatment",
              selectInput("k", label = "Number of Arms",
                choices = seq_len(6), selected = 2)))
          ),
          conditionalPanel(
            condition = "input.stage == 'Design stage' ||
                         (input.by_treatment &&
                          input.stage != 'Real-time after enrollment completion')",
            lapply(2:6, f_treatment_allocation)
          ),
          checkboxInput("fix_parameter",
            label = "Fix Model Parameters", value = FALSE),
          fluidRow(
            column(6, numericInput("nreps", label = "Simulations",
              value = 200, min = 100, max = 10000, step = 1)),
            column(6, numericInput("seed", label = "Seed",
              value = 2026, min = 0, max = 100000, step = 1))
          )
        )
      ),

      # ── Run Prediction Button ──────────────────────────────
      div(style = "text-align: center; margin-top: 0.8rem;",
        actionButton("predict", "Run Prediction",
          class = "btn-predict-sidebar",
          style = "width: 70%; padding: 10px 0; font-size: 14px; font-weight: 700;")),

      # ── Workflow hint ────────────────────────────────────
      div(style = "text-align: left; margin-top: 0.6rem; padding: 0 8px;",
        tags$p(style = "font-size: 11px; color: #777; line-height: 1.5;
                       margin-bottom: 0;",
          icon("info-circle"), tags$b(" Workflow:"),
          " ① Fill in all parameters in this sidebar.",
          " ② In the right-side tabs, review ", tags$b("Data Summary"), ",",
          " then choose models under ", tags$b("Enrollment Fit"), " (e.g. Poisson), ",
          tags$b("Event Fit"), " (e.g. Model averaging), and ",
          tags$b("Dropout Fit"), " (e.g. Exponential).",
          " ③ Click ", tags$b("Run Prediction"), " above."))
    ),


    mainPanel(
      tabsetPanel(
        id = "results",
        observedPanel,
        enrollmentPanel,
        eventPanel,
        dropoutPanel,
        predictPanel,
        sensitivityPanel,
        referencePanel
      )
    )
  )
)
}


# ============================================================
# Plotly Styling Helper (grid lines + Arial font)
# ============================================================
style_plotly <- function(p, top_margin = 60) {
  if (is.null(p)) return(NULL)
  p %>% plotly::layout(
    font = list(family = "Arial", size = 14),
    margin = list(t = top_margin),
    xaxis = list(showline = TRUE, linewidth = 1, linecolor = "grey85",
                 showgrid = TRUE, gridcolor = "grey92",
                 tickfont = list(size = 14)),
    yaxis = list(showline = TRUE, linewidth = 1, linecolor = "grey85",
                 showgrid = TRUE, gridcolor = "grey92",
                 tickfont = list(size = 14))
  )
}

# server function -------------
server <- function(input, output, session) {

  # ==========================================================
  # Page Router — transition from landing page to main app
  # ==========================================================
  observeEvent(input$start_btn, {
    shinyjs::hide("landing-wrapper")
    shinyjs::show("main-wrapper")
  })

  observeEvent(input$logout_btn, {
    shinyjs::hide("main-wrapper")
    shinyjs::show("landing-wrapper")
  })


  # whether to show or hide the observed data panel
  observeEvent(input$stage, {
    if (input$stage != "Design stage") {
      showTab(inputId = "results", target = "observed_data_panel")
    } else {
      hideTab(inputId = "results", target = "observed_data_panel")
    }
  })


  # whether to allow the user to specify the number of treatments
  observeEvent(input$stage, {
    shinyjs::toggleState("k", input$stage == "Design stage")
  })


  # what to predict at different stages
  to_predict <- reactive({
    stage <- input$stage
    if (is.null(stage)) return("Enrollment and event")
    if (stage != "Real-time after enrollment completion") {
      input$to_predict
    } else {
      input$to_predict2
    }
  })


  # whether to show or hide enrollment, event, and dropout panels
  observeEvent(to_predict(), {
    if (to_predict() == "Enrollment only") {
      showTab(inputId = "results", target = "enroll_model_panel")
      hideTab(inputId = "results", target = "event_model_panel")
      hideTab(inputId = "results", target = "dropout_model_panel")
    } else if (to_predict() == "Enrollment and event") {
      showTab(inputId = "results", target = "enroll_model_panel")
      showTab(inputId = "results", target = "event_model_panel")
      showTab(inputId = "results", target = "dropout_model_panel")
    } else if (to_predict() == "Event only") {
      hideTab(inputId = "results", target = "enroll_model_panel")
      showTab(inputId = "results", target = "event_model_panel")
      showTab(inputId = "results", target = "dropout_model_panel")
    }
  })


  # Sensitivity tab: hidden at design stage, shown after first prediction run
  observeEvent(input$stage, {
    if (input$stage == "Design stage") {
      hideTab(inputId = "results", target = "sensitivity_panel")
    }
  })

  observeEvent(pred(), {
    if (input$stage != "Design stage") {
      showTab(inputId = "results", target = "sensitivity_panel")
    }
  })


  target_n <- reactive({
    req(input$target_n)
    valid = (input$target_n > 0 && input$target_n == round(input$target_n))
    shinyFeedback::feedbackWarning(
      "target_n", !valid,
      "Target enrollment must be a positive integer")
    req(valid)
    as.numeric(input$target_n)
  })


  target_d <- reactive({
    req(input$target_d)
    valid1 = (input$target_d > 0 && input$target_d == round(input$target_d))
    shinyFeedback::feedbackWarning(
      "target_d", !valid1,
      "Target events must be a positive integer")

    if (to_predict() == "Enrollment and event") {
      valid2 = (input$target_d <= input$target_n)
      shinyFeedback::feedbackWarning(
        "target_d", !valid2,
        "Target events must be less than or equal to target enrollment")
    } else {
      valid2 = (input$target_d <= observed()$n0)
      shinyFeedback::feedbackWarning(
        "target_d", !valid2,
        "Target events must be less than or equal to sample size")
    }

    req(valid1 && valid2)

    as.numeric(input$target_d)
  })


  target_t <- reactive({
    req(input$target_t)
    valid1 = (input$target_t > 0 && input$target_t == round(input$target_t))
    shinyFeedback::feedbackWarning(
      "target_t", !valid1,
      "Target days must be a positive integer")

    valid2 = (input$target_t <= input$nyears*365)
    shinyFeedback::feedbackWarning(
      "target_t", !valid2,
      "Target days must be less than or equal to 365 x years after cutoff")

    req(valid1 && valid2)

    as.numeric(input$target_t)
  })


  nyears <- reactive({
    req(input$nyears)
    valid = (input$nyears > 0)
    shinyFeedback::feedbackWarning(
      "nyears", !valid,
      "Years after cutoff must be a positive number")
    req(valid)
    as.numeric(input$nyears)
  })


  pilevel <- reactive(as.numeric(input$pilevel))


  showEnrollment <- reactive({
    "Enrollment" %in% input$to_show
  })


  showEvent <- reactive({
    "Event" %in% input$to_show
  })


  showDropout <- reactive({
    "Dropout" %in% input$to_show
  })


  showOngoing <- reactive({
    "Ongoing" %in% input$to_show
  })


  nreps <- reactive({
    req(input$nreps)
    valid = (input$nreps > 0 && input$nreps == round(input$nreps))
    shinyFeedback::feedbackWarning(
      "nreps", !valid,
      "Number of simulations must be a positive integer")
    req(valid)
    as.numeric(input$nreps)
  })


  k <- reactive({
    stage <- input$stage
    if (is.null(stage)) return(1)
    if (!isTRUE(input$by_treatment) && stage != "Design stage") {
      k = 1
    } else if (stage != "Design stage" && !is.null(df())) {
      k = length(table(df()$treatment))
      updateSelectInput(session, "k", selected=k)
    } else {
      k = as.numeric(input$k)
    }
    k
  })


  treatment_allocation <- reactive({
    req(k())
    if (k() > 1) {
      d = input[[paste0("treatment_allocation_", k())]]
      d <- as.numeric(d)

      valid = all(d > 0 & d == round(d))
      if (!valid) {
        showNotification("Treatment allocation must be positive integers")
      }
      req(valid)
      d
    } else {
      1
    }
  })


  treatment_description <- reactive({
    stage <- input$stage
    k_val <- k()
    req(k_val)
    if (k_val > 1) {
      if (!isTRUE(input$by_treatment) && !identical(stage, "Design stage")) {
        a = "Overall"
      } else if (!identical(stage, "Design stage") && !is.null(df())) {
        treatment_mapping <- df()[
          , .(treatment, treatment_description)][
          , .SD[.N], by = "treatment"]

        a = treatment_mapping$treatment_description
      } else {
        a = rownames(input[[paste0("treatment_allocation_", k())]])
      }
    } else {
      a = "Overall"
    }
    a
  })


  observeEvent(treatment_description(), {
    if (input$stage == "Design stage") {
      updateMatrixInput(
        session, paste0("exponential_survival_", k()),
        value=matrix(exponential_survival(), ncol=k(),
                     dimnames = list(NULL, treatment_description())))

      updateMatrixInput(
        session, paste0("weibull_survival_", k()),
        value=matrix(weibull_survival(), nrow=2, ncol=k(),
                     dimnames = list(c("Shape", "Scale"),
                                     treatment_description())))

      updateMatrixInput(
        session, paste0("llogis_survival_", k()),
        value=matrix(llogis_survival(), nrow=2, ncol=k(),
                     dimnames = list(c("Location on log scale",
                                       "Scale on log scale"),
                                     treatment_description())))

      updateMatrixInput(
        session, paste0("lnorm_survival_", k()),
        value=matrix(lnorm_survival(), nrow=2, ncol=k(),
                     dimnames = list(c("Mean on log scale",
                                       "SD on log scale"),
                                     treatment_description())))

      npieces = nrow(piecewise_exponential_survival())
      updateMatrixInput(
        session, paste0("piecewise_exponential_survival_", k()),
        value=matrix(piecewise_exponential_survival(),
                     nrow=npieces, ncol=k()+1,
                     dimnames = list(
                       paste("Interval", seq_len(npieces)),
                       c("Starting time", treatment_description()))))

      updateMatrixInput(
        session, paste0("exponential_dropout_", k()),
        value=matrix(exponential_dropout(), ncol=k(),
                     dimnames = list(NULL, treatment_description())))

      updateMatrixInput(
        session, paste0("weibull_dropout_", k()),
        value=matrix(weibull_dropout(), nrow=2, ncol=k(),
                     dimnames = list(c("Shape", "Scale"),
                                     treatment_description())))

      updateMatrixInput(
        session, paste0("llogis_dropout_", k()),
        value=matrix(llogis_dropout(), nrow=2, ncol=k(),
                     dimnames = list(c("Location on log scale",
                                       "Scale on log scale"),
                                     treatment_description())))

      updateMatrixInput(
        session, paste0("lnorm_dropout_", k()),
        value=matrix(lnorm_dropout(), nrow=2, ncol=k(),
                     dimnames = list(c("Mean on log scale",
                                       "SD on log scale"),
                                     treatment_description())))

      npieces = nrow(piecewise_exponential_dropout())
      updateMatrixInput(
        session, paste0("piecewise_exponential_dropout_", k()),
        value=matrix(piecewise_exponential_dropout(),
                     nrow=npieces, ncol=k()+1,
                     dimnames = list(
                       paste("Interval", seq_len(npieces)),
                       c("Starting time", treatment_description()))))
    } else if (input$by_treatment && !is.null(df())) {
      updateMatrixInput(
        session, paste0("treatment_allocation_", k()),
        value=matrix(treatment_allocation(), ncol = 1,
                     dimnames = list(treatment_description(), "Size")))
    }
  })


  poisson_rate <- reactive({
    req(input$poisson_rate)
    valid = (input$poisson_rate > 0)
    shinyFeedback::feedbackWarning(
      "poisson_rate", !valid,
      "Monthly enrollment rate must be a positive number")
    req(valid)
    as.numeric(input$poisson_rate)
  })


  mu <- reactive({
    req(input$mu)
    valid = (input$mu > 0)
    shinyFeedback::feedbackWarning(
      "mu", !valid,
      "Base rate must be a positive number")
    req(valid)
    as.numeric(input$mu)
  })


  delta <- reactive({
    req(input$delta)
    valid = (input$delta > 0)
    shinyFeedback::feedbackWarning(
      "delta", !valid,
      "Decay rate must be a positive number")
    req(valid)
    as.numeric(input$delta)
  })


  piecewise_poisson_rate <- reactive({
    req(input$piecewise_poisson_rate)
    t = as.numeric(input$piecewise_poisson_rate[,1])
    lambda = as.numeric(input$piecewise_poisson_rate[,2])

    valid1 = all(diff(t) > 0) && (t[1] == 0)
    if (!valid1) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }

    valid2 = all(lambda >= 0)
    if (!valid2) {
      showNotification(
        "Enrollment rate must be nonnegative"
      )
    }

    valid3 = any(lambda > 0)
    if (!valid3) {
      showNotification(
        "At least one enrollment rate must be positive"
      )
    }

    req(valid1 && valid2 && valid3)

    matrix(c(t, lambda), ncol = 2,
           dimnames = list(paste("Interval", 1:length(t)),
                           c("Starting time", "Enrollment rate")))
  })


  nknots <- reactive({
    req(input$nknots)
    valid = (input$nknots >= 0 && input$nknots == round(input$nknots))
    shinyFeedback::feedbackWarning(
      "nknots", !valid,
      "Number of inner knots must be a nonnegative integer")
    req(valid)
    as.numeric(input$nknots)
  })


  lags <- reactive({
    req(input$lags)
    valid = (input$lags >= 0 && input$lags == round(input$lags))
    shinyFeedback::feedbackWarning(
      "lags", !valid,
      "Number of day lags must be a nonnegative integer")
    req(valid)
    as.numeric(input$lags)
  })


  accrualTime <- reactive({
    t = as.numeric(input$accrualTime)
    valid = all(diff(t) > 0) && (t[1] == 0)
    if (!valid) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }
    req(valid)
    t
  })


  # user-specified enrollment rate: constant
  user_constant_rate <- reactive({
    req(input$user_constant_rate)
    valid = (input$user_constant_rate > 0)
    shinyFeedback::feedbackWarning(
      "user_constant_rate", !valid,
      "Monthly enrollment rate must be a positive number")
    req(valid)
    as.numeric(input$user_constant_rate)
  })


  # user-specified enrollment rate: piecewise
  user_piecewise_rate <- reactive({
    req(input$user_piecewise_rate)
    t = as.numeric(input$user_piecewise_rate[,1])
    lambda = as.numeric(input$user_piecewise_rate[,2])

    valid1 = all(diff(t) > 0) && (t[1] == 0)
    if (!valid1) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }

    valid2 = all(lambda >= 0)
    if (!valid2) {
      showNotification(
        "Enrollment rate must be nonnegative"
      )
    }

    valid3 = any(lambda > 0)
    if (!valid3) {
      showNotification(
        "At least one enrollment rate must be positive"
      )
    }

    req(valid1 && valid2 && valid3)

    matrix(c(t, lambda), ncol = 2,
           dimnames = list(paste("Interval", 1:length(t)),
                           c("Starting time (days from cutoff)",
                             "Enrollment rate")))
  })

  # user-specified dropout hazard rate
  user_dropout_hazard_rate <- reactive({
    req(input$user_dropout_hazard_rate)
    valid = (input$user_dropout_hazard_rate > 0)
    shinyFeedback::feedbackWarning(
      "user_dropout_hazard_rate", !valid,
      "Dropout hazard rate must be a positive number")
    req(valid)
    as.numeric(input$user_dropout_hazard_rate)
  })

  # user-specified event hazard rate
  user_event_hazard_rate <- reactive({
    req(input$user_event_hazard_rate)
    valid = (input$user_event_hazard_rate > 0)
    shinyFeedback::feedbackWarning(
      "user_event_hazard_rate", !valid,
      "Event hazard rate must be a positive number")
    req(valid)
    as.numeric(input$user_event_hazard_rate)
  })


  # combined user enrollment rate for runPrediction
  # Note: user enters rates in subjects/month, convert to subjects/day for model
  user_enroll_rate <- reactive({
    if (input$enroll_rate_method == "user_specified") {
      if (input$user_rate_type == "constant") {
        user_constant_rate() / 30.4375
      } else {
        user_piecewise_rate()[,2] / 30.4375
      }
    } else {
      NULL
    }
  })


  # combined user accrual time for runPrediction
  user_accrualTime <- reactive({
    if (input$enroll_rate_method == "user_specified" &&
        input$user_rate_type == "piecewise") {
      user_piecewise_rate()[,1]
    } else {
      NULL
    }
  })

  # user-specified dropout hazard rate for runPrediction
  # Note: user enters rate in events/subject/month, convert to daily rate
  user_dropout_rate <- reactive({
    if (input$dropout_rate_method == "user_specified") {
      user_dropout_hazard_rate() / 30.4375
    } else {
      NULL
    }
  })

  # user-specified event rate for runPrediction
  user_event_rate <- reactive({
    if (input$event_rate_method == "user_specified") {
      user_event_hazard_rate() / 30.4375
    } else {
      NULL
    }
  })


  exponential_survival <- reactive({
    req(k())
    param = input[[paste0("exponential_survival_", k())]]
    lambda = as.numeric(param)
    valid = all(lambda > 0)
    if (!valid) {
      showNotification(
        "Hazard rate must be positive"
      )
    }
    req(valid)
    lambda
  })


  weibull_survival <- reactive({
    req(k())
    param = input[[paste0("weibull_survival_", k())]]
    shape = as.numeric(param[1,])
    scale = as.numeric(param[2,])

    valid1 = all(shape > 0)
    if (!valid1) {
      showNotification(
        "Weibull shape parameter must be positive"
      )
    }

    valid2 = all(scale > 0)
    if (!valid2) {
      showNotification(
        "Weibull scale parameter must be positive"
      )
    }

    req(valid1 && valid2)

    matrix(c(shape, scale), nrow = 2, byrow = TRUE)
  })


  llogis_survival <- reactive({
    req(k())
    param = input[[paste0("llogis_survival_", k())]]
    locationlog = as.numeric(param[1,])
    scalelog = as.numeric(param[2,])

    valid = all(scalelog > 0)
    if (!valid) {
      showNotification(
        "Scale on the log scale must be positive"
      )
    }

    req(valid)

    matrix(c(locationlog, scalelog), nrow = 2, byrow = TRUE)
  })


  lnorm_survival <- reactive({
    req(k())
    param = input[[paste0("lnorm_survival_", k())]]
    meanlog = as.numeric(param[1,])
    sdlog = as.numeric(param[2,])

    valid = all(sdlog > 0)
    if (!valid) {
      showNotification(
        "SD on the log scale must be positive"
      )
    }

    req(valid)

    matrix(c(meanlog, sdlog), nrow = 2, byrow = TRUE)
  })


  piecewise_exponential_survival <- reactive({
    req(k())
    param = input[[paste0("piecewise_exponential_survival_", k())]]
    t = as.numeric(param[,1])
    lambda = as.numeric(param[,-1])

    valid1 = all(diff(t) > 0) && (t[1] == 0)
    if (!valid1) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }

    valid2 = all(lambda > 0)
    if (!valid2) {
      showNotification(
        "Hazard rate must be positive"
      )
    }

    req(valid1 && valid2)

    matrix(c(t, lambda), nrow = length(t))
  })


  piecewiseSurvivalTime <- reactive({
    t = as.numeric(input$piecewiseSurvivalTime)
    valid = all(diff(t) > 0) && (t[1] == 0)
    if (!valid) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }
    req(valid)
    t
  })


  spline_k <- reactive({
    req(input$spline_k)
    valid = (input$spline_k >= 0 && input$spline_k == round(input$spline_k))
    shinyFeedback::feedbackWarning(
      "spline_k", !valid,
      "Number of inner knots must be a nonnegative integer")
    req(valid)
    as.numeric(input$spline_k)
  })


  m_event <- reactive({
    req(input$m_event)
    valid = (input$m_event >= 1 && input$m_event == round(input$m_event))
    shinyFeedback::feedbackWarning(
      "m_event", !valid,
      "Number of event time intervals must be a positive integer")
    req(valid)
    as.numeric(input$m_event)
  })


  exponential_dropout <- reactive({
    req(k())
    param = input[[paste0("exponential_dropout_", k())]]
    lambda = as.numeric(param)
    valid = all(lambda > 0)
    if (!valid) {
      showNotification(
        "Hazard rate must be positive"
      )
    }
    req(valid)
    lambda
  })


  weibull_dropout <- reactive({
    req(k())
    param = input[[paste0("weibull_dropout_", k())]]
    shape = as.numeric(param[1,])
    scale = as.numeric(param[2,])

    valid1 = all(shape > 0)
    if (!valid1) {
      showNotification(
        "Weibull shape parameter must be positive"
      )
    }

    valid2 = all(scale > 0)
    if (!valid2) {
      showNotification(
        "Weibull scale parameter must be positive"
      )
    }

    req(valid1 && valid2)

    matrix(c(shape, scale), nrow = 2, byrow = TRUE)
  })


  llogis_dropout <- reactive({
    req(k())
    param = input[[paste0("llogis_dropout_", k())]]
    locationlog = as.numeric(param[1,])
    scalelog = as.numeric(param[2,])

    valid = all(scalelog > 0)
    if (!valid) {
      showNotification(
        "Scale on the log scale must be positive"
      )
    }

    req(valid)

    matrix(c(locationlog, scalelog), nrow = 2, byrow = TRUE)
  })


  lnorm_dropout <- reactive({
    req(k())
    param = input[[paste0("lnorm_dropout_", k())]]
    meanlog = as.numeric(param[1,])
    sdlog = as.numeric(param[2,])

    valid = all(sdlog > 0)
    if (!valid) {
      showNotification(
        "SD on the log scale must be positive"
      )
    }

    req(valid)

    matrix(c(meanlog, sdlog), nrow = 2, byrow = TRUE)
  })


  piecewise_exponential_dropout <- reactive({
    req(k())
    param = input[[paste0("piecewise_exponential_dropout_", k())]]
    t = as.numeric(param[,1])
    lambda = as.numeric(param[,-1])

    valid1 = all(diff(t) > 0) && (t[1] == 0)
    if (!valid1) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }

    valid2 = all(lambda > 0)
    if (!valid2) {
      showNotification(
        "Hazard rate must be positive"
      )
    }

    req(valid1 && valid2)

    matrix(c(t, lambda), nrow = length(t))
  })


  piecewiseDropoutTime <- reactive({
    t = as.numeric(input$piecewiseDropoutTime)
    valid = all(diff(t) > 0) && (t[1] == 0)
    if (!valid) {
      showNotification(
        "Starting time must be increasing and start at zero"
      )
    }
    req(valid)
    t
  })


  spline_k_dropout <- reactive({
    req(input$spline_k_dropout)
    valid = (input$spline_k_dropout >= 0 &&
               input$spline_k_dropout == round(input$spline_k_dropout))
    shinyFeedback::feedbackWarning(
      "spline_k_dropout", !valid,
      "Number of inner knots must be a nonnegative integer")
    req(valid)
    as.numeric(input$spline_k_dropout)
  })


  m_dropout <- reactive({
    req(input$m_dropout)
    valid = (input$m_dropout >= 1 &&
               input$m_dropout == round(input$m_dropout))
    shinyFeedback::feedbackWarning(
      "m_dropout", !valid,
      "Number of dropout time intervals must be a positive integer")
    req(valid)
    as.numeric(input$m_dropout)
  })


  # input data set
  df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with "name",
    # "size", "type", and "datapath" columns. The "datapath"
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile)) {
      if (input$stage != "Design stage") {
        df <- data.table::setDT(data.table::copy(interimData1))
      } else {
        return(NULL)
      }
    } else {
      df <- data.table::setDT(readxl::read_excel(inFile$datapath))
    }

    if (to_predict() == "Enrollment only") {
      req_cols <- c("trialsdt", "usubjid", "randdt", "cutoffdt")
    } else {
      req_cols <- c("trialsdt", "usubjid", "randdt", "cutoffdt",
                    "time", "event", "dropout")
    }

    if (input$by_treatment) {
      req_cols <- c(req_cols, "treatment")
    }

    cols <- colnames(df)

    shiny::validate(
      need(all(req_cols %in% cols),
           paste("The following columns are missing from the input data:",
                 paste(req_cols[!(req_cols %in% cols)], collapse = ", "))))

    if (any(is.na(df[, ..req_cols]))) {
      stop(paste("The following columns have missing values:",
                 paste(req_cols[sapply(df, function(x) any(is.na(x)))],
                       collapse = ", ")))
    }

    if ("treatment" %in% cols && !("treatment_description" %in% cols)) {
      df[, `:=`(treatment_description = paste("Treatment", treatment))]

    }

    df$trialsdt <- as.Date(df$trialsdt)
    df$randdt <- as.Date(df$randdt)
    df$cutoffdt <- as.Date(df$cutoffdt)

    df
  })


  # summarize observed data
  observed <- reactive({
    if (!is.null(df()))
      dataSummary(df(), to_predict(), showplot = FALSE,
                        input$by_treatment)
  })


  # calculate current enrollment rate from observed data (例/月)
  current_enroll_rate <- reactive({
    if (!is.null(observed()) && observed()$t0 > 0) {
      rate_per_day <- observed()$n0 / observed()$t0
      rate_per_month <- rate_per_day * 30.4375
      round(rate_per_month, 2)
    } else {
      NULL
    }
  })

  # calculate current dropout hazard from observed data
  current_dropout_hazard <- reactive({
    if (!is.null(observed()) && observed()$c0 > 0) {
      df_data <- df()
      total_pt <- sum(df_data$time)
      daily_hazard <- observed()$c0 / total_pt
      monthly_hazard <- daily_hazard * 30.4375
      round(monthly_hazard, 4)
    } else {
      NULL
    }
  })

  # calculate current event hazard from observed data
  current_event_hazard <- reactive({
    if (!is.null(observed()) && observed()$d0 > 0) {
      df_data <- df()
      total_pt <- sum(df_data$time)
      daily_hazard <- observed()$d0 / total_pt
      monthly_hazard <- daily_hazard * 30.4375
      round(monthly_hazard, 4)
    } else {
      NULL
    }
  })

  # enrollment fit
  enroll_fit <- reactive({
    if (!is.null(df()))
      fitEnrollment(df(), input$enroll_model, nknots(),
                    accrualTime(), showplot = FALSE)
  })


  # ---- helper: minimum events required by event model ----
  min_events_for_model <- function(model, J, k) {
    switch(tolower(model),
      exponential          = 1,
      weibull              = 2,
      `log-logistic`       = 2,
      `log-normal`         = 2,
      `piecewise exponential` = max(1, J),
      `model averaging`    = 2,
      spline               = max(2, k + 2),
      cox                  = 1,
      1)  # fallback
  }


  # ---- helper: minimum dropouts required by dropout model ----
  min_dropouts_for_model <- function(model, J, k) {
    switch(tolower(model),
      exponential          = 1,
      weibull              = 2,
      `log-logistic`       = 2,
      `log-normal`         = 2,
      `piecewise exponential` = max(1, J),
      `model averaging`    = 2,
      spline               = max(2, k + 2),
      cox                  = 1,
      1)  # fallback
  }


  # event fit
  event_fit <- reactive({
    if (!is.null(df())) {
      d0 <- observed()$d0
      min_d <- min_events_for_model(input$event_model,
                                    length(piecewiseSurvivalTime()),
                                    spline_k())

      if (!input$by_treatment || k() == 1) {
        shiny::validate(
          need(d0 >= min_d,
               paste("Need at least", min_d, "event(s) to fit a",
                     input$event_model, "model, but only", d0,
                     "event(s) observed. Please choose a model with",
                     "lower data requirements (e.g. Exponential).")))
      } else {
        d0_by_trt <- df()[, .(d0 = sum(event)), by = "treatment"]
        bad_trt <- d0_by_trt[d0 < min_d]
        shiny::validate(
          need(nrow(bad_trt) == 0,
               paste("Each treatment group needs at least", min_d,
                     "event(s) to fit a", input$event_model, "model.",
                     "The following group(s) have insufficient events:",
                     paste(unique(
                       df()[treatment %in% bad_trt$treatment]$treatment_description),
                       collapse = ", "),
                     ". Consider choosing a simpler model (e.g. Exponential)")))
      }

      fitEvent(df(), input$event_model, piecewiseSurvivalTime(),
               spline_k(), input$spline_scale, m_event(),
               showplot = FALSE, input$by_treatment)
    }
  })


  # dropout fit
  dropout_fit <- reactive({
    if (!is.null(df()) && input$dropout_model != "None" &&
        input$dropout_rate_method != "user_specified") {
      c0 <- observed()$c0
      min_c <- min_dropouts_for_model(input$dropout_model,
                                      length(piecewiseDropoutTime()),
                                      spline_k_dropout())

      if (!input$by_treatment || k() == 1) {
        shiny::validate(
          need(c0 >= min_c,
               paste("Need at least", min_c, "dropout(s) to fit a",
                     input$dropout_model, "model, but only", c0,
                     "dropout(s) observed. Please choose a model with",
                     "lower data requirements (e.g. Exponential or None)," ,
                     "or specify a constant dropout rate manually.")))
      } else {
        c0_by_trt <- df()[, .(c0 = sum(dropout)), by = "treatment"]
        bad_trt <- c0_by_trt[c0 < min_c]
        shiny::validate(
          need(nrow(bad_trt) == 0,
               paste("Each treatment group needs at least", min_c,
                     "dropout(s) to fit a", input$dropout_model, "model.",
                     "The following group(s) have insufficient dropouts:",
                     paste(unique(
                       df()[treatment %in% bad_trt$treatment]$treatment_description),
                       collapse = ", "),
                     ". Consider choosing a simpler model (e.g. Exponential),",
                     "setting dropout model to 'None', or using",
                     "a user-specified dropout rate.")))
      }

      fitDropout(df(), input$dropout_model, piecewiseDropoutTime(),
                 spline_k_dropout(), input$spline_scale_dropout,
                 m_dropout(), showplot = FALSE, input$by_treatment)
    }
  })


  # enrollment and event prediction
  pred <- eventReactive(input$predict, {
    result <- NULL
    withProgress(message = "Running prediction...", value = 0, {
    set.seed(as.numeric(input$seed))

    if (to_predict() != "Enrollment only") {
      shiny::validate(
        need(showEnrollment() || showEvent() || showDropout() ||
               showOngoing(),
             "Need at least one parameter to show on prediction plot"))
    }

    incProgress(0.05, detail = "Preparing model specifications...")

    if (input$stage == "Design stage") {
      w = treatment_allocation()/sum(treatment_allocation())

      # enroll model specifications
      if (input$enroll_prior == "Poisson") {
        # convert monthly rate to daily rate for model
        theta = log(poisson_rate() / 30.4375)
      } else if (input$enroll_prior == "Time-decay") {
        theta = c(log(mu()), log(delta()))
      } else if (input$enroll_prior == "Piecewise Poisson") {
        # convert monthly rates to daily rates for model
        theta = log(piecewise_poisson_rate()[,2] / 30.4375)
        accrualTime = piecewise_poisson_rate()[,1]
      }

      enroll_prior <- list(
        model = input$enroll_prior,
        theta = theta,
        vtheta = diag(length(theta))*1e-8)

      if (input$enroll_prior == "Piecewise Poisson") {
        enroll_prior$accrualTime = accrualTime
      }

      # event model specifications
      if (to_predict() == "Enrollment and event") {
        model = input$event_prior
        event_prior <- list()

        for (i in 1:k()) {
          if (model == "Exponential") {
            theta = log(exponential_survival()[i])
          } else if (model == "Weibull") {
            theta = c(log(weibull_survival()[2,i]),
                      -log(weibull_survival()[1,i]))
          } else if (model == "Log-logistic") {
            theta = c(llogis_survival()[1,i], log(llogis_survival()[2,i]))
          } else if (model == "Log-normal") {
            theta = c(lnorm_survival()[1,i], log(lnorm_survival()[2,i]))
          } else if (model == "Piecewise exponential") {
            theta = log(piecewise_exponential_survival()[,i+1])
            piecewiseSurvivalTime = piecewise_exponential_survival()[,1]
          }

          if (model != "Piecewise exponential") {
            event_prior[[i]] <- list(
              model = model,
              theta = theta,
              vtheta = diag(length(theta))*1e-8,
              w = w[i])
          } else {
            event_prior[[i]] <- list(
              model = model,
              theta = theta,
              vtheta = diag(length(theta))*1e-8,
              piecewiseSurvivalTime = piecewiseSurvivalTime,
              w = w[i])
          }
        }

        if (k() == 1) event_prior <- event_prior[[1]]

        # dropout model specifications
        if (input$dropout_prior != "None") {
          model = input$dropout_prior
          dropout_prior <- list()

          for (i in 1:k()) {
            if (model == "Exponential") {
              theta = log(exponential_dropout()[i])
            } else if (model == "Weibull") {
              theta = c(log(weibull_dropout()[2,i]),
                        -log(weibull_dropout()[1,i]))
            } else if (model == "Log-logistic") {
              theta = c(llogis_dropout()[1,i], log(llogis_dropout()[2,i]))
            } else if (model == "Log-normal") {
              theta = c(lnorm_dropout()[1,i], log(lnorm_dropout()[2,i]))
            } else if (model == "Piecewise exponential") {
              theta = log(piecewise_exponential_dropout()[,i+1])
              piecewiseDropoutTime = piecewise_exponential_dropout()[,1]
            }

            if (model != "Piecewise exponential") {
              dropout_prior[[i]] <- list(
                model = model,
                theta = theta,
                vtheta = diag(length(theta))*1e-8,
                w = w[i])
            } else {
              dropout_prior[[i]] <- list(
                model = model,
                theta = theta,
                vtheta = diag(length(theta))*1e-8,
                piecewiseDropoutTime = piecewiseDropoutTime,
                w = w[i])
            }
          }

          if (k() == 1) dropout_prior <- dropout_prior[[1]]

        } else {
          dropout_prior = NULL
        }
      }

      # get prediction results based on what to predict
      if (to_predict() == "Enrollment only") {
        incProgress(0.2, detail = "Running enrollment prediction simulations...")
        result <- runPrediction(
          to_predict = to_predict(),
          target_n = target_n(),
          enroll_prior = enroll_prior,
          pilevel = pilevel(),
          nyears = nyears(),
          nreps = nreps(),
          showsummary = FALSE,
          showplot = FALSE,
          by_treatment = input$by_treatment,
          ngroups = k(),
          alloc = treatment_allocation(),
          treatment_label = treatment_description(),
          fix_parameter = input$fix_parameter)
      } else if (to_predict() == "Enrollment and event") {
        incProgress(0.2, detail = "Running event prediction simulations...")
        result <- runPrediction(
          to_predict = to_predict(),
          target_n = target_n(),
          target_d = target_d(),
          enroll_prior = enroll_prior,
          event_prior = event_prior,
          dropout_prior = dropout_prior,
          pilevel = pilevel(),
          nyears = nyears(),
          target_t = target_t(),
          nreps = nreps(),
          showEnrollment = showEnrollment(),
          showEvent = showEvent(),
          showDropout = showDropout(),
          showOngoing = showOngoing(),
          showsummary = FALSE,
          showplot = FALSE,
          by_treatment = input$by_treatment,
          ngroups = k(),
          alloc = treatment_allocation(),
          treatment_label = treatment_description(),
          fix_parameter = input$fix_parameter)
      }
    } else { # real-time prediction
      shiny::validate(
        need(!is.null(df()),
             "Please upload data for real-time prediction."))

      if (to_predict() == "Enrollment only") {
        shiny::validate(
          need(target_n() > observed()$n0,
               "Target enrollment has been reached."))

        incProgress(0.2, detail = "Fitting enrollment model and running simulations...")
        result <- runPrediction(
          df = df(),
          to_predict = to_predict(),
          target_n = target_n(),
          enroll_model = input$enroll_model,
          nknots = nknots(),
          lags = lags(),
          accrualTime = accrualTime(),
          pilevel = pilevel(),
          nyears = nyears(),
          nreps = nreps(),
          showsummary = FALSE,
          showplot = FALSE,
          by_treatment = input$by_treatment,
          alloc = treatment_allocation(),
          fix_parameter = input$fix_parameter,
          user_enroll_rate = user_enroll_rate(),
          user_accrualTime = user_accrualTime(),
          user_dropout_rate = user_dropout_rate(),
          user_event_rate = user_event_rate())
      } else if (to_predict() == "Enrollment and event") {
        shiny::validate(
          need(target_n() > observed()$n0,
               "Target enrollment has been reached."))

        shiny::validate(
          need(target_d() > observed()$d0,
               "Target number of events has been reached."))

        if (input$event_model == "Cox") {
          shiny::validate(
            need(observed()$d0 >= m_event(), paste(
              "The number of event time intervals must be less than",
              "or equal to the observed number of events.")))
        }

        if (input$dropout_model != "None" &&
            input$dropout_rate_method != "user_specified") {
          shiny::validate(
            need(observed()$c0 > 0, paste(
              "The number of dropouts must be positive",
              "to fit a dropout model.")))

          if (input$by_treatment && k() > 1) {
            c0_by_trt <- df()[, .(c0_trt = sum(dropout)),
                                by = .(treatment, treatment_description)]
            zero_trt <- c0_by_trt[c0_trt == 0]
            shiny::validate(
              need(nrow(zero_trt) == 0, paste(
                "Cannot fit dropout model: the following group(s) have 0 dropouts:",
                paste(zero_trt$treatment_description, collapse = ", "),
                ". Consider setting dropout model to 'None',",
                "using user-specified dropout rate, or unchecking",
                "'Stratify by Treatment'.")))
          }

          if (input$dropout_model == "Cox") {
            shiny::validate(
              need(observed()$c0 >= m_dropout(), paste(
                "The number of dropout time intervals must be less than",
                "or equal to the observed number of dropouts.")))
          }
        }

        incProgress(0.2, detail = "Fitting models and running event prediction simulations...")
        result <- runPrediction(
          df = df(),
          to_predict = to_predict(),
          target_n = target_n(),
          target_d = target_d(),
          enroll_model = input$enroll_model,
          nknots = nknots(),
          lags = lags(),
          accrualTime = accrualTime(),
          event_model = input$event_model,
          piecewiseSurvivalTime = piecewiseSurvivalTime(),
          k = spline_k(),
          scale = input$spline_scale,
          m = m_event(),
          dropout_model = input$dropout_model,
          piecewiseDropoutTime = piecewiseDropoutTime(),
          k_dropout = spline_k_dropout(),
          scale_dropout = input$spline_scale_dropout,
          m_dropout = m_dropout(),
          pilevel = pilevel(),
          nyears = nyears(),
          target_t = target_t(),
          nreps = nreps(),
          showEnrollment = showEnrollment(),
          showEvent = showEvent(),
          showDropout = showDropout(),
          showOngoing = showOngoing(),
          showsummary = FALSE,
          showplot = FALSE,
          by_treatment = input$by_treatment,
          alloc = treatment_allocation(),
          fix_parameter = input$fix_parameter,
          user_enroll_rate = user_enroll_rate(),
          user_accrualTime = user_accrualTime(),
          user_dropout_rate = user_dropout_rate(),
          user_event_rate = user_event_rate())
      } else if (to_predict() == "Event only") {
        shiny::validate(
          need(target_d() > observed()$d0,
               "Target number of events has been reached."))

        if (input$event_model == "Cox") {
          shiny::validate(
            need(observed()$d0 >= m_event(), paste(
              "The number of event time intervals must be less than",
              "or equal to the observed number of events.")))
        }

        if (input$dropout_model != "None" &&
            input$dropout_rate_method != "user_specified") {
          shiny::validate(
            need(observed()$c0 > 0, paste(
              "The number of dropouts must be positive",
              "to fit a dropout model.")))

          if (input$by_treatment && k() > 1) {
            c0_by_trt <- df()[, .(c0_trt = sum(dropout)),
                                by = .(treatment, treatment_description)]
            zero_trt <- c0_by_trt[c0_trt == 0]
            shiny::validate(
              need(nrow(zero_trt) == 0, paste(
                "Cannot fit dropout model: the following group(s) have 0 dropouts:",
                paste(zero_trt$treatment_description, collapse = ", "),
                ". Consider setting dropout model to 'None',",
                "using user-specified dropout rate, or unchecking",
                "'Stratify by Treatment'.")))
          }

          if (input$dropout_model == "Cox") {
            shiny::validate(
              need(observed()$c0 >= m_dropout(), paste(
                "The number of dropout time intervals must be less than",
                "or equal to the observed number of dropouts.")))
          }
        }

        incProgress(0.2, detail = "Fitting models and running event prediction simulations...")
        result <- runPrediction(
          df = df(),
          to_predict = to_predict(),
          target_d = target_d(),
          event_model = input$event_model,
          piecewiseSurvivalTime = piecewiseSurvivalTime(),
          k = spline_k(),
          scale = input$spline_scale,
          m = m_event(),
          dropout_model = input$dropout_model,
          piecewiseDropoutTime = piecewiseDropoutTime(),
          k_dropout = spline_k_dropout(),
          scale_dropout = input$spline_scale_dropout,
          m_dropout = m_dropout(),
          pilevel = pilevel(),
          nyears = nyears(),
          target_t = target_t(),
          nreps = nreps(),
          showEnrollment = showEnrollment(),
          showEvent = showEvent(),
          showDropout = showDropout(),
          showOngoing = showOngoing(),
          showsummary = FALSE,
          showplot = FALSE,
          by_treatment = input$by_treatment,
          fix_parameter = input$fix_parameter,
          user_dropout_rate = user_dropout_rate(),
          user_event_rate = user_event_rate())
      }
    }
    incProgress(0.95, detail = "Finalizing...")
    result
  }) # withProgress
    result
  })


  output$current_enroll_rate <- renderText({
    if (!is.null(current_enroll_rate())) {
      str1 <- paste0("Current average enrollment rate: <b>",
                     current_enroll_rate(), " subjects/month</b>",
                     " (total ", observed()$n0, " subjects enrolled over ",
                     observed()$t0, " days)")
      paste(str1, sep="<br/>")
    }
  })

  output$current_dropout_hazard <- renderText({
    if (!is.null(current_dropout_hazard()) && !is.null(observed())) {
      str1 <- paste0("Current dropout hazard rate (exponential MLE): <b>",
                     current_dropout_hazard(), " events/subject/month</b>",
                     " (total ", observed()$c0, " dropouts / ",
                     round(sum(df()$time), 0), " person-days)")
      paste(str1, sep="<br/>")
    } else if (!is.null(observed()) && observed()$c0 == 0) {
      paste("No dropouts observed yet — cannot estimate current dropout hazard.")
    }
  })

  output$current_event_hazard <- renderText({
    if (!is.null(current_event_hazard()) && !is.null(observed())) {
      str1 <- paste0("Current event hazard rate (exponential MLE): <b>",
                     current_event_hazard(), " events/subject/month</b>",
                     " (total ", observed()$d0, " events / ",
                     round(sum(df()$time), 0), " person-days)")
      paste(str1, sep="<br/>")
    } else if (!is.null(observed()) && observed()$d0 == 0) {
      paste("No events observed yet — cannot estimate current event hazard.")
    }
  })

  output$dates <- renderText({
    if (!is.null(observed())) {
      str1 <- paste("Trial start date:", observed()$trialsdt)
      str2 <- paste("Data cutoff date:", observed()$cutoffdt)
      str3 <- paste("Enrollment duration (days):", observed()$t0)
      paste(str1, str2, str3, sep="<br/>")
    }
  })


  output$statistics <- renderUI({
    if (!is.null(df())) {

      if (input$by_treatment && k() > 1) {
        if (to_predict() == "Enrollment and event" ||
            to_predict() == "Event only") {

          sum_by_trt <- data.table::rbindlist(list(
            df(), data.table::copy(df())[, `:=`(
              treatment = 9999, treatment_description = "Overall")]),
            use.names = TRUE)[, .(
              n0 = .N,
              d0 = sum(event),
              c0 = sum(dropout),
              r0 = sum(!(event | dropout)),
              rp = sum((time < as.numeric(cutoffdt - randdt + 1)) &
                         !event & !dropout)),
              by = c("treatment", "treatment_description")]


          if (any(sum_by_trt$rp) > 0) {
            table <- t(sum_by_trt[, .(n0, d0, c0, r0, rp)])
            colnames(table) <- sum_by_trt$treatment_description
            rownames(table) <- c("Current number of subjects",
                                 "Current number of events",
                                 "Current number of dropouts",
                                 "Number of ongoing subjects",
                                 "  With ongoing date before cutoff")
          } else {
            table <- t(sum_by_trt[, .(n0, d0, c0, r0)])
            colnames(table) <- sum_by_trt$treatment_description
            rownames(table) <- c("Current number of subjects",
                                 "Current number of events",
                                 "Current number of dropouts",
                                 "Number of ongoing subjects")
          }

        } else {
          sum_by_trt <- data.table::rbindlist(list(
            df(), data.table::copy(df())[, `:=`(
              treatment = 9999, treatment_description = "Overall")]),
            use.names = TRUE)[, .(n0 = .N), by = c(
              "treatment", "treatment_description")]

          table <- t(sum_by_trt[, .(n0)])
          colnames(table) <- sum_by_trt$treatment_description
          rownames(table) <- c("Current number of subjects")
        }
      } else {
        if (to_predict() == "Enrollment and event" ||
            to_predict() == "Event only") {
          sum_overall <- data.table(n0 = observed()$n0,
                                    d0 = observed()$d0,
                                    c0 = observed()$c0,
                                    r0 = observed()$r0,
                                    rp = observed()$rp)

          if (sum_overall$rp > 0) {
            table <- t(sum_overall[, .(n0, d0, c0, r0, rp)])
            colnames(table) <- "Overall"
            rownames(table) <- c("Current number of subjects",
                                 "Current number of events",
                                 "Current number of dropouts",
                                 "Number of ongoing subjects",
                                 "  With ongoing date before cutoff")
          } else {
            table <- t(sum_overall[, .(n0, d0, c0, r0)])
            colnames(table) <- "Overall"
            rownames(table) <- c("Current number of subjects",
                                 "Current number of events",
                                 "Current number of dropouts",
                                 "Number of ongoing subjects")
          }
        } else {
          table <- t(data.table(n0 = observed()$n0))
          colnames(table) <- "Overall"
          rownames(table) <- c("Current number of subjects")
        }
      }

      # Format table as HTML lines matching the dates display style
      if (ncol(table) == 1) {
        lines <- as.vector(table)
        labels <- rownames(table)
        text_lines <- paste0(labels, ": ", lines)
      } else {
        labels <- rownames(table)
        text_lines <- sapply(seq_len(nrow(table)), function(r) {
          vals <- paste0(colnames(table), " = ", table[r, ], collapse = ", ")
          paste0(labels[r], ": ", vals)
        })
      }
      HTML(paste(text_lines, collapse = "<br/>"))
    }
  })


  output$cum_accrual_plot <- renderPlotly({
    cum_accrual_plot <- observed()$cum_accrual_plot
    if (!is.null(cum_accrual_plot)) style_plotly(cum_accrual_plot)
  })


  output$gantt_plot <- renderPlotly({
    gantt_plot <- observed()$gantt_plot
    if (!is.null(gantt_plot)) style_plotly(gantt_plot, top_margin = 20)
  })



  output$event_km_plot <- renderPlotly({
    event_km_plot <- observed()$event_km_plot
    if (!is.null(event_km_plot)) style_plotly(event_km_plot, top_margin = 20)
  })


  output$na_plot <- renderPlotly({
    na_plot <- observed()$na_event_plot
    if (!is.null(na_plot)) style_plotly(na_plot, top_margin = 20)
  })


  output$schoenfeld_plot <- renderPlotly({
    schoenfeld_plot <- observed()$schoenfeld_plot
    if (!is.null(schoenfeld_plot)) style_plotly(schoenfeld_plot, top_margin = 20)
  })


  output$dropout_km_plot <- renderPlotly({
    dropout_km_plot <- observed()$dropout_km_plot
    if (!is.null(dropout_km_plot)) style_plotly(dropout_km_plot, top_margin = 20)
  })


  output$status_pie_plot <- renderPlotly({
    req(observed())
    obs <- observed()

    labels <- c("Event", "Dropout", "Ongoing")
    values <- c(obs$d0, obs$c0, obs$r0)

    plotly::plot_ly(
      labels = labels,
      values = values,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(
        colors = c("#E74C3C", "#F39C12", "#2E86AB"),
        line = list(color = "#FFFFFF", width = 2)
      )
    ) %>%
    plotly::layout(
      title = list(text = "Subject status distribution",
                   font = list(size = 18)),
      font = list(family = "Arial", size = 14)
    )
  })


  # BACKUP: Subject-level data table — commented out, kept for future use
  # output$input_df <- DT::renderDT(
  #   df(), options = list(pageLength = 10)
  # )


  output$enroll_fit <- renderPlotly({
    if (!is.null(enroll_fit())) style_plotly(enroll_fit()$fit_plot, top_margin = 20)
  })

  output$enroll_resid <- renderPlotly({
    if (!is.null(enroll_fit()) && !is.null(enroll_fit()$resid_plot))
      style_plotly(enroll_fit()$resid_plot, top_margin = 20)
  })


  # event fit information criteria
  output$event_fit_ic <- renderText({
    if (input$by_treatment && k() > 1 && !is.null(event_fit())) {
      aic = sum(sapply(event_fit(), function(fit) fit$fit$aic))
      bic = sum(sapply(event_fit(), function(fit) fit$fit$bic))
      aictext = paste("Total AIC:", formatC(aic, format = "f", digits = 2))
      bictext = paste("Total BIC:", formatC(bic, format = "f", digits = 2))
      text1 = paste0("<i>", aictext, ", ", bictext, "</i>")
    } else {
      text1 = NULL
    }

    if (!is.null(text1)) text1
  })


  # dropout fit information criteria
  output$dropout_fit_ic <- renderText({
    if (input$by_treatment && k() > 1 && input$dropout_model != "None"
        && !is.null(dropout_fit())) {
      aic = sum(sapply(dropout_fit(), function(fit) fit$fit$aic))
      bic = sum(sapply(dropout_fit(), function(fit) fit$fit$bic))
      aictext = paste("Total AIC:", formatC(aic, format = "f", digits = 2))
      bictext = paste("Total BIC:", formatC(bic, format = "f", digits = 2))
      text1 = paste0("<i>", aictext, ", ", bictext, "</i>")
    } else {
      text1 = NULL
    }

    if (!is.null(text1)) text1
  })


  observe({
    walk(1:6, function(i) {
      output[[paste0("event_fit_output", i)]] <- renderPlotly({
        p <- NULL
        if (i <= k() && !is.null(event_fit())) {
          if (input$by_treatment && k() > 1) {
            p <- event_fit()[[i]]$fit_plot
          } else {
            p <- event_fit()$fit_plot
          }
        }
        style_plotly(p, top_margin = 20)
      })

      output[[paste0("event_cs_resid_output", i)]] <- renderPlotly({
        p <- NULL
        if (i <= k() && !is.null(event_fit())) {
          if (input$by_treatment && k() > 1) {
            p <- event_fit()[[i]]$cs_resid_plot
          } else {
            p <- event_fit()$cs_resid_plot
          }
        }
        style_plotly(p, top_margin = 20)
      })

      output[[paste0("dropout_fit_output", i)]] <- renderPlotly({
        p <- NULL
        if (i <= k()) {
          if (input$by_treatment && k() > 1 && !is.null(dropout_fit())) {
            p <- dropout_fit()[[i]]$fit_plot
          } else {
            p <- dropout_fit()$fit_plot
          }
        }
        style_plotly(p, top_margin = 20)
      })

      output[[paste0("dropout_cs_resid_output", i)]] <- renderPlotly({
        p <- NULL
        if (i <= k() && !is.null(dropout_fit())) {
          if (input$by_treatment && k() > 1) {
            p <- dropout_fit()[[i]]$cs_resid_plot
          } else {
            p <- dropout_fit()$cs_resid_plot
          }
        }
        style_plotly(p, top_margin = 20)
      })
    })
  })


  event_fit_outputs <- reactive({
    outputs <- map(1:k(), function(i) {
      plotlyOutput(paste0("event_fit_output", i), height = "440px")
    })

    tagList(outputs)
  })


  output$event_fit <- renderUI({
    event_fit_outputs()
  })


  dropout_fit_outputs <- reactive({
    outputs <- map(1:k(), function(i) {
      plotlyOutput(paste0("dropout_fit_output", i), height = "440px")
    })

    tagList(outputs)
  })


  event_cs_resid_outputs <- reactive({
    outputs <- map(1:k(), function(i) {
      plotlyOutput(paste0("event_cs_resid_output", i), height = "440px")
    })

    tagList(outputs)
  })

  output$event_cs_resid <- renderUI({
    event_cs_resid_outputs()
  })


  dropout_cs_resid_outputs <- reactive({
    outputs <- map(1:k(), function(i) {
      plotlyOutput(paste0("dropout_cs_resid_output", i), height = "440px")
    })

    tagList(outputs)
  })

  output$dropout_cs_resid <- renderUI({
    dropout_cs_resid_outputs()
  })


  output$dropout_fit <- renderUI({
    dropout_fit_outputs()
  })


  # ==========================================================
  # Event model comparison — fit all candidate models
  # ==========================================================
  event_compare <- reactive({
    req(df())

    # Models that can be directly compared; exclude "Model averaging"
    # as it is a weighted combination, not an independent model
    models_to_fit <- c("Exponential", "Weibull", "Log-logistic",
                       "Log-normal", "Piecewise exponential", "Spline", "Cox")

    color_palette <- c(
      "Exponential"           = "#E74C3C",
      "Weibull"               = "#3498DB",
      "Log-logistic"          = "#2ECC71",
      "Log-normal"            = "#F39C12",
      "Piecewise exponential" = "#9B59B6",
      "Spline"                = "#1ABC9C",
      "Cox"                   = "#E67E22")

    all_fits <- list()

    for (m in models_to_fit) {
      res <- tryCatch({
        fitEvent(df(), m, piecewiseSurvivalTime(),
                 spline_k(), input$spline_scale, m_event(),
                 showplot = FALSE, input$by_treatment)
      }, error = function(e) NULL)
      if (!is.null(res)) all_fits[[m]] <- res
    }

    if (length(all_fits) == 0) return(NULL)

    # ---- Build comparison table ----
    if (input$by_treatment && k() > 1) {
      table_data <- lapply(names(all_fits), function(m) {
        fits_list <- all_fits[[m]]
        data.table(
          Model = m,
          AIC = round(sum(vapply(fits_list, function(f) f$fit$aic, numeric(1))), 2),
          BIC = round(sum(vapply(fits_list, function(f) f$fit$bic, numeric(1))), 2)
        )
      })
    } else {
      table_data <- lapply(names(all_fits), function(m) {
        f <- all_fits[[m]]
        data.table(
          Model = m,
          AIC = round(f$fit$aic, 2),
          BIC = round(f$fit$bic, 2)
        )
      })
    }

    tbl <- rbindlist(table_data)
    data.table::setorderv(tbl, "AIC")

    # ---- Build overlay plots ----
    if (input$by_treatment && k() > 1) {
      plots <- list()
      for (i in 1:k()) {
        p <- plotly::plot_ly()

        # KM curve (shared across models for same treatment group)
        kmdf <- all_fits[[names(all_fits)[1]]][[i]]$kmdf
        p <- p %>% plotly::add_lines(
          data = kmdf, x = ~time, y = ~surv,
          name = "Kaplan-Meier",
          line = list(shape = "hv", color = "black", width = 1.5))

        # Fitted curves from each model
        for (m in names(all_fits)) {
          dff <- all_fits[[m]][[i]]$dffit
          p <- p %>% plotly::add_lines(
            data = dff, x = ~time, y = ~surv,
            name = m,
            line = list(color = color_palette[m], width = 1.5))
        }

        trt_label <- all_fits[[names(all_fits)[1]]][[i]]$fit$treatment_description
        p <- p %>% plotly::layout(
          title = list(text = paste0("<b>", trt_label, "</b>"),
                       font = list(size = 14)),
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          legend = list(orientation = "h", x = 0.5, y = 1.05,
                        xanchor = "center", yanchor = "bottom",
                        font = list(size = 12)))

        plots[[i]] <- p
      }
    } else {
      p <- plotly::plot_ly()

      # KM curve
      kmdf <- all_fits[[names(all_fits)[1]]]$kmdf
      p <- p %>% plotly::add_lines(
        data = kmdf, x = ~time, y = ~surv,
        name = "Kaplan-Meier",
        line = list(shape = "hv", color = "black", width = 1.5))

      # Fitted curves from each model
      for (m in names(all_fits)) {
        dff <- all_fits[[m]]$dffit
        p <- p %>% plotly::add_lines(
          data = dff, x = ~time, y = ~surv,
          name = m,
          line = list(color = color_palette[m], width = 1.5))
      }

      p <- p %>% plotly::layout(
        xaxis = list(title = "Days since randomization", zeroline = FALSE),
        yaxis = list(title = "Survival probability", zeroline = FALSE),
        legend = list(orientation = "h", x = 0.5, y = 1.05,
                      xanchor = "center", yanchor = "bottom",
                      font = list(size = 12)))

      plots <- list(p)
    }

    list(table = tbl, plots = plots)
  })


  output$event_compare_table <- DT::renderDT({
    shiny::validate(
      need(length(event_compare()$table$Model) > 0,
           paste("None of the candidate event models could be fitted.",
                 "The data may have too few events. Try uploading data",
                 "with more observed events, or check the model-specific",
                 "requirements in the Event Fit tab.")))
    tbl <- event_compare()$table

    DT::datatable(tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all")))) %>%
      DT::formatStyle("AIC",
        backgroundColor = DT::styleEqual(min(tbl$AIC), "#d4edda")) %>%
      DT::formatStyle("BIC",
        backgroundColor = DT::styleEqual(min(tbl$BIC), "#d4edda"))
  })


  observe({
    walk(1:6, function(i) {
      output[[paste0("event_compare_plot", i)]] <- renderPlotly({
        p <- NULL
        if (!is.null(event_compare())) {
          if (i <= length(event_compare()$plots)) {
            p <- event_compare()$plots[[i]]
          }
        }
        style_plotly(p, top_margin = 20)
      })
    })
  })


  event_compare_plot_outputs <- reactive({
    n <- if (!is.null(event_compare())) length(event_compare()$plots) else 0
    outputs <- map(1:n, function(i) {
      plotlyOutput(paste0("event_compare_plot", i), height = "440px")
    })
    tagList(outputs)
  })


  output$event_compare_plots <- renderUI({
    shiny::validate(
      need(length(event_compare()$plots) > 0,
           paste("No event model comparison plot could be generated.",
                 "Check that the data has enough events for at least one",
                 "candidate model to fit successfully.")))
    event_compare_plot_outputs()
  })


  # ==========================================================
  # Dropout model comparison — fit all candidate models
  # ==========================================================
  dropout_compare <- reactive({
    req(df())
    req(input$dropout_model != "None")

    models_to_fit <- c("Exponential", "Weibull", "Log-logistic",
                       "Log-normal", "Piecewise exponential", "Spline", "Cox")

    color_palette <- c(
      "Exponential"           = "#E74C3C",
      "Weibull"               = "#3498DB",
      "Log-logistic"          = "#2ECC71",
      "Log-normal"            = "#F39C12",
      "Piecewise exponential" = "#9B59B6",
      "Spline"                = "#1ABC9C",
      "Cox"                   = "#E67E22")

    all_fits <- list()

    for (m in models_to_fit) {
      res <- tryCatch({
        fitDropout(df(), m, piecewiseDropoutTime(),
                   spline_k_dropout(), input$spline_scale_dropout,
                   m_dropout(), showplot = FALSE, input$by_treatment)
      }, error = function(e) NULL)
      if (!is.null(res)) all_fits[[m]] <- res
    }

    if (length(all_fits) == 0) return(NULL)

    # ---- Build comparison table ----
    if (input$by_treatment && k() > 1) {
      table_data <- lapply(names(all_fits), function(m) {
        fits_list <- all_fits[[m]]
        data.table(
          Model = m,
          AIC = round(sum(vapply(fits_list, function(f) f$fit$aic, numeric(1))), 2),
          BIC = round(sum(vapply(fits_list, function(f) f$fit$bic, numeric(1))), 2)
        )
      })
    } else {
      table_data <- lapply(names(all_fits), function(m) {
        f <- all_fits[[m]]
        data.table(
          Model = m,
          AIC = round(f$fit$aic, 2),
          BIC = round(f$fit$bic, 2)
        )
      })
    }

    tbl <- rbindlist(table_data)
    data.table::setorderv(tbl, "AIC")

    # ---- Build overlay plots ----
    if (input$by_treatment && k() > 1) {
      plots <- list()
      for (i in 1:k()) {
        p <- plotly::plot_ly()

        kmdf <- all_fits[[names(all_fits)[1]]][[i]]$kmdf
        p <- p %>% plotly::add_lines(
          data = kmdf, x = ~time, y = ~surv,
          name = "Kaplan-Meier",
          line = list(shape = "hv", color = "black", width = 1.5))

        for (m in names(all_fits)) {
          dff <- all_fits[[m]][[i]]$dffit
          p <- p %>% plotly::add_lines(
            data = dff, x = ~time, y = ~surv,
            name = m,
            line = list(color = color_palette[m], width = 1.5))
        }

        trt_label <- all_fits[[names(all_fits)[1]]][[i]]$fit$treatment_description
        p <- p %>% plotly::layout(
          title = list(text = paste0("<b>", trt_label, "</b>"),
                       font = list(size = 14)),
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Dropout-free probability", zeroline = FALSE),
          legend = list(orientation = "h", x = 0.5, y = 1.05,
                        xanchor = "center", yanchor = "bottom",
                        font = list(size = 12)))

        plots[[i]] <- p
      }
    } else {
      p <- plotly::plot_ly()

      kmdf <- all_fits[[names(all_fits)[1]]]$kmdf
      p <- p %>% plotly::add_lines(
        data = kmdf, x = ~time, y = ~surv,
        name = "Kaplan-Meier",
        line = list(shape = "hv", color = "black", width = 1.5))

      for (m in names(all_fits)) {
        dff <- all_fits[[m]]$dffit
        p <- p %>% plotly::add_lines(
          data = dff, x = ~time, y = ~surv,
          name = m,
          line = list(color = color_palette[m], width = 1.5))
      }

      p <- p %>% plotly::layout(
        xaxis = list(title = "Days since randomization", zeroline = FALSE),
        yaxis = list(title = "Dropout-free probability", zeroline = FALSE),
        legend = list(orientation = "h", x = 0.5, y = 1.05,
                      xanchor = "center", yanchor = "bottom",
                      font = list(size = 12)))

      plots <- list(p)
    }

    list(table = tbl, plots = plots)
  })


  output$dropout_compare_table <- DT::renderDT({
    shiny::validate(
      need(length(dropout_compare()$table$Model) > 0,
           paste("None of the candidate dropout models could be fitted.",
                 "The data may have too few dropouts. Try uploading data",
                 "with more observed dropouts, choose 'None' for the dropout",
                 "model, or specify a constant dropout rate manually.")))
    tbl <- dropout_compare()$table

    DT::datatable(tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all")))) %>%
      DT::formatStyle("AIC",
        backgroundColor = DT::styleEqual(min(tbl$AIC), "#d4edda")) %>%
      DT::formatStyle("BIC",
        backgroundColor = DT::styleEqual(min(tbl$BIC), "#d4edda"))
  })


  observe({
    walk(1:6, function(i) {
      output[[paste0("dropout_compare_plot", i)]] <- renderPlotly({
        p <- NULL
        if (!is.null(dropout_compare())) {
          if (i <= length(dropout_compare()$plots)) {
            p <- dropout_compare()$plots[[i]]
          }
        }
        style_plotly(p, top_margin = 20)
      })
    })
  })


  dropout_compare_plot_outputs <- reactive({
    n <- if (!is.null(dropout_compare())) length(dropout_compare()$plots) else 0
    outputs <- map(1:n, function(i) {
      plotlyOutput(paste0("dropout_compare_plot", i), height = "440px")
    })
    tagList(outputs)
  })


  output$dropout_compare_plots <- renderUI({
    shiny::validate(
      need(length(dropout_compare()$plots) > 0,
           paste("No dropout model comparison plot could be generated.",
                 "Check that the data has enough dropouts for at least one",
                 "candidate model to fit successfully. Consider choosing",
                 "a simpler dropout model or setting it to 'None'.")))
    dropout_compare_plot_outputs()
  })


  # enrollment predication date
  output$enroll_pred_date <- renderText({
    if (to_predict() == "Enrollment only" ||
        to_predict() == "Enrollment and event") {

      req(pred()$enroll_pred)
      req(pred()$stage == input$stage && pred()$to_predict == to_predict())

      if (input$stage != "Design stage") {
        shiny::validate(
          need(!is.null(df()),
               "Please upload data for real-time prediction."))

        shiny::validate(
          need(target_n() > observed()$n0,
               "Target enrollment has been reached."))

        if (!is.null(pred()$enroll_pred$enroll_pred_date)) {
          str1 <- paste0("Time from cutoff until ",
                         pred()$enroll_pred$target_n, " subjects: ",
                         pred()$enroll_pred$enroll_pred_date[1] -
                           observed()$cutoffdt + 1, " days",
                         " (95% CI: ",
                         pred()$enroll_pred$enroll_pred_date[2] -
                           observed()$cutoffdt + 1, " - ",
                         pred()$enroll_pred$enroll_pred_date[3] -
                           observed()$cutoffdt + 1, " days)")
          str2 <- paste0("Median prediction date: ",
                         pred()$enroll_pred$enroll_pred_date[1])
          str3 <- paste0("Prediction interval: ",
                         pred()$enroll_pred$enroll_pred_date[2], ", ",
                         pred()$enroll_pred$enroll_pred_date[3])
          text1 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text1 <- NULL
        }
      } else {
        if (!is.null(pred()$enroll_pred$enroll_pred_day)) {
          str1 <- paste0("Time from trial start until ",
                         pred()$enroll_pred$target_n, " subjects: ",
                         pred()$enroll_pred$enroll_pred_day[1], " days",
                         " (95% CI: ",
                         pred()$enroll_pred$enroll_pred_day[2], " - ",
                         pred()$enroll_pred$enroll_pred_day[3], " days)")
          str2 <- paste0("Median prediction day: ",
                         pred()$enroll_pred$enroll_pred_day[1])
          str3 <- paste0("Prediction interval: ",
                         pred()$enroll_pred$enroll_pred_day[2], ", ",
                         pred()$enroll_pred$enroll_pred_day[3])
          text1 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text1 <- NULL
        }
      }
    } else {
      text1 <- NULL
    }

    if (!is.null(text1)) text1
  })


  # event predication date
  output$event_pred_date <- renderText({
    if (to_predict() == "Enrollment and event" ||
        to_predict() == "Event only") {

      req(pred()$event_pred)
      req(pred()$stage == input$stage && pred()$to_predict == to_predict())

      if (input$stage != "Design stage") {
        shiny::validate(
          need(!is.null(df()),
               "Please upload data for real-time prediction."))

        shiny::validate(
          need(target_d() > observed()$d0,
               "Target number of events has been reached."))

        if (input$dropout_model != "None" &&
            input$dropout_rate_method != "user_specified") {
          shiny::validate(
            need(observed()$c0 > 0, paste(
              "The number of dropouts must be positive",
              "to fit a dropout model.")))
        }

        if (!is.null(pred()$event_pred$event_pred_date)) {
          str1 <- paste0("Time from cutoff until ",
                         pred()$event_pred$target_d, " events: ",
                         pred()$event_pred$event_pred_date[1] -
                           observed()$cutoffdt + 1, " days",
                         " (95% CI: ",
                         pred()$event_pred$event_pred_date[2] -
                           observed()$cutoffdt + 1, " - ",
                         pred()$event_pred$event_pred_date[3] -
                           observed()$cutoffdt + 1, " days)")
          str2 <- paste0("Median prediction date: ",
                         pred()$event_pred$event_pred_date[1])
          str3 <- paste0("Prediction interval: ",
                         pred()$event_pred$event_pred_date[2], ", ",
                         pred()$event_pred$event_pred_date[3])
          text2 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text2 <- NULL
        }
      } else {
        if (!is.null(pred()$event_pred$event_pred_day)) {
          str1 <- paste0("Time from trial start until ",
                         pred()$event_pred$target_d, " events: ",
                         pred()$event_pred$event_pred_day[1], " days",
                         " (95% CI: ",
                         pred()$event_pred$event_pred_day[2], " - ",
                         pred()$event_pred$event_pred_day[3], " days)")
          str2 <- paste0("Median prediction day: ",
                         pred()$event_pred$event_pred_day[1])
          str3 <- paste0("Prediction interval: ",
                         pred()$event_pred$event_pred_day[2], ", ",
                         pred()$event_pred$event_pred_day[3])
          text2 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text2 <- NULL
        }
      }
    } else {
      text2 <- NULL
    }

    if (!is.null(text2)) text2
  })



  # event predication at given date
  output$event_pred_at_t <- renderText({
    if ((to_predict() == "Enrollment and event" ||
        to_predict() == "Event only") && input$pred_at_t) {

      req(pred()$event_pred)
      req(pred()$stage == input$stage && pred()$to_predict == to_predict())

      if (input$stage != "Design stage") {
        shiny::validate(
          need(!is.null(df()),
               "Please upload data for real-time prediction."))

        shiny::validate(
          need(target_d() > observed()$d0,
               "Target number of events has been reached."))

        if (input$dropout_model != "None" &&
            input$dropout_rate_method != "user_specified") {
          shiny::validate(
            need(observed()$c0 > 0, paste(
              "The number of dropouts must be positive",
              "to fit a dropout model.")))
        }

        if (!is.null(pred()$event_pred$pred_at_t)) {
          dx <- pred()$event_pred$pred_at_t
          str1 <- paste0("Predicted number of events by ", dx$date)
          str2 <- paste0("Median prediction: ", round(dx$n))
          str3 <- paste0("Prediction interval: ", round(dx$lower),
                         ", ", round(dx$upper))
          text3 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text3 <- NULL
        }
      } else {
        if (!is.null(pred()$event_pred$pred_at_t)) {
          str1 <- paste0("Predicted number of events by day ", dx$t)
          str2 <- paste0("Median prediction: ", round(dx$n))
          str3 <- paste0("Prediction interval: ", round(dx$lower),
                         ", ", round(dx$upper))
          text3 <- paste(paste("<b>", str1, "</b>"), str2, str3, sep="<br/>")
        } else {
          text3 <- NULL
        }
      }
    } else {
      text3 <- NULL
    }

    if (!is.null(text3)) text3
  })


  output$pred_date <- renderUI({
    if (to_predict() == "Enrollment only") {
      htmlOutput("enroll_pred_date")
    } else if (to_predict() == "Event only") {
      if (input$pred_at_t) {
        tagList(
          htmlOutput("event_pred_date"),
          tags$br(),
          htmlOutput("event_pred_at_t")
        )
      } else {
        htmlOutput("event_pred_date")
      }
    } else {
      if (input$pred_at_t) {
        tagList(
          htmlOutput("enroll_pred_date"),
          tags$br(),
          htmlOutput("event_pred_date"),
          tags$br(),
          htmlOutput("event_pred_at_t")
        )
      } else {
        tagList(
          htmlOutput("enroll_pred_date"),
          tags$br(),
          htmlOutput("event_pred_date")
        )
      }
    }
  })


  # enrollment and event prediction plot
  pred_plot <- reactive({
    if (to_predict() == "Enrollment only") {
      req(pred()$enroll_pred)
      req(pred()$stage == input$stage && pred()$to_predict == to_predict())

      if (input$stage != "Design stage") {
        shiny::validate(
          need(!is.null(df()),
               "Please upload data for real-time prediction."))

        shiny::validate(
          need(target_n() > observed()$n0,
               "Target enrollment has been reached."))
      }

      enroll_pred_plot <- pred()$enroll_pred$enroll_pred_plot
      enroll_pred_df <- pred()$enroll_pred$enroll_pred_df
      if ((!input$by_treatment || k() == 1) ||
          ((input$by_treatment || input$stage == "Design stage") &&
           k() > 1 && "treatment" %in% names(enroll_pred_df) &&
           length(table(enroll_pred_df$treatment)) == k() + 1)) {
        g <- enroll_pred_plot
      } else {
        g <- NULL
      }
    } else { # predict event only or predict enrollment and event
      shiny::validate(
        need(showEnrollment() || showEvent() || showDropout() ||
               showOngoing(),
             "Need at least one parameter to show on prediction plot"))

      req(pred()$event_pred)
      req(pred()$stage == input$stage && pred()$to_predict == to_predict())

      if (input$stage != "Design stage") {
        shiny::validate(
          need(!is.null(df()),
               "Please upload data for real-time prediction."))

        if (to_predict() == "Enrollment and event")
          shiny::validate(
            need(target_n() > observed()$n0,
                 "Target enrollment has been reached."))

        shiny::validate(
          need(target_d() > observed()$d0,
               "Target number of events has been reached."))

        if (input$dropout_model != "None" &&
            input$dropout_rate_method != "user_specified") {
          shiny::validate(
            need(observed()$c0 > 0, paste(
              "The number of dropouts must be positive",
              "to fit a dropout model.")))
        }
      }


      dt_list <- list()
      if (showEnrollment())
        dt_list <- c(dt_list, list(pred()$event_pred$enroll_pred_df))
      if (showEvent())
        dt_list <- c(dt_list, list(pred()$event_pred$event_pred_df))
      if (showDropout())
        dt_list <- c(dt_list, list(pred()$event_pred$dropout_pred_df))
      if (showOngoing())
        dt_list <- c(dt_list, list(pred()$event_pred$ongoing_pred_df))

      dfs <- data.table::rbindlist(dt_list, use.names = TRUE)


      if ((!input$by_treatment || k() == 1) &&
          !("treatment" %in% names(dfs))) { # overall
        if (input$stage != "Design stage") {
          dfa <- dfs[is.na(lower)]
          dfb <- dfs[!is.na(lower)]

          dfa_enrollment <- dfa[parameter == "Enrollment"]
          dfb_enrollment <- dfb[parameter == "Enrollment"]
          dfa_event <- dfa[parameter == "Event"]
          dfb_event <- dfb[parameter == "Event"]
          dfa_dropout <- dfa[parameter == "Dropout"]
          dfb_dropout <- dfb[parameter == "Dropout"]
          dfa_ongoing <- dfa[parameter == "Ongoing"]
          dfb_ongoing <- dfb[parameter == "Ongoing"]

          g <- plotly::plot_ly() %>%
            # --- Enrollment (blue family) ---
            plotly::add_lines(
              data = dfa_enrollment, x = ~date, y = ~n,
              line = list(shape="hv", width=2, color="#1E6BA0"),
              name = "observed",
              legendgroup = "Enrollment",
              legendgrouptitle = list(text = "Enrollment")) %>%
            plotly::add_lines(
              data = dfb_enrollment, x = ~date, y = ~n,
              line = list(width=2, color="#4A90D9"),
              name = "median prediction",
              legendgroup = "Enrollment") %>%
            plotly::add_ribbons(
              data = dfb_enrollment, x = ~date, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(74,144,217,0.2)",
              line = list(width=0, color="rgba(74,144,217,0.4)"),
              name = "prediction interval",
              legendgroup = "Enrollment") %>%
            # --- Event (red family) ---
            plotly::add_lines(
              data = dfa_event, x = ~date, y = ~n,
              line = list(shape="hv", width=2, color="#B03A2E"),
              name = "observed",
              legendgroup = "Event",
              legendgrouptitle = list(text = "Event")) %>%
            plotly::add_lines(
              data = dfb_event, x = ~date, y = ~n,
              line = list(width=2, color="#E74C3C"),
              name = "median prediction",
              legendgroup = "Event") %>%
            plotly::add_ribbons(
              data = dfb_event, x = ~date, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(231,76,60,0.2)",
              line = list(width=0, color="rgba(231,76,60,0.4)"),
              name = "prediction interval",
              legendgroup = "Event") %>%
            # --- Dropout (amber family) ---
            plotly::add_lines(
              data = dfa_dropout, x = ~date, y = ~n,
              line = list(shape="hv", width=2, color="#B9770E"),
              name = "observed",
              legendgroup = "Dropout",
              legendgrouptitle = list(text = "Dropout")) %>%
            plotly::add_lines(
              data = dfb_dropout, x = ~date, y = ~n,
              line = list(width=2, color="#F5B041"),
              name = "median prediction",
              legendgroup = "Dropout") %>%
            plotly::add_ribbons(
              data = dfb_dropout, x = ~date, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(245,176,65,0.2)",
              line = list(width=0, color="rgba(245,176,65,0.4)"),
              name = "prediction interval",
              legendgroup = "Dropout") %>%
            # --- Ongoing (teal family) ---
            plotly::add_lines(
              data = dfa_ongoing, x = ~date, y = ~n,
              line = list(shape="hv", width=2, color="#117A65"),
              name = "observed",
              legendgroup = "Ongoing",
              legendgrouptitle = list(text = "Ongoing")) %>%
            plotly::add_lines(
              data = dfb_ongoing, x = ~date, y = ~n,
              line = list(width=2, color="#1ABC9C"),
              name = "median prediction",
              legendgroup = "Ongoing") %>%
            plotly::add_ribbons(
              data = dfb_ongoing, x = ~date, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(26,188,156,0.2)",
              line = list(width=0, color="rgba(26,188,156,0.4)"),
              name = "prediction interval",
              legendgroup = "Ongoing") %>%
            plotly::add_lines(
              x = rep(observed()$cutoffdt, 2),
              y = c(min(dfa$n), max(dfb$upper)),
              name = "cutoff", line = list(dash="dash", color="grey50"),
              showlegend = FALSE) %>%
            plotly::layout(
              annotations = list(
                x = observed()$cutoffdt, y = 0, text = "cutoff",
                xanchor = "left", yanchor = "bottom", textangle = -90,
                font = list(size = 12), showarrow = FALSE),
              xaxis = list(title = "", zeroline = FALSE),
              yaxis = list(zeroline = FALSE),
              legend = list(
                orientation = "h", x = 0.5, y = -0.12,
                xanchor = "center", yanchor = "top",
                traceorder = "grouped", tracegroupgap = 6,
                font = list(size = 13),
                bgcolor = "rgba(255,255,255,0.9)"))

          if (observed()$tp < observed()$t0) {
            g <- g %>%
              plotly::add_lines(
                x = rep(observed()$cutofftpdt, 2),
                y = c(min(dfa$n), max(dfb$upper)),
                name = "prediction start",
                line = list(dash="dash"),
                showlegend = FALSE) %>%
              plotly::layout(
                annotations = list(
                  x = observed()$cutofftpdt, y = 0,
                  text = "prediction start",
                  xanchor = "left", yanchor = "bottom", textangle = -90,
                  font = list(size=12), showarrow = FALSE))
          }

          if (showEvent()) {
            g <- g %>%
              plotly::add_lines(
                x = range(dfs$date), y = rep(target_d(), 2),
                name = "target events", showlegend = FALSE,
                line = list(dash="dot", color="rgba(128, 128, 128, 0.5")) %>%
              plotly::layout(
                annotations = list(
                  x = 0.95, xref = "paper", y = target_d(),
                  text = "target events", xanchor = "right",
                  yanchor = "bottom", font = list(size = 12),
                  showarrow = FALSE))
          }
        } else {  # Design stage
          dfs_enrollment <- dfs[parameter == "Enrollment"]
          dfs_event <- dfs[parameter == "Event"]
          dfs_dropout <- dfs[parameter == "Dropout"]
          dfs_ongoing <- dfs[parameter == "Ongoing"]

          g <- plotly::plot_ly() %>%
            plotly::add_lines(
              data = dfs_enrollment, x = ~t, y = ~n,
              line = list(width=2, color="#4A90D9"),
              name = "median prediction",
              legendgroup = "Enrollment",
              legendgrouptitle = list(text = "Enrollment")) %>%
            plotly::add_ribbons(
              data = dfs_enrollment, x = ~t, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(74,144,217,0.2)",
              line = list(width=0, color="rgba(74,144,217,0.4)"),
              name = "prediction interval",
              legendgroup = "Enrollment") %>%
            plotly::add_lines(
              data = dfs_event, x = ~t, y = ~n,
              line = list(width=2, color="#E74C3C"),
              name = "median prediction",
              legendgroup = "Event",
              legendgrouptitle = list(text = "Event")) %>%
            plotly::add_ribbons(
              data = dfs_event, x = ~t, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(231,76,60,0.2)",
              line = list(width=0, color="rgba(231,76,60,0.4)"),
              name = "prediction interval",
              legendgroup = "Event") %>%
            plotly::add_lines(
              data = dfs_dropout, x = ~t, y = ~n,
              line = list(width=2, color="#F5B041"),
              name = "median prediction",
              legendgroup = "Dropout",
              legendgrouptitle = list(text = "Dropout")) %>%
            plotly::add_ribbons(
              data = dfs_dropout, x = ~t, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(245,176,65,0.2)",
              line = list(width=0, color="rgba(245,176,65,0.4)"),
              name = "prediction interval",
              legendgroup = "Dropout") %>%
            plotly::add_lines(
              data = dfs_ongoing, x = ~t, y = ~n,
              line = list(width=2, color="#1ABC9C"),
              name = "median prediction",
              legendgroup = "Ongoing",
              legendgrouptitle = list(text = "Ongoing")) %>%
            plotly::add_ribbons(
              data = dfs_ongoing, x = ~t, ymin = ~lower, ymax = ~upper,
              fillcolor = "rgba(26,188,156,0.2)",
              line = list(width=0, color="rgba(26,188,156,0.4)"),
              name = "prediction interval",
              legendgroup = "Ongoing") %>%
            plotly::layout(
              xaxis = list(title = "Days since trial start",
                           zeroline = FALSE),
              yaxis = list(zeroline = FALSE),
              legend = list(
                orientation = "h", x = 0.5, y = -0.12,
                xanchor = "center", yanchor = "top",
                traceorder = "grouped", tracegroupgap = 6,
                font = list(size = 13),
                bgcolor = "rgba(255,255,255,0.9)"))

          if (showEvent()) {
            g <- g %>%
              plotly::add_lines(
                x = range(dfs$t), y = rep(target_d(), 2),
                name = "target events", showlegend = FALSE,
                line = list(dash="dot", color="rgba(128, 128, 128, 0.5")) %>%
              plotly::layout(
                annotations = list(
                  x = 0.95, xref = "paper", y = target_d(),
                  text = "target events", xanchor = "right",
                  yanchor = "bottom", font = list(size = 12),
                  showarrow = FALSE))
          }
        }
      } else if (((input$by_treatment || input$stage == "Design stage") &&
                  k() > 1) && ("treatment" %in% names(dfs)) &&
                 (length(table(dfs$treatment)) == k() + 1)) { # by treatment
        if (input$stage != "Design stage") {
          dfa <- dfs[is.na(lower)]
          dfb <- dfs[!is.na(lower)]

          g <- list()
          for (i in c(9999, 1:k())) {
            dfsi <- dfs[treatment == i]
            dfbi <- dfb[treatment == i]
            dfai <- dfa[treatment == i]

            dfai_enrollment <- dfai[parameter == "Enrollment"]
            dfbi_enrollment <- dfbi[parameter == "Enrollment"]
            dfai_event <- dfai[parameter == "Event"]
            dfbi_event <- dfbi[parameter == "Event"]
            dfai_dropout <- dfai[parameter == "Dropout"]
            dfbi_dropout <- dfbi[parameter == "Dropout"]
            dfai_ongoing <- dfai[parameter == "Ongoing"]
            dfbi_ongoing <- dfbi[parameter == "Ongoing"]

            g[[(i+1) %% 9999]] <- plotly::plot_ly() %>%
              # --- Enrollment (blue family) ---
              plotly::add_lines(
                data = dfai_enrollment, x = ~date, y = ~n,
                line = list(shape="hv", width=2, color="#1E6BA0"),
                name = "observed",
                legendgroup = "Enrollment",
                legendgrouptitle = list(text = "Enrollment")) %>%
              plotly::add_lines(
                data = dfbi_enrollment, x = ~date, y = ~n,
                line = list(width=2, color="#4A90D9"),
                name = "median prediction",
                legendgroup = "Enrollment") %>%
              plotly::add_ribbons(
                data = dfbi_enrollment, x = ~date,
                ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(74,144,217,0.2)",
                line = list(width=0, color="rgba(74,144,217,0.4)"),
                name = "prediction interval",
                legendgroup = "Enrollment") %>%
              # --- Event (red family) ---
              plotly::add_lines(
                data = dfai_event, x = ~date, y = ~n,
                line = list(shape="hv", width=2, color="#B03A2E"),
                name = "observed",
                legendgroup = "Event",
                legendgrouptitle = list(text = "Event")) %>%
              plotly::add_lines(
                data = dfbi_event, x = ~date, y = ~n,
                line = list(width=2, color="#E74C3C"),
                name = "median prediction",
                legendgroup = "Event") %>%
              plotly::add_ribbons(
                data = dfbi_event, x = ~date, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(231,76,60,0.2)",
                line = list(width=0, color="rgba(231,76,60,0.4)"),
                name = "prediction interval",
                legendgroup = "Event") %>%
              # --- Dropout (amber family) ---
              plotly::add_lines(
                data = dfai_dropout, x = ~date, y = ~n,
                line = list(shape="hv", width=2, color="#B9770E"),
                name = "observed",
                legendgroup = "Dropout",
                legendgrouptitle = list(text = "Dropout")) %>%
              plotly::add_lines(
                data = dfbi_dropout, x = ~date, y = ~n,
                line = list(width=2, color="#F5B041"),
                name = "median prediction",
                legendgroup = "Dropout") %>%
              plotly::add_ribbons(
                data = dfbi_dropout, x = ~date, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(245,176,65,0.2)",
                line = list(width=0, color="rgba(245,176,65,0.4)"),
                name = "prediction interval",
                legendgroup = "Dropout") %>%
              # --- Ongoing (teal family) ---
              plotly::add_lines(
                data = dfai_ongoing, x = ~date, y = ~n,
                line = list(shape="hv", width=2, color="#117A65"),
                name = "observed",
                legendgroup = "Ongoing",
                legendgrouptitle = list(text = "Ongoing")) %>%
              plotly::add_lines(
                data = dfbi_ongoing, x = ~date, y = ~n,
                line = list(width=2, color="#1ABC9C"),
                name = "median prediction",
                legendgroup = "Ongoing") %>%
              plotly::add_ribbons(
                data = dfbi_ongoing, x = ~date, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(26,188,156,0.2)",
                line = list(width=0, color="rgba(26,188,156,0.4)"),
                name = "prediction interval",
                legendgroup = "Ongoing") %>%
              plotly::add_lines(
                x = rep(observed()$cutoffdt, 2),
                y = c(min(dfai$n), max(dfbi$upper)),
                name = "cutoff", line = list(dash="dash", color="grey50"),
                showlegend = FALSE) %>%
              plotly::layout(
                xaxis = list(title = "", zeroline = FALSE),
                yaxis = list(zeroline = FALSE),
                legend = list(
                  orientation = "h", x = 0.5, y = -0.12,
                  xanchor = "center", yanchor = "top",
                  traceorder = "grouped", tracegroupgap = 6,
                  font = list(size = 13),
                  bgcolor = "rgba(255,255,255,0.9)")) %>%
              plotly::layout(
                annotations = list(
                  x = 0.5, y = 1,
                  text = paste0("<b>", dfsi$treatment_description[1],
                                "</b>"),
                  xanchor = "center", yanchor = "bottom",
                  showarrow = FALSE, xref="paper", yref="paper"))


            if (observed()$tp < observed()$t0) {
              g[[(i+1) %% 9999]] <- g[[(i+1) %% 9999]] %>%
                plotly::add_lines(
                  x = rep(observed()$cutofftpdt, 2),
                  y = c(min(dfai$n), max(dfbi$upper)),
                  name = "prediction start",
                  line = list(dash="dash"),
                  showlegend = FALSE)
            }


            if (i == 9999) {
              g[[1]] <- g[[1]] %>%
                plotly::layout(
                  annotations = list(
                    x = observed()$cutoffdt, y = 0, text = "cutoff",
                    xanchor = "left", yanchor = "bottom", textangle = -90,
                    font = list(size = 12), showarrow = FALSE))

              if (observed()$tp < observed()$t0) {
                g[[1]] <- g[[1]] %>%
                  plotly::layout(
                    annotations = list(
                      x = observed()$cutofftpdt, y = 0,
                      text = "prediction start",
                      xanchor = "left", yanchor = "bottom", textangle = -90,
                      font = list(size=12), showarrow = FALSE))
              }

              if (showEvent()) {
                g[[1]] <- g[[1]] %>%
                  plotly::add_lines(
                    x = range(dfsi$date), y = rep(target_d(), 2),
                    name = "target events", showlegend = FALSE,
                    line = list(dash="dot",
                                color="rgba(128, 128, 128, 0.5")) %>%
                  plotly::layout(
                    annotations = list(
                      x = 0.95, xref = "paper", y = target_d(),
                      text = "target events", xanchor = "right",
                      yanchor = "bottom", font = list(size = 12),
                      showarrow = FALSE))
              }
            }
          }
        } else {  # Design stage
          g <- list()
          for (i in c(9999, 1:k())) {
            dfsi <- dfs[treatment == i]

            dfsi_enrollment <- dfsi[parameter == "Enrollment"]
            dfsi_event <- dfsi[parameter == "Event"]
            dfsi_dropout <- dfsi[parameter == "Dropout"]
            dfsi_ongoing <- dfsi[parameter == "Ongoing"]

            g[[(i+1) %% 9999]] <- plotly::plot_ly() %>%
              plotly::add_lines(
                data = dfsi_enrollment, x = ~t, y = ~n,
                line = list(width=2, color="#4A90D9"),
                name = "median prediction",
                legendgroup = "Enrollment",
                legendgrouptitle = list(text = "Enrollment")) %>%
              plotly::add_ribbons(
                data = dfsi_enrollment, x = ~t, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(74,144,217,0.2)",
                line = list(width=0, color="rgba(74,144,217,0.4)"),
                name = "prediction interval",
                legendgroup = "Enrollment") %>%
              plotly::add_lines(
                data = dfsi_event, x = ~t, y = ~n,
                line = list(width=2, color="#E74C3C"),
                name = "median prediction",
                legendgroup = "Event",
                legendgrouptitle = list(text = "Event")) %>%
              plotly::add_ribbons(
                data = dfsi_event, x = ~t, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(231,76,60,0.2)",
                line = list(width=0, color="rgba(231,76,60,0.4)"),
                name = "prediction interval",
                legendgroup = "Event") %>%
              plotly::add_lines(
                data = dfsi_dropout, x = ~t, y = ~n,
                line = list(width=2, color="#F5B041"),
                name = "median prediction",
                legendgroup = "Dropout",
                legendgrouptitle = list(text = "Dropout")) %>%
              plotly::add_ribbons(
                data = dfsi_dropout, x = ~t, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(245,176,65,0.2)",
                line = list(width=0, color="rgba(245,176,65,0.4)"),
                name = "prediction interval",
                legendgroup = "Dropout") %>%
              plotly::add_lines(
                data = dfsi_ongoing, x = ~t, y = ~n,
                line = list(width=2, color="#1ABC9C"),
                name = "median prediction",
                legendgroup = "Ongoing",
                legendgrouptitle = list(text = "Ongoing")) %>%
              plotly::add_ribbons(
                data = dfsi_ongoing, x = ~t, ymin = ~lower, ymax = ~upper,
                fillcolor = "rgba(26,188,156,0.2)",
                line = list(width=0, color="rgba(26,188,156,0.4)"),
                name = "prediction interval",
                legendgroup = "Ongoing") %>%
              plotly::layout(
                xaxis = list(title = "Days since trial start",
                             zeroline = FALSE),
                yaxis = list(zeroline = FALSE),
                legend = list(
                  orientation = "h", x = 0.5, y = -0.12,
                  xanchor = "center", yanchor = "top",
                  traceorder = "grouped", tracegroupgap = 6,
                  font = list(size = 13),
                  bgcolor = "rgba(255,255,255,0.9)")) %>%
              plotly::layout(
                annotations = list(
                  x = 0.5, y = 1,
                  text = paste0("<b>", dfsi$treatment_description[1],
                                "</b>"),
                  xanchor = "center", yanchor = "bottom",
                  showarrow = FALSE, xref="paper", yref="paper"))


            if (i == 9999) {
              if (showEvent()) {
                g[[1]] <- g[[1]] %>%
                  plotly::add_lines(
                    x = range(dfsi$t), y = rep(target_d(), 2),
                    name = "target events", showlegend = FALSE,
                    line = list(dash="dot",
                                color="rgba(128, 128, 128, 0.5")) %>%
                  plotly::layout(
                    annotations = list(
                      x = 0.95, xref = "paper", y = target_d(),
                      text = "target events", xanchor = "right",
                      yanchor = "bottom", font = list(size = 12),
                      showarrow = FALSE))
              }
            }
          }
        }

      } else {
        g <- NULL
      }

    }

    g
  })


  mult_plot <- reactive({
    (to_predict() == "Enrollment only" &&
       (input$by_treatment || input$stage == "Design stage") && k() > 1 &&
       "treatment" %in% names(pred()$enroll_pred$enroll_pred_df) &&
       length(table(pred()$enroll_pred$enroll_pred_df$treatment)) ==
       k() + 1) ||
      (to_predict() != "Enrollment only" &&
         (input$by_treatment || input$stage == "Design stage") && k() > 1 &&
         "treatment" %in% names(pred()$event_pred$event_pred_df) &&
         length(table(pred()$event_pred$event_pred_df$treatment)) ==
         k() + 1)
  })


  observe({
    walk(1:6, function(i) {
      output[[paste0("pred_plot_output", i)]] <- renderPlotly({
        p <- NULL
        if (i <= k() + 1) {
          if (mult_plot()) {
            p <- pred_plot()[[i]]
          } else {
            p <- pred_plot()
          }
        }
        style_plotly(p) %>% plotly::layout(
          title = list(text = "Prediction curve", font = list(family = "Arial")),
          height = 700)
      })
    })
  })


  pred_plot_outputs <- reactive({
    n = ifelse(mult_plot(), k() + 1, 1)
    outputs <- map(1:n, function(i) {
      plotlyOutput(paste0("pred_plot_output", i), height = "700px")
    })

    tagList(outputs)
  })

  output$pred_plot <- renderUI({
    pred_plot_outputs()
  })


  # ==========================================================
  # Prediction Report Download
  # ==========================================================
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("SurvPred_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      p <- pred()
      o <- if (!is.null(df())) observed() else NULL
      ef <- enroll_fit()

      # Build model specification text
      enroll_model_text <- if (!is.null(ef)) {
        paste0(ef$text[1], ", ", ef$text[2], ", ", ef$text[3])
      } else if (input$stage == "Design stage") {
        input$enroll_prior
      } else {
        "Not specified"
      }

      event_model_text <- if (input$stage != "Design stage" &&
                              to_predict() != "Enrollment only") {
        if (input$by_treatment && k() > 1) {
          paste(input$event_model, "(by treatment)")
        } else {
          input$event_model
        }
      } else if (input$stage == "Design stage" &&
                 to_predict() != "Enrollment only") {
        input$event_prior
      } else {
        "N/A"
      }

      dropout_model_text <- if (input$dropout_model == "None") {
        "None"
      } else if (input$dropout_rate_method == "user_specified") {
        paste("User-specified:", input$user_dropout_hazard_rate,
              "events/subject/month")
      } else {
        input$dropout_model
      }

      # Prediction summary text
      enroll_pred_text <- ""
      if (!is.null(p$enroll_pred) && !is.null(p$enroll_pred$enroll_pred_summary)) {
        enroll_pred_text <- gsub("\n", "<br>", p$enroll_pred$enroll_pred_summary)
      }
      event_pred_text <- ""
      if (!is.null(p$event_pred) && !is.null(p$event_pred$event_pred_summary)) {
        event_pred_text <- gsub("\n", "<br>", p$event_pred$event_pred_summary)
      }

      # Observed data summary
      observed_section <- ""
      if (!is.null(o)) {
        observed_section <- paste0(
          "<h3>Observed Data Summary</h3>",
          "<table>",
          "<tr><td>Trial start date</td><td>", o$trialsdt, "</td></tr>",
          "<tr><td>Data cutoff date</td><td>", o$cutoffdt, "</td></tr>",
          "<tr><td>Enrollment duration (days)</td><td>", o$t0, "</td></tr>",
          "<tr><td>Subjects enrolled</td><td>", o$n0, "</td></tr>")
        if (!is.null(o$d0)) {
          observed_section <- paste0(observed_section,
            "<tr><td>Events observed</td><td>", o$d0, "</td></tr>",
            "<tr><td>Dropouts observed</td><td>", o$c0, "</td></tr>",
            "<tr><td>Subjects at risk</td><td>", o$r0, "</td></tr>")
        }
        observed_section <- paste0(observed_section, "</table>")
      }

      # Assemble HTML
      html <- paste0(
        "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
        "<title>SurvPred Prediction Report</title>",
        "<style>",
        "body{font-family:Arial,sans-serif;max-width:960px;margin:0 auto;",
        "padding:2rem;color:#334155;line-height:1.6}",
        ".header{background:linear-gradient(135deg,#0a1628,#1B3A5C,#2E86AB);",
        "color:white;padding:2rem;border-radius:12px;margin-bottom:2rem}",
        ".header h1{margin:0;font-size:2rem}",
        ".header p{margin:.5rem 0 0;opacity:.85}",
        "h2{color:#1B3A5C;border-bottom:2px solid #2E86AB;",
        "padding-bottom:.4rem;margin-top:2rem}",
        "h3{color:#234b73;margin-top:1.5rem}",
        "table{width:100%;border-collapse:collapse;margin:1rem 0}",
        "td{padding:.5rem 1rem;border-bottom:1px solid #e2e8f0}",
        "td:first-child{font-weight:600;color:#1B3A5C;background:#f8fafc;width:50%}",
        ".grid{display:grid;grid-template-columns:1fr 1fr;gap:.5rem 2rem}",
        ".item{display:flex;justify-content:space-between;padding:.35rem 0;",
        "border-bottom:1px solid #f1f5f9}",
        ".item span:first-child{font-weight:600;color:#64748b}",
        ".pred{background:#f8fafc;border-left:4px solid #2E86AB;",
        "padding:1rem;margin:1rem 0;white-space:pre-line;font-size:.95rem}",
        ".footer{text-align:center;color:#94a3b8;font-size:.85rem;",
        "margin-top:3rem;border-top:1px solid #e2e8f0;padding-top:1.5rem}",
        "</style></head><body>",
        "<div class='header'><h1>SurvPred &mdash; Prediction Report</h1>",
        "<p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p></div>",
        "<h2>Study Configuration</h2><div class='grid'>",
        "<div class='item'><span>Study Stage</span><span>", input$stage, "</span></div>",
        "<div class='item'><span>Prediction Target</span><span>", to_predict(), "</span></div>",
        "<div class='item'><span>Target Enrollment</span><span>", input$target_n, "</span></div>")

      if (to_predict() != "Enrollment only") {
        html <- paste0(html,
          "<div class='item'><span>Target Events</span><span>", input$target_d, "</span></div>")
      }

      html <- paste0(html,
        "<div class='item'><span>Prediction Interval</span><span>",
        as.numeric(input$pilevel)*100, "%</span></div>",
        "<div class='item'><span>Years After Cutoff</span><span>", input$nyears, "</span></div>",
        "<div class='item'><span>Stratify by Treatment</span><span>", input$by_treatment, "</span></div>",
        "<div class='item'><span>Simulations</span><span>", input$nreps, "</span></div>",
        "<div class='item'><span>Seed</span><span>", input$seed, "</span></div>",
        "</div>",

        "<h2>Model Specifications</h2><div class='grid'>",
        "<div class='item'><span>Enrollment Model</span><span>", enroll_model_text, "</span></div>")

      if (to_predict() != "Enrollment only") {
        html <- paste0(html,
          "<div class='item'><span>Event Model</span><span>", event_model_text, "</span></div>",
          "<div class='item'><span>Dropout Model</span><span>", dropout_model_text, "</span></div>")
      }

      html <- paste0(html, "</div>", observed_section, "<h2>Prediction Results</h2>")

      if (enroll_pred_text != "") {
        html <- paste0(html, "<h3>Enrollment Prediction</h3><div class='pred'>",
                       enroll_pred_text, "</div>")
      }
      if (event_pred_text != "") {
        html <- paste0(html, "<h3>Event Prediction</h3><div class='pred'>",
                       event_pred_text, "</div>")
      }

      html <- paste0(html,
        "<div class='footer'><p>Generated by SurvPred &mdash; Clinical Trial Survival Prediction</p>",
        "</div></body></html>")

      writeLines(html, file, useBytes = TRUE)
    }
  )


  # ============================================================
  # Sensitivity Analysis
  # ============================================================

  # -- helper: re-run prediction with perturbed rates ----------
  run_sensitivity_scenario <- function(user_enroll_rate = NULL,
                                       user_dropout_rate = NULL,
                                       dropout_model = "exponential") {
    if (input$stage == "Design stage") {
      # Design stage: perturb prior parameters
      stop("Sensitivity analysis is only available at the analysis stage.")
    }

    # Use the same seed as the Prediction module so that the
    # sensitivity baseline run is reproducible and comparable to
    # the Prediction result (when models and fix_parameter match).
    # All perturbation scenarios share this seed (common random
    # numbers) so differences are due to the parameter change,
    # not simulation noise.
    set.seed(as.numeric(input$seed))

    args <- list(
      df = df(),
      to_predict = to_predict(),
      pilevel = pilevel(),
      nyears = nyears(),
      target_t = target_t(),
      nreps = nreps(),
      showEnrollment = showEnrollment(),
      showEvent = showEvent(),
      showDropout = showDropout(),
      showOngoing = showOngoing(),
      showsummary = FALSE,
      showplot = FALSE,
      by_treatment = input$by_treatment,
      alloc = treatment_allocation(),
      # Always fix parameters in sensitivity scenarios so that the
      # baseline and all perturbed runs use the same parameter
      # handling.  Otherwise dropout-perturbation runs would be
      # forced to fix_parameter = TRUE inside eventPred() while the
      # baseline could use fix_parameter = FALSE — making the two
      # sets of predictions incomparable.
      fix_parameter = TRUE
    )

    if (to_predict() != "Event only") {
      # Sensitivity module always uses Poisson for enrollment,
      # independent of the user's selection in the Enrollment Fit tab.
      # Poisson needs no extra parameters (nknots, lags, accrualTime).
      args$target_n       <- target_n()
      args$enroll_model   <- "Poisson"
      args$user_enroll_rate  <- user_enroll_rate
      args$user_accrualTime  <- NULL
    }

    if (to_predict() != "Enrollment only") {
      # Sensitivity module always uses Exponential for time-to-event
      # and Exponential for dropout, independent of the user's
      # selections in the Event Fit / Dropout Fit tabs.
      # Exponential needs no extra parameters (piecewiseSurvivalTime,
      # spline_k, scale, m, piecewiseDropoutTime, etc.).
      args$target_d              <- target_d()
      args$event_model           <- "Exponential"
      args$dropout_model         <- dropout_model
      args$user_dropout_rate     <- user_dropout_rate
    }

    tryCatch({
      do.call(runPrediction, args)
    }, error = function(e) NULL)
  }


  # -- extract key metric (days from cutoff) from a pred result
  extract_days_to_target <- function(pred_result) {
    if (is.null(pred_result)) return(NA_real_)
    if (to_predict() == "Enrollment only") {
      if (!is.null(pred_result$enroll_pred$enroll_pred_date)) {
        pred_result$enroll_pred$enroll_pred_date[1] -
          observed()$cutoffdt + 1
      } else {
        NA_real_
      }
    } else {
      if (!is.null(pred_result$event_pred$event_pred_date)) {
        pred_result$event_pred$event_pred_date[1] -
          observed()$cutoffdt + 1
      } else {
        NA_real_
      }
    }
  }


  # -- baseline metric from the user's existing prediction ----
  baseline_days <- reactive({
    req(pred())
    extract_days_to_target(pred())
  })


  # -- main sensitivity eventReactive -------------------------
  sensitivity_results <- eventReactive(input$run_sensitivity, {
    req(df())

    # Require that the user has already run Prediction at least once
    shiny::validate(
      need(!is.null(pred()),
           paste("Please run the Prediction first (click \"Run Prediction\"",
                 "in the left sidebar), then return to the Sensitivity tab.")))

    # only analysis stage
    shiny::validate(
      need(input$stage != "Design stage",
           paste("Sensitivity analysis requires observed data.",
                 "Please switch to an analysis-stage study and",
                 "upload data first.")))

    pert_pcts <- seq(input$sens_range[1], input$sens_range[2],
                     by = input$sens_step)
    # ensure 0 (baseline) is included once
    pert_pcts <- sort(unique(c(pert_pcts, 0)))

    base_enroll_rate  <- observed()$n0 / observed()$t0  # daily

    # Since the sensitivity module always uses Exponential for dropout,
    # the baseline dropout hazard is simply the naive MLE:
    #   c0 / total_person_time
    # This matches the Exponential fit that runPrediction will produce
    # internally when dropout_model = "Exponential".
    get_fitted_dropout_hazard <- function() {
      c0 <- observed()$c0
      if (is.null(c0) || c0 <= 0) return(NA_real_)
      total_pt <- sum(df()$time, na.rm = TRUE)
      if (total_pt <= 0) return(NA_real_)
      c0 / total_pt  # daily dropout hazard (MLE for Exponential)
    }

    base_dropout_haz <- get_fitted_dropout_hazard()

    do_enroll  <- "enroll"  %in% input$sens_factors
    do_dropout <- "dropout" %in% input$sens_factors &&
      !is.na(base_dropout_haz) && base_dropout_haz > 0

    # When there are no observed dropouts, skip dropout model entirely
    # in the baseline run to avoid fitting errors.
    bl_dropout_model <- if (is.na(base_dropout_haz) || base_dropout_haz <= 0) {
      "none"
    } else {
      "exponential"
    }

    rows <- list()
    total <- length(pert_pcts) * ((do_enroll + do_dropout) + 1)  # +1 for baseline

    withProgress(message = "Running sensitivity analysis...",
                 value = 0, max = total, {
      step <- 0

      # ---- baseline (fix_parameter = TRUE for consistency) ----
      incProgress(0, detail = "Running baseline scenario...")
      bl_pred <- run_sensitivity_scenario(
        user_enroll_rate = NULL, user_dropout_rate = NULL,
        dropout_model = bl_dropout_model)
      bl_days <- extract_days_to_target(bl_pred)

      # ---- enrollment rate scenarios ----
      if (do_enroll) {
        for (pct in pert_pcts[pert_pcts != 0]) {
          step <- step + 1
          incProgress(1/total, detail = paste0(
            "Enrollment rate ", ifelse(pct>0,"+",""), pct, "%"))
          new_rate <- base_enroll_rate * (1 + pct/100)
          res <- run_sensitivity_scenario(
            user_enroll_rate = new_rate, user_dropout_rate = NULL,
            dropout_model = bl_dropout_model)
          rows[[length(rows) + 1]] <- data.table(
            Parameter  = "Enrollment rate",
            Change     = paste0(ifelse(pct>0,"+",""), pct, "%"),
            Rate       = round(new_rate * 30.4375, 2),
            DaysToTarget = extract_days_to_target(res))
        }
      }

      # ---- dropout rate scenarios ----
      if (do_dropout) {
        for (pct in pert_pcts[pert_pcts != 0]) {
          step <- step + 1
          incProgress(1/total, detail = paste0(
            "Dropout rate ", ifelse(pct>0,"+",""), pct, "%"))
          new_haz <- base_dropout_haz * (1 + pct/100)
          res <- run_sensitivity_scenario(
            user_enroll_rate = NULL, user_dropout_rate = new_haz,
            dropout_model = "exponential")
          rows[[length(rows) + 1]] <- data.table(
            Parameter  = "Dropout rate",
            Change     = paste0(ifelse(pct>0,"+",""), pct, "%"),
            Rate       = round(new_haz * 30.4375, 4),
            DaysToTarget = extract_days_to_target(res))
        }
      }

      incProgress(1, detail = "Done")
    })

    tbl <- rbindlist(rows)
    if (nrow(tbl) == 0) return(NULL)

    list(
      table      = tbl,
      baseline   = bl_days,
      parameters = unique(tbl$Parameter))
  })


  # -- baseline text ------------------------------------------
  output$sensitivity_baseline <- renderText({
    req(sensitivity_results())
    bl <- sensitivity_results()$baseline
    metric <- if (to_predict() == "Enrollment only")
      "target enrollment" else "target events"

    paste0(
      "<span style='font-size:15px;color:#334155;'>",
      "Baseline prediction: <b>", round(bl, 0), "</b> days from cutoff to ",
      metric, "</span>")
  })


  # -- tornado plot -------------------------------------------
  output$tornado_plot <- renderPlotly({
    req(sensitivity_results())
    sr  <- sensitivity_results()
    tbl <- sr$table
    bl  <- sr$baseline

    # for each parameter, find the worst (longest) and best (shortest)
    tornado_data <- tbl[, .(
      low_days  = min(DaysToTarget, na.rm = TRUE),
      high_days = max(DaysToTarget, na.rm = TRUE),
      low_pct   = Change[which.min(DaysToTarget)],
      high_pct  = Change[which.max(DaysToTarget)]
    ), by = Parameter]

    # impact = range width → sort descending
    tornado_data[, impact := high_days - low_days]
    data.table::setorderv(tornado_data, "impact", -1)

    n_params <- nrow(tornado_data)
    if (n_params == 0) return(NULL)

    # colors
    enroll_color <- "#4A90D9"   # blue — enrollment
    dropout_color <- "#F5B041"  # amber — dropout
    param_colors <- c(
      "Enrollment rate" = enroll_color,
      "Dropout rate"    = dropout_color)

    p <- plotly::plot_ly()

    for (i in seq_len(n_params)) {
      param <- tornado_data$Parameter[i]
      color <- param_colors[param]

      p <- p %>%
        plotly::add_trace(
          x = c(tornado_data$low_days[i], tornado_data$high_days[i]),
          y = c(param, param),
          type = "scatter", mode = "lines+markers",
          line = list(color = color, width = 18),
          marker = list(
            symbol = "line-ns", size = 12,
            line = list(color = color, width = 2)),
          name = param,
          legendgroup = param,
          showlegend = TRUE,
          hovertemplate = paste0(
            "<b>", param, "</b><br>",
            tornado_data$low_pct[i],  ": ", round(tornado_data$low_days[i]),  " days<br>",
            tornado_data$high_pct[i], ": ", round(tornado_data$high_days[i]), " days<br>",
            "<extra></extra>"))
    }

    # baseline reference line — use categorical y values so plotly
    # does not inject spurious numeric ticks on the y-axis
    p <- p %>%
      plotly::add_lines(
        x = c(bl, bl),
        y = c(tornado_data$Parameter[1], tornado_data$Parameter[n_params]),
        line = list(color = "#E74C3C", width = 2, dash = "dash"),
        name = "Baseline",
        showlegend = TRUE,
        hovertemplate = paste0("Baseline: ", round(bl), " days<extra></extra>"))

    p <- p %>%
      plotly::layout(
        title = list(
          text = paste0("Tornado Plot — Sensitivity of Predicted Time to Target",
                        "<br><sup style='font-size:12px;color:#888;'>",
                        "Models: Poisson (enrollment), Exponential (event), ",
                        "Exponential (dropout)</sup>"),
          font = list(size = 16, family = "Arial")),
        xaxis = list(
          title = "Days from cutoff to target",
          zeroline = FALSE,
          tickfont = list(size = 13)),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          tickfont = list(size = 13),
          automargin = TRUE),
        legend = list(
          orientation = "h", x = 0.5, y = -0.15,
          xanchor = "center", yanchor = "top",
          font = list(size = 12)),
        margin = list(l = 150),
        hovermode = "closest")

    p
  })


  # -- scenario results table ---------------------------------
  output$sensitivity_table <- DT::renderDT({
    req(sensitivity_results())
    sr <- sensitivity_results()

    tbl <- sr$table
    bl  <- sr$baseline

    display <- data.table::copy(tbl)
    display[, Deviation := DaysToTarget - bl]
    setnames(display,
      c("DaysToTarget", "Rate"),
      c("Days to Target", "Monthly Rate"))

    # order by absolute deviation descending
    display[, abs_dev := abs(Deviation)]
    data.table::setorderv(display, c("Parameter", "abs_dev"), c(1, -1))
    display[, abs_dev := NULL]

    DT::datatable(display,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")))) %>%
      DT::formatRound("Days to Target", 1) %>%
      DT::formatRound("Deviation", 1) %>%
      DT::formatStyle("Deviation",
        color = DT::styleInterval(0, c("#E74C3C", "#27AE60")))
  })


  observeEvent(input$add_accrualTime, {
    a = matrix(as.numeric(input$accrualTime),
               ncol=ncol(input$accrualTime))
    b = matrix(a[nrow(a),] + 90, nrow=1)
    c = rbind(a, b)
    rownames(c) = paste("Interval", seq(1,nrow(c)))
    colnames(c) = colnames(input$accrualTime)
    updateMatrixInput(session, "accrualTime", c)
  })


  observeEvent(input$del_accrualTime, {
    if (nrow(input$accrualTime) >= 2) {
      a = matrix(as.numeric(input$accrualTime),
                 ncol=ncol(input$accrualTime))
      b = matrix(a[-nrow(a),], ncol=ncol(a))
      rownames(b) = paste("Interval", seq(1,nrow(b)))
      colnames(b) = colnames(input$accrualTime)
      updateMatrixInput(session, "accrualTime", b)
    }
  })


  observeEvent(input$add_piecewise_poisson_rate, {
    a = matrix(as.numeric(input$piecewise_poisson_rate),
               ncol=ncol(input$piecewise_poisson_rate))
    b = matrix(a[nrow(a),], nrow=1)
    b[1,1] = b[1,1] + 90
    c = rbind(a, b)
    rownames(c) = paste("Interval", seq(1,nrow(c)))
    colnames(c) = colnames(input$piecewise_poisson_rate)
    updateMatrixInput(session, "piecewise_poisson_rate", c)
  })


  observeEvent(input$del_piecewise_poisson_rate, {
    if (nrow(input$piecewise_poisson_rate) >= 2) {
      a = matrix(as.numeric(input$piecewise_poisson_rate),
                 ncol=ncol(input$piecewise_poisson_rate))
      b = matrix(a[-nrow(a),], ncol=ncol(a))
      rownames(b) = paste("Interval", seq(1,nrow(b)))
      colnames(b) = colnames(input$piecewise_poisson_rate)
      updateMatrixInput(session, "piecewise_poisson_rate", b)
    }
  })


  observeEvent(input$add_user_piecewise_rate, {
    a = matrix(as.numeric(input$user_piecewise_rate),
               ncol=ncol(input$user_piecewise_rate))
    b = matrix(a[nrow(a),], nrow=1)
    b[1,1] = b[1,1] + 90
    c = rbind(a, b)
    rownames(c) = paste("Interval", seq(1,nrow(c)))
    colnames(c) = colnames(input$user_piecewise_rate)
    updateMatrixInput(session, "user_piecewise_rate", c)
  })


  observeEvent(input$del_user_piecewise_rate, {
    if (nrow(input$user_piecewise_rate) >= 2) {
      a = matrix(as.numeric(input$user_piecewise_rate),
                 ncol=ncol(input$user_piecewise_rate))
      b = matrix(a[-nrow(a),], ncol=ncol(a))
      rownames(b) = paste("Interval", seq(1,nrow(b)))
      colnames(b) = colnames(input$user_piecewise_rate)
      updateMatrixInput(session, "user_piecewise_rate", b)
    }
  })


  observeEvent(input$add_piecewiseSurvivalTime, {
    a = matrix(as.numeric(input$piecewiseSurvivalTime),
               ncol=ncol(input$piecewiseSurvivalTime))
    b = matrix(a[nrow(a),] + 90, nrow=1)
    c = rbind(a, b)
    rownames(c) = paste("Interval", seq(1,nrow(c)))
    colnames(c) = colnames(input$piecewiseSurvivalTime)
    updateMatrixInput(session, "piecewiseSurvivalTime", c)
  })


  observeEvent(input$del_piecewiseSurvivalTime, {
    if (nrow(input$piecewiseSurvivalTime) >= 2) {
      a = matrix(as.numeric(input$piecewiseSurvivalTime),
                 ncol=ncol(input$piecewiseSurvivalTime))
      b = matrix(a[-nrow(a),], ncol=ncol(a))
      rownames(b) = paste("Interval", seq(1,nrow(b)))
      colnames(b) = colnames(input$piecewiseSurvivalTime)
      updateMatrixInput(session, "piecewiseSurvivalTime", b)
    }
  })


  lapply(1:6, function(i) {
    pwexp <- paste0("piecewise_exponential_survival_", i)
    observeEvent(input[[paste0("add_piecewise_exponential_survival_", i)]], {
      a = matrix(as.numeric(input[[pwexp]]), ncol=ncol(input[[pwexp]]))
      b = matrix(a[nrow(a),], nrow=1)
      b[1,1] = b[1,1] + 90
      c = rbind(a, b)
      rownames(c) = paste("Interval", seq(1,nrow(c)))
      colnames(c) = colnames(input[[pwexp]])
      updateMatrixInput(session, pwexp, c)
    })
  })


  lapply(1:6, function(i) {
    pwexp <- paste0("piecewise_exponential_survival_", i)
    observeEvent(input[[paste0("del_piecewise_exponential_survival_", i)]], {
      if (nrow(input[[pwexp]]) >= 2) {
        a = matrix(as.numeric(input[[pwexp]]), ncol=ncol(input[[pwexp]]))
        b = matrix(a[-nrow(a),], ncol=ncol(a))
        rownames(b) = paste("Interval", seq(1,nrow(b)))
        colnames(b) = colnames(input[[pwexp]])
        updateMatrixInput(session, pwexp, b)
      }
    })
  })


  observeEvent(input$add_piecewiseDropoutTime, {
    a = matrix(as.numeric(input$piecewiseDropoutTime),
               ncol=ncol(input$piecewiseDropoutTime))
    b = matrix(a[nrow(a),] + 90, nrow=1)
    c = rbind(a, b)
    rownames(c) = paste("Interval", seq(1,nrow(c)))
    colnames(c) = colnames(input$piecewiseDropoutTime)
    updateMatrixInput(session, "piecewiseDropoutTime", c)
  })


  observeEvent(input$del_piecewiseDropoutTime, {
    if (nrow(input$piecewiseDropoutTime) >= 2) {
      a = matrix(as.numeric(input$piecewiseDropoutTime),
                 ncol=ncol(input$piecewiseDropoutTime))
      b = matrix(a[-nrow(a),], ncol=ncol(a))
      rownames(b) = paste("Interval", seq(1,nrow(b)))
      colnames(b) = colnames(input$piecewiseDropoutTime)
      updateMatrixInput(session, "piecewiseDropoutTime", b)
    }
  })


  lapply(1:6, function(i) {
    pwexp <- paste0("piecewise_exponential_dropout_", i)
    observeEvent(input[[paste0("add_piecewise_exponential_dropout_", i)]], {
      a = matrix(as.numeric(input[[pwexp]]), ncol=ncol(input[[pwexp]]))
      b = matrix(a[nrow(a),], nrow=1)
      b[1,1] = b[1,1] + 90
      c = rbind(a, b)
      rownames(c) = paste("Interval", seq(1,nrow(c)))
      colnames(c) = colnames(input[[pwexp]])
      updateMatrixInput(session, pwexp, c)
    })
  })


  lapply(1:6, function(i) {
    pwexp <- paste0("piecewise_exponential_dropout_", i)
    observeEvent(input[[paste0("del_piecewise_exponential_dropout_", i)]], {
      if (nrow(input[[pwexp]]) >= 2) {
        a = matrix(as.numeric(input[[pwexp]]), ncol=ncol(input[[pwexp]]))
        b = matrix(a[-nrow(a),], ncol=ncol(a))
        rownames(b) = paste("Interval", seq(1,nrow(b)))
        colnames(b) = colnames(input[[pwexp]])
        updateMatrixInput(session, pwexp, b)
      }
    })
  })


  # save inputs
  output$saveInputs <- downloadHandler(
    filename = function() {
      paste0("inputs_", Sys.Date(), "_survPred.RData")
    },

    content = function(file) {
      x <- list(
        stage = input$stage,
        to_predict = input$to_predict,
        to_predict2 = input$to_predict2,
        target_n = target_n(),
        target_d = input$target_d,
        pilevel = pilevel(),
        nyears = nyears(),
        pred_at_t = input$pred_at_t,
        target_t = target_t(),
        to_show = input$to_show,
        by_treatment = input$by_treatment,
        k = k(),
        treatment_allocation = matrix(
          treatment_allocation(), ncol=1,
          dimnames = list(treatment_description(), "Size")),
        fix_parameter = input$fix_parameter,
        nreps = nreps(),
        seed = input$seed,

        enroll_prior = input$enroll_prior,
        poisson_rate = poisson_rate(),
        mu = mu(),
        delta = delta(),
        piecewise_poisson_rate = piecewise_poisson_rate(),
        enroll_model = input$enroll_model,
        nknots = nknots(),
        lags = lags(),
        accrualTime = matrix(
          accrualTime(), ncol = 1,
          dimnames = list(paste("Interval", 1:length(accrualTime())),
                          "Starting time")),

        enroll_rate_method = input$enroll_rate_method,
        user_rate_type = input$user_rate_type,
        user_constant_rate = user_constant_rate(),
        user_piecewise_rate = user_piecewise_rate(),

        event_prior = input$event_prior,
        exponential_survival = matrix(
          exponential_survival(), nrow = 1,
          dimnames = list(NULL, treatment_description())),
        weibull_survival = matrix(
          weibull_survival(), nrow = 2,
          dimnames = list(c("Shape", "Scale"), treatment_description())),
        llogis_survival = matrix(
          llogis_survival(), nrow = 2,
          dimnames = list(c("Location on log scale", "Scale on log scale"),
                          treatment_description())),
        lnorm_survival = matrix(
          lnorm_survival(), nrow = 2,
          dimnames = list(c("Mean on log scale", "SD on log scale"),
                          treatment_description())),
        piecewise_exponential_survival = matrix(
          piecewise_exponential_survival(), ncol = k()+1,
          dimnames = list(paste("Interval",
                                1:nrow(piecewise_exponential_survival())),
                          c("Starting time", treatment_description()))),
        event_model = input$event_model,
        piecewiseSurvivalTime = matrix(
          piecewiseSurvivalTime(), ncol = 1,
          dimnames = list(paste("Interval",
                                1:length(piecewiseSurvivalTime())),
                          "Starting time")),
        spline_k = spline_k(),
        spline_scale = input$spline_scale,
        m_event = m_event(),

        dropout_prior = input$dropout_prior,
        exponential_dropout = matrix(
          exponential_dropout(), nrow = 1,
          dimnames = list(NULL, treatment_description())),
        weibull_dropout = matrix(
          weibull_dropout(), nrow = 2,
          dimnames = list(c("Shape", "Scale"), treatment_description())),
        llogis_dropout = matrix(
          llogis_dropout(), nrow = 2,
          dimnames = list(c("Location on log scale", "Scale on log scale"),
                          treatment_description())),
        lnorm_dropout = matrix(
          lnorm_dropout(), nrow = 2,
          dimnames = list(c("Mean on log scale", "SD on log scale"),
                          treatment_description())),
        piecewise_exponential_dropout = matrix(
          piecewise_exponential_dropout(), ncol = k()+1,
          dimnames = list(paste("Interval",
                                1:nrow(piecewise_exponential_dropout())),
                          c("Starting time", treatment_description()))),
        dropout_model = input$dropout_model,
        piecewiseDropoutTime = matrix(
          piecewiseDropoutTime(), ncol = 1,
          dimnames = list(paste("Interval",
                                1:length(piecewiseDropoutTime())),
                          "Starting time")),
        spline_k_dropout = spline_k_dropout(),
        spline_scale_dropout = input$spline_scale_dropout,
        m_dropout = m_dropout()
      )

      save(x, file = file)
    }
  )


  # load inputs
  observeEvent(input$loadInputs, {
    file <- input$loadInputs
    ext <- tools::file_ext(file$datapath)

    req(file)

    valid <- (ext == "RData")
    if (!valid) showNotification("Please upload an RData file")
    req(valid)

    load(file=file$datapath)

    updateRadioButtons(session, "stage", selected=x$stage)

    if (x$stage == "Design stage" ||
        x$stage == "Real-time before enrollment completion") {
      updateRadioButtons(session, "to_predict", selected=x$to_predict)
      updateNumericInput(session, "target_n", value=x$target_n)
    } else {
      updateRadioButtons(session, "to_predict2", selected=x$to_predict2)
    }

    if (x$to_predict == "Enrollment and event" ||
        x$stage == "Real-time after enrollment completion") {
      updateNumericInput(session, "target_d", value=x$target_d)
      updateCheckboxGroupInput(session, "to_show", selected=x$to_show)
    }

    updateNumericInput(session, "pilevel", value=x$pilevel)
    updateNumericInput(session, "nyears", value=x$nyears)

    if (x$to_predict == "Enrollment and event" ||
        x$stage == "Real-time after enrollment completion") {
      updateCheckboxInput(session, "pred_at_t", value=x$pred_at_t)
      if (x$pred_at_t) {
        updateNumericInput(session, "target_t", value=x$target_t)
      }
    }

    updateCheckboxInput(session, "by_treatment", value=x$by_treatment)

    if (x$stage == "Design stage" || x$by_treatment) {
      updateSelectInput(session, "k", selected=x$k)
    }

    if ((x$stage == "Design stage" ||
         (x$by_treatment &&
          x$stage != "Real-time after enrollment completion")) && x$k > 1) {
      updateMatrixInput(
        session, paste0("treatment_allocation_", x$k),
        value=x$treatment_allocation)
    }

    updateNumericInput(session, "fix_parameter", value=x$fix_parameter)
    updateNumericInput(session, "nreps", value=x$nreps)
    updateNumericInput(session, "seed", value=x$seed)


    if (x$stage == "Design stage") {
      updateRadioButtons(session, "enroll_prior", selected=x$enroll_prior)

      if (x$enroll_prior == "Poisson") {
        updateNumericInput(session, "poisson_rate", value=x$poisson_rate)
      } else if (x$enroll_prior == "Time-decay") {
        updateNumericInput(session, "mu", value=x$mu)
        updateNumericInput(session, "delta", value=x$delta)
      } else if (x$enroll_prior == "Piecewise Poisson") {
        updateMatrixInput(
          session, "piecewise_poisson_rate", value=x$piecewise_poisson_rate)
      }
    } else {
      if (x$stage == "Real-time before enrollment completion") {
        updateRadioButtons(session, "enroll_model", selected=x$enroll_model)

        if (x$enroll_model == "B-spline") {
          updateNumericInput(session, "nknots", value=x$nknots)
          updateNumericInput(session, "lags", value=x$lags)
        } else if (x$enroll_model == "Piecewise Poisson") {
          updateMatrixInput(
            session, "accrualTime", value=x$accrualTime)
        }

        # restore enrollment rate method settings
        if (!is.null(x$enroll_rate_method)) {
          updateRadioButtons(session, "enroll_rate_method",
                            selected = x$enroll_rate_method)
        }
        if (!is.null(x$user_rate_type)) {
          updateRadioButtons(session, "user_rate_type",
                            selected = x$user_rate_type)
        }
        if (!is.null(x$user_constant_rate)) {
          updateNumericInput(session, "user_constant_rate",
                            value = x$user_constant_rate)
        }
        if (!is.null(x$user_piecewise_rate)) {
          updateMatrixInput(session, "user_piecewise_rate",
                           value = x$user_piecewise_rate)
        }
      }
    }


    if (x$stage == "Design stage") {
      if (x$to_predict == "Enrollment and event") {
        updateRadioButtons(session, "event_prior", selected=x$event_prior)
      }

      if (x$event_prior == "Exponential") {
        updateMatrixInput(
          session, paste0("exponential_survival_", x$k),
          value=x$exponential_survival)
      }

      if (x$event_prior == "Weibull") {
        updateMatrixInput(
          session, paste0("weibull_survival_", x$k),
          value=x$weibull_survival)
      }

      if (x$event_prior == "Log-logistic") {
        updateMatrixInput(
          session, paste0("llogis_survival_", x$k),
          value=x$llogis_survival)
      }

      if (x$event_prior == "Log-normal") {
        updateMatrixInput(
          session, paste0("lnorm_survival_", x$k),
          value=x$lnorm_survival)
      }

      if (x$event_prior == "Piecewise exponential") {
        updateMatrixInput(
          session, paste0("piecewise_exponential_survival_", x$k),
          value=x$piecewise_exponential_survival)
      }
    } else {
      if ((x$stage == "Real-time before enrollment completion" &&
           x$to_predict == "Enrollment and event") ||
          x$stage == "Real-time after enrollment completion") {

        updateRadioButtons(session, "event_model", selected=x$event_model)

        if (x$event_model == "Piecewise exponential") {
          updateMatrixInput(
            session, "piecewiseSurvivalTime", value=x$piecewiseSurvivalTime)
        } else if (x$event_model == "Spline") {
          updateNumericInput(session, "spline_k", value=x$spline_k)
          updateRadioButtons(session, "spline_scale",
                             selected=x$spline_scale)
        } else if (x$event_model == "Cox") {
          updateNumericInput(session, "m_event",
                             value=x$m_event)
        }
      }
    }


    if (x$stage == "Design stage") {
      if (x$to_predict == "Enrollment and event") {
        updateRadioButtons(session, "dropout_prior",
                           selected=x$dropout_prior)
      }

      if (x$dropout_prior == "Exponential") {
        updateMatrixInput(
          session, paste0("exponential_dropout_", x$k),
          value=x$exponential_dropout)
      }

      if (x$dropout_prior == "Weibull") {
        updateMatrixInput(
          session, paste0("weibull_dropout_", x$k),
          value=x$weibull_dropout)
      }

      if (x$dropout_prior == "Log-logistic") {
        updateMatrixInput(
          session, paste0("llogis_dropout_", x$k),
          value=x$llogis_dropout)
      }

      if (x$dropout_prior == "Log-normal") {
        updateMatrixInput(
          session, paste0("lnorm_dropout_", x$k),
          value=x$lnorm_dropout)
      }

      if (x$dropout_prior == "Piecewise exponential") {
        updateMatrixInput(
          session, paste0("piecewise_exponential_dropout_", x$k),
          value=x$piecewise_exponential_dropout)
      }
    } else {
      if ((x$stage == "Real-time before enrollment completion" &&
           x$to_predict == "Enrollment and event") ||
          x$stage == "Real-time after enrollment completion") {

        updateRadioButtons(session, "dropout_model",
                           selected=x$dropout_model)

        if (x$dropout_model == "Piecewise exponential") {
          updateMatrixInput(
            session, "piecewiseDropoutTime", value=x$piecewiseDropoutTime)
        } else if (x$dropout_model == "Spline") {
          updateNumericInput(session, "spline_k_dropout",
                             value=x$spline_k_dropout)
          updateRadioButtons(session, "spline_scale_dropout",
                             selected=x$spline_scale_dropout)
        } else if (x$dropout_model == "Cox") {
          updateNumericInput(session, "m_dropout",
                             value=x$m_dropout)
        }
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
