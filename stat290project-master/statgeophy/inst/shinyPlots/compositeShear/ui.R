library(shiny)
##
## Begin Mamta Sinha code
##
ui <- fluidPage(
    h1("Statgeophy Plots using composite_shear: lm, random forest and gradient boosting"),
    h4("This model takes some time to calculate. Please be patient."),
    
    fluidRow(
      column(6,
        wellPanel(fluidRow(
          column(2, numericInput("datalength", "Length of data", 1000),
                 actionButton("goButton", "Go!")),
          column(5,
                 sliderInput("m1", "k1 mean:", value = 36, min = 1, max = 70),
                 sliderInput("m2", "mu1 mean:", value = 45, min = 1, max = 70),
                 sliderInput("m3", "k2 mean:", value = 36, min = 1, max = 70),
                 sliderInput("m4", "mu2 mean:", value = 45, min = 1, max = 70),
                 sliderInput("m5", "asp mean:", value = 1.0, min = 0.1, max = 10, step = 0.5),
                 sliderInput("m6", "phic mean:", value = 0.4, min = 0.4, max = 0.6, step = 0.1),
                 sliderInput("m7", "phi mean:", value = 0.25, min = 0, max = 0.5, step = 0.01)
                 ),
          column(5, 
                 sliderInput("s1", "k1 sd:", value = 10, min = 9, max = 11, step = 0.5),
                 sliderInput("s2", "mu1 sd:", value = 10, min = 9, max = 11, step = 0.5),
                 sliderInput("s3", "k2 sd:", value = 10, min = 9, max = 11, step = 0.5),
                 sliderInput("s4", "mu2 sd:", value = 10, min = 9, max = 11, step = 0.5),
                 sliderInput("s5", "asp sd:", value = 0.5, min = 0.4, max = 0.6, step = 0.02),
                 sliderInput("s6", "phic sd:", value = 0.1, min = 0.01, max = 0.2, step = 0.02),
                 sliderInput("s7", "phi sd:", value = 0.1, min = 0.01, max = 0.2, step = 0.02)
                 )
        )
      )
      ),
      column(6,
             tabsetPanel(
               tabPanel("Predictor Importance", plotOutput("predlm"),
                        plotOutput("predrf"),
                        plotOutput("predgb")),
               tabPanel("Test Vs Pred",  plotOutput("plotlm"),
                        plotOutput("plotrf"),
                        plotOutput("plotgb")),
               tabPanel("Summary", verbatimTextOutput("summarylm"),
                       verbatimTextOutput("summaryrf"),
                       verbatimTextOutput("summarygb"))
             )
      )
    )
)

##
## End Mamta Sinha code
##
