library(shiny)

##
## Begin Mamta Sinha code
##
ui <- fluidPage(
    h1("Statgeophy Plots using shearRange: lm, random forest and gradient boosting"),
    
    fluidRow(
      column(6,
        wellPanel(fluidRow(
          column(2, numericInput("datalength", "Length of data", 1000),
                 actionButton("goButton", "Go!")),
          column(5,
                 sliderInput("m1", "c33 mean:", value = 36, min = 1, max = 70),
                 sliderInput("m2", "c44 mean:", value = 36, min = 1, max = 70),
                 sliderInput("m3", "epsilon mean:", value = 1, min = 0, max = 2, step = 0.2),
                 sliderInput("m4", "gamma mean:", value = 1, min = 0, max = 2, step = 0.2),
                 sliderInput("m5", "delta mean:", value = 0.3, min = -0.5, max = 1, step = 0.1)
                 ),
          column(5, 
                 sliderInput("s1", "c33 sd:", value = 10, min = 9, max = 11, step = 0.2),
                 sliderInput("s2", "c44 sd:", value = 10, min = 9, max = 11, step = 0.2),
                 sliderInput("s3", "epsilon sd:", value = 0.4, min = 0.3, max = 0.5, step = 0.05),
                 sliderInput("s4", "gamma sd:", value = 0.4, min = 0.3, max = 0.5, step = 0.05),
                 sliderInput("s5", "delta sd:", value = 0.2, min = 0.1, max = 0.3, step = 0.05)
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
