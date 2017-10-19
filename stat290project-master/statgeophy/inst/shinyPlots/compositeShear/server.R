library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(gbm)
library(randomForest)

##
## Begin Mamta Sinha code
##

server <- function(input, output, session) {
      #Data calculation
      lengthinput <- reactive({input$datalength})
      meaninput <- reactive({list(input$m1, input$m2, input$m3, input$m4, input$m5, input$m6, input$m7)})
      sdinput <- reactive({list(input$s1, input$s2, input$s3, input$s4, input$s5, input$s6, input$s7)})
      fit <- eventReactive(input$goButton, 
                               {modelFitting(
                                 generateData(datalength=lengthinput(), modelfn = compositeShear, 
                                               v_mean=meaninput(), v_sd=sdinput()),
                                 modelFit = "all")})
      ##
      ## End Mamta Sinha code
      ##
      ##
      ## Begin Priyanka Dutta code
      ##
      #Predictor importance plots
      output$predlm <- renderPlot({
        s.lm = fit()$fits$lm %>% summary %>% use_series(coefficients) %>% extract(-1,) %>% extract(,3)%>% abs %>% sort(decreasing = TRUE)
        d = data.frame(predictors = names(s.lm),absoluteTStat = s.lm)
        d$predictors = factor(d$predictors, levels = d$predictors[order(d$absoluteTStat)])
        ggplot(data=d, aes(x = predictors, y=absoluteTStat, fill =absoluteTStat )) +
          geom_bar(stat="identity") + 
          coord_flip() +
          scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
          ggtitle("Variables importance from Linear Regression")
      })
      
      output$predrf <- renderPlot({
        s.rf = fit()$fits$rf$importance %>% extract(,1) %>% sort(decreasing = TRUE)
        d2 = data.frame(predictors = names(s.rf),incMSE = s.rf)
        d2$predictors = factor(d2$predictors, levels = d2$predictors[order(d2$incMSE)])
        ggplot(data=d2, aes(x = predictors, y=incMSE, fill =incMSE )) +
          geom_bar(stat="identity") + 
          coord_flip() +
          scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
          ggtitle("Variables importance from Random Forest")
      })
      
      output$predgb <- renderPlot({
        s.gb = fit()$fits$gb %>% summary.gbm(plotit = FALSE)
        d3 = data.frame(predictors = s.gb$var,relInf = s.gb$rel.inf)
        d3$predictors = factor(d3$predictors, levels = d3$predictors[order(d3$relInf)])
        ggplot(data=d3, aes(x = predictors, y=relInf, fill =relInf )) +
          geom_bar(stat="identity") + 
          coord_flip() +
          scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
          ggtitle("Variables importance from Boosted Trees")
      })
      ##
      ## End Priyanka Dutta code
      ##
      
      ##
      ## Begin Mamta Sinha code
      ##
      
      #Test data vs Predicted values and RMSE
      output$plotlm <- renderPlot({
        plot(fit()$test, fit()$pred$lm,
           main=paste("Linear Fit, RMSE=", format(round(fit()$testrmse$lm, 4), nsmall = 4)), 
           xlab="Test data", ylab="Predicted Values")
        abline(0,1)
      })
      output$plotrf <- renderPlot({
        plot(fit()$test, fit()$pred$rf,
           main=paste("Random Forest, RMSE =", format(round(fit()$testrmse$rf, 4), nsmall = 4)),
           xlab="Test data", ylab="Predicted Values")
        abline(0,1)
      })
      output$plotgb <- renderPlot({
        plot(fit()$test, fit()$pred$gb,
           main=paste("Gradient Boosting, RMSE =", format(round(fit()$testrmse$gb, 4), nsmall = 4)),
           xlab="Test data", ylab="Predicted Values")
        abline(0,1)
      })
    
      #Summary of all the fits
      output$summarylm <- renderPrint({
        print("Summary of lm")
        summary(fit()$fits$lm)
      })
      output$summaryrf <- renderPrint({
        print("Importance of random forest")
        importance(fit()$fits$rf)
      })
      output$summarygb <- renderPrint({
        print("Summary of boosted trees")
        summary(fit()$fits$gb, plotit = FALSE)
      })
}
##
## End Mamta Sinha code
##
