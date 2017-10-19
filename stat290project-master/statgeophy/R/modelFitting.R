#' Variable importance using statistical learning
#'
#' @description Given a dataframe produced of response and predictors, this function first 
#' divides the data into training and test sets. It does model fitting and returns the key results, and optionally, plots,
#' for further analysis by the user. The statistical methods used are linear regression (lm), random forest (rf) and
#' gradient boosting (gb). A fourth argument "all" can be passed to return all 3 results.
#'
#' @param df dataframe whose 1st column is the response to be analysed followed by all predictors to be tested
#' @param modelFit the statistical model used for fitting.
#' Values can be "lm", "gb", "rf" and "all". If any other value is given
#' it will give an error.
#' Default is lm.
#' @param plotPredImp if TRUE it will create a predictor importance plot for the input variables
#' Default is FALSE.
#' 
#' @return a list of test data, fit, predicted values and 
#' test RMSE of the modeling function.
#' 
#' @importFrom stats lm as.formula predict coefficients
#' @importFrom randomForest randomForest
#' @importFrom gbm gbm summary.gbm
#' @importFrom magrittr %>% use_series extract
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip scale_fill_gradient2 ggtitle
#' @importFrom grid grid.draw
#' @importFrom gridExtra grid.arrange
#' 
#' @export
#' 
#' @examples
#' c33 <- rnorm(3000, 36, 10)
#' c44 <- rnorm(3000, 36, 10)
#' epsilon <- rnorm(3000, 1, 0.4)
#' gamma   <- rnorm(3000, 1, 0.4)
#' delta <- rnorm(3000, 0.3, 0.2)
#' df <- shearRange(c33, c44, epsilon, gamma, delta)
#' lst <- modelFitting(df, modelFit = "all")
#' 
#'

##
## Begin Mamta Sinha code
##
modelFitting <- function(df, modelFit = "lm", plotPredImp = FALSE) {
  
  if (!(modelFit %in% c("lm", "rf", "gb", "all"))) {
    stop("Only one of lm, rf, gb or all are allowed")
  }
  ##
  ## End Mamta Sinha code
  ##
  
  ##
  ## Begin Priyanka Dutta code
  ##
  # splitting data into training and test sets
  idx = sample(1:dim(df)[1], round(2*dim(df)[1]/3))
  train <- df[idx,]
  test <- df[-idx,]
  
  out <- list("test" = test[,1])
  # formula for fitting
  f <- as.formula(paste('train[,1] ~', paste(colnames(train)[2:dim(df)[2]], collapse='+')))
  
  if (modelFit == "lm" | modelFit == "all") {
    # fit using linear regression
    #linear fit using training set
    fit.lm <- lm(f,train) 
    #test prediction using fit to training set
    pred.lm <- predict(fit.lm,test)
    #test RMSE (root mean square error)
    testrmse.lm <- sqrt(mean((test[,1] - pred.lm)^2))
    
    out$fits$lm <- fit.lm
    out$pred$lm <- pred.lm
    out$testrmse$lm <- testrmse.lm
    
    s.lm = out$fits$lm %>% summary %>% use_series(coefficients) %>% extract(-1,) %>% extract(,3)%>% abs %>% sort(decreasing = TRUE)
    d = data.frame(predictors = names(s.lm), absoluteTStat = s.lm)
    d$predictors = factor(d$predictors, levels = d$predictors[order(d$absoluteTStat)])
    plm <- ggplot(data=d, aes(x = predictors, y=absoluteTStat, fill =absoluteTStat )) +
      geom_bar(stat="identity") + 
      coord_flip() +
      scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
      ggtitle("Variables importance from Linear Regression")
  }
  
  if (modelFit == "rf" | modelFit == "all") {
    # fit using random forests
    #random forest fit using training set
    fit.rf <- randomForest(f,train, m.try = round(sqrt(dim(train)[1])),importance = TRUE) 
    #test prediction using fit to training set
    pred.rf <- predict(fit.rf,test) 
    # test RMSE
    testrmse.rf <- sqrt(mean((test[,1] - pred.rf)^2)) 
    
    out$fits$rf <- fit.rf
    out$pred$rf <- pred.rf
    out$testrmse$rf <- testrmse.rf
    
    s.rf = out$fits$rf$importance %>% extract(,1) %>% sort(decreasing = TRUE)
    d2 = data.frame(predictors = names(s.rf), incMSE = s.rf)
    d2$predictors = factor(d2$predictors, levels = d2$predictors[order(d2$incMSE)])
    prf <- ggplot(data=d2, aes(x = predictors, y=incMSE, fill =incMSE )) +
      geom_bar(stat="identity") + 
      coord_flip() +
      scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
      ggtitle("Variables importance from Random Forest")
  }
  
  if (modelFit == "gb" | modelFit == "all") {
    # fit using gradient boosting
    #gradient boost fit using training set
    fit.gb <- gbm(f,data = train, n.trees = 5000, interaction.depth = 1, distribution = "gaussian") 
    #test prediction using fit to training set
    pred.gb <- predict(fit.gb,newdata = test,n.trees = 5000) 
    # test RMSE
    testrmse.gb <- sqrt(mean((test[,1] - pred.gb)^2)) 
    
    out$fits$gb <- fit.gb
    out$pred$gb <- pred.gb
    out$testrmse$gb <- testrmse.gb
    
    s.gb = out$fits$gb %>% summary.gbm(plotit = FALSE)
    d3 = data.frame(predictors = s.gb$var, relInf = s.gb$rel.inf)
    d3$predictors = factor(d3$predictors, levels = d3$predictors[order(d3$relInf)])
    pgb <- ggplot(data=d3, aes(x = predictors, y=relInf, fill =relInf )) +
      geom_bar(stat="identity") + 
      coord_flip() +
      scale_fill_gradient2(mid='dodgerblue', high='darkblue', space='Lab')+
      ggtitle("Variables importance from Boosted Trees")
  }
  
  ##
  ## End Priyanka Dutta code
  ##
  
  ##
  ## Begin Mamta Sinha code
  ##
  
  if (plotPredImp) {
    if (modelFit == "lm") grid.draw(plm)
    if (modelFit == "rf") grid.draw(prf)
    if (modelFit == "gb") grid.draw(pgb)
    if (modelFit == "all") grid.draw(grid.arrange(plm, prf, pgb, nrow=3))
  }
  
  ##
  ## End Mamta Sinha code
  ##
  
  return(out)
}