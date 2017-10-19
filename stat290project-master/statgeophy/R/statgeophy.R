#' Geophysical modeling and statistical fitting
#' 
#' It has 2 functions, shearRange and compositeShear. 
#' Each function uses a different
#' algorithm to produce a dataframe with calculated output
#' as the first column and the all the inputs as the other 
#' columns.
#' statgeophy::shinyShearRange() to see the shearRange
#' results.
#' statgeophy::shinyCompositeShear() shows the compositeShear
#' results. This one takes a little time to do the calculations.
#' 
#' The \code{generateData} is provided to generate rnorm
#' data and feed it to the above modelling functions to get the output.
#' 
#' The \code{modelFitting} uses the output from \code{generateData}
#' to use linear regression, random forest and or boosting to fit a model
#' and optionally plot the predictor importance.
#' 
#' Two shiny apps \code{shinyShearRange} and \code{shinyCompositeShear}
#' are provided to show the results of model fitting on a webpage.
#' 
#' @seealso \code{\link{generateData}}
#' @seealso \code{\link{modelFitting}}
#' @seealso \code{\link{shearRange}}
#' @seealso \code{\link{compositeShear}}
#' 
#' @importFrom utils globalVariables
#' @docType package
#' @references Rock Physics Handbook
#' @name statgeophy
NULL

utils::globalVariables(names = c("predictors",
                              "absoluteTStat",
                              "incMSE",
                              "relInf"))