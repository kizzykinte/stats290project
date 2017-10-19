#' Run the shiny app for shearRange modelling
#' @description Runs a shiny app shearRange modelling with
#' 3 statistical methods - lm, random forest and gradient boosting.
#' @export
#' @importFrom shiny runApp
#' 
##
## Begin Mamta Sinha code
##
shinyShearRange <- function() {
  app_dir <- system.file("shinyPlots", "shearRange", package = 'statgeophy');
  if (app_dir == "") {
    stop("Could not find shinyPlots directory. Try re-installing `statgeophy`.", call. = FALSE)
  }
  
  runApp(app_dir, display.mode = "normal")
}
##
## End Mamta Sinha code
##
