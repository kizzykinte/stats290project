#' Data for shearRange or compositeShear modelling
#'
#' @description Function to generate data give datalength and modelfn 
#' = \code{shearRange}, \code{compositeShear}. It calculates the
#' output using geophysical modeling and returns a dataframe.
#' 
#' @param datalength The input datalength. Default = 1000
#' @param modelfn The model function we want to use. Default is \code{shearRange}. 
#' Another function in the library is \code{compositeShear}. The number of arguments
#' can be found by doing help(\code{compositeShear}). User defined function can also 
#' be passed. Make sure the to pass the function name without quotes.
#' @param v_mean A list of mean values for the inputs. Default is list of
#' # input means for \code{shearRange}.
#' @param v_sd A list of corresponding sd values for the inputs. Default is a 
#' a list of input sds for \code{shearRange}
#' @param visualize If true will create a scatterplot of the input data after
#' applying all the conditions specified in the geophysical models. Default is false.
#' @param userdata User can pass a dataframe with the input variables to the models.
#' Default is NULL.
#' 
#' @return a dataframe with output results and input 
#' variables. It also uses pair to make a scatterplot of the inputs
#'
#' @importFrom stats rnorm
#' @importFrom graphics pairs
#' 
#' @export
#' 
#' @examples 
#' df <- generateData()
#'
#'
#'
##
## Begin Mamta Sinha code
##
generateData <- function(datalength = 1000, modelfn = shearRange, userdata = NULL,
                         v_mean = list(36, 36, 1, 1, 0.3), 
                         v_sd = list(10, 10, 0.4, 0.4, 0.2), visualize = FALSE ) {
  #Given User input data
  if (!is.null(userdata)) {
    message("Using user specified data\n")
    v_i <- userdata
  }
  # Generate input data
  else { #Input data code begin
    message("Using generated data\n")
    if (length(v_mean) != length(v_sd)) {
      stop("The number of input means have to be equal to the number of sd")
    }
    # Generate rnorm data
    v_i <- mapply(rnorm, datalength, v_mean, v_sd)
    
    # For shearRange functions, v_mean = list(36, 36, 1, 1, 0.3), 
    # v_sd = list(10, 10, 0.4, 0.4, 0.2)
    if (deparse(substitute(modelfn)) == "shearRange") {
      if (length(v_mean) != 5) {
        stop("The function shearRange needs 5 inputs. Please check your inputs")
      }
      else {
        v_i <- subset(v_i, (v_i[,1] > 1) & (v_i[,2] > 1))
      }
    }
    
    # For compositeShear function, v_mean = list(36, 45, 36, 45, 1, 0.4, 0.25), 
    # v_sd = list(10, 10, 10, 10, 0.5, 0.1, 0.1)
    else if (deparse(substitute(modelfn)) == "compositeShear"){
      if (length(v_mean) != 7) {
        stop("The function compositeShear needs 7 inputs. Please check your inputs")
      }
      else {
        v_i <- subset(v_i, (v_i[,1] > 1) & (v_i[,2] > 0) & (v_i[,3] > 1)
                      & (v_i[,4] > 0) & (v_i[,5] > 0.001) & (v_i[,6] > 0)
                      & (v_i[,7] > 0))
      }
    }
  } #Input data code end
  
  if (deparse(substitute(modelfn)) == "shearRange") {
    message("Using shearRange model.\n")
    out_df <- modelfn(v_i[,1], v_i[,2], v_i[,3], v_i[,4], v_i[,5]) 
  }
  else if (deparse(substitute(modelfn)) == "compositeShear"){
    message("Using compositeShear function.\n")
    out_df <- modelfn(v_i[,1], v_i[,2], v_i[,3], v_i[,4], 
                      v_i[,5],v_i[,6], v_i[,7])
  }
  else {
    message("Using user specified function ", deparse(substitute(modelfn)), "\n")
    out_df <- modelfn(v_i)
  }
  
  #If visualize is true
  if (visualize) {
    pairs(out_df[-1])
  }
  return(out_df)
} 

##
## End Mamta Sinha code
##