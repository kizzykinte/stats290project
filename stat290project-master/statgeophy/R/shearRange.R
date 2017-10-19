#' Voigt-Reuss range (normailsed) for shear modulus
#'
#' @description Generates macro-scale isotropic Voigt-Reuss range (normailsed) for shear modulus (dfg) of a micro-scale anisotropic phase with VTI 
#' (vertical transverse isotropy)symmetry, Many instances of the micro-scale anisotropic domain oriented without directional preference give rise to 
#' the macro-scale isotropic composite. 
#'
#' @param c33 P-wave modulus along axis of symmetry (VTI domain).
#' @param c44 S-wave modulus along axis of symmetry (VTI domain).
#' @param epsilon Thomsen's anisotropy parameter (VTI domain).
#' @param gamma Thomsen's anisotropy parameter (VTI domain).
#' @param delta Thomsen's anisotropy parameter (VTI domain).
#' 
#' @return a dataframe whose first column is the model output (dfg) and the other columns are the input variables that went into the model.
#' 
#' @export
#' 
#' @examples
#' c33 <- rnorm(1000, 36, 10)
#' c44 <- rnorm(1000, 36, 10)
#' epsilon <- rnorm(1000, 1, 0.4)
#' gamma   <- rnorm(1000, 1, 0.4)
#' delta <- rnorm(1000, 0.3, 0.2)
#' df <- shearRange(c33, c44, epsilon, gamma, delta)
#' 

##
## Begin Priyanka Dutta code
##

shearRange <- function(c33, c44, epsilon, gamma, delta){
  
  # check if all input vectors are of equal length
  if(!all(sapply(list(c33, c44, epsilon, gamma, delta), function(x) length(x)==length(c33)))){
    stop("Input vectors need to be of equal length. Please check inputs!")
  }
  
  # function to check for valid inputs for a stiffness tensor
  check <- function(c33, c44, c11, c12, c13){
    check1 <- c33>c44
    check2 <- c11>abs(c12)
    check3 <- if (!is.na(c13)) {(c11+c12)*c33>2*c13^2} else{FALSE}
    check4 <- c44>0
    checksum <- sum(check1+check2+check3+check4)
    if (checksum == 4) {return(TRUE)}
    else {return(FALSE)}
  }
  
  # funtion to compute Voigt-Reuss bounds for shear modulus
  deltaShear <- function(c33, c44, c11, c12, c13, c66) {
    # 6X6 elastic stiffness tensor
    c1 <- matrix(c(c11, c12, c13, 0, 0, 0, 
                  c12, c11, c13, 0, 0, 0, 
                  c13, c13, c33, 0, 0, 0, 
                  0, 0, 0, c44, 0, 0, 
                  0, 0, 0, 0, c44, 0, 
                  0, 0, 0, 0, 0, c66),nrow = 6,ncol = 6, byrow = TRUE); 
    # 6X6 elastic compliance tensor
    s1 <- solve(c1);
    gv <- (1/15)*((c11+c11+c33)-(c12+c13+c13)+3*(c44+c44+c66));
    gr <- 15/(4*(s1[1,1]+s1[2,2]+s1[3,3])-4*(s1[1,2]+s1[2,3]+s1[3,1])+3*(s1[4,4]+s1[5,5]+s1[6,6]));
    dfg <- (gv - gr)/gv;
    return(dfg)
  }
  
  # computing other components of stiffness tensor from inputs
  c11 <- (2*epsilon + 1)*c33
  c66 <- (2*gamma + 1)*c44
  c13 <- suppressWarnings(sqrt(2*delta*c33*(c33-c44)+(c33-c44)^2) -c44)
  c12 <- c11 - 2*c66
  
  # check for valid physical inputs. Discard ones that are not.
  cat("Checking inputs... \n" )
  idx <- sapply(1:length(c33), function(i) check(c33[i], c44[i], c11[i], c12[i], c13[i]))
  if (sum(idx)<length(c33)) {cat("No. of input sets that did not pass check = ",length(c33)-sum(idx),"\n" )}
  
  # filtering out valid inputs (features)
  ivars <- data.frame(c33 = c33[idx],c44 = c44[idx], c11=c11[idx], c12=c12[idx], c13=c13[idx], c66=c66[idx])
  
  # computing normalized range for Voigt-Reuss bounds for shear modulus (response)
  cat("Computing Voigt-Reuss shear range with remaining",sum(idx),"inputs... \n" )
  fg <- unlist(lapply(1:sum(idx), function(i) deltaShear(ivars[i,1],ivars[i,2],ivars[i,3],ivars[i,4],ivars[i,5],ivars[i,6])))
  
  # output dataframe with model response as 1st column variable and features as the rest of the columns
  return(data.frame(fg = fg, c33 = c33[idx],c44 = c44[idx], epsilon = epsilon[idx], gamma = gamma[idx], delta = delta[idx]))
  
  ##
  ## End Priyanka Dutta code
  ## 
}
