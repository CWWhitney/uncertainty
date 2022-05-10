#' Generate data set for estimated outcome variable values given the influencing variable, 
#' based on a slice of 'z' from the kernel density plot of the variable and out_var data.
#'
#' Plot representing probabilities (shown along the y-axis) for the expected value of the outcome variable (shown along the x-axis). 
#' This is a cut through the density kernel from uncertainty::varkernel() function, which integrates to 1, the probability values are relative, not absolute measures.
#' 
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param n_runs is the number of runs for the density surface with {MASS::kde2d}. Default is 100
#' @param expectedin_var is the expected value of the input variable for which the outcome variable {out_var} should be estimated. 
#' @param xlab is a label for the influencing variable {in_var} on the x axis, the default label is "Influencing variable".
#' @param ylab is a label for the relative probability along the cut through the density kernel on the y axis, the default label is "Relative probability".
#' 
#' @importFrom MASS kde2d
#' @importFrom stats complete.cases
#' @importFrom graphics filled.contour
#' @importFrom graphics plot
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords kernel density influence
#'
#' @examples
#' in_var <- sample(x = 1:100, size = 25, replace = TRUE)
#' out_var <- sample(x = 1000:7000, size = 25, replace = TRUE)
#' varkernelslice_data(in_var, out_var, expectedin_var = 40)
#' 
#' @export varkernelslice_data
varkernelslice_data <- function(in_var, out_var, 
                           expectedin_var,  
                           n_runs = 100,
                           ylab = "Relative probability", 
                           xlab = "Output values for the given influence variable values") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Setting the variables to NULL first, appeasing R CMD check
  in_outdata <- in_out <- xvar <- yvar <- NULL 
  
  #add error stops with validate_that   
  assertthat::validate_that(length(in_var) == length(out_var), msg = "\"in_var\" and \"out_var\" are not equal lengths.")
  assertthat::validate_that(is.numeric(in_var), msg = "\"in_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(expectedin_var), msg = "\"expectedin_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(out_var), msg = "\"out_var\" is not numeric.")
  
  #create subset-able data
  in_out <- as.data.frame(cbind(in_var, out_var)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  in_outdata <- in_out[stats::complete.cases(in_out), ]
  
  #message about complete cases
  assertthat::see_if(length(in_out) == length(in_outdata), msg = "Rows with NA were removed.")
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d with n_runs grid points
  in_outkernel <- MASS::kde2d(x = in_outdata$in_var, 
                              y = in_outdata$out_var, 
                              n = n_runs)
  
  # A list of x and y coordinates of the grid points of length n_runs 
  # z is an n[1] by n[2] matrix of the estimated density: 
  # rows correspond to the value of x = in_outdata$in_var
  # columns correspond to the value of y = in_outdata$out_var
  ## generate x and y for analysis
  
  Relative_probability <- in_outkernel$z[, expectedin_var]
  Output_values <- in_outkernel$x
  
  data.frame(Relative_probability, Output_values)
}
