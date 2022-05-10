#' Generate data set for estimated outcome variable values given the influencing variable, 
#' based on a slice of 'z' from the kernel density plot of the variable and out_var data.
#'
#' Plot representing probabilities (shown along the y-axis) for the expected value of the outcome variable (shown along the x-axis). 
#' This is a cut through the density kernel from uncertainty::varkernel() function, which integrates to 1, the probability values are relative, not absolute measures.
#' 
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param expectedin_var is the expected value of the input variable for which the outcome variable {out_var} should be estimated. 
#' @param n_runs is the number of runs for the resampling from the density surface, default is 100
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
#' varslice_resample(in_var, out_var, expectedin_var = 40)
#' 
#' @export varslice_resample
varslice_resample <- function(in_var, out_var, 
                           expectedin_var,  
                           n_runs = 100) {
  
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
  
  ## create a density surface with kde2d with 100 grid points
  in_outkernel <- MASS::kde2d(x = in_outdata$in_var, 
                              y = in_outdata$out_var, 
                              n = 100)
  
  # A list of x and y coordinates of the grid points of length n_runs 
  # z is an n[1] by n[2] matrix of the estimated density: 
  # rows correspond to the value of x = in_outdata$in_var
  # columns correspond to the value of y = in_outdata$out_var
  # generate x and y for analysis

  Relative_probability <- in_outkernel$z[, expectedin_var]
  Output_values <- in_outkernel$y
 
  # create a data set with the selected data from the kernel density
  data <- data.frame(Relative_probability, Output_values)
  
  # resample the data to create a random sample
  # assign the probability of the values based on the Relative_probability
  data <- data[sample(seq_len(nrow(data)),
              size = n_runs,
              prob = data$Relative_probability, 
              replace = TRUE),]
  
  # return the resulting vector of the sampled 'Output_values'
  
  Output_values <- data$Output_values
  
  return(Output_values)
}
