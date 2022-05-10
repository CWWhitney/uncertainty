#' Estimated output variable values given the expected values of the influencing variable, 
#' based on a slice of 'z' from the Kernel density plot of the influencing variable and output 
#' variable data.
#'
#' Plot representing probabilities (shown along the y-axis) for the expected outcome variable (shown along the x-axis). 
#' This is a broad slice through the density kernel from uncertainty::varkernel() function, which integrates to 1, the probability values are relative, not absolute measures.
#' 
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param max_in_var is a value of the highest expected amount of a given influencing variable {in_var} for which the outcome variable {out_var} should be estimated (must be > {min_in_var}). 
#' @param min_in_var is a value of the lowest expected amount of {in_var} for which the outcome variable {out_var} should be estimated (must be < {max_in_var}). 
#' @param n_runs is the number of runs for the density surface with {MASS::kde2d}. Default is 100
#' @param xlab_vars is the x axis title that describes the two variables being associated
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
#' variable <- sample(x = 1:50, size = 20, replace = TRUE)
#' outcome <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' varkernelslicerange(variable, outcome, 10, 20, 
#' xlab_vars = "Dist. of outcome given influence variable range")
#' 
#' @export varkernelslicerange
varkernelslicerange <- function(in_var, 
                                out_var, 
                                min_in_var, 
                                max_in_var, 
                                n_runs = 100,
                                xlab_vars = "Outcome variable dist. given influence variable") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Setting the variables to NULL first, appeasing R CMD check
  in_outdata <- in_out <- NULL 
  
  #add error stops with validate_that   
  assertthat::validate_that(length(in_var) == length(out_var), msg = "\"in_var\" and \"out_var\" are not equal lengths.")
  assertthat::validate_that(is.numeric(in_var), msg = "\"in_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(min_in_var), msg = "\"min_in_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(max_in_var), msg = "\"max_in_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(out_var), msg = "\"out_var\" is not numeric.")
  
  #check that the min_in_var argument is consistent with the values in the kernel density
  assertthat::validate_that(min(in_outkernel$x)> min_in_var, msg = "\"min_in_var\" value is too high.")
  
  #create subset-able data
  in_out <- as.data.frame(cbind(in_var, out_var)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  in_outdata <- in_out[stats::complete.cases(in_out), ]
  
  #message about complete cases
  assertthat::see_if(length(in_out) == length(in_outdata), msg = "Rows with NA were removed.")
  
  #compare length of in_out and in_outdata and print 'you lost 'x' cases
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d with n_runs grid points
  in_outkernel <- MASS::kde2d(x = in_outdata$in_var, 
                              y = in_outdata$out_var, 
                              n = n_runs)
  
  ## Cut through density kernel and averaging over a range of x-values (x = variable)
  # sets the boundaries of in_var values over which to average
  lbound <- which(in_outkernel$x == min(in_outkernel$x[which(in_outkernel$x > min_in_var)]))
  rbound <- which(in_outkernel$x == max(in_outkernel$x[which(in_outkernel$x <= max_in_var)]))
  
 graphics::plot(x = in_outkernel$y, 
                y = rowMeans(in_outkernel$z[, lbound : rbound]), 
                type = "l", col = "seagreen", lwd = 2,
       xlab = paste(xlab_vars, as.character(min_in_var), "to", 
                    as.character(max_in_var)), 
       ylab = "Relative probability")
  #for print we need x for max(in_outkernel$z[, lbound : rbound])
 
  print("Relative probability (y) of the outcome variable for the given values of the influencing variable (x).")
}

