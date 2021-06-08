#' Perform a two-dimensional kernel density estimation for an outcome variable and a given influencing variable.
#'  
#' The function produces a matrix of the estimated density (z) of an outcome variable (y) and an observed variable expected to influence the outcome variable (x). 
#' As the density function restricts the shape of the kernel to a bivariate normal kernel, it looks slightly different compared to the scatter plot estimates in the uncertainty::varscatter() function.
#' Density surface plot of an influencing variable (x) and an outcome variable (y). The legend shows the value for the estimated density (z).
#' 
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param xlab is a label for the influencing variable {in_var} on the x axis, the default label is "Influencing variable".
#' @param ylab is a label for the outcome variable {out_var} on the y axis, the default label is "Outcome variable".
#' 
#' @importFrom MASS kde2d
#' @importFrom stats complete.cases
#' @importFrom graphics filled.contour
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords kernel density influence
#'
#' @examples
#' in_var <- sample(x = 1:50, size = 20, replace = TRUE)
#' out_var <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' varkernel(in_var, out_var)
#' 
#' @export varkernel
varkernel <- function(in_var, out_var, 
                      xlab = "Influencing variable", 
                      ylab = "Outcome variable") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  in_outdata <- in_out <- in_outkernel <- ylab <- xlab <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that   
  assertthat::validate_that(length(in_var) == length(out_var), msg = "\"in_var\" and \"out_var\" are not equal lengths.")
  assertthat::validate_that(is.numeric(in_var), msg = "\"in_var\" is not numeric.")

  assertthat::validate_that(is.numeric(out_var), msg = "\"out_var\" is not numeric.")

  #create subset-able data
  in_out <- as.data.frame(cbind(in_var, out_var)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  in_outdata <- in_out[stats::complete.cases(in_out), ]
  
  #message about complete cases
  assertthat::see_if(length(in_out) == length(in_outdata), msg = "Rows with NA were removed.")
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d
  in_outkernel <- MASS::kde2d(in_outdata$in_var, in_outdata$out_var, n = 100)
  
  graphics::filled.contour(in_outkernel, xlab = xlab, ylab = ylab)
  
  print("Density surface plot of an expected influential variable (x) and an outcome variable of interest (y).")
}

