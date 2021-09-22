#' Create a scatter plot of a given influencing variable and an outcome variable
#'
#' scatter plot of an influencing variable (x) and an outcome variable (y) with associated and estimated densities (using a loess smooth) given by the red dot (mean) and red ellipses (1 and 2 sigma from mean). The red line going across the plot shows the linear fit. Histograms are shown with smooth lines (loess smooth linear fits) density curves. The numeric value in the upper right gives the Spearman correlation coefficient between the influencing variable and the outcome variable.
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param xlab is a label for the influencing variable {in_var} on the x axis, the default label is "Influencing variable".
#' @param ylab is a label for the outcome variable {out_var} on the y axis, the default label is "Outcome variable".
#' 
#' @importFrom psych scatter.hist
#' @importFrom stats complete.cases
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords scatter histogram
#'
#' @examples
#' in_var <- sample(x = 1:50, size = 20, replace = TRUE)
#' yield <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' varscatter(in_var, yield)
#' 
#' @export varscatter
varscatter <- function(in_var, out_var, 
                       xlab = "Influencing variable", 
                       ylab = "Outcome variable") {
  
    if (!requireNamespace("psych", quietly = TRUE)) {
        stop("Package \"psych\" needed for this function to work. Please install it.",
            call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
 
  #add error stops with validate_that   
  assertthat::validate_that(length(in_var) == length(out_var), msg = "\"in_var\" and \"out_var\" are not equal lengths.")
  assertthat::validate_that(is.numeric(in_var), msg = "\"in_var\" is not numeric.")
  
  assertthat::validate_that(is.numeric(out_var), msg = "\"out_var\" is not numeric.")

  # Setting the variables to NULL first, appeasing R CMD check
  in_outdata <- in_out <- NULL 
  
  in_out <- as.data.frame(cbind(in_var, out_var)) #create subset-able data
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  in_outdata <- in_out[stats::complete.cases(in_out), ]
  
  #message about complete cases
  assertthat::see_if(length(in_out) == length(in_outdata), msg = "Rows with NA were removed.")
  
  ## build a scatter plot with a histogram of x and y with 'psych'
  scatter <- psych::scatter.hist(x = in_outdata$in_var, y = in_outdata$out_var, density = TRUE, 
                                 xlab = xlab, ylab = ylab)
  
    print("Scatter plot of influencing variable (x) and outcome variable (y) with associated and estimated densities.")
}
