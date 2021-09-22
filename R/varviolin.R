#' Create a scatter plot of an influencing variable and an outcome variable
#'
#' Violin plots of values of an influencing variable (x) and an outcome variable (y) with different intervals of the influencing variable.
#' Determine different possible variable intervals by calculating the optimal interval width for the given variable using the IQR() function in the stats() package of R, after the Freedman-Diaconis rule (IQR = interquartile range).
#'
#' @param in_var is a vector of observations of a given influencing variable corresponding to another list with observed values of an outcome variable {out_var}. 
#' @param out_var is a vector of observed values of an outcome variable corresponding to another list with observations of a given influencing variable {in_var}.
#' @param xlab is a label for the influencing variable {in_var} on the x axis, the default label is "Influencing variable".
#' @param ylab is a label for the outcome variable {out_var} on the y axis, the default label is "Outcome variable".
#' @param box_width is a number between 0 and 1 for the desired width of the violins in the plot. The default is 0.1.
#' @param legend_name is a label for the legend, the default label is "Influencing Variable Intervals".
#' @param ... arguments passed to ggplot2::theme
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 cut_width
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom stats complete.cases
#' @importFrom stats IQR
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords variable out_var Freedman-Diaconis IQR violin
#'
#' @examples
#' in_var <- sample(x = 1:50, size = 20, replace = TRUE)
#' out_var <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' varviolin(in_var, out_var)
#' 
#' @export varviolin
varviolin <- function(in_var, out_var, 
                      xlab = "Influencing variable", 
                      ylab = "Outcome variable", 
                      box_width = 0.1,
                      legend_name = "Influencing Variable Intervals", 
                      ...) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  in_outdata <- in_out <- width <- aes <- cut_width <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  ## a method to calculate the optimal bin width for the violin plots 
  ## after the Freedman-Diaconis rule (IQR = interquartile range from stats):
  width <- stats::IQR(in_outdata$in_var)
  
  ## violin plot with IQR cut_width
  in_outviolin <- ggplot2::ggplot(in_outdata, 
                                  ggplot2::aes(x = ggplot2::cut_width(x = in_var, 
                                                                      width = width, 
                                                                      labels = FALSE), 
                                               y = out_var, 
                                               color = ggplot2::cut_width(x = in_var, 
                                                                          width = width))) +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(width = box_width) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +  
    ggplot2::scale_color_discrete(name = legend_name) + 
    ggplot2::theme_classic() +
    ggplot2::theme(...)  
  
  print(in_outviolin)
  
  print("Violin plots of influencing variable (x) and outcome variable (y) with IQR intervals of the influencing variable.")

  }



