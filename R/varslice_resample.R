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
#' @param out_var_sampling sampling scheme for extracting values from the probability surface. This is
#' used to create a vector of out_var values, for which the probabilities are extracted. NOTE that only these values can later be returned in the resampling process.
#' This can either be a single number, which is then used to create evenly spaces points separated by intervals of the specified value
#' (defaults to 1000th of the out_var range). It is also possible to provide a numeric vector of values within the out_var range, in which case only probabilities
#' for the specified numbers are extracted (and only these values can be returned by the resampling).
#' 
#' @importFrom MASS kde2d
#' @importFrom stats complete.cases
#' @importFrom graphics filled.contour
#' @importFrom graphics plot
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' @importFrom raster extract
#' 
#' @keywords kernel density influence
#'
#' @examples
#' in_var <- sample(x = 1:200, size = 25, replace = TRUE)
#' out_var <- sample(x = 1000:7000, size = 25, replace = TRUE)
#' resampled<-varslice_resample(in_var, out_var, expectedin_var = 150)
#' plot(resampled$slice$Output_values,resampled$slice$Relative_probability)
#' hist(resampled$resampled)
#' 
#' # with a coarser resolution (100 out_var units between points)
#' resampled_coarse<-varslice_resample(in_var, out_var, expectedin_var = 40,out_var_sampling=100)
#' plot(resampled_coarse$slice$Output_values,resampled_coarse$slice$Relative_probability)
#' hist(resampled_coarse$resampled) 
#' 
#' # for isolated values only
#' resampled_iso<-varslice_resample(in_var, out_var, expectedin_var = 40,out_var_sampling=c(2000,3000,4000,5000))
#' plot(resampled_iso$slice$Output_values,resampled_iso$slice$Relative_probability)
#' hist(resampled_iso$resampled)  
#' 
#' 
#' @export varslice_resample
varslice_resample <- function(in_var, out_var, 
                           expectedin_var,  
                           n_runs = 1000,
                           out_var_sampling=(max(out_var)-min(out_var))/1000) {

  # some asserts on the out_var_sampling?
  if(is.numeric(out_var_sampling) & length(out_var_sampling)==1)
    sampling_scheme<-seq(min(out_var),max(out_var),out_var_sampling)
  if(is.numeric(out_var_sampling) & length(out_var_sampling)>1)
    sampling_scheme<-out_var_sampling
  # maybe add a warning/error, if the sampling scheme is very short or too long
  # maybe also if the sampling scheme includes values outside the out_var range
  
    
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

  slice<-data.frame(Output_values=sampling_scheme,
                    Relative_probability=raster::extract(raster(in_outkernel),cbind(expectedin_var,sampling_scheme)))
  
  data<-slice[sample(seq_len(nrow(slice)), size = n_runs, prob = slice$Relative_probability, 
                     replace = TRUE), ]
  Output_values <- data$Output_values
  return(list(slice=slice,
              resampled=Output_values))
  
}
