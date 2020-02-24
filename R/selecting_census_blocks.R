################################################
#
# Selecting census blocks to sample within wards
#
#  Code written by M. Fidino 2/10/2020 mdy
#
################################################

# load packages
library(BalancedSampling)
library(ks)

#' sample locations within a ward
#'
#' \code{sample_ward} generates a spatially balanced sample.
#'
#' @param x A data.frame of the population you wish to sample from, where each
#'            row is a possible sampling location.
#' @param n The number of points to sample
#' @param coord_names Either a numeric vector that describes the columns of the
#'                      coordinates you wish to include as auxilary variables
#'                      from the data.frame x or a character vector of the
#'                      column names.
#' @param covar_names Either a numeric vector that describes the columns of the
#'                      covariates you wish to include as auxilary variables
#'                      from the data.frame x or a character vector of the
#'                      column names.
#' @param seed  An optional value to set the seed for sampling. If NULL, this
#'                will be generated for you.

#'
#' @return  A named list. The first object 'row_indices' indicates the rows
#'            from data.frame x that represent a balanced sample. The second
#'            object 'seed' is the seed used to generate the balanced sample.
#'
#' @importFrom BalancedSampling cube
#' @importFrom ks kde
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   data <- read.csv(
#'     "./data/my_ward_data.csv",
#'     stringsAsFactors = FALSE
#'   )
#'
#'   my_samples <- sample_ward(
#'     x = data,
#'     n = 12,
#'     coord_df = data[,c("lat", "lon")],
#'     covar_names = c("medincome", "ratcomplaints", "popdens")
#'     )
#' }
#'
sample_ward <- function(
  x,
  n,
  coord_names,
  covar_names,
  seed = NULL
){
  x <- x[
    complete.cases(x),
  ]


  # If it's numeric just do the subset
  if(is.numeric(coord_names)){
    x_coord <- as.matrix(x[,coord_names])
  }
  # If it's characters
  if(is.character(coord_names)){
    if(!all(coord_names %in% colnames(x))){
      err <- covar_names[which(!coord_names %in% colnames(x))]
      warning(paste0("Column name: '", err, "' not in x."))
    }
    x_coord <- as.matrix(x[,which(colnames(x) %in% coord_names)])
  }

    # If it's numeric just do the subset
  if(is.numeric(covar_names)){
    x_cov <- as.matrix(x[,covar_names])
  }
   # If it's characters
  if(is.character(covar_names)){
    if(!all(covar_names %in% colnames(x))){
      err <- covar_names[which(!covar_names %in% colnames(x))]
      warning(paste0("Column name: '", err, "' not in x."))
    }
    x_cov <- as.matrix(x[,which(colnames(x) %in% covar_names)])
  }




  if(ncol(x_cov) > 3){
    stop("x can only have a maximum of 3 columns.
         Either specify which columns you want to use
         with 'covar_names' or subset the input to
         x before.")
  }

  # calculate the density within this multivariate space
  mv_density <- ks::kde(
    x = x_cov
  )

  # calculate the density at the specific points we have
  data_density <- ks::kde(
    x=x_cov,
    eval.points = x_cov
  )

  # calculate the inverse of this density to increase rare points
  inverse_density <- 1 / data_density$estimate

  # convert to inclusion probability. Divide by sum and then multiply by
  #  number of samples we want.

  inc_prob <- n * (inverse_density / sum(inverse_density))

  # take sample, starting with all NA
  our_sample <- rep(
    NA,
    n
  )

  # Doing this in iterations to make certain
  #  we have an appropriate number of samples.
  iter <- 1
  while(
    any(
      is.na(
        our_sample
      )
    )
  ){
    # set seed for reproducibility
    my_seed <- as.numeric(
      Sys.time()
    )
    if(!is.null(seed) & iter == 1) {
      my_seed <- seed
    }
    set.seed(
      my_seed
    )

    # collect sample based on multivariate
    #  inclusion probabilities
    our_sample <- BalancedSampling::cube(
      inc_prob,
      cbind(x_coord, x_cov)
    )[1:n]

    iter <- iter+1
    if(
      iter > 300
    ){
      stop("Cannot find balanced sample.")
    }
  }

 to_return <- list(row_indices = our_sample,
                   seed = my_seed)

 return(to_return)

}


