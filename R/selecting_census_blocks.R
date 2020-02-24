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

sample_ward <- function(
  x,
  n,
  covar_names = NULL
){


  if(!is.null(covar_names)){
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
  } else {
    x_cov <- as.matrix(x)
  }
  }

  if(ncol(x_cov) > 3){
    stop("x can only have a maximum of 3 columns.
         Either specify which columns you want to use
         with 'covar_names' or subset the input to
         x before.")
  }
  # get complete cases
  x_cov <- x_cov[
    complete.cases(x_cov),
    ]

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

    set.seed(
      my_seed
    )

    # collect sample based on multivariate
    #  inclusion probabilities
    our_sample <- BalancedSampling::cube(
      inc_prob,
      as.matrix(x_cov)
    )[1:n]

    iter <- iter+1
    if(
      iter > 100
    ){
      stop("Cannot find balanced sample.")
    }
  }

 to_return <- list(row_indices = our_sample,
                   seed = my_seed)

 return(to_return)

}

