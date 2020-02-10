library(BalancedSampling)
library(ks)

# Read in the data
dat <- read.csv(
  "./data/40th_ward.csv",
  stringsAsFactors = FALSE
)

# reduce down to the columns we need
dat <- dat[,c(
  "INTPTLAT",
  "INTPTLON",
  "medincome",
  "ratcomplaints",
  "popdens"
  )
]

# get complete cases
dat <- dat[
  complete.cases(dat),
]

# get the covariates
x <- as.matrix(
  dat[,c(
    "medincome",
    "ratcomplaints",
    "popdens")
  ]
)

# calculate the density within this multivariate space
mv_density <- ks::kde(
  x = as.matrix(x)
)

# take a look at this
if(
  do_plot
){
  plot(
    mv_density,
    drawpoints = TRUE
  )
  invisible(
    readline(
      prompt="Press [enter] to continue"
    )
  )
}
  
# calculate the density at the specific points we have
data_density <- ks::kde(
  x=x,
  eval.points = x
)

# calculate the inverse of this density to increase rare points
inverse_density <- 1 / data_density$estimate

# convert to inclusion probability. Divide by sum and then multiply by
#  number of samples we want.

n_samples <- 12

inc_prob <- n_samples * (inverse_density / sum(inverse_density))

# take sample, starting with all NA
our_sample <- rep(
  NA,
  n_samples
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
    as.matrix(dat)
  )[1:n_samples]
   
  iter <- iter+1
  if(
    iter > 100
    ){
    stop("Cannot find balanced sample.")
  }
}

sample_df <- dat[our_sample,]

if(
  do_plot
  ){
  plot(
    dat$INTPTLAT ~ dat$INTPTLON,
    xlab = "Longitude",
    ylab = "Latitude",
    cex = 2,
    las = 1,
    bty = 'l'
  )
  
  points(
    sample_df$INTPTLAT ~ sample_df$INTPTLON,
    cex = 2,
    pch = 21,
    bg = "black"
  )
  
  legend(
    "topleft",
    pch = c(
      1,
      21
    ),
    pt.cex = 2,
    legend = c(
      "Not sampled",
      "Sampled"),
    pt.bg = c(
      "white",
      "black"
      ),
    bty = "n"
  )
  invisible(
    readline(
      prompt="Press [enter] to continue"
    )
  )

  round_choose <- function(x, round_to, dir = "up") {
    if(dir == "up") {  ##ROUND UP
      x + (roundTo - x %% roundTo)
    } else {
      if(dir == "down") {  ##ROUND DOWN
        x - (x %% roundTo)
      }
    }
  }
  windows(height = 15, width = 10, restoreConsole = TRUE)
  par(mfrow = c(3,2))
  hist(
    sample_df$ratcomplaints,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$ratcomplaints
        ),
        100,
        "down"),
      round_choose(
        max(
          dat$ratcomplaints
        ),
        100,
        "up"
      )
    ),
    xlab = "rat complaints",
    main = "sample"
  )
  hist(
    dat$ratcomplaints,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$ratcomplaints
        ),
        100,
        "down"),
      round_choose(
        max(
          dat$ratcomplaints
        ),
        100,
        "up"
      )
    ),
    xlab = "rat complaints",
    main = "data"
  )
  hist(
    sample_df$medincome,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$medincome
        ),
        1000,
        "down"
        ),
      round_choose(
        max(
          dat$medincome
        ),
        1000,
        "up"
      )
    ),
    xlab = "median income",
    main = "sample"
  )
  hist(
    dat$medincome,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$medincome
        ),
        1000,
        "down"
        ),
      round_choose(
        max(
          dat$medincome
        ),
        1000,
        "up"
      )
    ),
    xlab = "median income",
    main = "data"
  )
  hist(
    sample_df$popdens,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$popdens
        ),
        1000,
        "down"
        ),
      round_choose(
        max(
          dat$popdens
        ),
        1000,
        "up"
      )
    ),
    xlab = "population density",
    main = "sample"
  )
  hist(
    dat$popdens,
    xlim = c(
      # Minimum
      round_choose(
        min(
          dat$popdens
        ),
        100,
        "down"
        ),
      round_choose(
        max(
          dat$popdens
        ),
        100,
        "up"
      )
    ),
    xlab = "population density",
    main = "data"
  )
}  

cor(hey$popdens, hey$ratcomplaints)
cor(hey$popdens, hey$medincome)
cor(hey$ratcomplaints, hey$medincome)


apply(hey, 2, mean)
apply(dat, 2, mean)

write.csv()
