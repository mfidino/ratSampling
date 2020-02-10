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
  x = x
)

# take a look at this
plot(
  mv_density
)

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

# Doing this in iterations
iter <- 1
while(
  any(
    is.na(
      our_sample
    )
  )
){
  my_seed <- as.numeric(
    Sys.time()
  )
  
  set.seed(
    my_seed
  )
  
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

windows(height = 5, width = 10)
par(mfrow = c(1,2))
hist(hey$ratcomplaints, xlim = range(dat$ratcomplaints))
hist(dat$ratcomplaints)
hist(hey$medincome, xlim = range(dat$medincome))
hist(dat$medincome)
hist(hey$popdens, xlim = range(dat$popdens))
hist(dat$popdens)

cor(hey$popdens, hey$ratcomplaints)
cor(hey$popdens, hey$medincome)
cor(hey$ratcomplaints, hey$medincome)
plot(hey$ratcomplaints ~ hey$medincome)

apply(hey2, 2, mean)
apply(dat, 2, mean)
plot(hey2$ratcomplaints ~ hey2$medincome)
plot(hey2$ratcomplaints ~ hey2$popdens)
plot(hey2$medincome ~ hey2$popdens)

hist(dat$ratcomplaints)
hist(hey$ratcomplaints, main = "rat complaints, rare upweight")
hist(hey2$ratcomplaints, main = "rat complaints, no upweight", xlim = c(0,600))
hist(hey2$ratcomplaints)
hist(hey2$medincome)
hist(hey2$popdens)
hist(dat$popdens)

plot(dat$INTPTLON ~ dat$INTPTLAT)
points(hey$INTPTLON ~ hey$INTPTLAT, pch = 21, bg = "red")


# get covars
x <- test$x[,1:2]

yo <- cbind(dat[,1:2], x)

plot(dat$medincome ~ dat$popdens)
