library(BalancedSampling)

# Read in the data
dat <- read.csv(
  "./data/40th_ward.csv",
  stringsAsFactors = FALSE
)

# reduce down to the columns we need
dat <- dat[,c("INTPTLAT", "INTPTLON", "medincome", "ratcomplaints", "popdens")]

# get complete cases
dat <- dat[complete.cases(dat),]

rdat <- dat

calc_rarity2 <- function(x){
  #x <- floor(x)
  dens <- density(x, n=diff(range(x)),
                  from = min(x),
                  to = max(x))
  dens$x <- floor(dens$x)
  x %in% dens$x
  
  yo <- 1/ dens$y[dens$x %in% x]
  
  return(unlist(yo))
  
}



calc_rarity(dat$medincome)
dens <- density(x, n=diff(range(x)),
                from = min(x),
                to = max(x))
dens$x <- floor(dens$x)
x %in% dens$x

yo <- dens$y[dens$x %in% x]

sum(dens$y)*diff(dens$x[1:2])

calc_rarity <- function(x){
  x <- scale(x)
  mu <- mean(x)
  sigma <- sd(x)
  to_return <- dnorm(x, mu, sigma)
  to_return <- 1 / to_return
  return(log(to_return))
}

x <- dat[,3:5]

j3 <- apply(x, 2,calc_rarity2 )


# use principal components analsyis?
test <- prcomp(dat[,3:5], scale. = TRUE)

y <- as.matrix(scale(dat[,3:5]))

x <- test$x[,1:2]
x <- dat[,3:5]
x <- scale(x)
j <- apply(x, 2, function(x) as.numeric(cut(x, 7)))


make_thing <- function(x) {
  tmp <- table(x)
  val <- 1 / (tmp / sum(tmp))
  to_return <- unname(val[x])
}

j3 <- apply(j, 2, make_thing)
#j3 <- apply(j3, 2, function(x) x / sum(x))
j4 <- rowSums(j3)
j4 <- (j4 / sum(j4)) * 13

n <- 12
N <- nrow(dat)

set.seed(150)
longshot <- cube( j4, as.matrix(dat))[1:12]

l2 <- cube(rep(n/N, N), as.matrix(dat))

hey <- dat[longshot,]
hey2 <- dat[l2,]
#apply(hey, 2, mean)
#apply(dat, 2, mean)
#plot(hey$ratcomplaints ~ hey$medincome)
#plot(hey$ratcomplaints ~ hey$popdens)
#plot(hey$medincome ~ hey$popdens)
#plot(dat$ratcomplaints ~ dat$medincome)

windows(height = 5, width = 10)
par(mfrow = c(1,2))
hist(hey$ratcomplaints, xlim = range(dat$ratcomplaints))
hist(dat$ratcomplaints)
hist(hey$medincome, xlim = range(dat$medincome))
hist(dat$medincome)
hist(hey$popdens, xlim = range(dat$popdens))
hist(dat$popdens)


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
