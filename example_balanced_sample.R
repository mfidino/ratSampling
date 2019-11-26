# generate a raster of spatially correlated values from which we 
#  want to collect a spatially balanced sample.

# I've already got a lot of code to make a raster from which to sample from.
source("./Documents/GitHub/conflict/sim_utility.R")
source("./Documents/GitHub/conflict/plot_utility.R")

packs <- c("raster", "mvtnorm", "scales",
           "coda", "rgeos", "sp", "BalancedSampling")

# this will load these packages (and download if necessary.)
package_load(packs)

# bounds of the plane we are sampling within
plane_xmin <- -1
plane_xmax <-  1
plane_ymin <- -1
plane_ymax <-  1

# number of pixels in space. 
npixels_x <- 100
npixels_y <- 100

# create a raster, currently has no data. We also make a blank raster
#  so that we can easily add other layers.
plane <- blank <- raster(ncol = npixels_x, nrow = npixels_y,
                         xmn = plane_xmin, xmx = plane_xmax, 
                         ymn=plane_ymin, ymx=plane_ymax)

# the x y coordinates of each pixel in the plane
plane_coord <- xyFromCell(plane, 1:ncell(plane))

# generate a spatial covariate. gen_mvn from sim_utility.R script.
x_cov <- 0.4 * gen_mvn(plane, c(0.1,0.8), c(1,1), 0.1) + 
  0.6 * gen_mvn(plane, c(0.7,0.2), c(0.5,0.75), 0.4)

# add this covariate to plane raster object.
values(plane) <- as.numeric(scale(x_cov))
names(plane) <- 'x'


# Make a second covariate from which we want to sample from
x_cov2 <- 0.2 * gen_mvn(plane, c(0.2,0.3), c(2,5), 0.1) + 
  0.8 * gen_mvn(plane, c(0.5,0.9), c(3,0.75), 0.05)

# Add it to the plane raster
temp <- blank
values(temp) <- as.numeric(scale(x_cov2))
names(temp) <- "x2"
plane <- addLayer(plane, temp)

plot(plane$x2)
# create a matrix of cells to choose from
tm <- cbind(plane_coord, values(plane))

N <- ncell(plane)
n <- 100
p <- rep(n/N,N)

test <- cube(p, tm)

# make some hexagons for stratefied sampling
tmp_poly <- SpatialPolygons(list(Polygons(list(Polygon(plane)),"x")))
tmp_poly = gBuffer(tmp_poly, width = 1)
HexPts <-spsample(tmp_poly, type="hexagonal", cellsize=0.5)
HexPols <- HexPoints2SpatialPolygons(HexPts)

HexPols <- SpatialPolygonsDataFrame(HexPols, data = data.frame(ID = 1:52),
                                    match.ID = FALSE)

HexPols <- crop(HexPols, extent(plane))

rp <- rasterize(HexPols, plane, field = "ID")

plot(plane$x)
plot(HexPols["ID"], add = TRUE)


# get cells we can sample from
my_vals <- sort(unique(values(rp), na.rm = TRUE))

my_areas <- sample(my_vals, 8)

all_vals <- values(rp)


to_go <- which(is.na(all_vals))
t2 <- cubestratified(p[-to_go],
                     tm[-to_go,],
                     all_vals[-to_go])
pc2 <- plane_coord[-to_go,]
hey <- pc2[which(t2 == 1),]

plot(plane$x)
points(hey, pch = 16)
plot(HexPols, add = TRUE)

pc3 <- values(plane)[-to_go,1]
good <- pc3[which(t2 == 1)] 




