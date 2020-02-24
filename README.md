# ratSampling
A repository to generate spatially balanced samples for rat sampling throughout Chicago

To install:
```
devtools::install_github("mfidino/ratSampling")
```

To use:

```
my_ward <- read.csv("./data/40th_ward.csv",
                 stringsAsFactors = FALSE)

my_sample <- sample_ward(
  x = test,
  n = 12,
  coord_names = c("INTPTLAT", "INTPTLON"),
  covar_names = c("medincome", "ratcomplaints", "popdens"),
  seed = NULL
)

my_sample
$row_indices
 [1] 29 14 16 10 17 65 71 47 55 67  8 28

$seed
[1] 1582563735

# what they sites are
my_locations <- my_ward[my_sample$row_indices,]

```
