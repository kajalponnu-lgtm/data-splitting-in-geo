library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union
> 
  > # assume your data.frame is named DEM_variability_points_main90_cleaned
  > df <- DEM_variability_points_main90_cleaned
Error: object 'DEM_variability_points_main90_cleaned' not found

> getwd("C:/Users/User/Documents/R script/data cleaning")
Error in getwd("C:/Users/User/Documents/R script/data cleaning") : 
  unused argument ("C:/Users/User/Documents/R script/data cleaning")

> setwd("C:/Users/User/Documents/R script/data cleaning")
Error in setwd("C:/Users/User/Documents/R script/data cleaning") : 
  cannot change working directory

> df <- read.csv("DEM_variability_points_main90_cleaned.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'DEM_variability_points_main90_cleaned.csv': No such file or directory

> setwd("C:/Users/User/Documents/R script/data cleaning")
Error in setwd("C:/Users/User/Documents/R script/data cleaning") : 
  cannot change working directory

> df <- read.csv("DEM_variability_points_main90_cleaned.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'DEM_variability_points_main90_cleaned.csv': No such file or directory

> setwd("C:/Users/User/Documents/R script/data cleaning")
Error in setwd("C:/Users/User/Documents/R script/data cleaning") : 
  cannot change working directory

> # Step 1: set working directory
  > setwd("C:/Users/User/Documents/R script/data cleaning")
Error in setwd("C:/Users/User/Documents/R script/data cleaning") : 
  cannot change working directory

> setwd("C:/Users/User/Documents/R script/data splitting in geo")
> list.files()
[1] "DEM_variability_points_main90_cleaned.csv"
> 
  > # Step 1: set working directory
  > setwd("C:/Users/User/Documents/R script/data splitting in geo")
> 
  > # Step 2: load your CSV
  > df <- read.csv("DEM_variability_points_main90_cleaned.csv")
> 
  > # Step 3: check it loaded correctly
  > head(df)        # see first few rows
dem_class      DEM    slope     aspect profile_curvature plan_curvature       twi longitude
1 [521,528] 523.3196 5.694897  41.622799         -0.001811       0.000748  8.322001  78.39544
2 [521,528] 524.0000 1.297200  16.409441         -0.001440       0.001071 16.866409  78.40712
3 [521,528] 524.2588 3.784554 290.200378         -0.000896      -0.000507 10.533705  78.40832
4 [521,528] 524.7022 3.022263 222.230270         -0.000372      -0.001677 15.033554  78.40831
5 [521,528] 524.9857 1.409592 263.608582          0.001206      -0.000049 17.361683  78.40692
6 [521,528] 525.2335 2.938956   2.875862          0.000001      -0.001314 10.299860  78.39648
latitude  n sample_n   ph_0_5  ph_5_15 ph_15_30    oc_0_5   oc_5_15  oc_15_30  ph_mean
1 17.32248 40       28 5.862733 5.326739 6.679536 3.5486519 0.6883060 1.0952629 5.956336
2 17.32164 40       28 7.364915 6.632755 5.624896 0.5220527 0.8206031 0.7646234 6.540856
3 17.32182 40       28 6.226931 5.858791 6.416581 0.7521999 2.7211944 0.5570083 6.167434
4 17.32215 40       28 7.649052 6.641895 5.530036 1.0747393 1.1319228 0.2105644 6.606994
5 17.32345 40       28 7.821402 5.800933 6.188686 3.1961693 1.2809115 0.5495775 6.603674
6 17.32134 40       28 5.136669 5.469228 5.962580 3.0731451 2.4166555 1.7911480 5.522826
oc_mean
1 1.7774070
2 0.7024264
3 1.3434676
4 0.8057422
5 1.6755527
6 2.4269829
> colnames(df)    # check column names
[1] "dem_class"         "DEM"               "slope"             "aspect"           
[5] "profile_curvature" "plan_curvature"    "twi"               "longitude"        
[9] "latitude"          "n"                 "sample_n"          "ph_0_5"           
[13] "ph_5_15"           "ph_15_30"          "oc_0_5"            "oc_5_15"          
[17] "oc_15_30"          "ph_mean"           "oc_mean"          
> 
  > library(dplyr)
> 
  > set.seed(42)
> 
  > # ---- Step 1: decide number of clusters
  > k <- round(sqrt(nrow(df) / 2))
> 
  > # ---- Step 2: run k-means on coordinates
  > km <- kmeans(scale(df[, c("longitude","latitude")]), centers = k, nstart = 25)
> df$cluster <- km$cluster
> 
  > # ---- Step 3: randomly order clusters and assign ~75% to calibration
  > clust_sizes <- df %>% count(cluster) %>% sample_frac(1) 
> clust_sizes <- clust_sizes %>% mutate(cum = cumsum(n)/sum(n))
> 
  > cal_clusters <- clust_sizes$cluster[clust_sizes$cum <= 0.75]
> if (sum(df$cluster %in% cal_clusters)/nrow(df) < 0.75) {
  +     next_cl <- clust_sizes$cluster[which.min(abs(clust_sizes$cum - 0.75))]
  +     cal_clusters <- unique(c(cal_clusters, next_cl))
  + }
> 
  > df$set <- ifelse(df$cluster %in% cal_clusters, "calibration", "validation")
> 
  > # ---- Step 4: check split
  > table(df$set) / nrow(df)

calibration  validation 
0.7444444   0.2555556 
> 
  > install.packages("FNN")   # only once
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/FNN_1.1.4.1.zip'
Content type 'application/zip' length 451566 bytes (440 KB)
downloaded 440 KB

package ‘FNN’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
C:\Users\User\AppData\Local\Temp\RtmpiwMbJA\downloaded_packages
> library(FNN)
> 
  > cal_coords <- df[df$set == "calibration", c("longitude", "latitude")]
> val_coords <- df[df$set == "validation", c("longitude", "latitude")]
> 
  > nn <- get.knnx(cal_coords, val_coords, k = 1)
> distances <- nn$nn.dist[,1]   # vector of min distances
> 
  > summary(distances)  # min, 1st quartile, median, mean, max
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0006592 0.0018448 0.0026861 0.0027184 0.0032980 0.0062647 
> 
  > hist(distances,
         +      breaks = 30,
         +      main = "Nearest Distances (Validation → Calibration)",
         +      xlab = "Distance (degrees)")
> 
  > library(FNN)
> 
  > # Extract coordinates
  > cal_coords <- df[df$set == "calibration", c("longitude", "latitude")]
> val_coords <- df[df$set == "validation", c("longitude", "latitude")]
> 
  > # Compute nearest calibration neighbor for each validation point
  > nn <- get.knnx(cal_coords, val_coords, k = 1)
> distances <- nn$nn.dist[,1]
> 
  > # Numeric summary
  > summary(distances)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0006592 0.0018448 0.0026861 0.0027184 0.0032980 0.0062647 
> 
  > # Extra: quantiles at 5%, 10%, 25%, 50%, 75%, 90%, 95%
  > quantile(distances, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
5%         10%         25%         50%         75%         90%         95% 
0.001353469 0.001601620 0.001844790 0.002686066 0.003298040 0.003992643 0.004455617 
> 
  > # Mean & SD
  > mean(distances)
[1] 0.002718424
> sd(distances)
[1] 0.001221335
> 
  > library(gstat)
> library(sp)
> 
  > # Suppose your dataframe is df with columns: longitude, latitude, and target variable (e.g., Zn)
  > coordinates(df) <- ~longitude+latitude  # make it spatial
> 
  > # Compute experimental variogram
  > vgm_exp <- variogram(target_variable ~ 1, df)   # replace target_variable with your column name
Error in eval(predvars, data, env) : object 'target_variable' not found

> install.packages("gstat")
Error in install.packages : Updating loaded packages

Restarting R session...
> install.packages("gstat")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gstat_2.1-4.zip'
Content type 'application/zip' length 2360054 bytes (2.3 MB)
downloaded 2.3 MB

package ‘gstat’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
C:\Users\User\AppData\Local\Temp\Rtmp6vTH67\downloaded_packages
Loading required namespace: sp
> library(gstat)
> library(sp)
> 
  > # Suppose your dataframe is df with columns: longitude, latitude, and target variable (e.g., Zn)
  > coordinates(df) <- ~longitude+latitude  # make it spatial
Error in `coordinates<-`(`*tmp*`, value = ~longitude + latitude) : 
  setting coordinates cannot be done on Spatial objects, where they have already been set

> library(gstat)
> library(sp)
> 
  > 
  > # Compute experimental variogram
  > vgm_exp <- variogram(target_variable ~ 1, df)   # replace target_variable with your column name
Error in eval(predvars, data, env) : object 'target_variable' not found

> names(df)
[1] "dem_class"         "DEM"               "slope"             "aspect"           
[5] "profile_curvature" "plan_curvature"    "twi"               "n"                
[9] "sample_n"          "ph_0_5"            "ph_5_15"           "ph_15_30"         
[13] "oc_0_5"            "oc_5_15"           "oc_15_30"          "ph_mean"          
[17] "oc_mean"           "cluster"           "set"              
> 
  > library(gstat)
> 
  > vgm_exp <- variogram(ph_0_5 ~ 1, df)   # experimental variogram
> plot(vgm_exp)
> 
  > # Convert to data frame
  > vgm_df <- as.data.frame(vgm_exp)
> 
  > # View first few rows
  > head(vgm_df)
np         dist     gamma dir.hor dir.ver   id
1  25 0.0004277693 0.6574224       0       0 var1
2  81 0.0009963529 0.7222895       0       0 var1
3 115 0.0015731058 0.8967793       0       0 var1
4 149 0.0021811146 0.7814891       0       0 var1
5 133 0.0027677839 0.6854310       0       0 var1
6 155 0.0034040061 0.7189869       0       0 var1
> 
  > # Save to CSV
  > write.csv(vgm_df, "variogram_output.csv", row.names = FALSE)
> 
  > install.packages("blockCV")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  Warning in install.packages :
  package ‘blockCV’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

> library(sf)
Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE
> 
  > cutoff <- 0.002
> set.seed(123)
> 
  > # Randomly pick calibration points
  > calib <- data[sample(1:nrow(data), floor(0.7*nrow(data))), ]
Error in 1:nrow(data) : argument of length 0

> library(sp)   # or sf if your data is in sf format
> 
  > # Example: data is a SpatialPointsDataFrame
  > coords <- coordinates(data)
Error: unable to find an inherited method for function ‘coordinates’ for signature ‘obj = "function"’

> library(sf)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union
> 
  > # Example cutoff (block size)
  > cutoff <- 0.002  
> 
  > # Convert df to sf object (if not already)
  > df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)  # change cols if needed
> 
  > # Make a grid of blocks
  > grid <- st_make_grid(df_sf, cellsize = cutoff)
> 
  > # Assign each point to a block
  > df_sf$block_id <- st_within(df_sf, grid) %>% as.integer()
> 
  > # Randomly select 70% of blocks for calibration
  > set.seed(123)
> unique_blocks <- unique(df_sf$block_id)
> calib_blocks <- sample(unique_blocks, size = floor(0.7*length(unique_blocks)))
> 
  > # Split
  > calib <- df_sf %>% filter(block_id %in% calib_blocks)
> valid <- df_sf %>% filter(!block_id %in% calib_blocks)
> 
  > library(sf)
> library(dplyr)
> 
  > # Example cutoff (block size)
  > cutoff <- 0.002  
> 
  > # Convert df to sf object (replace "longitude" and "latitude" with your column names)
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # Make a grid of blocks
  > grid <- st_make_grid(df_sf, cellsize = cutoff)
> 
  > # Assign each point to a block
  > df_sf$block_id <- st_within(df_sf, grid) %>% sapply(as.integer)
> 
  > # Randomly select 70% of blocks for calibration
  > set.seed(123)
> unique_blocks <- unique(df_sf$block_id)
> calib_blocks <- sample(unique_blocks, size = floor(0.7*length(unique_blocks)))
> 
  > # Split into calibration and validation
  > calib <- df_sf %>% filter(block_id %in% calib_blocks)
> valid <- df_sf %>% filter(!block_id %in% calib_blocks)
> 
  > # Check how many points in each
  > table(c(ifelse(df_sf$block_id %in% calib_blocks, "calibration", "validation")))

calibration  validation 
66          24 
> 
  > library(FNN)
> 
  > # Extract coordinates
  > cal_coords <- st_coordinates(calib)
> val_coords <- st_coordinates(valid)
> 
  > # Nearest neighbor distances from validation → calibration
  > nn <- get.knnx(cal_coords, val_coords, k = 1)
> distances <- nn$nn.dist[,1]
> 
  > # Summary
  > summary(distances)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0003714 0.0009844 0.0011543 0.0012411 0.0014845 0.0024085 
> 
  > # Keep only validation points at least cutoff away
  > valid_filtered <- valid[distances >= 0.002, ]
> 
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > # --- Step 1: define cutoff distance
  > cutoff <- 0.002
> set.seed(123)
> 
  > # --- Step 2: convert to sf (if not already)
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # --- Step 3: shuffle points
  > df_sf <- df_sf %>% slice_sample(prop = 1)
> 
  > # --- Step 4: initialize empty sets
  > calib <- df_sf[0, ]
> valid <- df_sf[0, ]
> 
  > # --- Step 5: loop through points
  > for (i in 1:nrow(df_sf)) {
    +     pt <- df_sf[i, ]
    +     
      +     # if calib empty, put first point there
      +     if (nrow(calib) == 0) {
        +         calib <- rbind(calib, pt)
        +     } else {
          +         # compute distance to all calibration points
            +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = 1)$nn.dist[,1]
            +         
              +         if (dist_to_cal >= cutoff) {
                +             valid <- rbind(valid, pt)  # add to validation if far enough
                +         } else {
                  +             calib <- rbind(calib, pt)  # otherwise keep in calibration
                  +         }
            +     }
    + }
> 
  > # --- Step 6: check
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 12 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 78 
> 
  > # --- Step 7: verify distances
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])   # min distance should now >= cutoff
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0003714 0.0062940 0.0108891 0.0106739 0.0144616 0.0214326 
> 
  > cutoff <- 0.005   # increase for more separation
> set.seed(123)
> 
  > calib <- df_sf[0, ]
> valid <- df_sf[0, ]
> 
  > for (i in 1:nrow(df_sf)) {
    +     pt <- df_sf[i, ]
    +     
      +     if (nrow(calib) == 0) {
        +         calib <- rbind(calib, pt)
        +     } else {
          +         # distance to all calibration points
            +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
            +         
              +         if (all(dist_to_cal >= cutoff)) {
                +             valid <- rbind(valid, pt)
                +         } else {
                  +             calib <- rbind(calib, pt)
                  +         }
            +     }
    + }
> 
  > # Verify
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 63 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 27 
> 
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0002380 0.0008497 0.0011143 0.0012630 0.0016956 0.0023611 
> 
  > cutoff <- 0.005   # increase for more separation
> set.seed(123)
> 
  > calib <- df_sf[0, ]
> valid <- df_sf[0, ]
> 
  > for (i in 1:nrow(df_sf)) {
    +     pt <- df_sf[i, ]
    +     
      +     if (nrow(calib) == 0) {
        +         calib <- rbind(calib, pt)
        +     } else {
          +         # distance to all calibration points
            +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
            +         
              +         if (all(dist_to_cal >= cutoff)) {
                +             valid <- rbind(valid, pt)
                +         } else {
                  +             calib <- rbind(calib, pt)
                  +         }
            +     }
    + }
> 
  > # Verify
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 63 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 27 
> 
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0002380 0.0008497 0.0011143 0.0012630 0.0016956 0.0023611 
> 
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > set.seed(123)
> 
  > # Convert to sf if not already
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # Shuffle points
  > df_sf <- df_sf %>% slice_sample(prop = 1)
> 
  > # Initialize
  > calib <- df_sf[0, ]
> valid <- df_sf[0, ]
> 
  > # Start with small cutoff and increase if needed
  > cutoff <- 0.002
> 
  > for (i in 1:nrow(df_sf)) {
    +     pt <- df_sf[i, ]
    +     
      +     if (nrow(calib) == 0) {
        +         calib <- rbind(calib, pt)
        +     } else {
          +         # compute distances to all calibration points
            +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
            +         
              +         if (all(dist_to_cal >= cutoff)) {
                +             valid <- rbind(valid, pt)
                +         } else {
                  +             calib <- rbind(calib, pt)
                  +         }
            +     }
    + }
> 
  > # If validation too small, lower cutoff slightly
  > while (nrow(valid) < 0.25 * nrow(df_sf)) {
    +     cutoff <- cutoff * 0.9  # reduce by 10%
    +     
      +     # Reset sets
      +     calib <- df_sf[0, ]
      +     valid <- df_sf[0, ]
      +     
        +     for (i in 1:nrow(df_sf)) {
          +         pt <- df_sf[i, ]
          +         if (nrow(calib) == 0) {
            +             calib <- rbind(calib, pt)
            +         } else {
              +             dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
              +             if (all(dist_to_cal >= cutoff)) {
                +                 valid <- rbind(valid, pt)
                +             } else {
                  +                 calib <- rbind(calib, pt)
                  +             }
              +         }
          +     }
      + }
> 
  > # Verify split
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 12 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 78 
> 
  > # Verify distances
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0003714 0.0062940 0.0108891 0.0106739 0.0144616 0.0214326 
> 
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > set.seed(123)
> 
  > # --- Step 1: define cutoff distance
  > cutoff <- 0.002  
> 
  > # --- Step 2: convert to sf
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # --- Step 3: randomly assign 70% of points to calibration
  > calib_indices <- sample(1:nrow(df_sf), size = floor(0.7 * nrow(df_sf)))
> calib <- df_sf[calib_indices, ]
> 
  > # --- Step 4: remaining points are candidates for validation
  > remaining <- df_sf[-calib_indices, ]
> 
  > # --- Step 5: filter remaining points by distance from calibration
  > valid <- remaining[0, ]
> 
  > for (i in 1:nrow(remaining)) {
    +     pt <- remaining[i, ]
    +     
      +     dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
      +     
        +     if (all(dist_to_cal >= cutoff)) {
          +         valid <- rbind(valid, pt)
          +     }
      + }
> 
  > # --- Step 6: final check
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 62 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 1 
> 
  > # --- Step 7: check nearest distances
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.002094 0.002094 0.002094 0.002094 0.002094 0.002094 
> 
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > set.seed(123)
> 
  > # Convert to sf if not already
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # Shuffle points
  > df_sf <- df_sf %>% slice_sample(prop = 1)
> 
  > # Desired proportion
  > calib_prop <- 0.7
> target_calib <- floor(calib_prop * nrow(df_sf))
> 
  > # Initialize
  > cutoff <- 0.002   # starting cutoff
> step <- 0.0002    # how much to reduce if validation too small
> 
  > repeat {
    +     # Step 1: randomly assign calibration points
      +     calib_indices <- sample(1:nrow(df_sf), size = target_calib)
      +     calib <- df_sf[calib_indices, ]
      +     
        +     # Step 2: remaining points
        +     remaining <- df_sf[-calib_indices, ]
        +     
          +     # Step 3: assign validation points if far enough from all calib points
          +     valid <- remaining[0, ]
          +     
            +     for (i in 1:nrow(remaining)) {
              +         pt <- remaining[i, ]
              +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = nrow(calib))$nn.dist[,1]
              +         if (all(dist_to_cal >= cutoff)) {
                +             valid <- rbind(valid, pt)
                +         }
              +     }
          +     
            +     # Step 4: check proportion of validation
            +     if (nrow(valid) >= 0.25 * nrow(df_sf)) break
          +     cutoff <- cutoff - step   # reduce cutoff slightly
          +     if (cutoff <= 0) break    # prevent negative cutoff
          + }
> 
  > # Final check
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 62 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 24 
> 
  > # Verify nearest distances
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0004755 0.0006473 0.0008417 0.0010441 0.0014076 0.0022851 
> 
  > # Convert sf objects back to regular data.frames (if needed)
  > calib_df <- as.data.frame(calib)
> valid_df <- as.data.frame(valid)
> 
  > # Save to CSV
  > write.csv(calib_df, "calibration_points.csv", row.names = FALSE)
> write.csv(valid_df, "validation_points.csv", row.names = FALSE)
> 
  