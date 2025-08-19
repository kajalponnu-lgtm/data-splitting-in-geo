library(sf)
Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE
> library(FNN)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union
> 
  > # Assume df_sf is your sf object with geometry
  > cutoff <- 0.002   # minimum distance between calib and validation
> set.seed(123)
> 
  > # Step 1: initialize empty sets
  > calib <- df_sf[0, ]
Error: object 'df_sf' not found

> library(sf)
> 
  > # Convert your data frame to an sf object if not already
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
Error in UseMethod("st_as_sf") : 
  no applicable method for 'st_as_sf' applied to an object of class "function"

> df <- read.csv("C:/Users/User/Documents/R script/data splitting in geo/DEM_variability_points_main90_cleaned.csv")
> 
  > head(df)
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
> class(df)  # should return "data.frame"
[1] "data.frame"
> 
  > # --- 1. Load libraries
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > # --- 2. Load CSV
  > df <- read.csv("C:/Users/User/Documents/R script/data cleaning/DEM_variability_points_main90_cleaned.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'C:/Users/User/Documents/R script/data cleaning/DEM_variability_points_main90_cleaned.csv': No such file or directory

> # --- 1. Load libraries
  > library(sf)
> library(FNN)
> library(dplyr)
> 
  > # --- 2. Load CSV
  > df <- read.csv("C:/Users/User/Documents/R script/data splitting in geo/DEM_variability_points_main90_cleaned.csv")
> 
  > # --- 3. Check data
  > head(df)
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
> class(df)  # should return "data.frame"
[1] "data.frame"
> 
  > # --- 4. Convert to spatial object
  > df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
> 
  > # --- 5. Set minimum distance for validation points
  > cutoff <- 0.002
> set.seed(123)
> 
  > # --- 6. Initialize empty sets
  > calib <- df_sf[0, ]
> valid <- df_sf[0, ]
> 
  > # --- 7. Shuffle points to randomize
  > df_sf_shuffled <- df_sf[sample(1:nrow(df_sf)), ]
> 
  > # --- 8. Loop to assign points
  > for (i in 1:nrow(df_sf_shuffled)) {
    +     pt <- df_sf_shuffled[i, ]
    +     
      +     if (nrow(calib) == 0) {
        +         calib <- rbind(calib, pt)  # first point always goes to calibration
        +     } else {
          +         dist_to_cal <- get.knnx(st_coordinates(calib), st_coordinates(pt), k = 1)$nn.dist[,1]
          +         
            +         # Assign to validation if far enough and 25% limit not exceeded
            +         if (dist_to_cal >= cutoff & (nrow(valid) / nrow(df_sf)) < 0.25) {
              +             valid <- rbind(valid, pt)
              +         } else {
                +             calib <- rbind(calib, pt)
                +         }
          +     }
    + }
> 
  > # --- 9. Check split
  > cat("Calibration points:", nrow(calib), "\n")
Calibration points: 67 
> cat("Validation points:", nrow(valid), "\n")
Validation points: 23 
> 
  > # --- 10. Verify nearest neighbor distances from validation → calibration
  > nn <- get.knnx(st_coordinates(calib), st_coordinates(valid), k = 1)
> summary(nn$nn.dist[,1])
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0002380 0.0007338 0.0009193 0.0010476 0.0011713 0.0023611 
> 
  > # --- 11. Optionally save outputs
  > st_write(calib, "calibration_points.shp", delete_layer = TRUE)
Writing layer `calibration_points' to data source 
  `calibration_points.shp' using driver `ESRI Shapefile'
Writing 67 features with 17 fields and geometry type Point.
Warning message:
In abbreviate_shapefile_names(obj) :
  Field names abbreviated for ESRI Shapefile driver
> st_write(valid, "validation_points.shp", delete_layer = TRUE)
Writing layer `validation_points' to data source 
`validation_points.shp' using driver `ESRI Shapefile'
Writing 23 features with 17 fields and geometry type Point.
Warning message:
In abbreviate_shapefile_names(obj) :
  Field names abbreviated for ESRI Shapefile driver
> 
> library(sf)
> library(dplyr)
> 
> # Extract coordinates as columns
> calib_df <- calib %>%
+     mutate(longitude = st_coordinates(.)[,1],
+            latitude = st_coordinates(.)[,2]) %>%
+     st_set_geometry(NULL)  # drop geometry for CSV
> 
> valid_df <- valid %>%
+     mutate(longitude = st_coordinates(.)[,1],
+            latitude = st_coordinates(.)[,2]) %>%
+     st_set_geometry(NULL)
> 
> # Save to CSV
> write.csv(calib_df, "calibration_points 75:25.csv", row.names = FALSE)
> write.csv(valid_df, "validation_points 75:25.csv", row.names = FALSE)
> 
> getwd()
[1] "C:/Users/User/Documents"
> library(sf)
> library(dplyr)
> 
> # Extract coordinates as columns
> calib_df <- calib %>%
+     mutate(longitude = st_coordinates(.)[,1],
+            latitude = st_coordinates(.)[,2]) %>%
+     st_set_geometry(NULL)  # drop geometry for CSV
> 
> valid_df <- valid %>%
+     mutate(longitude = st_coordinates(.)[,1],
+            latitude = st_coordinates(.)[,2]) %>%
+     st_set_geometry(NULL)
> 
> # Save to CSV
> write.csv(calib_df, "calibration_points.csv", row.names = FALSE)
> write.csv(valid_df, "validation_points.csv", row.names = FALSE)
> 
