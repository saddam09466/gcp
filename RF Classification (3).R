# import packages
library(raster)
library(randomForest)
 
# import image (img) and shapefile (shp)
setwd("//Users/test/Desktop/Project- cloud_Computing/PROJECT_ Cloud_Computing /Project/2013/Clipped_Pansharped")
img <- brick("clipped_2013.tif")
shp <- shapefile("Traning_Shp.shp")

plotRGB(img, r = 4, g = 3, b = 2, stretch = "lin")
plot(shp, col="green", add=TRUE)
 
# extract samples with class labels and put them all together in a dataframe
names(img) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5", "Band_6")
smp <- extract(img, shp, df = TRUE)
smp$cl <- as.factor( shp$MC_ID[ match(smp$ID, seq(nrow(shp)) ) ] )
smp <- smp[-1]

# tune and train rf model
smp.size <- rep(min(summary(smp$cl)), nlevels(smp$cl))
rfmodel <- tuneRF(x = smp[-ncol(smp)],
                  y = smp$cl,
                  sampsize = smp.size,
                  strata = smp$cl,
                  ntree = 250,
                  importance = TRUE,
                  doBest = TRUE
                  )

# save rf model 
save(rfmodel, file = "rfmodel.RData")


# predict image data with rf model
result <- predict(img,
                  rfmodel,
                  filename = "classification_RF.tif",
                  overwrite = TRUE
                  )
                  

plot(result, 
     axes = FALSE, 
     box = FALSE,
     col = c("#fbf793", # baresoil
             "#006601", # forest
             "#bfe578", # grassland
             "#d00000", # urban_hd
             "#fa6700", # urban_ld
             "#6569ff"  # water
     )
)

