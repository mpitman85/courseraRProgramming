setwd("/Users/meganpitman/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")

data <- read.csv("KAFBexport151001to161001duplicaterows.csv")
names(data)
species <- split(data, data$Group_Species_LatinName)
species
sapply(species, function(x) colSums(x[,]))
data$CameraPosition_Grid <- as.character(data$CameraPosition_Grid)
data$CameraPosition_Grid
levels(data$CameraPosition_Grid)
data$CameraPosition_Grid <- as.factor(data$CameraPosition_Grid)
