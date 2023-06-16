## 2023-05-07_Jurassic_corals_of_Kachchh_in_India
## Coral data

# set directory
setwd("C:/Users/lars-/Desktop/2023-05-07_Jurassic_corals_of_Kachchh_in_India/R")

# read in the data
dat <- read.csv("data/Corals_Kachchh.csv", sep=";")

attach(dat)

# Numbers
sn <- length(unique(Specimen.No)) # Number of specimen
gn <- length(unique(genus)) # Number of genus
unique(genus)
spn <- length(unique(species)) # Number of species
unique(species)

# Analysis of coloniality (How many corals are colonial or solitary)
col <- sum(coloniality == "colony") # Number of colonial corals
sol <- sum(coloniality == "solitary") # NUmber of solitary corals

# Subset to samples with incisions
inc <- dat[dat$number.of.incisions > 0, ]

# Number of samples with incisions
length(unique(inc$Specimen.No))

###########
# mean length of incision for first sample
#'Sample1 <- inc[inc$Specimen.No == "KCo1-1",]
#'View(Sample1)
#'
#'mean(Sample1$length.of.incisions..mm..top, na.rm = TRUE)
###########
# mean length of incision top
unique(inc$Specimen.No)
sampleInc <-unique(inc$Specimen.No)
litop <- NULL

for(i in 1:length(sampleInc)){
  currentSample <- sampleInc[i]
  sampleSub <- inc[inc$Specimen.No == currentSample, ]
  samplelitop <- mean(sampleSub$length.of.incisions..mm..top, na.rm = TRUE)
  litop <- c(litop, samplelitop)
}
names(litop) <-sampleInc
litop

# mean length of incisions bottom
unique(inc$Specimen.No)
sampleInc <-unique(inc$Specimen.No)
libottom <- NULL

for(i in 1:length(sampleInc)){
  currentSample <- sampleInc[i]
  sampleSub <- inc[inc$Specimen.No == currentSample, ]
  samplelibottom <- mean(sampleSub$length.of.incisions..mm..bottom, na.rm = TRUE)
  libottom <- c(libottom, samplelibottom)
}
names(libottom) <- sampleInc
libottom

# boxplot of lengths of incision top and bottom
boxplot(litop, libottom, ylab = "length [mm]", names = c("length of incisions top", "length of incisions bottom" )) # noch besser bennenen

# mean distances of incisions per specimen
unique(inc$Specimen.No)
sampleInc <- unique(inc$Specimen.No)
d <- NULL

for(i in 1:length(sampleInc)){
  currentSample <- sampleInc[i]
  sampleSub <- inc[inc$Specimen.No == currentSample, ]
  sampledistance <- mean(sampleSub$distance.to.next.incision..mm., na.rm = TRUE)
  d <- c(d, sampledistance)
}
names(d) <- sampleInc
d

boxplot(distance.to.next.incision..mm. ~ Specimen.No, data = inc, ylab = "distance to next incision [mm]")  
plot(distance.to.next.incision..mm., main = "Distances", ylab = "distance to next incision [mm]") # scatter plot of all distances
plot(d, main = "Distances", ylab = "distance to next incision [mm]") # plot of mean distances per specimen

# Plot colony width vs mean length of incisions bottom
colsize <- unique(inc$colony.width..mm.) #coral width per specimen

plot(libottom, colsize, xlab = "mean length of incisions bottom", ylab = "colony size")
model1 <- lm(colsize ~ libottom)
summary(model1)
abline(lm(colsize ~ libottom))

# Plot colony width vs mean length of incisions top
plot(litop, colsize, xlab = "mean length of incisions top", ylab = "colony size")
model2 <- lm(colsize ~ litop)
summary(model2)
abline(lm(colsize ~ litop))

# mean width of incisions top
unique(inc$Specimen.No)
sampleInc <-unique(inc$Specimen.No)
witop <- NULL

for(i in 1:length(sampleInc)){
  currentSample <- sampleInc[i]
  sampleSub <- inc[inc$Specimen.No == currentSample, ]
  samplewitop <- mean(sampleSub$width.of.incisons..mm..top, na.rm = TRUE)
  witop <- c(witop, samplewitop)
}
names(witop) <- sampleInc
witop

# mean width of incisions bottom
unique(inc$Specimen.No)
sampleInc <-unique(inc$Specimen.No)
wibottom <- NULL

for(i in 1:length(sampleInc)){
  currentSample <- sampleInc[i]
  sampleSub <- inc[inc$Specimen.No == currentSample, ]
  samplewibottom <- mean(sampleSub$width.of.incisions..mm..bottom, na.rm = TRUE)
  wibottom <- c(wibottom, samplewibottom)
}
names(wibottom) <- sampleInc
wibottom

# boxplot width of incisions top and bottom
boxplot(witop, wibottom, ylab = "width [mm]", names = c("width of incisions top", "width of incisions bottom" ))

## all plots in one figure
windows()
op <- par(xaxs="i", yaxs="i", mfrow = c(2,2))
boxplot(litop, libottom, ylab = "length [mm]", names = c("length of incisions top", "length of incisions bottom" )) # noch besser bennenen
boxplot(witop, wibottom, ylab = "width [mm]", names = c("width of incisions top", "width of incisions bottom" ))
plot(libottom, colsize, xlab = "mean length of incisions bottom", ylab = "colony size [mm]")
plot(litop, colsize, xlab = "mean length of incisions top", ylab = "colony size [mm]")

#boxplot(distance.to.next.incision..mm. ~ Specimen.No, data = inc, main = "Distance of incisions")  

par(op)
# T-test

# t.test(colsize ~ libottom)

# Dimorpharaea
unique(dat$genus)


