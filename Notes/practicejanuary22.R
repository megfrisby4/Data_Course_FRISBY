getwd()
names <- list.files(path= "Data/data-shell/names", recursive=TRUE, full.names = TRUE, pattern=".csv", include.dirs = TRUE)
names
names[10]
names[1:10]
for (lines in names) {print(readLines(lines,2))
  
}
txts <- list.files(recursive=TRUE, all.files = TRUE, pattern = ".txt")
tsts
txts
ess <- list.files(recursive=TRUE, all.files = TRUE, pattern = "es")
names
names
birds <- read.csv("Data/Bird_Measurements.csv")
birds$Species_number
birds$Species_name
plot(mass, family)
mass <- birds$M_mass
family <- birds$Family
vec1 <- c(1, 10, 20, 20, 40, 50)
vec2 <- c(24, 33, 32, 45, 56, 60)
vec1+vec2
mat3 <- matrix
mat3
nm <- c("parrot", "pigeon", "hawk", "dove", "eagle", "owl")
boymass <- birds$M_mass_N
girlmass <- birds$F_mass_N
plot(boymass,girlmass)
boymass/girlmass
species <- birds$Species_number
plot(species, birds$Egg_mass)

  
}
