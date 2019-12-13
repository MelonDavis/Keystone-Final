#Main script to set up all subsequent scripts in keystone_frame. Includes set 
  #up for folders, packages, and custom functions used

#===== [SYSTEM] =======

#get current r version
R.version.string
#"R version 3.5.1 (2018 - 07 - 02)"

#create object of working directory for later pathways
working.dir <- getwd()


#===== [PACKAGES] ======

#Relevent packages needed to run code in other scripts

#Install the thermimage package. This package allows analysis of thermal jpegs
  #taken from FLIR cameras
install.packages("Thermimage")
#call thermimage library
library(Thermimage)
#Thermimage calls exiftool path in many of its commands (exiftool is how it
  #reads the metadata of the image) and so we need to set up the path where 
  #exiftool exists
exiftool.p <- paste("/usr", "local", "bin/", sep = "/")
#We can check if our exiftool path works using the following.2 means total failure
system2(paste(exiftool.p, "exiftool", sep = "/"))

#We use the package imager to analyze standard RGB jpegs (such as for generating
  #histograms for the example scenes)
install.packages("imager")
library(imager)

#install.packages("staplr")
#library(staplr)

#library(dplyr)

#===== [FILE MANAGEMENT] =====

#This section is for the creation and path naming of data and results folders
  #We use three: raw data, clean data, and analysis

#Store names of folders into an object
output.folder.names <- c("raw.data", "clean.data", "analysis")

#Combine for loop with if-els so that it asks whether the each position of 
#output.folder.names exists as a file and if it's true that it doesn't exist
#it will create it.
for(i in 1:length(output.folder.names)) {
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])
}

#Now we want to make path names for each folder, first create empty object to 
  #store paths
p.final <- c(rep(NA, 3)) 

#Create for loop that generates paths for each folder and assigns path to 
  #corresponding position in p.final
for (i in 1:length(output.folder.names)) {
  p.final[i] <- paste(working.dir, "/", output.folder.names[i], "/", 
                      sep = "")
  
} 
#p.final should now contain each pathway stored in the same order as 
#output.folder.names.

print(p.final)
#To make it more intuitive we can save each path under a seperate objects to call
  #when needed
p.raw <- p.final[1]
p.cl <- p.final[2]
p.ana <- p.final[3]


#Some thermimage commands require some of the camera settings to translate raw
  #signal into temperature, therefore we must generate camera settings. Note that
  #this has to come after p.raw has been set
E60_set <- flirsettings(paste(p.raw, "E60/", "models/", "m21.in/", "E60_0002.jpg", 
                              sep = ""), exiftool.p, camvals = NULL)

#=====[FUNCTIONS] ======

#This is where any custom functions made for this project are stored

#----Renaming images----

#In order to standardize the names of image series in every folder I wrote this
  #funtion which renames all the files in a folder numarically based on inputed 
  #path and prefix

f.rename <- function(dir.path, prefix) {
  
  #We start by generating the list of file names corresponding to the inputted
    #path
  image.names <- c(list.files(dir.path))
  
  #We then create a for loop that turns those generated file names into paths
  for(i in 1:length(image.names)) {
    image.names[i] <- c(paste(dir.path, "/", 
                              image.names[i] , sep = ""))
    
  }
  
  #Now we generate an empty object the length of how many images are in the 
    #folder
  image.number.list <- c(rep("NA", length(image.names)))
  
  #For loop that combines inputed suffix with the number of the loop to numerically
    #name each file in order. I also wanted each name to have the same # of
    #characters, so they are devided such that every number becomes 4 digits long
    #(0004, 0040, 0400 etc). We store each of these into our empty object
  for(i in 1:length(image.names)) {
    
    #For images number 9 and under
    image.number.list[i] <- paste(dir.path , "/" , prefix , "_000", i, 
                                  ".jpg", sep = "")
    #For images over 9
    if(i > 9) 
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_00", i, 
                                     ".jpg", sep = ""))
    #Images over 99
    if(i > 99)
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_0", i, 
                                     ".jpg", sep = ""))
    #Images over 999
    if(i > 999)
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_", i, 
                                     ".jpg", sep = ""))
  }
  
  
  #now for every existing file name we rename it with the simpler title from 
    #our generated preffix + numbers.
  for(i in 1:length(image.names)) {
    file.rename(image.names[i], image.number.list[i])
  }
  
}

#This is how to input should look, with path referring to the folder containing
  #the files, and preffix being the letters to rename each image with, it will
  #go at the beggening of each image followed by an underscore.
#f.rename(paste("path", sep = ""), "preffix")


