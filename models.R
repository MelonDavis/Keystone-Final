#This script is for the cleaning and analysis of model data. I analyze three 
  #days of data: jn17 (outside), m21, and d10 (indoors). 

#====[1.Setup]====

#To start we need to set the paths to the relevent raw folders and save the 
  #names of the files inside

#Start by saving the path to the master folder for models and saving it to an 
  #object
master.folder.mod <- paste(p.raw, "E60/", "models/", sep = "")

#Then list the files within that folder to subfolders.mod
subfolders.mod <- list.files(master.folder.mod)

#We want simplified names to save generated data under 
subf.names <- c("d10", "jn17", "m21")

#Select each folder to analyze (jn17 and m21) and store the list of images it
  #contains
photo.files.17 <- list.files(paste(master.folder.mod, subfolders.mod[2], sep = ""))
photo.files.21 <- list.files(paste(master.folder.mod, subfolders.mod[3], sep = ""))
photo.files.10 <- list.files(paste(master.folder.mod, subfolders.mod[1], sep = ""))

#====[2.Target.Signal]====

#We want to define the targets in each image to analyze them seperately from
  #the rest of the scene. For this script target refers to the models. Position
  #of pixels was found using Photoshop and ImageJ and converted from row column
  #format into position through position = ((column # - 1) * 240) + row #

#----jn17.targets----

#We also want to save the positions of the model (the target in this case)
  #in each photo from june 17.

#The first 11 images (1 - 11) of jn17 are the same as the the camera didn't move 
  #there are two models in this scene, in the code they are referred to as "mat"
  #and "bby"

#First the adult model who I nicknamed "the matriarch" or "mat" for short
mat.signal <- c(19885:(19885 + 5), 20114:(20114 + 24), 20354:(20354 + 25), 
                 20594:(20594 + 26), 20834:(20834 + 26), 21076:(21076 + 24), 
                21318:(21318 + 20), 21562:(21562 + 11), 21806:(21806 + 6), 
                22048:(22048 + 4))

#Since I did not see great variation in environment throughout imaging, I
  #decided to compare these first 11 just with the final image (28), which was 
  #the closest to sunrise and thus had the most environmental change

mat.signal.28 <- c(29967:(29967 + 7), 30195:(30195 + 24), 30435:(30435 + 25), 
                   30675:(30675 + 26), 30917:(30917 + 24), 31157:(31157 + 24),
                   31399:(31399 + 22), 31643:(31643 + 16), 31886:(31886 + 10),
                   32128:(32128 + 5), 32370:(32370 + 2), 32358:(32358 + 5), 
                   32600:(32600 + 5), 32840:(32840 +6))

#We want to get an idea of how big it is (can relate real size and distance to
  #pixel size)
length(mat.signal) #matriarch is 181 pixels in images 1 - 11
length(mat.signal.28) #matriarch is 215 pixels in image 28

#Next we save the positions of the juvenile model who I nicknamed "baby boy", or
  #"bby" for short.
bby.signal <- c(23452:(23452 + 4), 43692:(43692 + 6), 43932:(43932 + 8), 
                 44171:(44171 + 10), 44411:(44411 + 11), 44651:(44651 + 12), 
                 44891:(44891 + 12), 45130:(45130 + 13), 45370:(45370 + 8))

#same as for the matriarch we want to the bby signal from the final frame 
bby.signal.28 <- c(51849:(51849 + 6), 52088:(52088 + 8), 52328:(52328 + 9), 
                   52567:(52567 + 11), 52807:(52807 + 12), 53047:(53047 + 13),
                   53287:(53287 + 13), 53528:(53528 + 12), 53768:(53768 + 8))

#bby.signal is 93 pixels in images 1 - 11
length(bby.signal)
#bby.signal is 101 pixels in image 28
length(bby.signal.28)

#Combine both birds into an object representing the bird signal of each image
jn17.bird <- c(mat.signal, bby.signal)
jn17.bird.28 <- c(mat.signal.28, bby.signal.28)


#----indoor.targets----

#For the indoor days we just want one example bird signal histogram and percantage
  #for one scene. Therefore I chose image 5 for m21 and d10. It is worth noting the model is in
  #the same position for the first images of m21 and all images in d10 had the
  #same model position

#First m21 bby position. I used an 80% threshold
bby.21.pos <- c(37556:(37556 + 7), 37794:(37794 + 10), 38034:(38034 + 14), 
                38273:(38273 + 15), 38513:(38513 + 16), 38753:(38753 + 16), 
                38993:(38993 + 15), 39233:(39233 + 14), 39473:(39473 + 9),
                39714:(39714 + 8))

#Now we can do d10 position. I used a 75% threshold for both models. First baby
bby.10.pos <- c(37315:(37315 + 2), 37552:(37552 + 7), 37791:(37791 + 9), 
                38029:(38029 + 13), 38269:(38269 + 14), 38509:(38509 + 14), 
                38750:(38750 + 16), 38991:(38991 + 15), 39231:(39231 + 15),
                39471:(39471 + 14), 39713:(39713 + 4))

#Now mat
mat.10.pos <- c(58342:(58342 + 3), 58581:(58581 + 6), 58820:(58820 + 8), 
                59058:(59058 + 13), 59298:(59298 + 14), 59536:(59536 + 18),
                59775:(59775 + 20), 60015:(60015 + 19), 60256:(60256 + 16),
                60497:(60497 + 13), 60737:(60737 + 11), 60978:(60978 + 8), 
                61221:(61221 + 2))

#Combine them into one object
bird.10.pos <- c(bby.10.pos, mat.10.pos)

#====[3.Cam.Temperature]====

#For camera temperatures I selected the middle pixels of a model, over 10 pixels
  #at least, usually settling on a 3 x 4 grid (12 pixels). This is done to 
  #avoid taking the temperature of pixels that represent mixed values of the bird
  #and background (see field geometry section)

#----jn17.fsa----

#For this data collection day, the first 11 frames are the same whereas after 
  #almost every image is a different view (different area where model is and so
  #temperature to be extracted)

#We can start by making an overall data frame to put in the signal and
  #temperatures
temp.jn17 <- data.frame(matrix(ncol = 6, nrow = length(photo.files.17)))
colnames(temp.jn17) <- c("name","time","mat_signal", "bby_signal", "mat_temp",
                         "bby_temp")
#Save the image names into the data frame
temp.jn17$name <- photo.files.17

#Extract the photo time and save it to the data frame
for(i in 1:length(photo.files.17)) {
  
  temp.jn17$time[i] <- substr(file.mtime(paste(master.folder.mod, subfolders.mod[2], "/", 
                                               photo.files.17[i], sep = "")), 12, 16)
  
}


#I start with the first 11 frames and where the models are
#For matriarch the temperature grid is: (86, 205), (86, 208), (88, 205), (88, 208)
#for 1 - 11 bby signal starts: (185,  14), (185, 17), (187, 14), (187, 17)
  
#go into folder to do analyses for images 1 - 11
  for(j in 1:11){

    #save image as a temporary matrix
    ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                               photo.files.17[j], sep = ""), exiftool.p)
    
    temp.jn17$mat_signal[j] <- mean(ma.IR[205:208, 86:88])
    temp.jn17$bby_signal[j] <- mean(ma.IR[14:17, 185:187])
    
  }

#because almost every image after changes 11 view they'll have to be done manually, 
  #i've chosen images to do based on being spaced ~ 15 min apart or having more  
  #than one image of the same view. Combined that means analyzing images 14 and 
  #17, 20 through 21, and finally 28. The following code just repeats above for
  #each of the images. Start corner for the area to extract model signal was 
  #found using outside applications

#14 mat sig : col = 102; y row = 210
#14 bby sig : col = 199; row = 17


for(j in 14){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  photo.files.17[j], 
                             sep = ""), exiftool.p)
  
  temp.jn17$mat_signal[j] <- mean(ma.IR[210:213, 102:104])
  temp.jn17$bby_signal[j] <- mean(ma.IR[17:20, 199:201])
  
}

#17 mat sig : col = 118; y row = 215
#17 bby sig : col = 206; row = 19

for(j in 17){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                             photo.files.17[j], sep = ""), exiftool.p)
  
  temp.jn17$mat_signal[j] <- mean(ma.IR[215:218, 118:120])
  temp.jn17$bby_signal[j] <- mean(ma.IR[19:22, 206:208])
  
}

#20 - 21 mat sig : col = 107; y row = 213
#20 - 21 sig : col = 197; row = 15

for(j in 20:21){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                             photo.files.17[j], sep = ""), exiftool.p)
  
  temp.jn17$mat_signal[j] <- mean(ma.IR[213:216, 107:109])
  temp.jn17$bby_signal[j] <- mean(ma.IR[15:18, 197:199])
  
}

#28 mat sig : col = 128; y row = 207
#28 bby sig : col = 220; row = 11

for(j in 28){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                            photo.files.17[j], 
                             sep = ""), exiftool.p)

  temp.jn17$mat_signal[j] <- mean(ma.IR[207:210, 128:130])
  temp.jn17$bby_signal[j] <- mean(ma.IR[11:14, 220:222])
  
}

#Finally now that we have signal for several photos over time, we can convert
  #the model signal into model temperature. I used basic geometry to calculate
  #exact distance between camera and model based on distance of tripod to bird
  #and height from ground
for(i in 1:length(photo.files.17)) {
  
  temp.jn17$mat_temp[i] <- raw2temp(temp.jn17$mat_signal[i], E = 0.95, OD = 5.03,
                                   PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
                                   PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
                                   PR2 = E60_set$Info$PlanckR2)
  
  temp.jn17$bby_temp[i] <- raw2temp(temp.jn17$bby_signal[i], E = 0.95, OD = 7.14,
                                      PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
                                      PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
                                      PR2 = E60_set$Info$PlanckR2)
  
}


#Images 12, 22, 24 - 27 were (failed) attempts to capture live birds in the area
  #so we can eliminate those rows from the table to clean it a little
temp.jn17.cl <- temp.jn17[-c(12, 22, 24:27),]


#----m21.in----

#Once again we start by making a data frame to store signal and temperature. For
  #this day only baby boy was imaged
temp.m21 <- data.frame(matrix(ncol = 4, nrow = length(photo.files.21)))
colnames(temp.m21) <- c("name","time","bby_signal", "bby_temp")

#Save image names
temp.m21$name <- photo.files.21

#Create a for loop that extracts the time of photos from the file and then saves
  #it to the dataframe
for(i in 1:length(photo.files.21)) {
  
  temp.m21$time[i] <- substr(file.mtime(paste(master.folder.mod, subfolders.mod[3], "/", 
                                               photo.files.21[i], sep = "")), 12, 16)
  
}

#On this day we have the same scene for three sections of images: from 1 to 16, 
  #17 to 26, and 27 to 39. Therefore we need to do the extraction process three
  #times with each time using the relevent model location. Also in this folder
  #are two images (12, 24) edited using preview which cause an error if it is 
  #put through the readflirJPG command. Instead I manually avoid them.

#The signal for the model from images 1 to 16 starts in: col = 161; row = 119

#create a for loop for the first 11 images that uses the above selection to 
  #extract signal and save it into the data frame
for(j in 1:11){
    
    ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  
                              photo.files.21[j], sep = ""), exiftool.p)
    temp.m21$bby_signal[j] <- mean(ma.IR[119:122, 161:163])
    

}

#Put na into the image that causes error
temp.m21$bby_signal[12] <- NA

#pick the for loop back up on the other side and complete analysis of first 
  #sequence of images
for(j in 13:16){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  photo.files.21[j], 
                             sep = ""), exiftool.p)
  temp.m21$bby_signal[j] <- mean(ma.IR[119:122, 161:163])
  
}

#For the second sequence, the bby sig started at: col = 159; y row = 117

#Run for loop to extract signal from relevent area, end before image 24
for(j in 17:23){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  photo.files.21[j], 
                             sep = ""), exiftool.p)

  temp.m21$bby_signal[j] <- mean(ma.IR[117:120, 159:161])
  
}

#Put NA into the second image that causes issues
temp.m21$bby_signal[24] <- NA

#finish second section of for loop
for(j in 25:26){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  photo.files.21[j], 
                             sep = ""), exiftool.p)
  
  temp.m21$bby_signal[j] <- mean(ma.IR[117:120, 159:161])
  
}

#For the final images the bby signal started at: col = 160; row = 113

for(j in 27:39){
  
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  photo.files.21[j], 
                             sep = ""), exiftool.p)
  
  temp.m21$bby_signal[j] <- mean(ma.IR[113:116, 160:162])
  
}

#Now that we have the signal for all possible images, we can convert it into 
  #temperature. First we need to make sure the column is numeric
temp.m21$bby_signal <- as.numeric(temp.m21$bby_signal)

#Now we can run a for loop that converts signal to temperature and saves it
for(i in 1:nrow(temp.m21)) {
  
  temp.m21$bby_temp[i] <- 
    raw2temp(temp.m21$bby_signal[i], E = 0.95, OD = 6.22,
             PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
             PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
             PR2 = E60_set$Info$PlanckR2)
  
}

#----d10.in----

#On this day all photos were with the same view, therefore they can all be
  #analyzed the same. Middle 12 pixels for each model was generated using other
  #apps

#matriarch pixels: (249, 23), (249, 26), (251, 23), (251, 26)
#baby boy pixels: (160, 115), (160, 118), (162, 115), (162, 118)

#Make dataframe to store the signals and temperatures for each photo
temp.d10 <- data.frame(matrix(ncol = 6, nrow = length(photo.files.10)))
colnames(temp.d10) <- c("name","time","bby_signal", "bby_temp", "mat_signal", 
                        "mat_temp")

#Save the image names to the data frame
temp.d10$name <- photo.files.10

#Extract the time of each photo and save it to the data frame. 
for(i in 1:length(photo.files.10)) {
  
  #extract time from the file and save into data frame
  temp.d10$time[i] <- substr(file.mtime(paste(master.folder.mod, subfolders.mod[1], 
                                              "/", photo.files.10[i], sep = "")), 
                             12, 16)
  
}

#Create a for loop that reads the signal for each photo and saves them to temp.d10
for(i in 1:length(photo.files.10)){
  
  #Save image signal matrix into a temporary object
  ma.IR <- readflirJPG(paste(master.folder.mod, subfolders.mod[1], "/",  
                             photo.files.10[i], sep = ""), exiftool.p)
  #Take area of temporary image that corresponds to the signals of the models
    #row and column numbers given above
  temp.d10$mat_signal[i] <- mean(ma.IR[23:26, 249:251])
  temp.d10$bby_signal[i] <- mean(ma.IR[115:118, 160:162])
}


#Now that we have saved the signals, we use a for loop to convert them into
  #temperature. In both cases our distance to object was around 5 m. 
for(i in 1:nrow(temp.d10)) {

  temp.d10$bby_temp[i] <- 
    raw2temp(temp.d10$bby_signal[i], E = 0.95, OD = 5,
             PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
             PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
             PR2 = E60_set$Info$PlanckR2)
  
  temp.d10$mat_temp[i] <- 
    raw2temp(temp.d10$mat_signal[i], E = 0.95, OD = 5,
             PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
             PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
             PR2 = E60_set$Info$PlanckR2)
  
}


#====[3.5.Logger.Data]====

#----jn17----

#This and all subsequent thermologger data is first exported using the logger
  #application from .gmbl to .csv. From there we can read it into an object
jn17.log <- read.csv(paste(p.raw, "thermlogger/jn17.fsa.csv", sep = ""), 
                     stringsAsFactors = FALSE)

#Rename columns into cleaner titles
colnames(jn17.log) <- c("time_since_start", "internal_temp", "outer_temp")

#create a time column to save actual time of each temperature measurement
jn17.log$time <- rep(NA, nrow(jn17.log))

#The start time in this case was 4:04, so we want a for loop where we divide
  #each of the hour increments and save it under the appropriate hour
for(i in 1:nrow(jn17.log)) {
  
  #first we save the relevent hours, in this case imaged during 4:00 and 5:00
  hours <- c("04", "05")
  
  #We want the first row to equal start time so we put first hour and add 4 
    #minutes to an innitial start time of 0
  if(i < 57)
    jn17.log$time[i] <- paste(hours[1], ":",
                             as.numeric(jn17.log$time_since_start[i] + 4), 
                             sep = "")
  
  #From 4:04 it takes 53 increments of one minute (so to row 54) to reach the
    #next hour. Once we reach the next hour we also want to reset the minutes
    #by how many minutes have elapsed since the start
  if(i >= 57)
    jn17.log$time[i] <- paste(hours[2], ":", 
                             as.numeric(jn17.log$time_since_start[i] - 56), 
                             sep = "")
  
} 

#The above code turns single numbers into single digits whereas we always want 
  #two digits (ex: 15:01 not 15:1). We're gonna rough it by generating a for
  #loop for the two sections where a new hour starts and we have this issue

#First the first 6 rows
for(i in 1:6) {
  
  #generate relevent minutes
  minutes <- seq(4, 9, by = 1)
  
  jn17.log$time[i] <- paste("04:0", minutes[i], sep = "")
  
} 

#Next for row 57 and onwards
for(i in 1:10) {
  
  #generate relevent rows
  rows <- seq(57, 66, by = 1)
  #generate relevent minutes
  minutes <- seq(0, 9, by = 1)
  
  jn17.log$time[rows[i]] <- paste("05:0", minutes[i], sep = "")
  
} 

#----m21.in----

#Used the logger app to export .gmbl to .csv than read into an object
m21.log <- read.csv(paste(p.raw, "thermlogger/m21.in.csv", sep = ""), 
                       stringsAsFactors = FALSE)

#Rename columns
colnames(m21.log) <- c("time_since_start", "time", "internal_temp", "outer_temp")

#Make a time column and forloop to generate time
for(i in 1:nrow(m21.log)) {
  
  #save relevent hours into an object
  hours <- c(12, 13, 14, 15)
  
  #We seperate them manually by hour based on how many rows until the next hour
    #in this case because they were one every 5 minutes, next hour is reached
    #on row 9
  if(i < 9)
    m21.log$time[i] <- paste(hours[1], ":",
                                as.numeric(m21.log$time_since_start[i] + 20), 
                                sep = "")
  
  #Above row nine is the next hour
  if(i >= 9)
    m21.log$time[i] <- paste(hours[2], ":", 
                                as.numeric(m21.log$time_since_start[i] - 40), 
                                sep = "")
  
  #another 12 increments of 5 minutes (21 total) until the next hour
  if(i >= 21)
    m21.log$time[i] <- paste(hours[3], ":", 
                                as.numeric(m21.log$time_since_start[i] - 100), 
                                sep = "")
  
  #And a final 12 increments (rows) to begin the last hour
  if(i >= 33)
    m21.log$time[i] <- paste(hours[4], ":", 
                                as.numeric(m21.log$time_since_start[i] - 
                                  160), sep = "")
} 

#Same as previously the  code saves single digits singly versus we want them 
  #prefaced by a 0 (ex: 15:01 not 15:1). We can change :0 to :00 manually
m21.log$time[grep(":0", m21.log$time)] <- 
  paste(substr(m21.log$time[grep(":0", m21.log$time)], 1, 2), ":00", 
        sep = "")

#because there are :50 & :55 it doesn't work in with :5, so we're doing a quick 
  #work around where we just identify relevent rows
fix.log <- c(10, 22, 34)

#create for loop to replace the ":5" in each of the saved rows
for(i in 1:3) { 
  
  #replace relevent rows with ":05"
  m21.log$time[fix.log[i]] <- 
    paste(substr(m21.log$time[fix.log[i]], 1, 2), ":05", 
          sep = "")
  
}

#----d10.in----

#I realized that the camera is around 5 minutes and 30 seconds ahead (first 
  #photo was taken in real time (PST) at 13:11:00) -> this is only important in 
  #that we have to match the logger data to it (easier than converting it into 
  #real time)

#First used logger application to export .gmbl to .csv. Then call and store into
  #object
d10.log <- read.csv(paste(p.raw, "thermlogger/d10.in.csv", sep = ""), 
                    stringsAsFactors = FALSE)

#rename columns into something sensical
colnames(d10.log) <- c("time_since_start", "internal_temp", "outer_temp")

#Generate empty column to save time in
d10.log$time <- c(rep(NA, nrow(d10.log)))

#Make a for loop to generate the time
for(i in 1:nrow(d10.log)) {
  
  #first we define the hours, in this case from 13:00 - 15:00
  hours <- c(13, 14, 15)
  
  #Break them down by hour manually. We want first collection to line up with 
    #photo = 13:16. 44 minutes until the next hour (the row number is one extra
    #therefore: 45)
  if(i < 45)
    d10.log$time[i] <- paste(hours[1], ":",
                             as.numeric(d10.log$time_since_start[i] + 16), 
                             sep = "")
  
  #after 44 minutes (starting row 45) we want to set it to hour 2 and reset 
    #minutes to the start of the hour by subtracting number of minutes
  if(i >= 45)
    d10.log$time[i] <- paste(hours[2], ":", 
                             as.numeric(d10.log$time_since_start[i] - 44), 
                             sep = "")
  
  #after another 60 minutes (104 total, row 105), it switches to hour 3 and once
    #again the minutes need to be reset by number of minutes elapsed
  if(i >= 105)
    d10.log$time[i] <- paste(hours[3], ":", 
                             as.numeric(d10.log$time_since_start[i] - 104), 
                             sep = "")
  
} 

#Like the previous two times, we want to fix the single digit rows so none of them
  #have only four characters (15:1). We can use the same code as for jn17 to fix 
  #the single digit rows and add a 0 for five characters (15:01)

#First for rows 45 on. We want the for loop to be those numbers so we can call
  #the correct placement from within the minutes object
for(i in 1:10) {
  
  #generate relevent rows
  rows <- seq(45, 54, by = 1)
  #generate relevent minutes
  minutes <- seq(0, 9, by = 1)
  
  d10.log$time[rows[i]] <- paste("14:0", minutes[i], sep = "")
    
} 

#Next for row 105 onwards
for(i in 1:10) {
  
  #generate relevent rows
  rows <- seq(105, 114, by = 1)
  #generate relevent minutes
  minutes <- seq(0, 9, by = 1)
  
  d10.log$time[rows[i]] <- paste("15:0", minutes[i], sep = "")
  
} 

#====[4.Weather.Data]====

#Weather data is only relevent for the outside day june 17

#----jn17----

#We start by reading the csv and saving it into an object
jn17.weath <- read.csv(paste(p.raw, "weather/jn17.weather.csv", sep = ""), 
                        stringsAsFactors = FALSE)

#Now we want to clean. First remove the unwanted columns: extra row numbers, 
  #Solar radiation because it doesn't change, Rain (no change), and Wind speed 
  #(no change)
jn17.weath.sec <- jn17.weath[,c(-1, -3, -4, -7)]
#Now we want to rename the columns into something more manageable
colnames(jn17.weath.sec) <- c("time", "temp", "RH")

#We just want the time from the date/time column. Create a for loop that subtracts
  #the relevent characters
for(i in 1:nrow(jn17.weath.sec)) {
  
  jn17.weath.sec$time[i] <- substr(jn17.weath$Date.Time..GMT.07.00[i], 10, 14)
  
}

#Since it took data for every 10 seconds we want an average value for each minute
#First we want to remove the first two rows because they are subsets of a minute
jn17.weath.sec <- jn17.weath.sec[-c(1, 2),]
#Now we can calculate how many minutes total there were (for 10 second increments
  #that means dividing by 6)
nrow(jn17.weath.sec)/6
#Now we can make new data frame that equals the number of minutes
jn17.weath.cl <- data.frame("time" = rep(NA, 165), "temp" = rep(NA, 165), 
                            "RH" = rep(NA, 165))

#create column of the minutes from the original data frame. By only selecting
  #unique time values we get one of each minute
jn17.weath.cl$time <- unique(jn17.weath.sec$time)

#Now we want a for loop that finds all of the related values for each unique
  #minute 
for(i in 1:nrow(jn17.weath.cl)) {
  
  #Take the mean temperature of the rows that euqal the [i] unique minute from
    #the clean data frame and save that mean into the clean data frame
  jn17.weath.cl$temp[i] <- mean(jn17.weath.sec$temp[grep(jn17.weath.cl$time[i], 
                                                         jn17.weath.sec$time)])
  
  #Do the same as above but with relative humidity (RH)
  jn17.weath.cl$RH[i] <- mean(jn17.weath.sec$RH[grep(jn17.weath.cl$time[i], 
                                                         jn17.weath.sec$time)])
  
}

#====[5.Combining Data]====

#Here we want to merge the relevent data frames for each day to have an overall
  #data frame to work with for visualization

#----jn17----

#First we want to see each of the data frames we'll be merging. We want to make
  #sure the time column matches
head(jn17.log)
head(temp.jn17.cl)
head(jn17.weath.cl)

#Everything looks good so we can start by merging camera temperature and logger
  #data by time while keeping all time values of the camera data. The logger
  #has extra time values that we aren't concerned about
jn17.d.p1 <- merge(temp.jn17.cl, jn17.log, by = "time", all.x = TRUE)

#Now we combine it with weather data by time, keeping all data from the previously
  #merged frame (so again the camera times)
jn17.d <- merge(jn17.d.p1, jn17.weath.cl, by = "time", all.x = TRUE)

#Now we can clean it by removing unwanted columns: name, mat and bby signals
jn17.d.cl <- jn17.d[,-c(2:4)]

#Also in this data frame we want the difference between the outer logger 
  #temperature and the camera temperature. Start by adding a column for difference 
  #between outer log temp and camera
jn17.d.cl$ex_temp_diff <- rep("NA", nrow(jn17.d))

#add column for difference between inner log temp and outer log temperature
jn17.d.cl$log_temp_diff <- rep("NA", nrow(jn17.d))

#Create a for loop that calculates and inputs difference of each
for(i in 1:nrow(jn17.d.cl)) {
  
  #external temperature difference = outer log temperature - camera temperature
  jn17.d.cl$ex_temp_diff[i] <- (jn17.d.cl$outer_temp[i] - jn17.d.cl$bby_temp[i])
  #log temperature difference = internal log temperature - outer log temperature
  jn17.d.cl$log_temp_diff[i] <- (jn17.d.cl$internal_temp[i] - jn17.d.cl$outer_temp[i])
  
}

#Now we can save the full table as a csv into the clean data folder [?]
write.csv(jn17.d.cl, paste(p.cl, "E60/data.frames/jn17.d.csv", sep = ""))

#----m21----

#Now we want to do the same thing for m21. In this case we don't
  #have to deal with weather data, just log and camera. We start by visualizing
  #the tables
head(m21.log)
head(temp.m21)

#We now merge them by time while keeping all x values of the camera data
m21.d <- merge(temp.m21, m21.log, by = "time", all.x = TRUE)

#Now we want to clean it. Firs remove the unwanted columns: name, bby_signal,
m21.d.cl <- m21.d[,-c(2:3)]
#We also want to remove the first two rows and row 22, since they have no
  #corresponding camera data because of being edited in preview
m21.d.cl <- m21.d.cl[-c(1:2, 22),]

#Now we add columns for difference between outer log temp and camera temp, and
  #difference between internal and external log temperature
m21.d.cl$ex_temp_diff <- rep("NA", nrow(m21.d.cl))
m21.d.cl$log_temp_diff <- rep("NA", nrow(m21.d.cl))

#For loop that calculates and inputs that difference, same formula as for
  #jn17
for(i in 1:nrow(m21.d.cl)) {
  
  m21.d.cl$ex_temp_diff[i] <- (m21.d.cl$outer_temp[i] - m21.d.cl$bby_temp[i])
  m21.d.cl$log_temp_diff[i] <- (m21.d.cl$internal_temp[i] - m21.d.cl$outer_temp[i])
  
}

#now we can save as a csv
write.csv(m21.d.cl, paste(p.cl, "E60/data.frames/m21.d.csv", sep = ""))

#----d10----

#Now we want to do the same thing for d10, we only have log temperature and camera
  #data. Once again, we start by visualizing.
head(d10.log)
head(temp.d10)

#We now merge them by time while keeping all x values of the camera data
d10.d <- merge(temp.d10, d10.log, by = "time", all.x = TRUE)

#Now to clean it, removing unwanted columns: name, bby and mat signal
d10.d.cl <- d10.d[,-c(2:3, 5)]

#Now we add columns for difference between outer log temp and camera temp, and
#difference between internal and external log temperature
d10.d.cl$ex_temp_diff <- rep("NA", nrow(d10.d.cl))
d10.d.cl$log_temp_diff <- rep("NA", nrow(d10.d.cl))

#For loop that calculates and inputs that difference, same formula as for
#jn17
for(i in 1:nrow(d10.d.cl)) {
  
  d10.d.cl$ex_temp_diff[i] <- (d10.d.cl$outer_temp[i] - d10.d.cl$bby_temp[i])
  d10.d.cl$log_temp_diff[i] <- (d10.d.cl$internal_temp[i] - d10.d.cl$outer_temp[i])
  
}

#now we can save as a csv
write.csv(d10.d.cl, paste(p.cl, "E60/data.frames/d10.d.csv", sep = ""))

#====[6.Stat.Summary]====

#We're going to start by generating the data into objects and getting statistical
#summaries

#Call the overall csv for jn17 and save into an object. Make sure to include
#NA conversion because there are NAs in this data set
jn17.ana <- read.csv(paste(p.cl, "E60/data.frames/jn17.d.csv", sep = "" ), 
                     stringsAsFactors = FALSE, strip.white = TRUE, 
                     na.strings = c("NA",""))

#Call the csv for m21 
m21.ana <- read.csv(paste(p.cl, "E60/data.frames/m21.d.csv", sep = "" ), 
                    stringsAsFactors = FALSE, strip.white = TRUE, 
                    na.strings = c("NA",""))

#Call the csv for d10
d10.ana <- read.csv(paste(p.cl, "E60/data.frames/d10.d.csv", sep = "" ), 
                    stringsAsFactors = FALSE, strip.white = TRUE, 
                    na.strings = c("NA",""))

#Let's generate the statistical summaries for the differences of each day

#First m21
m21.cam.stats <- summary(m21.ana$ex_temp_diff)
m21.log.stats <- summary(m21.ana$log_temp_diff)
sd(m21.ana$ex_temp_diff, na.rm = TRUE)

#Now jn17
jn17.cam.stats <- summary(jn17.ana$ex_temp_diff)
jn17.log.stats <- summary(jn17.ana$log_temp_diff)
sd(jn17.ana$ex_temp_diff, na.rm = TRUE)

#Finally d10
d10.cam.stats <- summary(d10.ana$ex_temp_diff)
d10.log.stats <- summary(d10.ana$log_temp_diff)
sd(d10.ana$ex_temp_diff, na.rm = TRUE)


#====[7.Visualization]====

#We'll start by visualizing the two indoor days. We want to compare log
#temp difference and camera temperature difference

#First define a range for the y-limit to cover both day's ranges
indoor.range <- range(0, m21.ana$ex_temp_diff, d10.ana$ex_temp_diff, 
                      na.rm = TRUE)

#We want to save the plot of the two indoor camera temperature difference days
#as a PDF
pdf(paste(p.ana, "/graphs/indoor_cam.pdf", sep = ""), width = 5, height = 5)

#create scatter plot of m21 data
plot(m21.ana$time_since_start, m21.ana$ex_temp_diff, las = 1,
     ylim = indoor.range, col = "red",
     main =  "Indoor Surface Temperature Difference Over Time", 
     xlab = "Time (min)", ylab = "Temperature Difference (Surface - TI)") 
#To get a visual idea on whether there was a trend over time we can generate
#and plot a linear regression line
fit.m21 <- lm(m21.ana$ex_temp_diff ~ m21.ana$time_since_start)
#Read the line summary so we get some idea of how strong it is
m21.lm <- summary(fit.m21)

#Now, let's add d10 data to the graph
points(d10.ana$time_since_start, d10.ana$ex_temp_diff, las = 1,
       ylim = indoor.range, col = "blue")
#To get a visual idea on whether there was a trend over time we can generate
#and plot a linear regression line
fit.d10 <- lm(d10.ana$ex_temp_diff ~ d10.ana$time_since_start)
#Read the line summary so we get some idea of how strong it is
m21.lm <- summary(fit.d10)

#Finish the pdf
dev.off()

#We also want to experiment with removing the outliers to see how it influences 
#the fit line
m21.ana.rmout <- m21.ana[c(- 18, - 29),]
#Generate the linear model for when the outliers are removed
fit.m21.rmout <- lm(m21.ana.rmout$ex_temp_diff ~ m21.ana.rmout$time_since_start)
#read line summary
m21.lm.rmout <- summary(fit.m21.rmout)

#Now let's do the same thing for the logger internal to external data. First 
  #create the range for the log difference
indoor.log.range <- range(0, m21.ana$log_temp_diff, d10.ana$log_temp_diff,
                          jn17.ana$log_temp_diff,
                          na.rm = TRUE)

#We want to save it as a PDF
pdf(paste(p.ana, "/graphs/indoor_log.pdf", sep = ""), width = 5, height = 5)

#Now let's do the same thing as with the surface temperature difference but with 
  #the logger temperature difference
plot(m21.ana$time_since_start, m21.ana$log_temp_diff, las = 1,
     col = "red", ylim = indoor.log.range,
     main =  "Logger Temperature Difference Over Time", 
     xlab = "Time (min)", ylab = "Temperature Difference (Surface - Internal)") 
#To get a visual idea on whether there was a trend over time we can generate
#and plot a linear regression line
fit.m21 <- lm(m21.ana$cam_temp_diff ~ m21.ana$time_since_start)
#Read the line summary so we get some idea of how strong it is
m21.lm <- summary(fit.m21)

#Now, let's add d10 data to the graph
points(d10.ana$time_since_start, d10.ana$log_temp_diff, las = 1, col = "blue")
#To get a visual idea on whether there was a trend over time we can generate
#and plot a linear regression line
fit.d10 <- lm(d10.ana$log_temp_diff ~ d10.ana$time_since_start)
#Read the line summary so we get some idea of how strong it is
d10.log.lm <- summary(fit.d10)

#Finally we add the outdoor (jn 17) data
points(jn17.ana$time_since_start, jn17.ana$log_temp_diff, las = 1, col = "purple")
#To get a visual idea on whether there was a trend over time we can generate
#and plot a linear regression line
fit.jn17 <- lm(jn17.ana$log_temp_diff ~ jn17.ana$time_since_start)
#Read the line summary so we get some idea of how strong it is
jn17.log.lm <- summary(fit.jn17)

#Finish the pdf
dev.off()

#Finally we want to plot the jn17 surface temperature difference over time and 
  #save it as a pdf
pdf(paste(p.ana, "/graphs/jn17.cam.pdf", sep = ""), width = 5, height = 5)

plot(jn17.ana$time_since_start, jn17.ana$ex_temp_diff, las = 1,
     ylim = range(0, jn17.ana$ex_temp_diff, na.rm = TRUE),
     main =  "Outdoor Surface Temperature Over Time", 
     xlab = "Time (min)", ylab = "Temperature Difference (Surface - TI)")
fit.jn17 <- lm(jn17.ana$ex_temp_diff ~ jn17.ana$time_since_start)
#Read line summary and save it into an object
jn17.lm <- summary(fit.jn17)

dev.off()

#====[8.Histograms]====

#To quantify the contrast and uniformity of each image, I used histograms to
#represent the distribution of temperatures within the image as well as
#where the target (model) signal falls within that distribution. Also in this
#section I generate percentage of target signal in the top distribution

#----jn17----

#Start with june 17th data. Start by generating names for the histograms
hist.names.jn17 <- paste("jn17.", substr(photo.files.17, 5, 8), sep = "")

#Create a for loop that creates a histogram for each image in jn17
for(i in 1:length(photo.files.17)) {
  
  #For each photo store matrix of values into a temporary object
  signal.d.t <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                                  photo.files.17[i], 
                                  sep = ""), exiftool.p)
  
  #We want to save these histograms so we begin the saving command, 5 x 3 inches
  #is the size I chose for all my histograms
  pdf(paste(p.ana, "histograms/models/", hist.names.jn17[i], ".pdf", sep = ""), 
      width = 5, height = 3)
  
  #Because we want to compare the histogram of the image and histogram of model
  #inside the image, we want to set the x-limit and bin width to depend on the
  #distribution within the image, but be the same for both graphs generated for
  #that image
  xlim.t <- c(min(signal.d.t) - 100, max(signal.d.t) + 100)
  
  bins.t <- seq(min(signal.d.t) - 100, max(signal.d.t) + 100, by = (((max(signal.d.t) + 100) - 
                                                                       (min(signal.d.t) - 100))/20))
  
  #create histogram of overall signal
  hist(signal.d.t, xlim = xlim.t, breaks = bins.t, xlab = "Scene Signal")
  
  #finish saving histogram as pdf
  dev.off()
  
}

#Now we want to make a histogram for the bird in image 2. First we have to
#translate the positions of the bird into the signal within that image

#Read the jpg and save the signals into an object
signal.17.02 <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                                  photo.files.17[2], sep = ""), exiftool.p)
#Return values of the bird positions from within the signal matrix
bird17.s <- signal.17.02[jn17.bird]

#We also want to set the bin and x lims for the histogram for the first image 
#so it's the same as the overall scene generated in the for loop above. Keep 
#in mind the first image (image 1) didn't wasn't the first actual imaging 
#attempt, second image was

xlim.jn17.01 <- c(min(signal.17.02) - 100, max(signal.17.02) + 100)

bins.jn17.01  <- seq(min(signal.17.02) - 100, max(signal.17.02) + 100, 
                     by = (((max(signal.17.02) + 100) - (min(signal.17.02) - 
                                                           100))/20))

#We want to save the generated histogram as a pdf
pdf(paste(p.ana, "histograms/models/bird.", hist.names.jn17[2], ".pdf", sep = ""), 
    width = 5, height = 3)

hist(bird17.s, xlim = xlim.jn17.01, breaks = bins.jn17.01, xlab = "Model Signal")

dev.off()

#Now we want to figure out how many of the bird signal is in the top 100 pixels
#of the scene to contrast with the same number derived from live birds. Since
#the scene remains similar for the first images but subsequently changes we 
#derived % for the first eleven images

#First make a data frame to store the percentages
jn17.perc <- data.frame("image" = photo.files.17[1:11], "percentage" = 
                          rep(NA, 11))


for(i in 1:11) {
  
  #Read each of the first 11 images and store it into a temporary matrix
  signal.t <- readflirJPG(paste(master.folder.mod, subfolders.mod[2], "/",  
                                photo.files.17[i], sep = ""), exiftool.p)
  
  #Sort the matrix so that it goes from highest to lowest
  signal.s <- sort(signal.t, decreasing = TRUE)
  
  #we also want to generate the signals for the targets (models) in the scene
  bird17.s.t <- signal.t[jn17.bird]
  
  #Create a temporary percentage object to store the percentage points
  perc.t <- rep(NA, length(bird17.s.t))
  
  #for each of the pixels of the target models (each position in bird17.s.t) 
  #do the following
  for(j in 1:length(bird17.s.t)) {
    
    #If the signal of the pixel is greater than the 100th hottest pixel in the 
    #scene then add a percent such that if all the pixels were above the 
    #threshold they would add to 100 percent (so 1 divided by number of pixels)
    if(bird17.s.t[j] > signal.s[99]) {
      
      perc.t[j] <- 1/length(bird17.s.t)
      
    } else {
      
      #If it's not above than put it at 0 since it counts as not getting that
      #percent
      perc.t[j] <- 0
      
    }
  }
  
  #Now we can add them all up and save them into our data frame for percentages
  jn17.perc$percentage[i] <- sum(perc.t)
  
}

#The percentage for image 2 which is the one we're analyzing is 35.0%
jn17.perc$percentage[2]

#----m21----

#Now we want to generate histogram example for m21. I chose image 5 since it was
#a bit after begenning but still within the initial images that had the same
#frame

hist.names.m21 <- paste("m21.", substr(photo.files.21, 5, 8))

#Save the signal of image 5 from m21 into an object
signal.21.05 <- readflirJPG(paste(master.folder.mod, subfolders.mod[3], "/",  
                                  photo.files.21[5], sep = ""), exiftool.p)

#Now we want to generate x limit and bins
xlim.m21.05 <- c(min(signal.21.05) - 100, max(signal.21.05) + 100)

bins.m21.05  <- seq(min(signal.21.05) - 100, max(signal.21.05) + 100, 
                    by = (((max(signal.21.05) + 100) - (min(signal.21.05) - 
                                                          100))/20))
#Now we can make a histogram of the overall scene as save it as a pdf to the 
#histogram folder
pdf(paste(p.ana, "histograms/models/", hist.names.m21[5], ".pdf", sep = ""), 
    width = 5, height = 3)

#Manually adjusted range for visual clarity
hist(signal.21.05, xlim = range(16500, 18505), breaks = bins.m21.05)

dev.off()

#Save the signals at baby birds position
bird21.s <- signal.21.05[bby.21.pos]

#Make histogram of the bird signal and save it 
pdf(paste(p.ana, "histograms/models/bird.", hist.names.m21[5], ".pdf", sep = ""), 
    width = 5, height = 3)

#Same range and breaks as overall histogram
hist(bird21.s, xlim = range(16500, 18505), breaks = bins.m21.05)

dev.off()


#We can now generate the percentage for this scene. First we want to create an
#empty object to store positives and false for each pixel
perc.21 <- rep(NA, length(bird21.s))

#We now want to make a sorted version of all the signals to draw the 100th 
#hottest pixel from
signal.21.s <- unique(sort(signal.21.05, decreasing = TRUE))

for(j in 1:length(bird21.s)) {
  
  #If the signal of the pixel is greater than the 100th hottest pixel in the 
  #scene then add a percent such that if all the pixels were above the 
  #threshold they would add to 100 percent (so 1 divided by number of pixels)
  if(bird21.s[j] > signal.21.s[99]) {
    
    perc.21[j] <- 1/length(bird21.s)
    
  } else {
    
    #If it's not above than put it at 0 since it counts as not getting that
    #percent
    perc.21[j] <- 0
    
  }
}

#The sum is the total percentage, for this image (0005) it was 68.7%
sum(perc.21) 

#----d10----

#Similar to m21 we want to generate a histogram example for m21. I chose image 8
#this time

#Save the signal of image 5 from m21 into an object
signal.10.08 <- readflirJPG(paste(master.folder.mod, subfolders.mod[1], "/",  
                                  photo.files.10[8], sep = ""), exiftool.p)

#Now we want to generate x limit and bins
xlim.d10.08 <- c(min(signal.10.08) - 200, max(signal.10.08) + 400)

bins.d10.08  <- seq(min(signal.10.08) - 100, max(signal.10.08) + 100, 
                    by = (((max(signal.10.08) + 100) - (min(signal.10.08) - 
                                                          100))/20))
#Now we can make a histogram of the overall scene as save it as a pdf to the 
#histogram folder
pdf(paste(p.ana, "histograms/models/d10.0008.pdf", sep = ""), 
    width = 5, height = 3)

#Manually adjusted range for visual clarity
hist(signal.10.08, xlim = xlim.d10.08, breaks = bins.d10.08)

dev.off()

#Save the signals at baby birds position
bird10.s <- signal.10.08[bird.10.pos]

#Make histogram of the bird signal and save it 
pdf(paste(p.ana, "histograms/models/bird.d10.0008.pdf", sep = ""), 
    width = 5, height = 3)

#Same range and breaks as overall histogram
hist(bird10.s, xlim = xlim.d10.08, breaks = bins.d10.08)

dev.off()


#We can now generate the percentage for this scene. First we want to create an
#empty object to store positives and false for each pixel
perc.10 <- rep(NA, length(bird10.s))

#order signal so we can choose the top 100th
signal.10.s <- unique(sort(signal.10.08, decreasing = TRUE))

for(j in 1:length(bird10.s)) {
  
  #If the signal of the pixel is greater than the 100th hottest pixel in the 
  #scene then add a percent such that if all the pixels were above the 
  #threshold they would add to 100 percent (so 1 divided by number of pixels)
  if(bird10.s[j] > signal.10.s[99]) {
    
    perc.10[j] <- 1/length(bird10.s)
    
  } else {
    
    #If it's not above than put it at 0 since it counts as not getting that
    #percent
    perc.10[j] <- 0
    
  }
}

#The sum is the total percentage. For this image (0008) it was around 39.3%
sum(perc.10)

#====[ExampleJPGS]====

#I also created example JPEGs using photoshop that I wanted to get histograms
  #for in order to compare to my live scenes

#Start by listing the photos (within the example folder)
hist.ex <- c(list.files(paste(p.ana, "histograms/example", sep = "")))

#Now we can make a forloop that extracts the image data and turns it into a
  #histogram
for(i in 1:length(hist.ex)) {
  
  #Load the image in greyscale into a temporary object
  example.t <- grayscale(load.image(paste(p.ana, "histograms/example/", 
                                          hist.ex[i], sep = "")))
  
  #Save the overall histogram as a pdf
  pdf(paste(p.ana, "histograms/", substr(hist.ex[i], 1, (nchar(hist.ex[i]) - 4)), 
            ".pdf", sep = ""), width = 5, height = 3)
  
  #Create the histogram using specific limits and breaks
  hist(example.t, xlim = c(0, 1), breaks = c(seq(0, 1, 0.05)), 
       xlab = "Pixel Color", main = "Histogram of Scene Pixels")
  
  dev.off()
  
  #Save the target into a temporary object
  target.t <- color.at(example.t, c(158:163) , c(116:123))
  
  #Save it as a pdf
  pdf(paste(p.ana, "histograms/target.", substr(hist.ex[i], 1, 
                                                (nchar(hist.ex[i]) - 4)), 
            ".pdf", sep = ""), width = 5, height = 3)
  
  #Generate histogram of the target using the same xlim and breaks as overall
    #the overall scene
  hist(target.t, xlim = c(0, 1), breaks = c(seq(0, 1, 0.05)), 
       xlab = "Pixel Color", main = "Histogram of Target Pixels")
  
  dev.off()
  
}



