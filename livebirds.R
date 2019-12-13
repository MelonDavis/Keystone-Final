#This script is to clean and analyze the data in live birds 

#Out of the outdoor image days m13, m16, and a9 all have associated weather data 
  #from the HOBO weather station therefore I draw all my photos from there; 
  #a9 also has distance measurement from a rangefinder for some of the photos
  #in; I chose 12 photos from m13 and a9 to focus on two days with different
  #timing of collection

#====[1.Setup]====

#First we want to setup the data by saving the folder names to objects. For live
  #birds I copied and moved selected images into a clean data folder

#call and save the master folder containing the livebird data
master.folder.live <- paste(p.cl, "E60/", "livebirds/", sep = "")

#first we save the subfolder names in our clean data folder to access the images
  #we've selected and put into those folders
subfolders.live <- list.files(master.folder.live)

#Create objects consisting of every photo file in each subfolder
photo.files.a9 <- list.files(paste(master.folder.live, subfolders.live[1], sep = ""))
photo.files.m13 <- list.files(paste(master.folder.live, subfolders.live[2], sep = ""))

#we also want to save images in the original raw folder in order to generate
  #the time stamp for each later. There are several live bird image collection
  #days but we are focused on april 9th (a9) and march 13 (m13), or position
  #2 and 5 of the overall list
raw.subf.live <- list.files(paste(p.raw, "E60/livebirds", sep = "" ))

#We want to generate the time stamps for each photo. Can create a for loop
  #that goes into each subfolder
for(i in 1:2) {
  
  #For each subfolder list files into a temporary object and save a temporary
    #path to that file
  files.t <- list.files(paste(p.raw, "E60/livebirds/", raw.subf.live[i], sep = "" ))
  p.t <- paste(p.raw, "E60/livebirds/" , raw.subf.live[i], sep = "")
  
  #create a data frame based on how many files are in the folder
  d.t <- data.frame(matrix(ncol = 2, nrow = length(files.t)))
  #Name the columns
  colnames(d.t) <- c("name","time")
  
  #List the file names into the name column of the data frame
  d.t$name <- list.files(p.t)
  
  #Create a nested for loop that for each image subtracts the time each file 
    #was taken
  for(j in 1:length(files.t)) {
    
    #Save it to the temporary data frame time column
    d.t$time[j] <- substr(file.mtime(paste(p.t,"/",files.t[j], 
                                            sep = "")), 12, 16)
    
  }
  
  #Assign temporary dataframe a name based on the folder + ".time"
  assign(paste(subfolders.live[i], ".time", sep = ""), d.t)
  
}


#====[2.Target.Pixels]====

#Same as for the models used photoshop and imagej to generate positions for birds

#----m13----

#IMG E60_0023 - juncos - 4
  #80% grey is the cut off

junco.23.1 <- c(33513, 33753, 33274, 33514, 33754)
#5 pixels
junco.23.2 <- c(38850)
#1 pixel
junco.23.3 <- c(45268, 45508)
#2 pixels
junco.23.4 <- c(47182, 46943, 47183)
#3 pixels

junco.23 <- c(junco.23.1, junco.23.2, junco.23.3, junco.23.4)

#mean size for juncos in this image was 2.75 pixels
mean(c(5, 1, 2, 3))

#IMG E60_0061 - S. Towhee - 1
  # 80% grey cut off

towhee.61 <- c(39486:(39486 + 4), 39726:(39726 + 5), 39966:(39966 + 5), 
               40207: (40207 + 4), 40448: (40448 + 3), 40689: (40689 + 1))

#towhee is 28 pixels
length(towhee.61)

#IMG E60_0075 - Mallards - 4
  #80% threshold

#identify corresponding pixels and bird size in pixels
mallard.75.1 <- c(23188, 23427:23428, 23667:23668, 23665, 23904:23905)
length(mallard.75.1) #8 pixels big

mallard.75.2 <- c(26549, 26788:26789, 27028:27029, 27268:27269, 27508:27509, 
                  27745:27748, 27985)
length(mallard.75.2) #14 pixels big

mallard.75.3 <- c(32070, 32310, 32550, 32790, 33030, 33270, 33267:33268, 33507)
length(mallard.75.3) #9 pixels big

mallard.75.4 <- c(37348, 37588)
#2 pixels (partially obscured by bank)

mallard.75 <- c(mallard.75.1, mallard.75.2, mallard.75.3, mallard.75.4)

#IMG E60_0139 - RW BB - 1
  #75% threshold

rwbb.139 <- c(38978:38979, 39215:(39215 + 6), 39455:(39455 + 12), 
              39695:(39695 + 12), 39936:(39936 + 7), 40177:(40179))
length(rwbb.139) # 46 pixels big

#IMG E60_0151 - Crows - 2
  #80%

crow.151.1 <- c(37269:37270, 37508:37511, 37751:37753, 37990:37993, 38231:38234,
                38470:38474, 38710:38714, 38952:38953)
length(crow.151.1) #29 pixels large
crow.151.2 <- c(36805:36807, 37044:37047, 37284:37288, 37521:37528, 37760:37768,
                38000:38007)
length(crow.151.2) #37 pixels large

#generate average bird size
mean(c(29, 37))

crow.151 <- c(crow.151.1, crow.151.2)

#IMG E60_0155 - juncos - 11

junco.155.1 <- c(44542, 44782) #2 pixels
junco.155.2 <- c(45730, 45969:45970) #3 pixels
junco.155.3 <- c(47408, 47647:47648, 47888) #4 pixels
junco.155.4 <- c(44762:44763, 45002:45003) #4
junco.155.5 <- c(43319, 43559) #2
junco.155.6 <- c(43323) #1
junco.155.7 <- c(41645:41646, 41885:41886) #4
junco.155.8 <- c(40922, 41162:41163) #3
junco.155.9 <- c(39962:39963, 40203) #3
junco.155.10 <- c(38757:38758, 38997:38998) #4
junco.155.11 <- c(38044:38045) #2

#mean length is 2.91
mean(c(2, 3, 4, 4, 2, 1, 4, 3, 3, 4, 2))

#combine juncos
junco.155 <- c(junco.155.11, junco.155.10, junco.155.9, junco.155.8, junco.155.7, 
              junco.155.6, junco.155.5, junco.155.4, junco.155.3, junco.155.2,
              junco.155.1)

#IMG E60_0181 - s. towhee
  #85%
towhee.181 <- c(29375:29376, 29612:29617, 29852:29859, 30092:30099, 30332:30339,
                30573:30579, 30814:30819, 31054:31058, 31295:31298)
length(towhee.181) #54 pixels

#Create a list of all the positions. It is important that they are stored in the same
  #order as the images being analyzed as they will be matched in that order in
  #the following code.
m13.pos.l <- list(junco.23, towhee.61, mallard.75, rwbb.139, crow.151, junco.155, 
               towhee.181)

#create duplicate list to fill with values
m13.birds.l <- m13.pos.l

for(i in 1:length(photo.files.m13)) {
  
  #We have it read each JPEG in the livebirds m13 folder. For each JPEG it selects
    #the positions of the corresponding (i) bird positions from the bird position 
    #object. m13.birds.l now has the signal values of every bird for every JPEG
    #in folder m13
  m13.birds.l[[i]] <- readflirJPG(paste(master.folder.live, subfolders.live[2], "/",  
                                   photo.files.m13[i], sep = ""), exiftool.p)[m13.pos.l[[i]]]
  
}


#----a9----

#There were four photos selected for a9. Pixels comprising of the bird were 
  #identified in Photoshop and located in ImageJ for each photo. 

#IMG E60_0019 - mallards - 4
  #The threshold for pixels within the model was 60%
mallard.19.1 <- c(41405:41409, 41646) #6
mallard.19.2 <- c(39725:39726) #2
mallard.19.3 <- c(35647) #1
mallard.19.4 <- c(33965:33967, 34206) #4

#Calculate the mean of the bird size
mean(c(6, 2, 1, 4))

mallard.19 <- c(mallard.19.4, mallard.19.3, mallard.19.2, mallard.19.1)

#IMG E60_0023 - mallards - 5
  #The threshold for the mallards was 70%
mallard.23.1 <- c(62318:62319, 62558:62559) #4
mallard.23.2 <- c(34958:34961, 35439:35440, 35679:35680) #8
mallard.23.3 <- c(28239:28241, 28479:28480, 28718:28720, 28958:28960) #11
mallard.23.4 <- c(26078:26079, 26318:26320, 26558:26560, 26798:26800) #11
mallard.23.5 <- c(18398, 18638:18639, 18879, 19119) #5

#get mean bird size
mean(c(4, 8, 11, 11 , 5))

mallard.23 <- c(mallard.23.1, mallard.23.2, mallard.23.3, mallard.23.4, 
                mallard.23.4, mallard.23.5)

#IMG E60_0043 - crows - 2
  #80%
crow.43.1 <- c(11606:11609, 11845:11852, 12086:12094, 12325:12334, 12566:12575,
               12809:12812, 13050:13052)
length(crow.43.1) #48 pixels big
crow.43.2 <- c(61088, 61327:61330, 61567:61570, 61808:61811, 62049:62051, 
               62289:62291, 62530)
length(crow.43.2) #20 pixels big

#Get mean bird size
mean(c(48, 20))

crow.43 <- c(crow.43.1, crow.43.2)

#IMG E60_0045 - crow - 1
  #80%
crow.45 <- c(36096:36097, 36334:36337, 36574:36578, 36814:36818, 37054:37059, 
             37295:37301, 37536:37542, 37776:37782, 38014:38022, 38255:38262,
             38495:38502, 38736:38741, 38976:38980, 39218:39218)
length(crow.45) #80 pixels

#Now that all positions for every image has been saved, we do the same thing
  #as for m13. We start by creating a list of the positions of every bird,
  #keeping the same order as the images in the folder.
a9.pos.l <- list(mallard.19, mallard.23, crow.43, crow.45)

#create duplicate list to fill with values
a9.birds.l <- a9.pos.l

#We create the same loop as above to translate positions to values
for(i in 1:length(photo.files.a9)) {
  
  #In this case we are calling the first subfolder.live(a9) and the files of
    #that folder. The rest of the code is the same. By the end a9.birds.l is
    #a list containing the signal values of each bird pixel in each image in 
    #the a9 folder in order.
  a9.birds.l[[i]] <- readflirJPG(paste(master.folder.live, subfolders.live[1], "/",  
                                        photo.files.a9[i], sep = ""), exiftool.p)[a9.pos.l[[i]]]
  
}

#====[3.Histograms]====

#In this section we load camera data and create histograms to observe uniformity 
  #of signals within the scene. We do this for each day, taking overall signal 
  #and the signal of the bird

#----[m13]----

#Start by naming the histograms that will be produced. Named by month letter
  #and date of data collection as well as IMG # as was set by f.rename.
hist.names.m13 <- paste("m13.", substr(photo.files.m13, 5, 8), sep = "")

#We also need to create an object that is the length of the number of files in
  #order to store the percentages that we will generate within the loop
m13.perc <- data.frame("name" = photo.files.m13, "perc" = rep(NA, length(photo.files.m13)))

#We also want to create an empty object to store temperory x limits for the 
  #histograms so we can have the general and bird graphs follow the same x axis
xlim.t <- c(NA, NA)

#Create a loop to go into each photo file that we want analyzed from m13
for(i in 1:length(photo.files.m13)) {
  
  #i <- 1
  
  #Generate temperory signal from each photo. Paste calls each of the photos 
  #within the folder of selected live bird photos for m13 within the clean data 
  #folder
  signal.d.t <- readflirJPG(paste(master.folder.live, subfolders.live[2], "/",  photo.files.m13[i], 
                                  sep = ""), exiftool.p)
  
  #Sort values so they go from greatest to least. This will be useful later
  #when taking the top 1000 pixels for analysis.
  signal.s <- unique(sort(signal.d.t, decreasing = TRUE))
  
  #Create x limit based on minimum and maximum signal of overall image signals.
    #The limit runs from the minimum value minus 100 and rounded to the nearest
    #hundred to the maximum value of the scene plus 100 rounded to the nearest
    #one hundred. We will save these into a temporary object.
  signal.min.t <- round(min(signal.d.t) - 100, digits = -2)
  signal.max.t <- round(max(signal.d.t) + 100, digits = -2)
  
  xlim.t <- c(signal.min.t, signal.max.t)
  
  #We then want to create bins to use for both the histogram and the bird signal.
    #It spans the range of the x limit split by the difference of those minimum
    #and maximum values used for the range divided by 20 (difining the actual 
    #amount of breaks)
  bins.t <- seq(signal.min.t, signal.max.t, 
                by = ((signal.max.t - signal.min.t)/20))

  #saving
  pdf(paste(p.ana, "histograms/live/", hist.names.m13[i], ".pdf", sep = ""), width = 5, height = 3) #can put in paste to 
  #give path; dimensions read in inches
  
  #Make histogram of the overall scene
  hist(signal.d.t, xlim = xlim.t, breaks = c(bins.t), xlab = "Scene Signal",
       main = paste("Image ", substr(hist.names.m13[i], 5, 8), sep = ""))
  
  dev.off()
  
  #We now want to call the signal of the bird in the relevent image from the 
    #m13 bird list
  bird.s <- m13.birds.l[[i]]
  
  #saving
  pdf(paste(p.ana, "histograms/live/", "bird.", hist.names.m13[i], ".pdf", sep = ""), 
      width = 5, height = 3)
  
  hist(bird.s, xlim = xlim.t, breaks = c(bins.t), xlab = "Bird Signal", main = "")
  
  dev.off()
  
  #Now we want to figure out how many of the bird signal is in the top 100 pixels
  #of the scene to quantify contrast/uniformity
  
  #take top 1000 values
  max.t <- signal.s[1:99]
  
  #Create a temporary object to store each pixel percentage point
  perc.t <- rep(NA, length(bird.s))
  
  #Nested for loop for every pixel in the selected image
  for(j in 1:length(bird.s)) {
    
    #For every pixel in the image, if it is above the 100th hottest pixel in the
      #scene save a percentage point (equal to one divided by the number of
      #pixels in the bird)
  if(bird.s[j] > signal.s[99]) {
    
   perc.t[j] <- 1/length(bird.s)
    
   #If it is not above the 100th hottest pixel save 0
  } else {
    
    perc.t[j] <- 0
    
  }
  }
  
  #Save the sum of the percantage point to get the overall percentage and save
    #it into the m13 percentage data frame
  m13.perc$perc[i] <- sum(perc.t)
  
}

#We now have - for each image of m13 - the percentage of bird pixels in the 100
  #hottest pixels as a decimal stored into the following dataframe
m13.perc

#----a9----

#We want to do the same thing for the a9 images - generate a percentage table

#names for the histograms to be generated
hist.names.a9 <- paste("a9.", substr(photo.files.a9, 5, 8), sep = "")

#Create a data frame to store the percentages in
a9.perc <- data.frame("name" = photo.files.a9, 
                      "perc" = rep(NA, length(photo.files.a9)))

for(i in 1:length(photo.files.a9)) {
  
  #For each photo generate matrix of signal values
  signal.d.t <- readflirJPG(paste(master.folder.live, subfolders.live[1], "/",  
                                  photo.files.a9[i], sep = ""), exiftool.p)
  
  #sort values of table and store into object; made unique because otherwise we
  #run into coding issues below
  signal.s <- unique(sort(signal.d.t, decreasing = TRUE))
  
  #Create x limit based on minimum and maximum signal of overall image signals.
    #The limit runs from the minimum value minus 100 and rounded to the nearest
    #hundred to the maximum value of the scene plus 100 rounded to the nearest
    #one hundred. We will save these into a temporary object.
  signal.min.t <- round(min(signal.d.t) - 100, digits = -2)
  signal.max.t <- round(max(signal.d.t) + 100, digits = -2)
  
  #Create x limit based on minimum and maximum signal of overall image signals
  xlim.t <- c(signal.min.t, signal.max.t)
  
  #Create the bin same as for m13 based on minimum and maximum signals
  bins.t <- seq(signal.min.t, signal.max.t, by = ((signal.max.t - signal.min.t)/20))
  
  #saving the histograms as a pdf
  pdf(paste(p.ana, "histograms/live/", hist.names.a9[i], ".pdf", sep = ""), 
      width = 5, height = 3)
  
  #Histogram of the scene signal
  hist(signal.d.t, xlim = xlim.t, breaks = c(bins.t), xlab = "Scene Signal",
       main = paste("Image ", substr(hist.names.a9[i], 4, 7), sep = ""))
  
  dev.off()
  
  #We also want to generate a histogram for each of the histograms. First we save
    #all the bird signals from each a9 image into a temporary object
  bird.s <- a9.birds.l[[i]]
  
  #Save histogram of the bird with the same name as the image with the prefix
    #"bird."
  
  pdf(paste(p.ana, "histograms/live/", "bird.", hist.names.a9[i], ".pdf", sep = ""), 
      width = 5, height = 3)
  
  #Histogram of bird signal using the same range and breaks as overall scene
  hist(bird.s, xlim = xlim.t, breaks = c(bins.t), main = "",
       xlab = "Bird Signal")
  
  dev.off()
  
  #Now we want to figure out how many of the bird signal is in the top 100 pixels
  #of the scene
  
  #take top 1000 values
  max.t <- signal.s[1:99]
  
  #Create a temporary object to store percentage points
  perc.t <- rep(NA, length(bird.s))
  
  #For each pixel in the selected image do the following
  for(j in 1:length(bird.s)) {
    
    #If it is above the 100th hottest pixel save a percentage point
    if(bird.s[j] > signal.s[99]) {
      
      perc.t[j] <- 1/length(bird.s)
      
      #If not, save 0
    } else {
      
      perc.t[j] <- 0
      
    }
  }
  
  #Sum the temporary percentage point object for the overall percentage of that
    #image and store it into the data frame
  a9.perc$perc[i] <- sum(perc.t)

}

#====[4.Combine.Data]====

#Now that we have the percentages for each image we want to format the table
  #for analysis. We want to first add in columns to each dataframe: group,
  #time, and date

#First we add group so we can compare the two days independent of time
m13.perc$group <- rep(1, length(m13.perc$perc))
a9.perc$group <- rep(2, length(a9.perc$perc))

#We are also interested in percentage versus time of day. Merge percentage and 
#time dataframes by image name to get the time stamp for each image.
m13.perc <- merge(m13.perc, m13.time, by = "name", all.x = TRUE)
a9.perc <- merge(a9.perc, a9.time, by = "name", all.x = TRUE)

#in order to plot time we need to convert them using as.POSIXct with the 
#collection data.
m13.perc$date <- rep(NA, nrow(m13.perc))
a9.perc$date <- rep(NA, nrow(a9.perc))


for(i in 1:nrow(m13.perc)) {
  
  m13.perc$date[i] <- as.POSIXct(paste("2019-04-09", m13.perc$time[i], sep = " "))
  
}


for(i in 1:nrow(a9.perc)) {
  
  a9.perc$date[i] <- as.POSIXct(paste("2019-04-09", a9.perc$time[i], sep = ""))
  
}

#We now want to store the csv with the percentage, group, time and date for both
  #m13 and a9
write.csv(m13.perc, paste(p.cl, "E60/data.frames/m13.d.csv", sep = ""))
write.csv(a9.perc, paste(p.cl, "E60/data.frames/a9.d.csv", sep = ""))

#====[5.Summarize.Stats]====

#We want to compare the percentage and size calculated and created manually
#as a new table in excel. We thus want to start by loading the data into
#R objects. Then it can be summarized and used for visualization below
m13.size <- read.csv(paste(p.ana, "tables/m13.table.csv", sep = "" ), 
                     stringsAsFactors = FALSE, strip.white = TRUE)
a9.size <- read.csv(paste(p.ana, "tables/a9.table.csv", sep = "" ), 
                    stringsAsFactors = FALSE, strip.white = TRUE)


#Let's summarize the percentage and get the sd
summary(m13.size$percentage)
sd(m13.size$percentage)
summary(a9.size$percentage)
sd(a9.size$percentage)

summary(m13.size$bird.size)
sd(m13.size$bird.size)
summary(a9.size$bird.size)
sd(a9.size$bird.size)

#====[6.Visualization]====

#Now we can merge the percentages in order to compare them in a graph
live.perc <- merge(m13.perc, a9.perc, all = TRUE)

#Save the graph as a pdf
pdf(paste(p.ana, "graphs/percent.live", ".pdf", sep = ""), width = 4, height = 4)

#let's visualize the two days
plot(live.perc$group, live.perc$perc, las = 1,
     ylim = range(0, 1), xlim = range(0, 3), xaxt = "no",
     main =  "Clarity Proportion by Day", xlab = "Collection Day", 
     ylab = "Proportion (decimal)")

dev.off()

#need to generate an even range between minimum and maximum temperatures
x.label <- seq(min(a9.perc$date, m13.perc$date), max(a9.perc$date, m13.perc$date), 
               by = (max(a9.perc$date, m13.perc$date) - 
                       min(a9.perc$date, m13.perc$date))/10)
y.label.pos <- seq(0, 1, by = 0.1)
y.label.name <- seq(0, 1,  by = 0.1)

#Save the graph as a pdf into the analysis folder
pdf(paste(p.ana, "graphs/percent.time.live", ".pdf", sep = ""), width = 4, height = 4)

plot(m13.perc$date, m13.perc$perc, las = 1,
     ylim = range(-0.1, 1.2), col = "red", axes = FALSE, xlim = range(a9.perc$date, m13.perc$date),
     main =  "Clarity Proportion over time", xlab = "Time of day", 
     ylab = "Proportion")
points(a9.perc$date, a9.perc$perc, col = "blue")
axis(1, at = c(x.label), labels = c(a9.perc$time,m13.perc$time))
axis(2, at = y.label.pos, labels = y.label.name)

dev.off()

#Save the graph of percent by size
pdf(paste(p.ana, "graphs/percent.size.live", ".pdf", sep = ""), width = 4, height = 4)

#Now we can plot the size by the percentage
plot(m13.size$bird.size, m13.size$percentage, las = 1, col = "red", 
     xlim = range(1, 82), ylim = range(0,1), main =  "Clarity Proportion by Bird Size", 
     xlab = "Bird Size (pixels)", ylab = "Proportion")

#add the second day points
points(a9.size$bird.size, a9.size$percentage, las = 1, col = "blue")

dev.off()





