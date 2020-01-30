
Run `main.R` script first, including downloading the Thermpackage image.

Images are in seperate zip files downloadable at the following link: 
https://drive.google.com/drive/folders/19qrdl6-SCVkSUNOjf9B_mc-52JoQceQr?usp=sharing
Once downloaded add the uncompressed folders of the following days to the path `raw.data/E60/models/`:
`m21.in`
`jn17.fsa`
`d10.in`
Add the folders of the rest of the days to the path `raw.data/E60/livebirds`:
`ap9.est`
`m13.est`
Now that it has the data in the appropriate folders the code should run reading the unzipped images within each of their day files.

Legend for folder names to dates and notes on the images themselves can be found in `raw.guide.xlsx`. 
  
R scripts reading image data and compiling it into a table can be found in the folder `clean.data`. The `models` script is relevent for our results and analysis.

The final piece of coding can be found in the `analysis` folder. This includes statistical summaries and data visualization.
  

