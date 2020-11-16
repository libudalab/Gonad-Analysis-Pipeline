######################################################
### Gonad Linearization Algorithm R Implementation ###
######################################################

### Packages ###
library(tidyverse)
###

### Import Data ###

## Set working directory to retrieve .csv files
# Type the full filepath to the .csv files for your data below, with each folder separated by forward slashes /
# The files exported by Imaris are usually contained within individual folders for each cropped gonad section analyzed
# Your 'home directory' (the variable home_dir) is the folder in which these individual folders are organized
home_dir <- ('Enter your home directory file path here') 

## Identify directories containing data
# This code reads the subfolders within your home directory
setwd(home_dir)
dirs <- list.files(home_dir) %>%
  .[grep(pattern=c(".R"),.,invert=TRUE)] %>%
  .[grep(pattern=c(".csv"),.,invert=TRUE)]


## Read in 'fdata' ("F"oci per nucleus data) and 'pdata' (xy nuclei "P"osition data)
# This code goes into each subfolder of within your home directory and retrieves the specific information required for spots per nucleus analysis
# It is important that the exported csv demarcating the positions of the central line drawn through your gonad be organized in a folder which is alphabetically LAST in the home directory
for (i in 1:(length(dirs)-1)) {
  setwd(paste(as.character(home_dir),'/',as.character(dirs[i]),'/', sep=""))
  files <- list.files()
  
  fdat_temp <- read_csv(grep(pattern='Overall',x=files, value=TRUE),skip=3) %>%
    filter(Variable == "Total Number of Spots") %>%
    filter(str_detect(pattern="close to",string = .$'Surpass Object') == TRUE) %>%
    select(Variable, Value, 'Surpass Object')
  
  pdat_temp <- read_csv(grep(pattern='Position',x=files, value=TRUE),skip=3) %>%
    filter(Category == "Surface") %>%
    select('Position X','Position Y','Surpass Object')
  
  if (i == 1) {
    fdata = fdat_temp
    pdata = pdat_temp
  } else {
    fdata = bind_rows(fdata,fdat_temp)
    pdata = bind_rows(pdata,pdat_temp)
  }
}

## Read in ldata (coordinates of central gonad axis "L"ine)
setwd(paste(as.character(home_dir),'/',as.character(dirs[length(dirs)]),'/', sep=""))
files <- list.files()
ldata <- read_csv(grep(pattern='Position',x=files, value=TRUE),skip=3) %>%
  select('Position X','Position Y','Name')

### Merge fdata and pdata ###
for (a in 1:nrow(pdata)) {
  nucleus <- pdata$'Surpass Object'[a] #retrieve name of nucleus (surface)
  nucleus <- str_replace(nucleus,"\\[",".") #Replace problematic bracket with flexible "."

  foci <- fdata %>% 
    filter(str_detect(.$'Surpass Object',nucleus) == TRUE) %>%
    .$'Value' #Pull number of spots associated with that nucleus
  
  if (length(foci) == 0) {foci <- 0} #If surface isn't listed in fdata, its focus count is 0
  
  if (a == 1) {
    data <- pdata %>% filter(str_detect(.$'Surpass Object',nucleus) == TRUE) %>% mutate(spots=foci) #create new df with all important info
  } else {
    temp <- pdata %>% filter(str_detect(.$'Surpass Object',nucleus) == TRUE) %>% mutate(spots=foci)
    data <- bind_rows(data,temp) #assemble complete dataframe
  }
  if (a == nrow(pdata)) {
    data <- data %>% mutate (X = .$'Position X',
                             Y = .$'Position Y',
                             SurpassObject = .$'Surpass Object') %>%
      select(X,Y,SurpassObject,spots)
  }
}

### Quality Check 1: Plot of individual nucleus position and associated focus counts ###
# The below code renders a plot of the positons of each nucleus based on their xy positions, as well as the number of foci associated with each nucleus
# If you notice that there are nuclei missing, or the numbers of foci are incorrect, this means that there are problems with your data being recognized and imported
data %>%
  ggplot() +
  geom_point(aes(X,Y,color=spots),size=8) +
  geom_text(aes(X,Y,label=spots),color="white")+
  scale_color_continuous(low="black",high="green")+
  theme_classic()

### Draw line through center of gonad ###
for (b in 1:(nrow(ldata)-1)) {
  if (b == 1) {
    cline <- tribble(
      ~'x1',~'y1',~'x2',~'y2',~'segnum',
      pull(ldata[b,'Position X']),pull(ldata[b,'Position Y']),pull(ldata[b+1,'Position X']),pull(ldata[b+1,'Position Y']),b
    )
  } else {
    temp <- tribble(
      ~'x1',~'y1',~'x2',~'y2',~'segnum',
      pull(ldata[b,'Position X']),pull(ldata[b,'Position Y']),pull(ldata[b+1,'Position X']),pull(ldata[b+1,'Position Y']),b
    )
    cline <- bind_rows(cline,temp)
  }
  if (b == (nrow(ldata)-1)) {
    cline <- cline %>% mutate(
      A = (y2-y1)/(x2-x1),
      B = -1,
      C = y1-A*x1,
      length = sqrt((x2-x1)^2 + (y2-y1)^2)
    ) #The above if statement calculates the standard equation of the line Ax - By + C = 0
  }
}

### Quality Check 2: Plot of individual nucleus position and focus count with central axis line ###
# The code below renders a plot of the individual nuclei in your datastet, with the central gonad line drawn on.
# Check this plot to ensure all datapoints and line segments you expect to see have been properly imported
data %>%
  ggplot() +
  geom_point(aes(X,Y,color=spots),size=8) +
  geom_text(aes(X,Y,label=spots),color="white")+
  scale_color_continuous(low="black",high="green")+
  geom_segment(data=cline, aes(x=x1,y=y1,xend=x2,yend=y2),size=2,color="turquoise")+
  theme_classic()

### Calculate the perpendicular intersection point of each nucleus with each central segment ###
# If you change s_param or manually assign nuclei positions in the code below, you MUST rerun this code block in order for your changes to be implemented
for (c in 1:nrow(cline)) {
  temp <- data %>%
    mutate(m = -1/pull(cline[c,'A']),
           b = Y - (m * X),
           newX = (b - pull(cline[c,'C'])) / (pull(cline[c,'A']) - m), # X coordinate of perpendicular intersection
           newY = m * newX + b, # Y coordinate of perpendicular intersection
           displacement = sqrt((X-newX)^2 + (Y-newY)^2), # total distance between original XY coordinates and perpendicular intersection point
           distance1 = sqrt( (pull(cline[c,'x1']) - newX)^2 + (pull(cline[c,'y1']) - newY)^2 ), # distance from start of segment to perp. intersection point
           distance2 = sqrt( (pull(cline[c,'x2']) - newX)^2 + (pull(cline[c,'y2']) - newY)^2 ), # distance from end of segment to perp. intersection point
           delta_distance1 = (distance1-pull(cline[c,'length']))/pull(cline[c,'length']), 
           delta_distance2 = (distance2-pull(cline[c,'length']))/pull(cline[c,'length']),
           segment = c)
  if (c==1) {
    tdata <- temp 
  } else {
    tdata <- bind_rows(tdata,temp)
  }
}

### Determine which segment intersection best fits each nucleus ("T"ransformed nucleus position data) ###
# Set your alignment stringency paramter ('s_param') below
# The default value of s_param is 0. If you find that nuclei are being grossly misaligned at central line segment points, increase the value of s_param
s_param <- 0.00
# If you want to MANUALLY assign the segment assignment of a particular nucleus, delete the #s in the code below and type the specific SurpassObject name in the code at the designated points
tdata <- tdata %>%
  filter(delta_distance1 <= s_param & delta_distance2 <= s_param) %>%
  #filter(SurpassObject !="SurpassObject Name") %>%
  #bind_rows(.,tdata %>%
              #filter("SurpassObject Name" & segment == "Desired Segment Assignment")) %>%
  group_by(SurpassObject) %>%
  filter(displacement == min(displacement)) %>%
  ungroup()

### Quality Check 3: Plot Nucleus translation to center line segments ###
# Check for the quality of the algorithm's determination of which central line segment each nucleus is best approximated by
# If you find multiple errors, consider altering the value of s_param in the code above
# If one particular nucleus, or a subset of nuclei, are continuously misaligned, you may manually align them to a particular line segment in the above block of code
tdata %>%
  ggplot() +
  geom_segment(data=cline, aes(x=x1,y=y1,xend=x2,yend=y2,color=as.factor(segnum)),size=2,alpha = 0.5) + 
  geom_point(aes(X,Y,color=as.factor(segment)),size=3) +
  geom_segment(aes(x=X,y=Y,xend=newX,yend=newY,color=as.factor(segment)),arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  #geom_text(aes(x=X, y=Y,label=SurpassObject),size=2) +
  theme_classic()

### Combine line segements into single linear axis###
for (c in 1:nrow(cline)) {
  if (c == 1) {
    temp <- tdata %>% mutate(tX = 0)
    tdata <- temp %>% filter(segment == c) %>% mutate(tX = distance1)
    temp <- temp %>% filter(segment != c)
    compound <- 0} else {
    compound <- compound + pull(cline[c-1,'length'])
    tdata <- bind_rows(tdata,(temp %>% filter(segment == c) %>% mutate(tX = distance1 + compound)))
    temp <- temp %>% filter(segment != c)
    }
  if (c == nrow(cline)) {tdata <- tdata %>% mutate(ntX = tX / sum(cline[,'length']))}
}

### Quality Check 4: Plot final transformed data output ### 
tdata %>%
  ggplot() +
  geom_point(aes(ntX,spots,color=as.factor(segment))) + 
  theme_classic() +
  xlab('Relative Gonad Position') +
  ylab('Foci per Nucleus')

### Output Data as CSV for future analysis! ###
setwd(home_dir)
write.csv(tdata, file="YOUR FILE NAME.csv")
