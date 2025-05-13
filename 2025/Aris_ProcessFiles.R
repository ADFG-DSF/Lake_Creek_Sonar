## Read in functions and creation of objects

#The following chunk of code was provided by Carl Pfisterer and creates three functions that, together, read in ARIS .txt files

library(data.table)
############################################################
#Takes a duration string in the format H:M:S and returns duration in minutes
############################################################
DurationToNumeric = function(value){
  h = as.numeric(unlist(strsplit(value,":"))[1])
  m = as.numeric(unlist(strsplit(value,":"))[2])
  s = as.numeric(unlist(strsplit(value,":"))[3])
  d = 60*h+m+s/60
  d
}

############################################################
#Reads a single file and returns the data in a data frame.
############################################################

ReadFile = function(file){
  cat(file,"\n")
  header = readLines(file,19)
  n = as.numeric(unlist(strsplit(header[1]," "))[15])   #Extract the number of rows
  duration = unlist(strsplit(header[9]," "))[7]   #Extract the file duration
  if(n > 0){    #If there are fish in the table read it.
    data = fread(file=file,header = FALSE,sep=" ",skip=25,nrows=n)  #Must only read n rows because the file extends beyond the table of data
    data = data[,-c(13:22,27:28)]   #Remove unneeded columns
    names(data) = c("File","Total","Frame","Dir","Range","Theta","Length","dR","L/dR",
                    "Aspect","Time","Date","Pan","Tilt","Roll","Species","Q","N")
    data$Date = as.Date(data$Date)
    data$filename = file
    data$Duration = DurationToNumeric(duration)
  }
  else{         #Otherwise create a record with null values except for the file name and duration
    data = data.table(data.frame(File=NA,Total=NA,Frame=NA,Dir=NA,R=NA,Theta=NA,L=NA,dR=NA,
                                 LdR=NA,Aspect=NA,Time=NA,Date=as.Date('1970-01-01'),Pan=NA,Tilt=NA,Roll=NA,
                                 Species=NA,Q=NA,N=NA,filename=file,Duration=DurationToNumeric(duration)))
    setnames(data,c("Frame","R","L","dR","LdR"),c("Frame","Range","Length","dR","L/dR"))
  }
  data
}


############################################################
#  Reads all the files in the passed directory and appends
#  them into one data frame.
############################################################


ProcessFiles = function(dir){
  cat(paste("\nProcessing files in directory:",dir,"\n"));
  
  files = list.files(dir,recursive = TRUE, pattern=".txt$",full.names=TRUE);  #Only read files ending in .txt
  if(length(files) > 0){
    progress = txtProgressBar(style=3,min=0,max=length(files),initial=0);
    cnt = 0;
    for(file in files){
      Data = ReadFile(file);
      if(cnt==0){
        AllData = list(Data);
      }
      else{
        AllData = append(AllData,list(Data));   #Append to list of data
      }
      cnt = cnt+1;
      if((cnt%%20) == 0) setTxtProgressBar(progress,value=cnt); #Update every 20 files
    }
    setTxtProgressBar(progress,value=max(length(files)))
    AllData = setDF(rbindlist(AllData))          #combines all the tables into one large table.  data.table library must be present
  }
  else{
    AllData = NA;         #Return NA if there are no files in the directory
  }
  
  AllData
}
