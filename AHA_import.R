AHA_Import= function(folder,dataname,headername,mode){
  
  # Asset health analytics import script, 
  # (c) Roel Stijl (Bearingpoint), Jacco Heres (Alliander), 2014
  
  # This file imports select collumns from raw files and modifies the headers based on an xlsx file
  # The xlsx file is <filename>_header.xlsx, and is automatically created importing a CSV
  # Default for the xlsx file is all collumns and default headers, can be modified using shiny GUI or excel
  # The functions operates in several modes these can be be shiny, save or load
  # - shiny provides a GUI for editing the header files
  # - load provides a mode to load a file to workspace (e.g. output = AHA_impot(a,b,c))
  # - save will save the data to file <filename>.Rda
  # Multiple files is supported, initial input dataname is a partial search
  
#   sapply(list.files(pattern="[.]R$", path=getwd(), full.names=TRUE), source);
  options(stringsAsFactors = FALSE)
  source("AHA_inspect_raw_data.R")
  
  # Activate some scripts that might be usefull
  require("xlsx")
  require("plyr")
  require("shiny")
  require("data.table")
  library("pracma")
  
  # Define the location of your data based on the system used
  if (Sys.info()["nodename"] =="L-AW23JB") {
    Sourcelocation = "N:/Multivariate Analyse/Asset Health Analytics/0. Ongebruikte en brondata"}
  else if (Sys.info()["nodename"] =="NLAMS4043734X") {
    Sourcelocation= "I:/2. Datasets/1. Alliander/3. Asset health analytics pipeline/0. Ongebruikte en brondata"}
  else{
    Sourcelocation= "0. Ongebruikte en brondata"}
    
  datafolder    = "1. Ruwe datasets";  
  setfolder     = list.files(Sourcelocation,pattern=folder)[1]; dir.create(paste0("1. Ruwe datasets/",setfolder), showWarnings = FALSE)
  datafiles     = list.files(paste0(Sourcelocation,"/",setfolder),pattern=paste0(dataname,".{0,}\\.csv|",dataname,".{0,}\\.ssv"))
  headerfile    = paste0(headername,"_headers.xlsx");
  shinyfolder   = "x. Shiny"
  
  # Import data and rename cols
  for (filenumber in 1:length(datafiles))
  {
  curdataname = substring(datafiles[filenumber],1,nchar(datafiles[filenumber])-4);
  curdataext  = substring(datafiles[filenumber],nchar(datafiles[filenumber])-2,nchar(datafiles[filenumber]));
  cat(paste0("Start data-import of file : ",datafiles[filenumber]),".\n" ); tic()
  
  # Choose the correct import method
  switch (folder,
          NOR={
            if(!grepl("ELCVERBINDINGEN_14", curdataname) & any(lapply(c("ELCVERBINDINGEN","ELCVERBINDINGSDELEN_1106","ELCVERBINDINGSDELEN_0708"),grepl,curdataname))){
              dataset = data.frame(read.csv(paste0(Sourcelocation,"/",setfolder,"/",datafiles[filenumber]),row.names=NULL));names(dataset)[1:(length(names(dataset))-1)]= names(dataset)[2:length(names(dataset))]; names(dataset)[(length(names(dataset)))]="DUPLICATE"
            } else if (curdataext == "csv"){
              dataset =data.frame( fread(paste0(Sourcelocation,"/",setfolder,"/",datafiles[filenumber]),sep=",",header=TRUE))
            } else if (curdataext == "ssv"){
              dataset =data.frame( fread(paste0(Sourcelocation,"/",setfolder,"/",datafiles[filenumber]),sep=";",header=TRUE))
            }
          },
          KLAK={
            dataset  = data.frame(read.csv(paste0(Sourcelocation, "/", setfolder,"/",datafiles[filenumber])))
          },
          #Else
            {dataset  = data.frame(fread(paste0(Sourcelocation, "/", setfolder,"/",datafiles[filenumber])))}
  )
  
  # Convert header into the same format as the xlsx file
  toc(); cat("converting header. \n"); tic();
  header       = data.frame(matrix(0,length(colnames(dataset)),4))
  header[,1]   = data.frame(colnames(dataset))
  header[,2]   = (colnames(dataset))
  header[,3]   = matrix(0,length(colnames(dataset)))
  header[,4]   = matrix("comment",length(colnames(dataset)))
  colnames(header) = c(curdataname,"Original name","Meenemen","Notities")
  
  # Load the xlsx file or create it if non existing
  if (file.exists(paste0(datafolder, "/", setfolder,"/",headerfile))) {
    savedheader   = read.xlsx(paste0(datafolder, "/", setfolder,"/",headerfile),1, as.data.frame=TRUE)
  }else{ 
    cat("Creating new header file.\n")
    header[,3]   = matrix(1,length(colnames(dataset)))
    write.xlsx(header,file=paste0(datafolder, "/", setfolder,"/",headerfile),row.names=FALSE)
    savedheader  = header
  }
  
  # Check if the headers saved and actual are equal and correct where nog
  pat    = pmatch(header[,2], savedheader[,2], dup = TRUE,nomatch=0)
  header[pat>0,] = savedheader[pat[pat>0],]
  setnames(dataset,colnames(dataset), t(header[1]))
  
  # Choose what output to generate (last element)
  if(mode=="shiny"){
    AHA_inspect_raw_data(dataset,header,headerfile); cat("Copy file\n"); file.copy(paste0(shinyfolder,"/",headerfile),paste0(datafolder, "/", setfolder,"/",headerfile),overwrite=TRUE);      
    cat("Done\n") ;    return()} 
  
  else if(mode=="load") {
    cat("Done\n");    return(dataset[,header[header[,3]==1,1]])
    
  } else if(mode=="save") {
    toc();cat("Saving to file\n");tic()
    mindataset = dataset[,header[header[,3]==1,1]]
    elements = length(unique(mindataset$ID_Bron));
    uniqueelement = lapply(dataset[[1]],function(x) length(unique(x)));
    
    save(mindataset,elements,uniqueelement,file=paste0(datafolder, "/", setfolder,"/",curdataname,".Rda"))
    cat(paste0("Saved: ",paste0(datafolder, "/", setfolder,"/",curdataname,".Rda"),"\n"));toc();
  } else {
    cat("Wrong mode selected, load, save or shiny")
  }  
  }
}