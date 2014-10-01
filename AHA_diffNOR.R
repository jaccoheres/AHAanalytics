AHA_diffNOR = function(NORtable, source)
  # Used to derive the monthly change version of the NOR using first month as a basis
{
  library(compare)
  library(pracma)
  # source = "backup"
  # NORtable = "ELCVERBINDINGEN"
  datafolder    = "1. Ruwe datasets/6. NOR";
  
  par(mfrow=c(2,1))
  
  files = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder,full.names=TRUE)
  filesshort = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder)
  files=files[!grepl("masterset_backup",filesshort)]
  filesshort=filesshort[!grepl("masterset_backup",filesshort)]
  comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN="ID_Bron",ELCVERBINDINGSDELEN=c("ID_Verbinding","Lengte"),
                        ELCVERBINDINGEN=c("ID_Verbinding","Lengte"),cat("Please added headers to compate\n\n"))
  
  dataset = list()
  plot(file.info(files)$size)
  
  cat("Loading master set\n");tic()
  switch(source,
         # Create the master dataset from the first file
         file={
           load(files[1])
           info = data.frame()  
           mindataset$file = 1; mindataset$DateAdded = "0701"; mindataset$DateRemoved = ""
           masterset = mindataset[!duplicated(mindataset[c(comparecols,"file")]),]
           firstfile = 2
         },
         
         # Or load from a backup
         backup ={
           backups = list.files(pattern=paste0("masterset_backup",".*\\.Rda"), path=datafolder,full.names=TRUE);
           print(backups)
           filenumber <- readline(prompt="Select a backup file: ")
           load(paste0(backups[as.numeric(filenumber)]))
           print(filesshort)
           firstfile <- readline(prompt= "Continue from what file?: "); firstfile = as.numeric(firstfile)
         });toc
  
  # Calculate the difference between the master dataset and each file
  
  par(mfrow=c(2,1))
  for (n in firstfile:length(files))
  {
    tic()
    cat(paste0("Starting import of dataset: ",filesshort[n],"\n"));tic()
    load(files[n])  ;toc()
    
    cat("Comparing sets\n");tic()
    mindataset$file = n
    mindataset$DateAdded = gsub("[^0-9]","",filesshort[n])
    mindataset$DateRemoved = ""
    pm = pmatch(colnames(masterset),colnames(mindataset), dup = TRUE,nomatch=0)
    if (sum(grepl(0,pm))>0) mindataset[,colnames(masterset)[pm == 0]] = rep(rownames(mindataset),2)
    
    mindataset = mindataset[,colnames(masterset)]
    
    if(class(mindataset$Lengte)=="character")   {cat("Correcting numbers \n"); mindataset$Lengte=as.integer(lapply(mindataset$Lengte,fixnumber))}
    
    unique1 = !duplicated(masterset[c(comparecols,"file")]); u1 = sum(unique1);
    unique2 = !duplicated(mindataset[c(comparecols,"file")]); u2 = sum(unique2);
    
    cat("Starting comparison of sets\n")
    if(class(mindataset$Lengte)=="integer" & class(masterset$Lengte)== "numeric") {
      # Correct for when a file is integer but the set is numeric
      temp = masterset[unique1,c(comparecols,"file")]
      temp$Lengte = as.integer(temp$Lengte)
      combinedset = rbind(temp[unique1,c(comparecols,"file")],mindataset[unique2,c(comparecols,"file")])
    } else{
      combinedset = rbind(masterset[unique1,c(comparecols,"file")],mindataset[unique2,c(comparecols,"file")])
    }
    
    datadups = cbind(combinedset,duplicated(combinedset[,comparecols])|duplicated(combinedset[,comparecols], fromLast = TRUE))
    colnames(datadups)[length(datadups)]="dup";
    
    #     datadups = cbind(combinedset, dup=dupsBetweenGroups(combinedset,"file"))
    
    masterset = rbind(masterset,mindataset[unique2,][!datadups$dup[(u1+1):(u1+u2)],])
    masterset[1:u1,][!datadups$dup[1:u1]&masterset[1:u1,"DateRemoved"]=="","DateRemoved"]= gsub("[^0-9]","",filesshort[n])
    
    if (n%%6 == 0) {cat("Saving backup\n"); save(masterset,file=paste0(datafolder,"/masterset_backup_",filesshort[n]))
    
    toc();cat("Plotting\n")
    barplot(rbind(table(masterset[,"DateRemoved"])[2:n],table(masterset[,"DateAdded"])[2:n]),beside=TRUE);  
    legend("topleft",c("DateRemoved","DateAdded"))
    toc()}
    
  }
  
  cat("Saving backup\n"); save(masterset,file=paste0(datafolder,"/masterset_",filesshort[n]))  
  
}

fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); cor=ifelse(nchar(tail(val,1))==2,100,1000)
    if(len==1) {a=val[1]
    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {
      a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }
  }
  else{
    a=NA
  }
  #cat(paste0(a,", "))
  return(as.integer(a))
}

dupsBetweenGroups <- function (df, idcol) {
  
  datacols <- setdiff(names(df), idcol)
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  dupWithin <- duplicated(df)
  
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  goodIdx <- !is.na(dupBetween)
  goodVals <- c(NA, dupBetween[goodIdx])
  fillIdx <- cumsum(goodIdx)+1
  dupBetween <- goodVals[fillIdx]
  dupBetween[sortorder] <- dupBetween
  
  return(dupBetween)
}
