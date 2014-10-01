AHA_Main = function (){
  # Packages
  install.packages(c("xlsxjars", "xlsx"))
  install.packages("plyr")
  install.packages("Rserve"); 
  install.packages("shiny");
  install.packages("XML");
  install.packages("data.table");
  
  # Activate some scripts that might be usefull
  require("xlsx")
  require("plyr")
  require("shiny")
  require("data.table")
  
  # Set directory
  sapply(list.files(pattern="[.]R$", path=getwd(), full.names=TRUE), source);
  if (Sys.info()["nodename"] =="L-AW23JB") outputfolder = "N:/Multivariate Analyse/Asset Health Analytics"
  if (Sys.info()["nodename"] =="NLAMS4043734X") outputfolder = "~"

  
  
  # Run required functions
  cat("\n Asset health analytics loaded\n\n")
  return()
}