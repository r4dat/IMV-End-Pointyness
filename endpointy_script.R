library(readxl)
library(tidyverse)
library(plyr)

cbind.all <- function (...) 
{
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - nrow(x), ncol(x)))))
}


func_endedness <- function(df,columns){
  keepcols = c("MRID","RA1PRAGE","RA1PRSEX")
  tmp = data.frame()
  ncols = length(columns)
  count_na <- function(x) ncols-sum(is.na(x))
  
  for (cl in columns) {
    tmpdf = df[,cl]
    mincol = min(tmpdf,na.rm=TRUE)
    maxcol = max(tmpdf,na.rm=TRUE)
    medcol = median(seq(mincol,maxcol,by=1)) 
    maxdist = round(maxcol-medcol)
    
    tmpdf["lft"]=tmpdf[,cl]-mincol
    tmpdf["rht"]=maxcol-tmpdf[,cl]
    
    cname = paste(cl,"_point",sep="")
    tmpdf = transform(tmpdf,min=pmin(lft,rht))
    
    colnames(tmpdf)[4]=cname
    
    if (ncol(tmp)==0){
      tmp = cbind.all(tmp,tmpdf[,cname])
    }
    else{
      tmp = cbind.all(tmp,tmpdf[,cname])
    }
  }
  
  colnames(tmp) = paste(columns,"_point",sep="")
  colnm_string = paste(columns,"_point",sep="")
  tmp = as.data.frame(tmp)
  #sum, cOUNT, Dist, Total Pos (cnt*dist), endpointy
  tmp["Sum"] = rowSums(tmp,na.rm = TRUE)
  # Sets NA + NA + NA to 0 so fix
  tmp[rowSums(is.na(tmp[colnm_string]))==ncols,"Sum"]=NA
  
  tmp["Count"] = apply(tmp[,colnm_string],1,count_na)
  tmp["Distance"] = maxdist
  tmp["TotalPossible"] = maxdist*tmp["Count"]
  tmp["End_Pointyness"] = 1-(tmp["Sum"]/tmp["TotalPossible"])
  
  tmp = cbind(df[keepcols],tmp)
  return(tmp)
}

# Read in File
MIDUS_Response_Bias <- read_excel("~/IMV-End-Pointyness/MIDUS_Response Bias.xlsx",sheet = "FullData")

# Assume all -1, 8, and "." are NAs.
MIDUS_Response_Bias[MIDUS_Response_Bias==-1] = NA
MIDUS_Response_Bias[MIDUS_Response_Bias==8] = NA
MIDUS_Response_Bias[MIDUS_Response_Bias=="."] = NA

# The columns of interest (all questions in scale)
lotcols = c("RA1SF10A","RA1SF10B","RA1SF10C","RA1SF10D","RA1SF10E","RA1SF10F")
# Call the function, tell it where the feedback goes
LOT = func_endedness(MIDUS_Response_Bias,lotcols)

mcscols = c("RA1SF4A","RA1SF4B","RA1SF4C","RA1SF4D","RA1SF4E","RA1SF4F","RA1SF4G","RA1SF4H","RA1SF4I","RA1SF4J","RA1SF4K","RA1SF4L")
MCS = func_endedness(MIDUS_Response_Bias,mcscols)

panascols = c("RA1SA20H","RA1SA20I","RA1SA20J","RA1SA20K","RA1SA20L","RA1SA22I","RA1SA22J","RA1SA22K","RA1SA22L")
PANAS = func_endedness(MIDUS_Response_Bias,panascols)

onegcols = c("RA1SA20A","RA1SA20B",	"RA1SA20C",	"RA1SA20D",	"RA1SA20E",	"RA1SA20F")
othernega = func_endedness(MIDUS_Response_Bias,onegcols)


write.csv(x=PANAS,"C:/panas.csv",row.names = FALSE)