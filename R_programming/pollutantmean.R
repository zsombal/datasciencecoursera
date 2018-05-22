pollutantmean <- function(directory="specdata", pollutant="sulfate", id=1:332) {
  yy=c()
  print(id)
  jj=1
  for (ii in id) {
    fold_name=paste(directory,'/',sprintf("%03d", id[jj]),'.csv',sep = '')
    x=read.csv(fold_name)
    x=x[pollutant]
    yy=c(yy,x[!is.na(x)])
    jj=jj+1
  }
  mean(yy)
}

# <- pollutantmean("specdata", "sulfate", 1:4)
pollutantmean("specdata", "nitrate", 70:72)
