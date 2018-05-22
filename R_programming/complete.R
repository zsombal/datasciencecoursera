complete <- function(directory="specdata", id=1:332) {
  y=c() #data.frame(NULL, row.names = {})
  jj=1
  for (ii in id) {
    fold_name=paste(directory,'/',sprintf("%03d", id[jj]),'.csv',sep = '')
    x=read.csv(fold_name)
    #print(x)
    xs=x['sulfate']
    xn=x['nitrate']
    num=sum((!is.na(xs))&(!is.na(xn)))
    #num=sum(!is.na(x))
    y=c(y,num)
    jj=jj+1
  }
  print(y)
  data.frame('id'=id, 'nobs'=y)
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))