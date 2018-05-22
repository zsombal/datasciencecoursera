corr <- function(directory="specdata", threshold = 0, id=1:332) {
  yy=c()
  jj=1
  for (ii in id) {
    fold_name=paste(directory,'/',sprintf("%03d", id[jj]),'.csv',sep = '')
    x=read.csv(fold_name)
    xs=x['sulfate']
    xn=x['nitrate']
    yys=xs[(!is.na(xs))&(!is.na(xn))]
    yyn=xn[(!is.na(xs))&(!is.na(xn))]
    if (length(yys)>threshold) {
      yy=c(yy,cor(yys, yyn))
    }
    jj=jj+1
  }
  
  if (is.null(yy)){
    0
  }
  else{
    yy
  }

}

corr(directory="specdata", threshold = 1200, id=1:23)
