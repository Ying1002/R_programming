cc<- function(expr,cachedir=".cache", srcfile=Null,...){
    if(!identical(globalenv(),parent.frame()))
      stop(""cc" must be called from the global environment")
    subexpr<-substitute(expr)
    exprtext<- deparse(subexpr,width.cutoff = 60)
  
    if(is.null(srcfile))
      srcfile<- file.path(tempdir(),hash(exprtext))
    writelines(exprtext,srcfile)
    cacher(srcfile, cachefir,...)
}

createLogFile<-function(cachedir,logfile,srcfile){
    if(is.null(logfile)) {
      logfile<- file.path(logdir(cachedir),
                          paste(basename(srcfile),"log",sep="."))
      file.create(logfile)
  }
  
    else if(is.na(logfile))
      logfile<-stderr()
  
    else
      file.create(logfile)
    setConfig("logfile",logfile)
}

identicalFiles<-function(x,y){
    ## Are the contents of two files the same?
    checksum<-c(hashFile(x),hashFile(y))
    identical(checksum[1],checksum[2])
}