library(assertthat)

save_object <- function(o){
  cache.dir <- ".cache"
  dir.create(cache.dir, showWarnings = FALSE) #file.path(mainDir, subDir)
  name <- deparse(substitute(o))
  write(paste("Saving object ", name, sep=""), stderr())
  saveRDS(o, file.path(cache.dir, paste(name, ".rds", sep="")))
  write(paste("Object ", name, " saved", sep=""), stderr())
}

load_object <- function(name, global=F){
  if (is.null(global)){global=F}
  cache.dir <- ".cache"
  assert_that(is.string(name))
  assert_that(file.cached(name))
  assert_that(dir.exists(cache.dir))
  write(paste("Loading object ", name, sep=""), stderr())
  if(global){
    assign(name, readRDS(file.path(cache.dir, paste(name, ".rds", sep=""))), envir=globalenv())
    write(paste("Object ", name, " loaded", sep=""), stderr())
  }
  else{
    o <- readRDS(file.path(cache.dir, paste(name, ".rds", sep="")))
    write(paste("Object ", name, " loaded", sep=""), stderr())
    return(o)
  }
}

file.cached <- function(name){
  cache.dir <- ".cache"
  if(!is.string(name)){return(F)}
  if(file.exists(file.path(cache.dir, paste(name, ".rds", sep="")))){return(T)}
  return(F)
}

on_failure(file.cached) <- function(call, env) {
  "File is not cached"
}

get.file.cached <- function(){
  cache.dir <- ".cache"
  assert_that(dir.exists(cache.dir))
  files <- list.files(cache.dir)
  objects <- c()
  for(f in files){
    objects <- c(objects, sub("^(.*)\\.rds", "\\1", basename(f)))
  }
  return(objects)
}

load.all.cached.files <- function(){
  for(x in get.file.cached()){
    load_object(x, global=T)
  }
}
