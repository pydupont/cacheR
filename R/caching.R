library(assertthat)

#' Save an object (cache)
#'
#' @param o object to save
#' @return nothing
#' @export
#' @examples
#' save_object(iris)
save_object <- function(o){
  cache.dir <- ".cache"
  dir.create(cache.dir, showWarnings = FALSE) #file.path(mainDir, subDir)
  name <- deparse(substitute(o))
  write(paste("Saving object ", name, sep=""), stderr())
  saveRDS(o, file.path(cache.dir, paste(name, ".rds", sep="")))
  write(paste("Object ", name, " saved", sep=""), stderr())
}

#' Load an object (cache)
#'
#' @param name name of the object to load
#' @param global load in the global environment (if TRUE) or as an object (if FALSE, default)
#' @return object or nothing
#' @export
#' @examples
#' load_object("iris", global=T)
#' iris2 <- load_object("iris")
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

#' Check if an object is cached
#'
#' @param name name of the object to search
#' @return bool TRUE if object is cached, else FALSE
#' @export
#' @examples
#' file.cached("iris")
file.cached <- function(name){
  cache.dir <- ".cache"
  if(!is.string(name)){return(F)}
  if(file.exists(file.path(cache.dir, paste(name, ".rds", sep="")))){return(T)}
  return(F)
}

#' For assertthat library. What to do if assert_that(file.cached("iris")) fails
on_failure(file.cached) <- function(call, env) {
  "File is not cached"
}

#' Return a list of the cached files
#'
#' @return vector of the names of the cached objects (as strings)
#' @export
#' @examples
#' get.file.cached()
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

#' Loads all cached objects in the global environment. Used to restore an envirnoment quickly
#'
#' @return nothing
#' @export
#' @examples
#' load.all.cached.files()
load.all.cached.files <- function(){
  for(x in get.file.cached()){
    load_object(x, global=T)
  }
}
