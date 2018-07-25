# library(parallel)
library(assertthat)
library(digest)
# library(foreach)
# # library(doSNOW)
# library(doParallel)


#' Save an object (cache)
#'
#' @param o object to save
#' @return nothing
#' @export
#' @examples
#' save.object(iris)
save.object <- function(o, name = NULL){
  cache.dir <- ".cache"
  dir.create(cache.dir, showWarnings = FALSE) #file.path(mainDir, subDir)
  if(is.null(name)){name <- deparse(substitute(o))}
  write(paste("Saving object ", name, sep=""), stderr())
  saveRDS(o, file.path(cache.dir, paste(name, ".rds", sep="")))
  write(paste("Object ", name, " saved", sep=""), stderr())
  name
}

#' Load an object (cache)
#'
#' @param name name of the object to load
#' @param global load in the global environment (if TRUE) or as an object (if FALSE, default)
#' @return object or nothing
#' @export
#' @examples
#' load.object("iris", global=T)
#' iris2 <- load.object("iris")
load.object <- function(name, global=F){
  if (is.null(global)){global=F}
  cache.dir <- ".cache"
  assert_that(is.string(name))
  assert_that(object.cached(name))
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
#' object.cached("iris")
object.cached <- function(name){
  cache.dir <- ".cache"
  if(!is.string(name)){return(F)}
  if(file.exists(file.path(cache.dir, paste(name, ".rds", sep="")))){return(T)}
  return(F)
}

#' For assertthat library. What to do if assert_that(object.cached("iris")) fails
on_failure(object.cached) <- function(call, env) {
  "File is not cached"
}

#' Return a list of the cached files
#'
#' @return vector of the names of the cached objects (as strings)
#' @export
#' @examples
#' get.object.cached()
get.object.cached <- function(){
  cache.dir <- ".cache"
  if(!dir.exists(cache.dir)){return(c())}
  # assert_that(dir.exists(cache.dir))
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
#' load.all.cached.objects()
load.all.cached.objects <- function(){
  for(x in get.object.cached()){
    load.object(x, global=T)
  }
}

#' Saves all cached objects in the global environment. Used to save an envirnoment quickly.
#' This seems to be much faster than save.data It is also much more modular. It alows to
#' load individual objects in other projects for example to combine data analysed separately.
#'
#' @return nothing
#' @export
#' @examples
#' save.all.objects()
save.all.objects <- function(env){
  obj <- list.objects(env=env)
  obj <- obj[!(obj$CLASS == "function"),]$OBJECT
  # print(obj)

  for(x in obj){
    write(paste("Object ", x, " to be saved", sep=""), stderr())
    save.object(mget(x, envir=env), name=x)
  }
}

#' Saves all cached objects in the global environment. Used to save an envirnoment quickly.
#' This seems to be much faster than save.data It is also much more modular. It alows to
#' load individual objects in other projects for example to combine data analysed separately.
#'
#' @return nothing
#' @export
#' @examples
#' save.all.objects()
save.all.objects <- function(env, cores = NULL){
  obj <- list.objects(env=env)
  obj <- obj[!(obj$CLASS == "function"),]$OBJECT

  names <- obj
  obj.values <- lapply(names, mget, envir=env)
  names(obj.values) <- names

  if(is.null(cores) || cores <= 1){
    lapply(obj.values, function(x) {
      n <- names(x)
      v <- x[[1]]
      write(paste("Object ", n, " to be saved", sep=""), stderr())
      save.object(v, name=n)
    })
  }
}


#' List all objects and return them in a dataframe associated with their type
#'
#' @return dataframe of boject names associated with their types
#' @export
#' @examples
#' list.objects()
list.objects <- function(env = .GlobalEnv)
{
  if(!is.environment(env)){
    env <- deparse(substitute(env))
    stop(sprintf('"%s" must be an environment', env))
  }
  obj.type <- function(x) class(get(x, envir = env))
  foo <- sapply(ls(envir = env), obj.type)
  object.name <- names(foo)
  names(foo) <- seq(length(foo))
  dd <- data.frame(CLASS = foo, OBJECT = object.name,
                   stringsAsFactors = FALSE)
  dd[order(dd$CLASS),]
}

#' Remove object from cache
#'
#' @return name of the object removed
#' @export
#' @examples
#' uncache.object('df')
uncache.object <- function(name){
  cache.dir <- ".cache"
  assert_that(is.string(name))
  assert_that(object.cached(name))
  file.remove(file.path(cache.dir, paste(name, ".rds", sep="")))
}
