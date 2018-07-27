# library("caching")
source("R/caching.R")

df <- iris
save.object(df)
df2 <- load.object("df")
assert_that(identical(df, df2, ignore.environment = T))
df3 <- mtcars
save.object(df3)

rm(list = ls())
source("R/caching.R")
if(object.cached("df4")){uncache.object("df4")}

load.all.cached.objects()

summary(df3)
getwd()

df4 <- iris
save.all.objects(.GlobalEnv, cores = 1)

assert_that(object.cached("df4"))
