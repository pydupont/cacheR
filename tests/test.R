# library("caching")
source("R/caching.R")

df <- iris
save_object(df)
df2 <- load_object("df")
assert_that(identical(df, df2, ignore.environment = T))
df3 <- mtcars
save_object(df3)

rm(list = ls())

load.all.cached.files()

summary(df3)
getwd()
