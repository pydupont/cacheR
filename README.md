# R caching library

I needed some caching functions for the CSC metagenomics projects. This library is 
based on the `RDS` format. It provides functions to help making file caching easier. 
It creates a `.cache` folder at the root of the worksapce where it saves RDS files. 
The objects are __cached__ (or saved) when the command `save_object(my_object)` is called. 
The object can be loaded using `load_object(my_object_name)`, all cached objects can be 
loaded using `load.all.cached.files()`. The function `save.all.objects()` is also parallelized,
making it faster than the session saving function from R.