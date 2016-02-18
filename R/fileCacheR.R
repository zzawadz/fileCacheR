file_function = function(fnc, verbose = TRUE)
{
  fnc = force(fnc)
  fncName = force(as.character(match.call()$fnc))
  verbose = force(verbose)

  cache = cache_function
  namesParams = transform_params_list(fnc)

  body = as.list(cache)[[2]]
  body = as.list(body)
  body = as.call(c(body[1], namesParams, body[3]))

  paramsList = c(head(as.list(fnc), -1))
  cache = as.function(c(paramsList, body))
  cache

}

transform_params_list = function(fun)
{
  paramsList = c(head(as.list(fun), -1))

  namesParams = names(paramsList)
  namesParams = sapply(namesParams, as.name)

  if(any(names(namesParams) == "..."))
  {
    k = which(names(namesParams) == "...")
    names(namesParams)[k] = ""
  }

  namesParams
}

cache_function = function(...)
{
  allParams = as.list(...)

  cacheDir = file.path(getwd(), ".cache", fncName)

  if(!dir.exists(cacheDir))
  {
    dir.create(cacheDir, recursive = TRUE)
  }

  modTime = file.mtime(file)
  allParams[["MODTIME"]] = modTime

  hash = digest::digest(allParams)

  cacheFile = file.path(cacheDir, hash)

  if(file.exists(cacheFile))
  {
    if(verbose)
    {
      message(sprintf("Loaded %s, last access time: %s, hash: %s", file, modTime, hash))
    }
    return(readRDS(cacheFile))
  }

  value = fnc(...)
  saveRDS(value, cacheFile)

  if(verbose)
  {
    message(sprintf("Saved %s, last access time: %s, hash: %s", file, modTime, hash))
  }

  return(value)
}


# cache_read = file_function(readLines)
# a = cache_read("R/fileCacheR.R")
