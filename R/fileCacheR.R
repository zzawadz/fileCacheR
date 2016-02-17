file_function = function(fnc, verbose = TRUE)
{
  fnc = force(fnc)
  fncName = as.character(match.call()$fnc)
  verbose = force(verbose)

  function(...)
  {
    allParams = as.list(...)

    cacheDir = file.path(getwd(), ".cache", fncName)
    file = allParams[[1]]
    
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
}

# cache_read = file_function(readLines)
# cache_read("R/fileCacheR.R")
