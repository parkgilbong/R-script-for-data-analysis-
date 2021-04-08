# read .pul 'treeinfo' which contains pointers to trace data
read.bundletree <- function(myfile, bundlename = ".pul", con=NA) {
  finally_close_con=is.na(con)
  if(is.na(con)){
    con <- file(myfile, "rb")
  }
  seek(con, 0)
  signature <- readChar(con, 8)
  
  if(signature=="DAT2"){
    # we are in a *.dat bundle file and have to find the start position
    version <- readChar(con, 32)
    time <- readBin(con, "double")
    nitems <- readBin(con, "int", size = 1)
    liddle_endian <- readBin(con, "logical")
    reserved <- readChar(con, 11)
    bundleitems <- do.call(rbind, (lapply(0:nitems, function(item) {
      start <- readBin(con, "int", size = 4)
      end <- readBin(con, "int", size = 4)  #end
      name <- readChar(con, 8)
      data.frame(
        start = start,
        end = end,
        name = name,
        stringsAsFactors = F
      )
    })))
    
    start <- subset(bundleitems, name == ".pul")$start
    seek(con, where = start)
  }else{
    # we are in *.pul or *.pgf file, starting at 0
    start <- 0 
  }
  
  seek(con, where=start )
  magic <- readChar(con, nchars = 4)
  stopifnot(magic == "eerT")
  
  nLevels <- readBin(con, "int", size = 4)
  lvl_sizes <- lapply(1:nLevels, function(i) {
    readBin(con, "int", size = 4)
  })
  
  tree <- pm_load_nodes(con, nLevels, lvl_sizes, 1)
  
  if(finally_close_con )
    close(con)
  tree
}


# recursively load all nodes of a HEKA tree (can be .pul , .amp, etc)
pm_load_nodes <- function(con, nlevels, lvl_sizes, level) {
  stopifnot(level <= nlevels)
  size <- lvl_sizes[[level]]  #size of the data block
  dataptr <- seek(con)
  # skip the 'data block'
  seek(con, seek(con) + size)
  nchildren <- readBin(con, "int", size = 4)
  if (nchildren == 0) {
    node <- "trace"
    attr(node, "dataptr") <- dataptr
  } else {
    node <- lapply(1:nchildren, function(child) {
      pm_load_nodes(con, nlevels, lvl_sizes, level + 1)
    })
    attr(node, "dataptr") <- dataptr
  }
  node
}




#' get a HEKA series
#'
#' @param node      a series node
#' @param sweeps    integer vector: a collection of sweeps
#' @param trace     integer: specify a single tracenumber that we want to read
#' @param read_data F if we just wnat to get the metadata 
#'
#' @return  object of class HEKASeries (but not a proto)
getSeries_from_node <-
  function(node,
           sweeps = 1:length(node),
           trace = 1, read_data=T) {
    
    path <- attr(node, "path")
    filename <- path[1]
    
    if (!file.exists(filename)) {
      filename <- paste("../", filename, sep = "")
    }
    
    con <- file(filename, "rb")
    signature<-readChar(con, 4)
    if(signature=="DAT1"){
      mypul<-stringr::str_replace(filename, ".dat$", ".pul")
      if(!file.exists(mypul))
        stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pul file"   )
      con_pul<- file(mypul, "rb")
    }else{
      con_pul<-con
    }
    
    series <- lapply(node[sweeps], function(sweep) {
      getTrace_(con_pul, sweep[[trace]], read_data=read_data, name=names(sweep)[[trace]], con_dat=con)
    })
    
    # read the sweep times 
    SwTimer <- unlist(lapply(node[sweeps], function(sweep) {
      #read.swTimer(sweep, con_pul)
      readAny(attr(sweep, "dataptr"), con_pul, 56, "double", 8)  #longreal swTime
    }))
    
    # close connection(s)
    close(con)
    if(signature=="DAT1"){
      close(con_pul)
    }
    
    # bring all traces to the same length ( padding with NAs )
    maxLength <- max(unlist(lapply(series, length)))
    series <- lapply(series, function(s) {
      length(s) <- maxLength
      s
    })
    
    
    series_Y  <- do.call(cbind, series)
    
    Xinterval    <- attr(series[[1]], "Xinterval")
    nDatapoints_ <- attr(series[[1]], "nDatapoints_")
    series_X1 <- 1:nDatapoints_ * Xinterval
    series_X  <-
      matrix(rep(series_X1, length(series_Y[1,])),
             nDatapoints_,
             length(series_Y[1,]))
    
    series <-
      list(
        y = series_Y,
        x = series_X,
        filename = filename,
        exp =   path[2],
        ser =   path[3],
        trace = trace,
        tracename =   attr(series[[1]], "name"),
        Trace_Time =  SwTimer,
        Serieslabel = getStimName_from_unique_seriesName(path[3]),
        Stimulus = attr(node,        "StimulusName")
      )
    
    class(series) <- "HEKAseries"
    series
  }





# function to get a trace data from a file and a 'treeinfo' node (ptr)
# start= first datapoint to read
# n number of datapoints to read
getTrace_ <- function(con, ptr, start=0, n=NA, read_data=T, name="", con_dat=con) {
  SIZE = 2 
  tracename<-name
  ptr <- attr(ptr, "dataptr")
  #con = file(file, "rb")
  seek(con, ptr + 40)
  offset <- readBin(con, "int", size = 4)
  nDatapoints_ <- readBin(con, "int", size = 4)
  nDatapoints <- nDatapoints_ - start
  if(!is.na(n)){
    nDatapoints <- min(nDatapoints, n)
  }
  seek(con, ptr + 96)
  Unit = readBin(con, "char")
  if (Unit == "V")
    Unit_ = 1000
  else
    Unit_ = 1e+09
  seek(con, ptr + 72)
  DataScaler = readBin(con, "double", size = 8)
  seek(con_dat, where = offset+start*SIZE)
  if(read_data){
    trace <-
      readBin(con_dat,
              what = "int",
              size = SIZE,
              n = nDatapoints) * Unit_ * DataScaler
  }else{
    trace <- NA
  }
  
  seek(con, ptr + 104)
  Xinterval <- readBin(con, "double", size = 8)
  
  
  
  #close(con)
  attr(trace, "Xinterval") <- Xinterval
  attr(trace, "nDatapoints_") <- nDatapoints_
  attr(trace, "name") <- tracename
  trace
}

# label (offset4) only for exp, ser, swp, trace
# text (offset 36 for exp and ser, offset120 for root)
readlabel <- function(ptr, con, offset=4) {
  seek(con, where = ptr + offset)
  readBin(con, "char")
}



readAny <- function(ptr, con, offset, what, size) {
  seek(con, where = ptr + offset)
  readBin(con, what, size = size)
}


# subset.tree <- function(tree, file, exp) {
#   tree[-(file)] <- NULL
#   tree[[file]][-(exp)] <- NULL
#   tree
# }