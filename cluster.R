cluster.env <- new.env()

cluster.env$cluster <- NULL
cluster.env$is.mc.cluster <- NULL

start.cluster <- function(mc=TRUE){
  cluster.env$is.mc.cluster <- mc
  if(mc){
    require(doMC)
    registerDoMC(2)
  }
  else{
    require(doSNOW)
    
    #
    # Nodes' description
    #
    inform2 <-
      list(host="192.168.130.137",
           rscript="C:/Program Files/R/R-2.15.3/bin/Rscript.exe",
           snowlib="C:/Program Files/R/R-2.15.3/library")
    
    inform3 <-
      list(host="192.168.130.169",
           rscript="C:/Program Files/R/R-2.15.3/bin/Rscript.exe",
           snowlib="C:/Program Files/R/R-2.15.3/library")
    
    inform4 <-
      list(host="192.168.130.191",
           rscript="C:/Program Files/R/R-2.15.3/bin/Rscript.exe",
           snowlib="C:/Program Files/R/R-2.15.3/library")
    
    home <-
      list(host = "192.168.0.101",
           rscript = "/usr/bin/Rscript",
           snowlib = "/home/ales/R/i586-suse-linux-gnu-library/2.15")
    
    #master <- "192.168.0.100"
    master <- "localhost"
    manual <- FALSE
    
    #
    # Combines nodes
    #
    nodes <- c(#lapply(1:1, function(i) inform2),
               #lapply(1:2, function(i) inform3),
               #lapply(1:2, function(i) inform4),
               #lapply(1:1, function(i) home),
               rep("localhost",2))
    
    #
    # Register & run
    #
    cluster.env$cluster <- makeCluster(nodes, type = "SOCK", manual=manual, master=master)
    registerDoSNOW(cluster.env$cluster)
  }
}

stop.cluster <- function(){
  if(!cluster.env$is.mc.cluster){
    stopCluster(cluster.env$cluster)
    
    cluster.env$cluster <- NULL
    cluster.env$is.mc.cluster <- NULL
  }
}