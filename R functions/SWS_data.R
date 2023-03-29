# Import Data From SWS 

#-------------------------------------------------------------------------------
SWS_data <- function(domain,
                     dataset,
                     keys=NULL,
                     Flag=F) {
    
    dimension_name = GetDatasetConfig(domain,dataset)$dimensions
    ndim=length(dimension_name)

    if ( is.null(keys)|length(keys) != ndim){ 
        
        writeLines(c("Provide keys in a list with the following order : "," ",
                         paste(1:ndim, dimension_name, sep = ". ")))
      
        # error if not enough keys provided --------------------------------------------
        if (length(keys) != ndim & !is.null(keys)) { stop("Different number of dimentions") }
          
    } else {
    
    if (!is.list(keys)){ stop("keys parameter is not a list" )}
    #-------------------------------------------------------------------------------
    
    dimensions = list()
    
    for (i in 1:ndim) {
        
        dimensions[[i]] <- Dimension(name = dimension_name[i],
                                     keys =  as.character(na.omit( unlist(keys[i]))))
    }
    
    DatasetKey <- DatasetKey(
        domain = domain,
        dataset =  dataset,
        dimensions = dimensions)
    
    if (Flag == T) {Data <- GetData(DatasetKey)} else {Data <- GetData(DatasetKey, flags = F)}
    
    return(Data)
    }
}

#-------------------------------------------------------------------------------