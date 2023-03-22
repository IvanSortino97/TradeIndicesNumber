# Plugin settings --------------------------------------------------------------

plugin_name = "Trade Indices Numbers"
wd ="~/TradeIndicesNumber" # Rstudio Directory
env = "PROD" # QA, PROD or SWS

status_message <- function(text){message(paste(c("Your", plugin_name, "Plugin", text), collapse = " "))}
status_message("has started.")

# Loading libraries ------------------------------------------------------------

status_message("is loading libraries.")
message(paste(c("Your", plugin_name,"Plugin is loading libraries."), collapse = " "))

if (env == "QA") .libPaths("/newhome/shared/R/3.3.3/lib") 
if (env == "PROD") .libPaths("/newhome/shared/Library/3.3.3/") 


suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(data.table)
    library(stringr)
    library(dplyr)
    library(methods)
})


# Setting the environment ------------------------------------------------------

if(CheckDebug()){
    suppressWarnings({
        
    library(faoswsModules)
    SETT <- ReadSettings(paste(wd,"sws.yml",sep="/"))
    SetClientFiles(SETT[["certdir"]])
    GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
    
#   Load R functions
    invisible(lapply(list.files(paste0(wd,"/R functions"),
                                full.names = T,
                                pattern = ".R"),
                                source))
    })
}

# Dataset chosen  ---------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters --------------------------------------------------------------

status_message("is reading its parameters.")

# Plugin parameters

#swsContext.computationParams$ #Parameter

# Selected query

# swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys


