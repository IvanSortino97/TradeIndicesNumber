domain = "trade"
dataset = "total_trade_cpc_m49"


# SWS LOAD ----------------------------------------------------------------

wd = "~/TradeIndicesNumber" # Rstudio Directory
env = "PROD" # QA, PROD or SWS

if (env == "QA")
    .libPaths("/newhome/shared/R/3.3.3/lib")
if (env == "PROD")
    .libPaths("/newhome/shared/Library/3.3.3/")


suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(data.table)
    library(stringr)
    library(dplyr)
    library(methods)
})

if (CheckDebug()) {
    suppressWarnings({
        library(faoswsModules)
        SETT <- ReadSettings(paste(wd, "sws.yml", sep = "/"))
        SetClientFiles(SETT[["certdir"]])
        GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
        
        #   Load R functions
        invisible(lapply(
            list.files(
                paste0(wd, "/R functions"),
                full.names = T,
                pattern = ".R"
            ),
            source
        ))
    })
}


# code tree ---------------------------------------------------------------


g_world  <- faosws::GetCodeTree(    domain = "trade",
                                      dataset = "total_trade_cpc_m49",
                                      dimension = "geographicAreaM49",
                                      roots = list(1216))


g_region <- faosws::GetCodeList(domain = "trade",
                                   dataset = "total_trade_cpc_m49",
                                   dimension = "geographicAreaM49",
                                   1)
