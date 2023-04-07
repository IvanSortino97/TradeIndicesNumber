# Plugin settings --------------------------------------------------------------

plugin_name = "Trade Indices Numbers"
wd = "~/TradeIndicesNumber" # Rstudio Directory
env = "PROD" # QA, PROD or SWS

status_message <-
    function(text) {
        message(paste(c("Your", plugin_name, "Plugin", text), collapse = " "))
    }
status_message("has started.")

# Loading libraries ------------------------------------------------------------

status_message("is loading libraries.")
message(paste(
    c("Your", plugin_name, "Plugin is loading libraries."),
    collapse = " "
))

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


# Setting the environment ------------------------------------------------------

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

# Dataset chosen  --------------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters -------------------------------------------------------------------

status_message("is reading its parameters.")

# Plugin parameters

base_year = 2015
base_periode = as.character(c(base_year - 1,
                              base_year,
                              base_year + 1))

#swsContext.computationParams$ #Parameter

# Selected query

# swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys


# Support Datatables

status_message("is reading support datatables.")

TI_item_list <- ReadDatatable("ti_item_list",
                              columns = c("ti_code",
                                          "cpc_code"))
setnames(TI_item_list,
         old = "cpc_code",
         new = "measuredItemCPC")

TI_multipliers <- ReadDatatable("ti_fob_cif_multipliers")
TI_multipliers[, area_code := NULL]
setnames(TI_multipliers,
         old = "areacode_m49",
         new = "geographicAreaM49")
TI_multipliers[is.na(end_year)]$end_year <-
    as.character(format(Sys.Date(),
                        format = "%Y"))

TI_item_group <- ReadDatatable("ti_item_group",
                               columns = c('code_group_cpc',
                                           'code_item_cpc',
                                           'code_elem',
                                           'factor'))

# setnames(TI_item_group,
#          old = "cpc_item_code",
#          new = "measuredItemCPC")

TI_countries <-
    fread("~/TradeIndicesNumber/FAOSTAT/TI-country-regions.csv")
TI_countries$`M49 Code`[TI_countries$`M49 Code` == "156"] = "1248"

TI_countries_group <-
    fread("~/TradeIndicesNumber/FAOSTAT/TI-region aggregation.csv")
TI_countries_group$`M49 Code`[TI_countries_group$`M49 Code` == "156"] = "1248"



# Pull Data from SWS ----------------------------------------------------------

status_message("is puling data from SWS.")

key_item = TI_item_list$measuredItemCPC %>%
    na.exclude() %>%
    as.character()

key_geo = TI_countries$`M49 Code` %>%
    as.character() %>%
    sub(pattern = "^0+", replacement = "")
key_geo = key_geo[!key_geo %in% "97"] # Need to add europe in codelist

key_elem = c('5610', '5622', '5910', '5922')

key_year = 2014:2019

data_SWS = SWS_data(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    keys = list(key_geo, key_elem, key_item, key_year)
)

# --------------------------------------------------------------------------------------------------------------------------------------------
#############################################################################################################################################



TI_item_group <- ReadDatatable("ti_item_group",
                               columns = c('code_group_cpc',
                                           'code_item_cpc',
                                           'code_elem',
                                           'factor'))

setnames(TI_item_group,
         old = "code_item_cpc",
         new = "measuredItemCPC")

factor_diff <- unique(TI_item_group$code_elem)
TI_item_group[ code_elem == " ", code_elem := "Default" ]

TI_item_group <- dcast(TI_item_group, code_group_cpc + measuredItemCPC ~ code_elem, value.var = "factor")

if ('5610' %in% factor_diff) TI_item_group[is.na(`5610`), `5610` := Default]
if ('5910' %in% factor_diff) TI_item_group[is.na(`5910`), `5910` := Default]
if ('5622' %in% factor_diff) TI_item_group[is.na(`5622`), `5622` := Default]
if ('5922' %in% factor_diff) TI_item_group[is.na(`5922`), `5922` := Default]

data_test <- data_SWS[timePointYears == "2018" & geographicAreaM49 == "100"]

test <- merge(data_test,
              TI_item_group,
              by = c("measuredItemCPC"), 
              allow.cartesian = T)
test[, Value_factor := Value* as.numeric(Default)]

if ('5610' %in% factor_diff) test[measuredElementTrade == "5610", Value_factor := Value * as.numeric(`5610`)]
if ('5910' %in% factor_diff) test[measuredElementTrade == "5910", Value_factor := Value * as.numeric(`5910`)]
if ('5622' %in% factor_diff) test[measuredElementTrade == "5622", Value_factor := Value * as.numeric(`5622`)]
if ('5922' %in% factor_diff) test[measuredElementTrade == "5922", Value_factor := Value * as.numeric(`5922`)]
