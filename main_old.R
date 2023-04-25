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

# Item list, according to QCL domain in FAOSTAT

TI_item_list <- ReadDatatable("ti_item_list")
setnames(TI_item_list,
         old = "cpc_code",
         new = "measuredItemCPC")

# Multipliers for area not FOB/CIF, if the end_date is not specified in the
# parameter table, it will be applied up to the current date

TI_multipliers <- ReadDatatable("ti_fob_cif_multipliers",
                                columns = c("areacode_m49",
                                            "start_year",
                                            "end_year",
                                            "multiplier"))
setnames(TI_multipliers,
         old = "areacode_m49",
         new = "geographicAreaM49")
TI_multipliers[is.na(end_year)]$end_year <-
    as.character(format(Sys.Date(),
                        format = "%Y"))

# Item group composition 

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

# Countries list

TI_countries <-
    fread("~/TradeIndicesNumber/FAOSTAT/TI-country-regions.csv")
TI_countries$`M49 Code`[TI_countries$`M49 Code` == "156"] = "1248"

# TI_countries_group <-
#     fread("~/TradeIndicesNumber/FAOSTAT/TI-region aggregation.csv")
# TI_countries_group$`M49 Code`[TI_countries_group$`M49 Code` == "156"] = "1248"



# Pull Data from SWS ----------------------------------------------------------

status_message("is puling data from SWS.")

key_item = TI_item_list$measuredItemCPC

key_geo = TI_countries$`M49 Code` %>%
    as.character() %>%
    sub(pattern = "^0+", replacement = "")
key_geo = key_geo[!key_geo %in% "97"] # Need to add Europe in codelist

key_elem = c('5610', '5622', '5910', '5922')

key_year = 2014:2019

data_SWS = SWS_data(
    domain = "trade",
    dataset = "total_trade_cpc_m49",
    keys = list(key_geo, key_elem, key_item, key_year)
)

# Single Item -------------------------------------------------------------

data_item.single <- data_SWS[, list(Value = sum(Value, na.rm = T)),
                             by = c("measuredElementTrade",
                                    "geographicAreaM49",
                                    "timePointYears",
                                    "ti_code")]

data_item.single[, measuredItemCPC := fcl2cpc(str_pad(ti_code,
                                                      width = 4,
                                                      pad = "0"))]
data_item.single <- data_item.single[!is.na(measuredItemCPC), ]

# Area not FOB/CIF multiplier ---------------------------------------------


data_item.single <- merge(data_item.single,
                          TI_multipliers,
                          by = "geographicAreaM49",
                          all.x = T)

data_item.single[timePointYears >= start_year &
                     timePointYears <= end_year &
                     measuredElementTrade == "5622",
                 Value := Value * as.numeric(multiplier)]

# Item aggregated  --------------------------------------------------------


data_item.aggr <- merge(data_SWS[, .(measuredElementTrade,
                                     geographicAreaM49,
                                     measuredItemCPC,
                                     timePointYears,
                                     Value)],
                        TI_item_group,
                        by = "measuredItemCPC",
                        allow.cartesian = TRUE)
data_item.aggr <-
    data_item.aggr[, list(Value = sum(Value, na.rm = T)),
                   by = c("measuredElementTrade",
                          "geographicAreaM49",
                          "timePointYears",
                          "sws_group_code")]

setnames(data_item.aggr, "sws_group_code", "measuredItemCPC")


# Calculations  -----------------------------------------------------------

status_message("is caluculating selected elements.")

ELEMENTS <- list()

## Element 461 - 491 -------------------------------------------------------


elem.461.491 <-
    copy(data_item.single[measuredElementTrade %in% c("5610", "5910"),
                          .(measuredElementTrade,
                            geographicAreaM49,
                            measuredItemCPC,
                            timePointYears,
                            Value)])

elem.461.491.avg <- elem.461.491[timePointYears %in% base_periode,]
elem.461.491.avg <-
    elem.461.491.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.461.491.avg <- elem.461.491.avg[timePointYears == base_year,]
elem.461.491.avg <-
    elem.461.491.avg[, c('Value', 'timePointYears') := NULL]
elem.461.491 <- merge(
    elem.461.491,
    elem.461.491.avg,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)
rm(elem.461.491.avg)
elem.461.491[, Value := Value / Mean]
elem.461.491[, Mean := NULL]
elem.461.491[measuredElementTrade == "5610"]$measuredElementTrade = "461"
elem.461.491[measuredElementTrade == "5910"]$measuredElementTrade = "491"

ELEMENTS[["461.491"]] <- elem.461.491
rm(elem.461.491)

## Element 64 - 94  --------------------------------------------------------

elem.64.94 <-
    copy(data_item.single[measuredElementTrade %in% c("5622", "5922"),
                          .(measuredElementTrade,
                            geographicAreaM49,
                            measuredItemCPC,
                            timePointYears,
                            Value)])

# Change elements code to match the merge keys
elem.64.94[measuredElementTrade == "5622"]$measuredElementTrade = "461"
elem.64.94[measuredElementTrade == "5922"]$measuredElementTrade = "491"

elem.64.94 <- merge(
    elem.64.94,
    ELEMENTS$`461.491`,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC",
        "timePointYears"
    )
)

elem.64.94[, Value := Value.x / Value.y]
elem.64.94[, c("Value.x", "Value.y") := NULL]

elem.64.94[measuredElementTrade == "461"]$measuredElementTrade = "64"
elem.64.94[measuredElementTrade == "491"]$measuredElementTrade = "94"

ELEMENTS[["64.94"]] <- elem.64.94
rm(elem.64.94)

## Elements 65 - 95 --------------------------------------------------------

elem.65.95 <-
    copy(data_item.single[measuredElementTrade %in% c("5622", "5922") &
                              timePointYears %in% base_periode,
                          .(measuredElementTrade,
                            geographicAreaM49,
                            measuredItemCPC,
                            timePointYears,
                            Value)])

elem.65.95 <- elem.65.95[, Value := list(mean(Value, na.rm = T)),
                         by = c("measuredElementTrade",
                                "geographicAreaM49",
                                "measuredItemCPC")]

elem.65.95 <- elem.65.95[timePointYears == base_year]
elem.65.95 <- elem.65.95[, c('timePointYears') := NULL]

elem.65.95[measuredElementTrade == "5622"]$measuredElementTrade = "461"
elem.65.95[measuredElementTrade == "5922"]$measuredElementTrade = "491"

elem.65.95 <- merge(
    ELEMENTS$`461.491`,
    elem.65.95,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    ),
    all.x = T
)
elem.65.95[, Value := Value.x * Value.y]
elem.65.95[, c("Value.x", "Value.y") := NULL]

elem.65.95[measuredElementTrade == "461"]$measuredElementTrade = "65"
elem.65.95[measuredElementTrade == "491"]$measuredElementTrade = "95"

ELEMENTS[["65.95"]] <- elem.65.95
rm(elem.65.95)

## Element 462 - 492  ------------------------------------------------------

elem.462.492 <-
    copy(data_item.single[measuredElementTrade %in% c("5622", "5922"),
                          .(measuredElementTrade,
                            geographicAreaM49,
                            measuredItemCPC,
                            timePointYears,
                            Value)])

elem.462.492.avg <- elem.462.492[timePointYears %in% base_periode,]
elem.462.492.avg <-
    elem.462.492.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.462.492.avg <- elem.462.492.avg[timePointYears == base_year,]
elem.462.492.avg <-
    elem.462.492.avg[, c('Value', 'timePointYears') := NULL]
elem.462.492 <- merge(
    elem.462.492,
    elem.462.492.avg,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)
rm(elem.462.492.avg)
elem.462.492[, Value := (Value * 100) / Mean]
elem.462.492[, Mean := NULL]
elem.462.492[measuredElementTrade == "5622"]$measuredElementTrade = "462"
elem.462.492[measuredElementTrade == "5922"]$measuredElementTrade = "492"

ELEMENTS[["462.492"]] <- elem.462.492
rm(elem.462.492)

## Element 464 - 494 -------------------------------------------------------

elem.464.494 <- ELEMENTS$`64.94`[timePointYears %in% base_periode,]
elem.464.494 <-
    elem.464.494[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.464.494 <- elem.464.494[timePointYears == base_year,]
elem.464.494 <- elem.464.494[, c('Value', 'timePointYears') := NULL]
elem.464.494 <- merge(
    ELEMENTS$`64.94`,
    elem.464.494,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.464.494[, Value := (Value * 100) / Mean]
elem.464.494[, Mean := NULL]

elem.464.494[measuredElementTrade == "64"]$measuredElementTrade = "464"
elem.464.494[measuredElementTrade == "94"]$measuredElementTrade = "494"

ELEMENTS[["464.494"]] <- elem.464.494
rm(elem.464.494)

## Element 465 - 495 -------------------------------------------------------

elem.465.495 <- ELEMENTS$`65.95`[timePointYears %in% base_periode,]
elem.465.495 <-
    elem.465.495[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.465.495 <- elem.465.495[timePointYears == base_year,]
elem.465.495 <- elem.465.495[, c('Value', 'timePointYears') := NULL]
elem.465.495 <- merge(
    ELEMENTS$`65.95`,
    elem.465.495,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.465.495[, Value := (Value * 100) / Mean]
elem.465.495[, Mean := NULL]

elem.465.495[measuredElementTrade == "65"]$measuredElementTrade = "465"
elem.465.495[measuredElementTrade == "95"]$measuredElementTrade = "495"

ELEMENTS[["465.495"]] <- elem.465.495
rm(elem.465.495)

# Merge Results -----------------------------------------------------------

ELEMENTS <- do.call("rbind", ELEMENTS)


# Aggregated Items Elements -----------------------------------------------

status_message("is caluculating aggregated elements.")

ELEMENTS.aggr <- list()

## Aggr. Element 461 - 491 -------------------------------------------------------


elem.461.491 <-
    copy(data_item.aggr[measuredElementTrade %in% c("5610", "5910"),
                        .(measuredElementTrade,
                          geographicAreaM49,
                          measuredItemCPC,
                          timePointYears,
                          Value)])

elem.461.491.avg <- elem.461.491[timePointYears %in% base_periode,]
elem.461.491.avg <-
    elem.461.491.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.461.491.avg <- elem.461.491.avg[timePointYears == base_year,]
elem.461.491.avg <-
    elem.461.491.avg[, c('Value', 'timePointYears') := NULL]
elem.461.491 <- merge(
    elem.461.491,
    elem.461.491.avg,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)
rm(elem.461.491.avg)
elem.461.491[, Value := Value / Mean]
elem.461.491[, Mean := NULL]
elem.461.491[measuredElementTrade == "5610"]$measuredElementTrade = "461"
elem.461.491[measuredElementTrade == "5910"]$measuredElementTrade = "491"

ELEMENTS.aggr[["461.491"]] <- elem.461.491
rm(elem.461.491)

## Aggr. Element 64 - 94  --------------------------------------------------------

elem.64.94 <-
    copy(data_item.aggr[measuredElementTrade %in% c("5622", "5922"),
                        .(measuredElementTrade,
                          geographicAreaM49,
                          measuredItemCPC,
                          timePointYears,
                          Value)])

# Change elements code to match the merge keys
elem.64.94[measuredElementTrade == "5622"]$measuredElementTrade = "461"
elem.64.94[measuredElementTrade == "5922"]$measuredElementTrade = "491"

elem.64.94 <- merge(
    elem.64.94,
    ELEMENTS.aggr$`461.491`,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC",
        "timePointYears"
    )
)

elem.64.94[, Value := Value.x / Value.y]
elem.64.94[, c("Value.x", "Value.y") := NULL]

elem.64.94[measuredElementTrade == "461"]$measuredElementTrade = "64"
elem.64.94[measuredElementTrade == "491"]$measuredElementTrade = "94"

ELEMENTS.aggr[["64.94"]] <- elem.64.94
rm(elem.64.94)

## Aggr. Element 65 - 95 --------------------------------------------------------

elem.65.95 <-
    copy(data_item.aggr[measuredElementTrade %in% c("5622", "5922") &
                            timePointYears %in% base_periode,
                        .(measuredElementTrade,
                          geographicAreaM49,
                          measuredItemCPC,
                          timePointYears,
                          Value)])

elem.65.95 <- elem.65.95[, Value := list(mean(Value, na.rm = T)),
                         by = c("measuredElementTrade",
                                "geographicAreaM49",
                                "measuredItemCPC")]

elem.65.95 <- elem.65.95[timePointYears == base_year]
elem.65.95 <- elem.65.95[, c('timePointYears') := NULL]

elem.65.95[measuredElementTrade == "5622"]$measuredElementTrade = "461"
elem.65.95[measuredElementTrade == "5922"]$measuredElementTrade = "491"

elem.65.95 <- merge(
    ELEMENTS.aggr$`461.491`,
    elem.65.95,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    ),
    all.x = T
)
elem.65.95[, Value := Value.x * Value.y]
elem.65.95[, c("Value.x", "Value.y") := NULL]

elem.65.95[measuredElementTrade == "461"]$measuredElementTrade = "65"
elem.65.95[measuredElementTrade == "491"]$measuredElementTrade = "95"

ELEMENTS.aggr[["65.95"]] <- elem.65.95
rm(elem.65.95)

## Aggr. Element 462 - 492  ------------------------------------------------------

elem.462.492 <-
    copy(data_item.aggr[measuredElementTrade %in% c("5622", "5922"),
                        .(measuredElementTrade,
                          geographicAreaM49,
                          measuredItemCPC,
                          timePointYears,
                          Value)])

elem.462.492.avg <- elem.462.492[timePointYears %in% base_periode,]
elem.462.492.avg <-
    elem.462.492.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.462.492.avg <- elem.462.492.avg[timePointYears == base_year,]
elem.462.492.avg <-
    elem.462.492.avg[, c('Value', 'timePointYears') := NULL]
elem.462.492 <- merge(
    elem.462.492,
    elem.462.492.avg,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)
rm(elem.462.492.avg)
elem.462.492[, Value := (Value * 100) / Mean]
elem.462.492[, Mean := NULL]
elem.462.492[measuredElementTrade == "5622"]$measuredElementTrade = "462"
elem.462.492[measuredElementTrade == "5922"]$measuredElementTrade = "492"

ELEMENTS.aggr[["462.492"]] <- elem.462.492
rm(elem.462.492)

## Aggr. Element 464 - 494 -------------------------------------------------------

elem.464.494 <-
    ELEMENTS.aggr$`64.94`[timePointYears %in% base_periode,]
elem.464.494 <-
    elem.464.494[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.464.494 <- elem.464.494[timePointYears == base_year,]
elem.464.494 <- elem.464.494[, c('Value', 'timePointYears') := NULL]
elem.464.494 <- merge(
    ELEMENTS.aggr$`64.94`,
    elem.464.494,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.464.494[, Value := (Value * 100) / Mean]
elem.464.494[, Mean := NULL]

elem.464.494[measuredElementTrade == "64"]$measuredElementTrade = "464"
elem.464.494[measuredElementTrade == "94"]$measuredElementTrade = "494"

ELEMENTS.aggr[["464.494"]] <- elem.464.494
rm(elem.464.494)

## Aggr. Element 465 - 495 -------------------------------------------------------

elem.465.495 <-
    ELEMENTS.aggr$`65.95`[timePointYears %in% base_periode,]
elem.465.495 <-
    elem.465.495[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.465.495 <- elem.465.495[timePointYears == base_year,]
elem.465.495 <- elem.465.495[, c('Value', 'timePointYears') := NULL]
elem.465.495 <- merge(
    ELEMENTS.aggr$`65.95`,
    elem.465.495,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.465.495[, Value := (Value * 100) / Mean]
elem.465.495[, Mean := NULL]

elem.465.495[measuredElementTrade == "65"]$measuredElementTrade = "465"
elem.465.495[measuredElementTrade == "95"]$measuredElementTrade = "495"

ELEMENTS.aggr[["465.495"]] <- elem.465.495
rm(elem.465.495)

# Merge Results -----------------------------------------------------------

ELEMENTS.aggr <- do.call("rbind", ELEMENTS.aggr)
