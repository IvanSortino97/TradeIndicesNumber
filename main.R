START <- Sys.time()

# Plugin settings --------------------------------------------------------------

plugin_name = "Trade Indices Numbers"
wd = "~/TradeIndicesNumber" # Rstudio Directory
env = "PROD" # QA, PROD or SWS

# Status function --------------------------------------------------------------

status_message <-
    function(text = NULL, time = NULL) {
        if (!is.null(text)) {
            if (text == "ok") {
                message(paste0(" - Done", "\n"))
            } else {
                message(paste(
                    c("Your", plugin_name, "Plugin", text),
                    collapse = " "
                ))
                return(invisible(Sys.time()))
            }
            
        }
        
        if (!is.null(time)) {
            message(paste0(" - Done : ", format(Sys.time() - time), "\n"))
        }
    }

ok = "ok"

# Loading libraries ------------------------------------------------------------


status_message("has started.")

status_message("is loading libraries.")

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

status_message(ok)

# Dataset chosen  --------------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters -------------------------------------------------------------------

time <- status_message("is reading its parameters.")

# Plugin parameters

base_year = 2015
base_periode = as.character(c(base_year - 1,
                              base_year,
                              base_year + 1))

#swsContext.computationParams$ #Parameter

# Selected query

# swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys

status_message(time = time)

# Support Datatables

time <- status_message("is reading support datatables.")

# Item list, according to QCL domain in FAOSTAT

TI_item_list <- ReadDatatable("ti_item_list")
setnames(TI_item_list,
         old = "cpc_code",
         new = "measuredItemCPC")

# Multipliers for area not FOB/CIF, if the end_date is not specified in the
# parameter table, it will be applied up to the current date

TI_multipliers <- ReadDatatable(
    "ti_fob_cif_multipliers",
    columns = c("areacode_m49",
                "start_year",
                "end_year",
                "multiplier")
)
setnames(TI_multipliers,
         old = "areacode_m49",
         new = "geographicAreaM49")
TI_multipliers[is.na(end_year)]$end_year <-
    as.character(format(Sys.Date(),
                        format = "%Y"))

# Item group composition

TI_item_group <- ReadDatatable(
    "ti_aggregation_table",
    where = c("var_type = 'item'"),
    columns = c("var_group_code",
                "var_code",
                "var_element",
                "factor")
)

setnames(TI_item_group,
         old = "var_code",
         new = "measuredItemCPC")

factor_diff <- unique(TI_item_group$var_element)

TI_item_group[var_element == " ", var_element := "Default"]

TI_item_group <-
    dcast(TI_item_group,
          var_group_code + measuredItemCPC ~ var_element,
          value.var = "factor")

for (i in c('5610', '5910', '5622', '5922')) {
    if (i %in% factor_diff) {
        eval(parse(
            text =
                paste0("TI_item_group[is.na(`", i, "`), `", i, "` := Default]")
        ))
    }
}

# Countries list

TI_countries_list <- ReadDatatable("ti_country_list",
                                   columns = c("m49_code",
                                               "start_year",
                                               "end_year"))

TI_countries_list$m49_code[TI_countries_list$m49_code == "156"] = "1248"
TI_countries_list[, m49_code := sub("^0+", "", m49_code)]
TI_countries_list[is.na(start_year)]$start_year = "1900"
TI_countries_list[is.na(end_year)]$end_year = "9999"

setnames(TI_countries_list,
         old = "m49_code",
         new = "geographicAreaM49")

# Countries group composition

TI_countries_group <- ReadDatatable(
    "ti_aggregation_table",
    where = c("var_type = 'area' AND factor <> '0'"),
    columns = c("var_group_code",
                "var_code")
)
TI_countries_group[, var_group_code := sub("^0+", "", var_group_code)]
TI_countries_group[, var_code := sub("^0+", "", var_code)]

setnames(TI_countries_group,
         old = "var_code",
         new = "geographicAreaM49")

# Conversion table for codes mapping patch

TI_convertion = ReadDatatable("ti_code_conversion",
                              columns = c("faostat_code",
                                          "sws_code"))

status_message(time = time)

# Patch items codes -----------------------------------------------------------

# 'Beet Pulp' item is saved under CPC code 39149.01 in FAOSTAT and code 39140.01 in SWS (' Beet Pulp ')
# 'Fine animal hair, n.e.c.' is saved under CPC code 02943.90 in FAOSTAT and code 02943.02 in SWS (' Fine hair, n.e. ')
# 'Juice of fruits n.e.c.' is saved under CPC code 21439.90 in FAOSTAT and code  in SWS (' Juice of fruits n.e. ')
# 'Rice, paddy (rice milled equivalent)' is saved under CPC code F0030 in FAOSTAT and code 23161.02 in SWS (' Rice, Milled ')

status_message("is correcting CPC codes.")

TI_item_list <- ReadDatatable("ti_item_list")
setnames(TI_item_list,
         old = "cpc_code",
         new = "measuredItemCPC")

switch_code <- function(DT, COL, OLD, NEW) {
    dt <- as.data.frame(DT)
    dt[which(dt[, which(names(dt) == COL)] == OLD), which(names(dt) == COL)] = NEW
    dt <- as.data.table(dt)
    return(dt)
    
}

for (i in 1:nrow(TI_convertion)) {
    TI_item_list <- switch_code(
        TI_item_list,
        'measuredItemCPC',
        as.character(TI_convertion[i, 1, with = F]),
        as.character(TI_convertion[i, 2, with = F])
    )
}

TI_item_list = TI_item_list[!duplicated(TI_item_list$measuredItemCPC)]

if (any(TI_convertion$sws_code %in% TI_item_group$measuredItemCPC)) {
    TI_item_group = TI_item_group[!measuredItemCPC == TI_convertion$faostat_code[TI_convertion$sws_code %in% TI_item_group$measuredItemCPC]]
}

for (i in 1:nrow(TI_convertion)) {
    TI_item_group <- switch_code(
        TI_item_group,
        'measuredItemCPC',
        as.character(TI_convertion[i, 1, with = F]),
        as.character(TI_convertion[i, 2, with = F])
    )
}

status_message(ok)

# Pull Data from SWS ----------------------------------------------------------

time <- status_message("is pulling data from SWS.")


key_item = TI_item_list$measuredItemCPC

key_geo = TI_countries_list$geographicAreaM49

key_elem = c('5610', '5622', '5910', '5922')

key_year = 2014:2019
if (base_year)
    
    data_SWS = SWS_data(
        domain = "trade",
        dataset = "total_trade_cpc_m49",
        keys = list(key_geo, key_elem, key_item, key_year)
    )



# Remove countries out of validity period

data_SWS <- merge(data_SWS,
                  TI_countries_list,
                  by = "geographicAreaM49")

data_SWS <- data_SWS[timePointYears <= end_year &
                         timePointYears >= start_year, ]

data_SWS <- data_SWS[, c('end_year', 'start_year') := NULL]

status_message(time = time)

# Add Regions / Special Regions -------------------------------------------

time <- status_message("is calculating Regions aggregates.")

# Aggregate by regions

# TI_countries_group <- split(TI_countries_group, TI_countries_group$var_group_code )

# setkey(TI_countries_group, geographicAreaM49)
# setkey(data_SWS, geographicAreaM49)
#
# data_merged <-  data_SWS[TI_countries_group,  allow.cartesian=TRUE ]

# atomic.merge <- function(grp, df1, df2, var) {
#
#     Merge <- merge(df1,
#                    df2[var_group_code == grp],
#                    by = c("geographicAreaM49"),
#                    #all.y = T,
#                    allow.cartesian = TRUE)
#
#     Merge <- Merge[, Value := sum(Value, na.rm = T),
#                    by = c("var_group_code",
#                           "measuredElementTrade",
#                           "timePointYears",
#                           "measuredItemCPC")]
#
#     Merge[, geographicAreaM49 := var_group_code]
#     Merge[, var_group_code := NULL]
#     Merge <- unique(Merge)
#
#     return(Merge)
# }
#
# data_regions <- lapply(names(TI_countries_group),
#                        function(x) atomic.merge(x, data_SWS, TI_countries_group[[x]]))
# data_regions <- rbindlist(data_regions)



data_SWS <-
    rbind(data_SWS, regions_aggregate(data_SWS, TI_countries_group))

#rm(data_regions)

status_message(time = time)

# Area not FOB/CIF multiplier ---------------------------------------------

status_message("is applying Area not FOB/CIF multiplier.")

data_SWS <- merge(data_SWS,
                  TI_multipliers,
                  by = "geographicAreaM49",
                  all.x = T)

data_SWS[timePointYears >= start_year &
             timePointYears <= end_year &
             measuredElementTrade == "5622",
         Value := Value * as.numeric(multiplier)]

data_SWS[, c("start_year", "end_year", "multiplier") := NULL]

status_message(ok)

# Item aggregated  --------------------------------------------------------

time = status_message("is calculating item aggregates.")

data_SWS <-
    rbind(data_SWS, items_aggregate(data_SWS, TI_item_group))

status_message(time = time)

# Calculations  -----------------------------------------------------------

status_message("is caluculating selected elements.")

ELEMENTS <- list()

## Element 461 - 491 -------------------------------------------------------

elem.461.491 <-
    setDT(data_SWS[measuredElementTrade %in% c("5610", "5910")])

elem.461.491.avg <-
    setDT(elem.461.491[timePointYears %in% base_periode, ])
elem.461.491.avg <-
    elem.461.491.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.461.491.avg <-
    elem.461.491.avg[timePointYears == base_year, ][, c('Value', 'timePointYears') := NULL]

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
elem.461.491[, Value := Value / Mean][, Mean := NULL]

elem.461.491[measuredElementTrade == "5610"]$measuredElementTrade = "461"
elem.461.491[measuredElementTrade == "5910"]$measuredElementTrade = "491"

ELEMENTS[["461.491"]] <- elem.461.491
rm(elem.461.491)

## Element 64 - 94  --------------------------------------------------------

elem.64.94 <-
    setDT(data_SWS[measuredElementTrade %in% c("5622", "5922")])

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

elem.64.94[, Value := Value.x / Value.y][, c("Value.x", "Value.y") := NULL]

elem.64.94[measuredElementTrade == "461"]$measuredElementTrade = "64"
elem.64.94[measuredElementTrade == "491"]$measuredElementTrade = "94"

ELEMENTS[["64.94"]] <- elem.64.94
rm(elem.64.94)

## Elements 65 - 95 --------------------------------------------------------

elem.65.95 <-
    setDT(data_SWS[measuredElementTrade %in% c("5622", "5922") &
                       timePointYears %in% base_periode])

elem.65.95 <- elem.65.95[, Value := list(mean(Value, na.rm = T)),
                         by = c("measuredElementTrade",
                                "geographicAreaM49",
                                "measuredItemCPC")]

elem.65.95 <-
    elem.65.95[timePointYears == base_year][, c('timePointYears') := NULL]

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
elem.65.95[, Value := Value.x * Value.y][, c("Value.x", "Value.y") := NULL]

elem.65.95[measuredElementTrade == "461"]$measuredElementTrade = "65"
elem.65.95[measuredElementTrade == "491"]$measuredElementTrade = "95"

ELEMENTS[["65.95"]] <- elem.65.95
rm(elem.65.95)

## Element 462 - 492  ------------------------------------------------------

elem.462.492 <-
    setDT(data_SWS[measuredElementTrade %in% c("5622", "5922")])

elem.462.492.avg <-
    setDT(elem.462.492[timePointYears %in% base_periode])
elem.462.492.avg <-
    elem.462.492.avg[, Mean := list(mean(Value, na.rm = T)),
                     by = c("measuredElementTrade",
                            "geographicAreaM49",
                            "measuredItemCPC")]
elem.462.492.avg <-
    elem.462.492.avg[timePointYears == base_year, ][, c('Value', 'timePointYears') := NULL]
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
elem.462.492[, Value := (Value * 100) / Mean][, Mean := NULL]

elem.462.492[measuredElementTrade == "5622"]$measuredElementTrade = "462"
elem.462.492[measuredElementTrade == "5922"]$measuredElementTrade = "492"

ELEMENTS[["462.492"]] <- elem.462.492
rm(elem.462.492)

## Element 464 - 494 -------------------------------------------------------

elem.464.494 <- ELEMENTS$`64.94`[timePointYears %in% base_periode, ]
elem.464.494 <-
    elem.464.494[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.464.494 <-
    elem.464.494[timePointYears == base_year, ][, c('Value', 'timePointYears') := NULL]
elem.464.494 <- merge(
    ELEMENTS$`64.94`,
    elem.464.494,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.464.494[, Value := (Value * 100) / Mean][, Mean := NULL]

elem.464.494[measuredElementTrade == "64"]$measuredElementTrade = "464"
elem.464.494[measuredElementTrade == "94"]$measuredElementTrade = "494"

ELEMENTS[["464.494"]] <- elem.464.494
rm(elem.464.494)

## Element 465 - 495 -------------------------------------------------------

elem.465.495 <- ELEMENTS$`65.95`[timePointYears %in% base_periode, ]
elem.465.495 <-
    elem.465.495[, Mean := list(mean(Value, na.rm = T)),
                 by = c("measuredElementTrade",
                        "geographicAreaM49",
                        "measuredItemCPC")]
elem.465.495 <-
    elem.465.495[timePointYears == base_year, ][, c('Value', 'timePointYears') := NULL]
elem.465.495 <- merge(
    ELEMENTS$`65.95`,
    elem.465.495,
    by = c(
        "measuredElementTrade",
        "geographicAreaM49",
        "measuredItemCPC"
    )
)

elem.465.495[, Value := (Value * 100) / Mean][, Mean := NULL]

elem.465.495[measuredElementTrade == "65"]$measuredElementTrade = "465"
elem.465.495[measuredElementTrade == "95"]$measuredElementTrade = "495"

ELEMENTS[["465.495"]] <- elem.465.495
rm(elem.465.495)

# Merge Results -----------------------------------------------------------

ELEMENTS <- do.call("rbind", ELEMENTS)


# Save data ---------------------------------------------------------------

status_message("is saving data.")


