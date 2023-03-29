data_FS = fread("~/TradeIndicesNumber/FAOSTAT/Trade_Indices_E_All_Data_(Normalized).csv",
                                select = c( "Area Code (M49)", "Area",
                                            "Item Code (CPC)", "Item",
                                            "Element Code", "Element",
                                            "Year Code", "Value" ))


setnames(data_FS, old = c("Area Code (M49)",  "Item Code (CPC)", "Item", "Element Code", "Year Code", "Value"),
         new = c("geographicAreaM49","measuredItemCPC","Item Name", "measuredElementTrade", "timePointYears", "FS_Value" ))

data_FS[, geographicAreaM49 := gsub(pattern = "'", replacement = "", geographicAreaM49)]
data_FS[, geographicAreaM49 := sub(pattern = "^0+",replacement = "", geographicAreaM49)]
data_FS[, measuredItemCPC := gsub(pattern = "'", replacement = "", measuredItemCPC)]

data_FS <- data_FS[timePointYears %in% key_year]

compare <- merge(data_FS,
                 ELEMENTS,
                 by = c("measuredElementTrade",
                        "timePointYears",
                        "geographicAreaM49",
                        "measuredItemCPC"),
                 all.y = TRUE)
compare[, diff := round(as.numeric(FS_Value) - round(Value))]
View(compare[!is.na(diff) & diff != 0])
# View(compare[!is.na(diff) & `Item Name`=='Barley'])
# View(compare[!is.na(diff) & diff != 0 & `Item Name`=='Barley'])
