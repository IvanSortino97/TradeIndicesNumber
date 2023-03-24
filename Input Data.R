# import item list from SWS Datatable

TI_item_list <- ReadDatatable("ti_item_list")
TI_multipliers <- ReadDatatable("ti_fob_cif_multipliers")

# Convert to CPC

tcl_code <- str_pad(TI_item_list$tcl_code,4, pad="0")
tcl_code.cpc <- fcl2cpc(tcl_code)
#tcl_code.cpc <- tcl_code.cpc %>% gsub(pattern = "'", replacement = "") %>% na.exclude() %>% as.character()
TI_item_list[, CPC_code := tcl_code.cpc]
#View(TI_item_list[is.na(CPC_code)])  # Missing CPC Codes
tcl_code.cpc <- tcl_code.cpc %>%  na.exclude() %>% as.character()

#Import Trade Data From SWS 

data_FS = fread("~/TradeIndicesNumber/FAOSTAT/data_FS_1419.csv",
                select = c( "Area Code (M49)", "Area",           
                            "Item Code (CPC)", "Item",           
                            "Element Code", "Element",
                            "Year Code", "Value" ))
setnames(data_FS, old = c("Area Code (M49)",  "Item Code (CPC)", "Item", "Element Code", "Year Code", "Value"),
                  new = c("geographicAreaM49","measuredItemCPC","Item Name", "measuredElementTrade", "timePointYears", "FS_Value" ))

data_FS[, geographicAreaM49 := gsub(pattern = "'", replacement = "", geographicAreaM49)]
data_FS[, geographicAreaM49 := sub(pattern = "^0+",replacement = "", geographicAreaM49)]
data_FS[, measuredItemCPC := gsub(pattern = "'", replacement = "", measuredItemCPC)]

TI_countries <- fread("~/TradeIndicesNumber/FAOSTAT/TI-country-regions.csv")
TI_countries_group <- fread("~/TradeIndicesNumber/FAOSTAT/TI-region aggregation.csv")
country_group_names <- unique(TI_countries_group$`Country Group`)

# Items from Definition and Standard - TI FAOSTAT - 196 elements
#TI_items <- fread("~/TradeIndicesNumber/FAOSTAT/TI-items.csv")




key_geo = TI_countries$`M49 Code` %>% 
            as.character() %>% 
            sub(pattern = "^0+",replacement = "") 
key_geo = key_geo[! key_geo %in% "97"]

key_elem = c('5610','5622','5910','5922')

key_item = tcl_code.cpc

key_year = data_FS$`Year Code` %>% unique()

data_SWS = SWS_data(domain = "trade",
                    dataset = "total_trade_cpc_m49",
                    keys = list(key_geo,key_elem,key_item,key_year))
data_check = merge(data_FS,data_SWS, by = c("geographicAreaM49",
                                            "measuredElementTrade",
                                            "measuredItemCPC",
                                            "timePointYears"), all.x = T)
# Remove regions by names
data_check <- data_check[! Area %in% country_group_names, ]
data_check <- data_check[,FS_Value := as.numeric(FS_Value)]
data_check <- data_check[FS_Value != 0]
data_check <- data_check[measuredElementTrade %in% key_elem]
data_check <- data_check[measuredItemCPC %in% key_item]
data_check[, diff := round(FS_Value - Value)] 
View(data_check[is.na(Value) ,])
unique(data_check[is.na(Value),]$Area)
# keys <- list(key_geo,key_elem,key_item,key_year)
# keys <- sapply(keys, '[', seq(max(sapply(keys, length))))
# write.csv(keys, "keys.csv" )

