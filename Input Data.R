# import item list from SWS Datatable

TI_item_list <- ReadDatatable("ti_item_list")

# Convert to CPC
tcl_code <- str_pad(TI_item_list$tcl_code,4, pad="0")
tcl_code.cpc <- fcl2cpc(tcl_code)
TI_item_list[, CPC_code := tcl_code.cpc]
View(TI_item_list[is.na(CPC_code)])  # Missing CPC Codes

m49_code<- read.csv('~/TradeIndicesNumber/FAOSTAT/Trade_CropsLivestock_E_AreaCodes.csv')
m49_code<- m49_code$M49.Code
m49_code<- gsub(m49_code,pattern = "'", replacement = "")
m49_code<- sub("^0+","", as.character(m49_code))

#Import Trade Data From SWS 

data_SWS = SWS_data(domain = "trade",
                    dataset = "total_trade_cpc_m49",
                    keys = list(m49_code,
                                c("5610" , "5910"),
                                tcl_code.cpc,
                                as.character(2010:2012)))
data_SWS[, c("flagObservationStatus","flagMethod"):= NULL ]

data_FS = fread("~/TradeIndicesNumber/FAOSTAT/Trade_CropsLivestock_E_All_Data_(Normalized).csv",
                select = c( "Area Code (M49)", "Area",           
                            "Item Code (CPC)", "Item",           
                            "Element Code", "Element",
                            "Year Code", "Value" ))
# data_FS <-  data_FS[, .( "Area Code (M49)", "Area",           
#                          "Item Code (CPC)", "Item",           
#                          "Element Code", "Element",
#                          "Year Code", "Value" )]
