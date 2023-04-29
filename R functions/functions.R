### Functions ###


`%!in%` = Negate(`%in%`)


# Regions aggregate --------------------------------------------------------

regions_aggregate <- function(data_SWS, TI_countries_group) {
    atomic.merge <- function(grp, df1, df2, var) {
        Merge <- merge(df1,
                       df2[var_group_code == grp],
                       by = c("geographicAreaM49"),
                       #all.y = T,
                       allow.cartesian = TRUE)
        
        Merge <- Merge[, Value := sum(Value, na.rm = T),
                       by = c(
                           "var_group_code",
                           "measuredElementTrade",
                           "timePointYears",
                           "measuredItemCPC"
                       )]
        
        Merge[, geographicAreaM49 := var_group_code]
        Merge[, var_group_code := NULL]
        Merge <- unique(Merge)
        
        return(Merge)
    }
    
    TI_countries_group <-
        split(TI_countries_group, TI_countries_group$var_group_code)
    
    
    data_regions <- lapply(names(TI_countries_group),
                           function(x)
                               atomic.merge(x, data_SWS, TI_countries_group[[x]]))
    data_regions <- rbindlist(data_regions)
    
    return(data_regions)
    
}


# items aggregates --------------------------------------------------------

items_aggregate <- function(data_SWS, TI_item_group) {
    data_item.aggr <- merge(data_SWS[, .(
        measuredElementTrade,
        geographicAreaM49,
        measuredItemCPC,
        timePointYears,
        Value
    )],
    TI_item_group,
    by = "measuredItemCPC",
    allow.cartesian = TRUE)
    
    # Applying multipliers for aggregation
    
    for (i in c('5610', '5910', '5622', '5922')) {
        if (i %in% factor_diff) {
            data_item.aggr[measuredElementTrade == i , Value := Value * as.numeric(get(i))]
        }
    }
    
    data_item.aggr[measuredElementTrade %!in% factor_diff, Value := Value * as.numeric(Default)]
    
    data_item.aggr <-
        data_item.aggr[, list(Value = sum(Value, na.rm = T)),
                       by = c(
                           "measuredElementTrade",
                           "geographicAreaM49",
                           "timePointYears",
                           "var_group_code"
                       )]
    
    setnames(data_item.aggr, "var_group_code", "measuredItemCPC")
    
    return(data_item.aggr)
}
