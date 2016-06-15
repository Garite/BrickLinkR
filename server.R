lapply(list("XML", "httr", "data.table", "dplyr", "tidyr", "ggplot2"), function(x) library(x, character.only=T))

source("tokens.R") # hidden

get_baseURL <- function(site="bricklink") if(site=="bricklink") "https://api.bricklink.com/api/store/v1/" else stop("Invalid site.")

site_access <- function(app_name, consumer_key, consumer_secret, token, token_secret){
    app <- oauth_app(app_name, key=consumer_key, secret=consumer_secret)
    list(app=app, token=token, token_secret=token_secret)
}

blkey <- site_access("bricklinkR", consumer_key, consumer_secret, token, token_secret)

colpal <- c("dodgerblue2", "orange2", "black")

# @knitr get_bl
# Make data request to API
get_bl <- function(site_key, uri, pars=list(), base_url=get_baseURL()){
    pars <- if(length(pars)==0) "?" else paste(names(pars), pars, sep="=", collapse="&")
    o <- oauth_signature(paste0(base_url, uri, "?", pars), app=site_key$app, token=site_key$token, token_secret=site_key$token_secret)
    o$oauth_signature <- RCurl::curlEscape(o$oauth_signature)
    o$oauth_timestamp <- as.character(o$oauth_timestamp)
    o <- paste(paste0("\"", names(o), "\""), paste0("\"", o, "\""), sep=":", collapse=",")
    if(pars!="") pars <- paste0(pars, "&")
    x <- GET(paste0(base_url, uri, paste0("?", pars, "Authorization={", o, "}")))
    x <- content(x, type="application/json")
    x
}

# @knitr support_functions
# Extract message in metadata of returned results
meta_message <- function(x) x$meta$message

# @knitr get_cart
# get shopping cart as data frame from offline xml file. Does not require API.
get_cart <- function(file){
    require(XML)
    data <- xmlParse(file)
    lev <- c("stock", "sold", "cart")
    d <- xmlToDataFrame(nodes=getNodeSet(data, "//BrickStockXML/Inventory/Item")) %>% data.table %>%
        mutate(Condition=ifelse(Condition=="N", "New", "Used"), ItemID=as.character(ItemID), Price=as.numeric(as.character(Price)), Qty=as.integer(as.character(Qty)), Listing=factor("cart", levels=lev), PQ=Price/Qty) %>%
        select(Condition, ItemTypeID, ItemID, ColorID, Qty, Price, Listing, PQ) %>%
        group_by(Condition, ItemTypeID, ItemID, ColorID) %>% arrange(Condition, ItemTypeID, ItemID, ColorID)
    setnames(d, sapply(names(d), function(x) if(x %in% c("ItemTypeID", "ItemID", "ColorID", "Qty", "Price"))
        switch(x, ItemTypeID="type", ItemID="no", ColorID="color_id", Qty="quantity", Price="unit_price") else x))
    d$type <- sapply(as.character(d$type), function(x) switch(x, P="part", M="minifig", S="set", I="set")) # BrickLink API or BrickStock bug: sets show as "I" instead of "S"
    d %>% group_by
}

# @knitr pg_functions
# Priceguide-related wrapper functions for get_bl
get_pg <- function(site_key, type, no, pars=list()){
    types <- c("MINIFIG", "PART", "SET")
    if(!(tolower(type) %in% tolower(types))) stop("Type must be one of minifig, part, or set, case-insensitive.")
    if(!is.character(no)) stop("Item id number, 'no', must be a character string.")
    guide_type <- if(is.null(pars$guide_type)) "stock" else pars$guide_type
    color_id <- if(is.null(pars$color_id)) NA else pars$color_id
    cond <- if(is.null(pars$new_or_used)) "New" else pars$new_or_used
    cond <- switch(cond, "N"="New", "U"="Used")
    uri <- file.path("items", type, no, "price")
    x <- get_bl(site_key, uri, pars)
    if(meta_message(x)!="OK") return(meta_message(x))
    x <- rbindlist(x$data$price_detail)
    if(is.null(x$unit_price)) return("Insufficient data.")
    x <- mutate(x, unit_price=as.numeric(unit_price), type=type, no=no, color_id=color_id, Condition=cond)[]
    lev <- c("stock", "sold", "cart")
    x <- mutate(x, Listing=factor(guide_type, levels=lev), PQ=unit_price/quantity) %>% select(-qunatity) # BrickLink database typo
    x
}

get_sold <- function(site_key, type, no, pars=list(), ...){
    daily <- list(...)$daily
    pars$guide_type <- "sold"
    x <- get_pg(site_key, type, no, pars)
    if(is.data.table(x) && (is.null(daily) || daily)) x <- mutate(x, date_ordered=as.Date(date_ordered))[]
    x
}

get_stock <- function(site_key, type, no, pars=list()){
    pars$guide_type <- "stock"
    get_pg(site_key, type, no, pars)
}


get_soldAndStock <- function(site_key, type, no, pars=list(), ...){
    sold <- get_sold(site_key, type, no, pars)
    stock <- get_stock(site_key, type, no, pars)
    x <- c()
    if(is.data.table(sold) && is.data.table(stock)){
        x <- rbindlist(list(sold,stock), fill=TRUE)
    } else if(is.character(sold)) {
        x <- sold
    } else if(is.character(stock)) {
        x <- c(x, stock)
    }
    x
}

get_soldStockCart <- function(site_key, cart, ...){
    if(is.character(cart)) cart <- get_cart(cart)
    pars <- lapply(1:nrow(cart), function(i, x) list(color_id=x$color_id[i], new_or_used=substr(x$Condition[i], 1, 1), country_code="US", region="north_america"), x=cart)
    pg <- lapply(1:nrow(cart), function(i, type, no, pars, ...) get_soldAndStock(blkey, type=type[i], no=no[i], pars=pars[[i]]),
        blkey=blkey, type=cart$type, no=cart$no, pars=pars)
    pg <- rbindlist(pg[sapply(pg, function(x) !is.character(x))])
    rbindlist(list(pg, cart), fill=TRUE)
}

get_colortable <- function(site_key){
    x <- get_bl(site_key, "colors")
    if(meta_message(x)!="OK") return(x)
    x <- rbindlist(x$data)
    mutate(x, color_code=paste0("#", color_code))[]
}

get_colorsknown <- function(site_key, type, no){
    uri <- file.path("items", type, no, "colors")
    x <- get_bl(site_key, uri)
    if(meta_message(x)!="OK") return(x)
    rbindlist(x$data) %>% select(-quantity)
}

get_categories <- function(site_key){
    x <- get_bl(site_key, "categories")
    if(meta_message(x)!="OK") return(x)
    rbindlist(x$data)
}

colortable <- get_colortable(blkey)

# @knitr BL_xml_function
get_BLxml <- function(file){
    require(XML)
    xmlToDataFrame(nodes=getNodeSet(xmlParse(file), "/INVENTORY/ITEM")) %>% data.table %>%
        mutate(QTY=as.numeric(as.character(QTY)), PRICE=as.numeric(as.character(PRICE)))
}

toBLxml <- function(d){
    stopifnot(is.data.frame(d))
    if(!is.data.table(d) && is.data.frame(d)) d <- data.table(d)
    xml <- xmlTree()
    xml$addTag("INVENTORY", close=FALSE)
    for (i in 1:nrow(d)){
        xml$addTag("ITEM", close=FALSE)
        for (j in names(d)){
            x <- d[i, j, with=F]
            if(!is.na(x)) xml$addTag(j, x)
        }
        xml$closeTag()
    }
    xml$closeTag()
}

setMyCost <- function(x, cost=NULL, pct=NULL, out="xml"){
    if(is.null(cost) && is.null(pct)) stop("cost and pct cannot both be NULL.")
    if(is.character(x)) x <- get_BLxml(x)
    if("MYCOST" %in% names(x)) stop("MYCOST field already present.")
    x <- mutate(x, MYCOST=PRICE)
    price <- summarise(x, Total=sum(QTY*PRICE))$Total
    if(!is.null(cost)) pct <- cost/price
    x <- mutate(x, MYCOST=round(pct*PRICE, 3))
    if(out=="table") return(x)
    toBLxml(x)
}

shinyServer(function(input, output, session){

# reactive expressions
N_Conditions <- reactive({ if(input$pg_condition=="Both") 2 else 1 })

validItem <- reactive({
    x <- input$pg_itemno
    if(is.null(x) || x=="") return()
    if(input$pg_itemtype=="Part" && gsub("[a-zA-Z0-9]", "", x)=="") return(TRUE)
    if(input$pg_itemtype=="Minifig" && gsub("[a-zA-Z0-9]", "", x)=="") return(TRUE)
    if(input$pg_itemtype=="Set" && nchar(x)==nchar(gsub("-", "", x))+1 && substr(x, nchar(x)-1, nchar(x)-1)=="-") return(TRUE)
    return(FALSE)
})

ItemColorsKnown <- reactive({
    if(is.null(validItem()) || !validItem()) return()
    x <- get_colorsknown(blkey, type=input$pg_itemtype, no=input$pg_itemno)
    colortable$color_name[match(x$color_id, colortable$color_id)]
})

output$PG_itemcolor <- renderUI({
    x <- ItemColorsKnown()
    if(length(x) > 0) selectInput("pg_itemcolor", "Item colors", x, x[1], multiple=TRUE, width="100%") else NULL
})

ColorIDsSelected <- reactive({ input$pg_itemcolor })
N_Colors <- reactive({ length(ColorIDsSelected()) })
ColorsSelected <- reactive({ colortable$color_id[match(ColorIDsSelected(), colortable$color_name)] })

ParsList <- reactive({
    if(!(validItem() && input$pg_condition %in% item_conditions && input$pg_countrycode %in% countrycodes && input$pg_region %in% regions && input$pg_vat %in% vat_options)) return()
    x <- vector("list", N_Colors()*N_Conditions())
    clrs <- rep(ColorsSelected(), N_Conditions())
    clrs[is.na(clrs)] <- 0
    conds <- substr(input$pg_condition, 1, 1)
    if(conds=="B") conds <- c("N", "U")
    conds <- rep(conds, each=N_Colors())
    for(i in 1:length(x)){
        x[[i]] <- list(color_id=clrs[i], new_or_used=conds[i], country_code=input$pg_countrycode, region=gsub(" ", "_", tolower(input$pg_region)), vat=switch(input$pg_vat,"Exclude"="N", "Include"="Y", "Norway"="O"))
        x[[i]] <- x[[i]][sapply(x[[i]], function(x) !is.null(x) && x!="")]
    }
    x
})

TypeVec <- reactive({ rep(input$pg_itemtype, N_Colors()*N_Conditions()) })
NoVec <- reactive({ rep(input$pg_itemno, N_Colors()*N_Conditions()) })

InFilesExist_PG <- reactive({ length(input$infile_pg) && !any(input$infile_pg=="") })
InFilesExist_MC <- reactive({ length(input$infile_mc) && !any(input$infile_mc=="") })

# priceguide data tables
PG_data <- reactive({
    if(InFilesExist_PG()){
        x <- lapply(input$infile_pg$datapath, get_cart)
        if(all(sapply(x, is.character))) return(x[[1]])
        x <- get_soldStockCart(blkey, rbindlist(x))
    } else if(N_Colors()==0) {
        return()
    } else {
        x <- lapply(1:length(ParsList()), function(i, type, no, pars, ...) get_soldAndStock(site_key=blkey, type=type[i], no=no[i], pars=pars[[i]]),
            blkey=blkey, type=TypeVec(), no=NoVec(), pars=ParsList())
        ischar <- sapply(x, is.character)
        if(all(ischar)) return(x[[1]])
        if(any(ischar)) x <- x[!ischar]
        x <- rbindlist(x)
    }
    x <- arrange(x, Listing, type, Condition, no, color_id, unit_price, quantity, date_ordered)
    x <- group_by(x, Listing, type, Condition, no, color_id) %>% mutate(Color=colortable$color_name[match(color_id, colortable$color_id)])
    #x <- group_by(x, Listing, type, Condition, no, color_id) %>% mutate(Color=colortable$color_name[match(color_id, colortable$color_id)], Color=ifelse(is.na(Color), "Color N/A", Color)) # Change Color NA to string
    setcolorder(x, c("type", "no", "Color", "Condition", "Listing", "quantity", "unit_price", "PQ", "seller_country_code", "buyer_country_code", "date_ordered", "color_id", "shipping_available"))
    x
})

PG_summary <- reactive({
    if(is.null(PG_data()) || is.character(PG_data())) return()
    x <- filter(PG_data(), Listing=="sold") %>% group_by(type, Condition, no, Color) %>%
        summarize(Sales_value=sum(quantity*unit_price), Sales_days=max(as.numeric(Sys.Date()-date_ordered)), Sales_PQ_mean=mean(PQ), Sales_PQ_min=min(PQ)) %>%
        left_join( filter(PG_data(), Listing=="stock") %>% group_by(type, Condition, no, Color) %>% summarize(Supply_value=sum(quantity*unit_price), Supply_PQ_mean=mean(PQ), Supply_PQ_min=min(PQ)) )
    if(InFilesExist_PG()) x <- left_join( x, filter(PG_data(), Listing=="cart") %>% group_by(type, Condition, no, Color) %>% summarize(Cart_value=sum(quantity*unit_price), Cart_PQ=mean(PQ)) )
    x <- mutate(x, Value_per_day=Sales_value/Sales_days, Supply_days=Supply_value/Value_per_day,
            SalSupDays_Ratio=Sales_days/Supply_days, SalSupPQmean_Ratio=Sales_PQ_mean/Supply_PQ_mean, SalSupPQmin_Ratio=Sales_PQ_min/Supply_PQ_min)
    cols <- c("type", "no", "Color", "Condition", "Sales_value", "Supply_value", "Value_per_day", "Supply_days", "SalSupDays_Ratio", "SalSupPQmean_Ratio", "SalSupPQmin_Ratio")
    if(InFilesExist_PG()){
        x <- mutate(x, `Mean Cart`=Sales_PQ_mean/Cart_PQ, `Max Cart`=Sales_PQ_min/Cart_PQ)
        cols <- c(cols, c("Cart_value", "Cart_PQ", "Mean Cart", "Max Cart"))
        x <- select(x, type, no, Color, Condition, Sales_value, Supply_value, Value_per_day, Supply_days, SalSupDays_Ratio, SalSupPQmean_Ratio, SalSupPQmin_Ratio, Cart_value, Cart_PQ, `Mean Cart`, `Max Cart`) %>% data.table
    } else {
        x <- select(x, type, no, Color, Condition, Sales_value, Supply_value, Value_per_day, Supply_days, SalSupDays_Ratio, SalSupPQmean_Ratio, SalSupPQmin_Ratio) %>% data.table
    }
    setcolorder(x, cols)
    arrange(x, desc(SalSupDays_Ratio), type, Condition, no, Color)
})

output$PriceByQty_text <- renderUI({ if(is.character(PG_data())) h4(PG_data()) else NULL })


PG_data_filtered <- reactive({
    if(is.null(PG_data())) return()
    if(is.character(PG_data())) return(PG_data())
    PG_data()[input$PG_table_full_rows_all,]
})

PG_data_cdf <- reactive({
    if(is.null(PG_data_filtered()) || is.character(PG_data_filtered())) return()
    f <- function(data, newdata){
        x <- filter(newdata, type==data$type & Condition==data$Condition & no==data$no)
        x <- if(is.na(data$Color)) filter(x, is.na(Color)) else filter(x, Color==data$Color)
        x %>% group_by %>% select(Listing, unit_price, quantity)
    }
    x <- filter(PG_data_filtered(), Listing!="cart") #%>% group_by(type, Condition, no, Color, Listing, unit_price) %>% summarise(quantity=sum(quantity)) %>% group_by
    x <- filter(PG_data_filtered(), Listing=="cart") %>% mutate(`Cart Price`=unit_price) %>% group_by(type, Condition, no, Color, `Cart Price`) %>% do(PG=f(., x)) %>% group_by
    x
})

PG_data_cdf_summary <- reactive({
    if(is.null(PG_data_cdf())) return()
    delta <- 0.0001
    pnames <- c("P(lot < cart)", "P(lot sale < cart)", "P(item < cart)", "P(item sale < cart)")
    x <- unnest(PG_data_cdf(), PG) %>% group_by(type, Condition, no, Color, Listing) %>% mutate(CQty=cumsum(quantity)) %>%# group_by(Listing, add=T) %>%
        summarise(P1=ecdf(unit_price)(`Cart Price`[1]-delta), P2=ecdf(rep(unit_price, times=quantity))(`Cart Price`[1]-delta))
    x1 <- dcast(x, type + Condition + no + Color ~ Listing, value.var="P1")
    x2 <- dcast(x, type + Condition + no + Color ~ Listing, value.var="P2")
    setnames(x1, sapply(names(x1), function(x) if(x %in% c("stock", "sold")) switch(x, stock=pnames[1], sold=pnames[2]) else x))
    setnames(x2, sapply(names(x2), function(x) if(x %in% c("stock", "sold")) switch(x, stock=pnames[3], sold=pnames[4]) else x))
    x1 %>% bind_cols(x2 %>% select(`P(item < cart)`, `P(item sale < cart)`)) %>% data.table
})

# BL xml mass upload/update data for setting MYCOST field
MC_data <- reactive({
    if(InFilesExist_MC()){
        x <- lapply(input$infile_mc$datapath, get_BLxml)
        stopifnot(all(sapply(x, is.data.table)))
        x <- rbindlist(x)
    } else x <- NULL
    x
})

MC_data2 <- reactive({
    d <- MC_data()
    m <- input$mc_method
    tot <- input$mc_total
    pct <- input$mc_pct
    if(is.null(d) || is.null(m) || is.null(tot) || is.null(pct)) return()
    if(m=="Total cost" && is.na(tot)) return() else if(m=="Total cost") pct <- NULL
    if(m=="Price percentage" && is.na(pct)) return() else if(m=="Price percentage") tot <- NULL
    setMyCost(d, cost=tot, pct=pct)
})

MC_data2b <- reactive({
    d <- MC_data()
    m <- input$mc_method
    tot <- input$mc_total
    pct <- input$mc_pct
    if(is.null(d) || is.null(m) || is.null(tot) || is.null(pct)) return()
    if(m=="Total cost" && is.na(tot)) return() else if(m=="Total cost") pct <- NULL
    if(m=="Price percentage" && is.na(pct)) return() else if(m=="Price percentage") tot <- NULL
    setMyCost(d, cost=tot, pct=pct, out="table")
})

# reactive outputs
# output plots
output$PriceByQty_plot <- renderPlot({
    d <- PG_data_filtered()
    if(is.null(d) || is.character(d) || nrow(d)==0) return()
    d$grp <- paste(d$no, d$Color, d$Condition)
    grps <- unique(d$grp)
    max.panels <- 9
    if(length(grps) > max.panels) d <- filter(d, grp %in% grps[1:max.panels])
    ggplot(d, aes(quantity, unit_price, shape=Listing, colour=Listing)) + geom_point(size=3, position=position_jitter()) + facet_wrap(no ~ Condition + Color, scales="free") +
        theme_grey(base_size=16) + theme(legend.position="bottom", legend.box="horizontal") + labs(x="Quantity", y="Price") +
        scale_colour_manual(values=colpal)
}, height=function(){ 0.7*session$clientData$output_PriceByQty_plot_width }, width="auto")

output$CQtyByPrice_plot <- renderPlot({
    d <- PG_data_cdf()
    if(is.null(d)) return()
    d <- unnest(d, PG) %>% group_by(type, Condition, no, Color, Listing) %>% mutate(CQty=cumsum(quantity))
    d$grp <- paste(d$no, d$Color, d$Condition)
    grps <- unique(d$grp)
    max.panels <- 9
    if(length(grps) > max.panels) d <- filter(d, grp %in% grps[1:max.panels])
    ggplot(d, aes(unit_price, CQty, colour=Listing)) +
        geom_line() + geom_point(data=d %>% filter(unit_price==`Cart Price`), size=1) +
        facet_wrap(no ~ Condition + Color, scales="free") +
        theme_grey(base_size=16) + theme(legend.position="bottom", legend.box="horizontal") + labs(x="Price", y="Cumulative quantity") +
        scale_colour_manual(values=colpal)
}, height=function(){ 0.7*session$clientData$output_CQtyByPrice_plot_width }, width="auto")

output$CDFqtiles_plot <- renderPlot({
    d <- PG_data_cdf_summary()
    if(is.null(d)) return()
    d <- melt(d, id.vars=c("type", "Condition", "no", "Color"), variable.name="Metric", value.name="Probability")
    d$grp <- paste(d$no, d$Color, d$Condition)
    grps <- unique(d$grp)
    max.panels <- 9
    if(length(grps) > max.panels) d <- filter(d, grp %in% grps[1:max.panels])
    ggplot(d, aes(x=Metric, y=Probability, colour=Metric, fill=Metric)) + geom_bar(stat="identity", colour="black") +
        facet_wrap(no ~ Condition + Color, scales="free") +
        theme_grey(base_size=16) + theme(legend.position="bottom", legend.box="horizontal") + labs(x="Metric", y="P(X < x)") +
        scale_fill_manual(values=c("dodgerblue2", "orange2", "dodgerblue4", "orange4")) + scale_x_discrete(breaks=NULL) + ylim(0, 1)
}, height=function(){ 0.7*session$clientData$output_CDFqtiles_plot_width }, width="auto")

# output tables
output$PG_table_full <- renderDataTable({
    if(!is.null(PG_data())) PG_data() %>%
    select(-seller_country_code, -buyer_country_code, -date_ordered, -color_id, -shipping_available) %>%
    datatable(style="bootstrap", filter="bottom", rownames=FALSE, colnames=c('Type'=1, 'No'=2, 'Cond'=4, 'Qty'=6, 'Price'=7, 'PQR'=8),
        caption=htmltools::tags$caption(style='caption-side: bottom; text-align: center;', 'Table 1: ', htmltools::em('Price guide and shopping cart data.')),
        options=list(lengthMenu=list(c(5, 10, 20, -1), c('5', '10', '20', 'All')), pageLength=5, orderClasses=TRUE)) %>%
    formatCurrency(c('Price')) %>% formatRound(c('PQR'), 4)
})

output$PG_table_summary <- renderDataTable({
    if(is.null(PG_summary())) return()
    x <- PG_summary()
    if("Cart_value" %in% names(x)){
        tailnames <- c('Day Ratio', 'Mean Demand', 'Max Demand', 'Mean Cart', 'Max Cart')
        x <- select(x, -Sales_value, -Supply_value, -Cart_value, -Cart_PQ)
    } else {
        tailnames <- c('Day Ratio', 'Mean Demand', 'Max Demand')
        x <- select(x, -Sales_value, -Supply_value)
    }
    x %>% datatable(style="bootstrap", rownames=FALSE, colnames=c('Type'=1, 'No'=2, 'Cond'=4, 'VPD'=5, 'Day Supply'=6, 'Day Ratio'=7, 'Mean Demand'=8, 'Max Demand'=9),
        caption=htmltools::tags$caption(style='caption-side: bottom; text-align: center;', 'Table 2: ', htmltools::em('Price guide and shopping cart summary.')),
        options=list(lengthMenu=list(c(5, 10, 20, -1), c('5', '10', '20', 'All')), pageLength=5, orderClasses=TRUE)) %>%
        formatCurrency(c('VPD')) %>% formatRound('Day Supply', 0) %>% formatRound(tailnames, 2)
})

#output$PG_table_filtered <- renderDataTable({
#    if(is.null(PG_data_filtered()) || is.character(PG_data_filtered())) return()
#    PG_data_filtered() %>% select(-seller_country_code, -buyer_country_code, -date_ordered, -color_id, -shipping_available) %>%
#    datatable(style="bootstrap", filter="bottom", rownames=FALSE, colnames=c('Type'=1, 'No'=2, 'Cond'=4, 'Qty'=6, 'Price'=7, 'PQR'=8),
#        caption=htmltools::tags$caption(style='caption-side: bottom; text-align: center;', 'Table 1: ', htmltools::em('Price guide and shopping cart data.')),
#        options=list(lengthMenu=list(c(5, 10, 20, -1), c('5', '10', '20', 'All')), pageLength=5, orderClasses=TRUE)) %>%
#    formatCurrency(c('Price')) %>% formatRound(c('PQR'), 4)
#})

output$PG_table_cdf <- renderDataTable({
    if(is.null(PG_data_cdf_summary())) return()
    PG_data_cdf_summary() %>%
    datatable(style="bootstrap", rownames=FALSE, colnames=c('Type'=1, 'Cond'=2, 'No'=3, 'Color'=4),
        caption=htmltools::tags$caption(style='caption-side: bottom; text-align: center;', 'Table 3: ', htmltools::em('Shopping cart PG CDF quantiles.')),
        options=list(lengthMenu=list(c(5, 10, 20, -1), c('5', '10', '20', 'All')), pageLength=5, orderClasses=TRUE)) %>%
    formatRound(names(PG_data_cdf_summary())[-c(1:4)])
})

# PDF download buttons for each plot
#output$dl_PriceByQty_plotPDF <- downloadHandler(
#	filename='PriceByQty.pdf',
#	content=function(file){	pdf(file=file, width=10, height=7, pointsize=8); doPlot_PriceByQty(); dev.off() }
#)

output$BL_XML_MC_input <- renderDataTable({
    if(!is.null(MC_data2b())) MC_data2b() %>%
    datatable(style="bootstrap", filter="bottom", rownames=FALSE,
        caption=htmltools::tags$caption(style='caption-side: bottom; text-align: center;', 'Table 1: ', htmltools::em('BrickLink XML input with added MYCOST field.')),
        options=list(lengthMenu=list(c(5, 10, 20, -1), c('5', '10', '20', 'All')), pageLength=5, orderClasses=TRUE)) %>%
    formatCurrency(c('PRICE'))
})

MC_xml_tmp <- reactive({
    if(!is.null(MC_data2())){
        saveXML(MC_data2(), file="bl_xml_mycost_tmp.xml", prefix=NULL, encoding="UTF-8")
        x <- readLines("bl_xml_mycost_tmp.xml")
        x <- paste0(x, collapse="\n")
        cat(x)
        return(x)
    } else NULL
})
observe(MC_xml_tmp())

output$BL_XML_MC_output <- renderText({
    MC_xml_tmp()
})

})
