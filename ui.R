shinyUI(navbarPage(theme=shinytheme("cosmo"),
	title=HTML('BrickLinkR'),
	tabPanel("Price ~ Qty", value="pg1"),
    tabPanel("Qty CDF(Price)", value="pg2"),
    tabPanel("BL xml MyCost", value="pg3"),
	windowTitle="BrickLinkR",
	collapsible=TRUE,
	id="tsp",
	conditionalPanel("input.tsp!='about'",
	fluidRow(
        conditionalPanel("input.tsp!='pg3'",
		column(4, h2("BrickLink Price Guide"), h3("Analyze item prices, shop smart"),
            wellPanel(
                fluidRow(
                    column(12, fileInput("infile_pg", "Upload .bsx files", accept=c(".bsx"), multiple=T, width="100%"))
                )
            ),
            wellPanel(
                fluidRow(
                    column(4, selectInput("pg_itemtype", "Item type", item_types, selected="Part", multiple=F, width="100%")),
                    column(4, textInput("pg_itemno", "Item number", width="100%")),
                    column(4, selectInput("pg_condition", "Condition", item_conditions, selected="New", multiple=F, width="100%"))
                ),
                uiOutput("PG_itemcolor")
            ),
            wellPanel(
                #selectInput("pg_itemcolor", "Item colors", "", "", multiple=TRUE, width="100%"),
                fluidRow(
                    column(4, selectInput("pg_countrycode", "Country code", countrycodes, selected="US", multiple=F, width="100%")),
                    column(4, selectInput("pg_region", "Region", regions, selected="North America", multiple=F, width="100%")),
                    column(4, selectInput("pg_vat", "VAT", vat_options, selected="Exclude", multiple=F, width="100%"))
                )
            )
        )
        ),
        conditionalPanel("input.tsp=='pg3'",
		column(4, h2("BrickLink XML"), h3("Set MyCost field for mass upload/update"),
            wellPanel(
                fluidRow(
                    column(12, fileInput("infile_mc", "Upload .xml files", accept=c(".xml"), multiple=T, width="100%"))
                )
            ),
            wellPanel(
                fluidRow(
                    column(4, selectInput("mc_method", "MyCost based on:", c("Total cost", "Price percentage"), selected="Total cost", multiple=F, width="100%")),
                    column(4, numericInput("mc_total", "Total cost", 1, width="100%")), # finish these lines; cost overrides percentage
                    column(4, numericInput("mc_pct", "Price percentage", 1, width="100%"))
                )
            )
        )
        ),
        conditionalPanel("input.tsp!='pg3'",
        column(8,
            uiOutput("PriceByQty_text"),
            conditionalPanel("input.tsp=='pg1'",
                dataTableOutput("PG_table_full"),
                plotOutput("PriceByQty_plot", width="100%", height="auto"),
                dataTableOutput("PG_table_summary")
            ),
            conditionalPanel("input.tsp=='pg2'",
                dataTableOutput("PG_table_cdf"),
                plotOutput("CQtyByPrice_plot", width="100%", height="auto"),
                plotOutput("CDFqtiles_plot", width="100%", height="auto")
            )
        )
        ),
        conditionalPanel("input.tsp=='pg3'",
            column(8,
                dataTableOutput("BL_XML_MC_input"), # develop this line
                verbatimTextOutput("BL_XML_MC_output") # develop this line
            ),
            tags$style(type='text/css', '#BL_XML_MC_output {background-color: #333333; color: #cccccc; font-size: 10px;}')
        )
	),
	br(),
    bsTooltip("pg_itemtype", "Three item types currently available.", "top", options = list(container="body")),
	bsTooltip("pg_itemno", "Enter a valid BrickLink item number, e.g., 3626b.", "top", options = list(container="body")),
	bsTooltip("pg_condition", "Item condition may be new, used, or both.", "top", options = list(container="body")),
    bsTooltip("pg_countrycode", "Country code and region must both be specified to filter BrickLink stores. Otherwise ignored.", "top", options = list(container="body")),
	bsTooltip("pg_region", "Country code and region must both be specified to filter BrickLink stores. Otherwise ignored.", "top", options = list(container="body")),
	bsTooltip("pg_vat", "Include Value-added tax in prices for VAT-enabled stores. Norway settings available.", "top", options = list(container="body"))#,
	)
))
