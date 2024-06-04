library(shinydashboard)
library(shiny)
library(ggplot2) 
library(scales)
library(DT)
library(ggpubr)
library(purrr) 
library(dplyr) 
library(vtable)
library(gtsummary)
library(gt)
library(data.table)
library(viridis)
library("gridExtra")
library(stringr)
# https://rstudio.github.io/shinydashboard/structure.html
# https://rstudio.github.io/shinydashboard/appearance.html#sidebar-width
# https://github.com/rstudio/shiny-examples/tree/main/048-including-html-text-and-markdown-files
ui <- dashboardPage(
  dashboardHeader(title = "Chicken Genome Atlas",titleWidth = 250),
  ## Sidebar content
    dashboardSidebar( width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("KnockoutGeneNGS", tabName = "mousedashboard", icon = icon("dashboard")),
      menuItem("ChickenGeneNGS", tabName = "chickendashboard", icon = icon("dashboard")),
      menuItem("ChickenGeneHub", tabName = "genedashboard", icon = icon("dashboard")),
      menuItem("ChickenSVHub", tabName = "svdashboard", icon = icon("dashboard")),
      menuItem("ChickenQTLHub", tabName = "qtldashboard", icon = icon("dashboard")),
      menuItem("PanGenome", tabName = "panGenomedashboard", icon = icon("dashboard")),
      menuItem("MetaGenomics", tabName = "metaGenomicsdashboard", icon = icon("dashboard")),
      menuItem("ChickenGPT", tabName = "GPTdashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
## Body content
  dashboardBody(
    tabItems(
      # First tab content
      # tabItem(tabName = "dashboard",
        # fluidRow(
          # box(plotOutput("plot1", height = 250)),

          # box(
            # title = "Controls",
            # sliderInput("slider", "Number of observations:", 1, 100, 50)
          # )
        # )
      # ),

    tabItem(tabName = "dashboard",
      h2("Chicken Genome Atlas : Navigating the Secrets of Chickens (Under Development)")
    ),
    tabItem(tabName = "mousedashboard",
      h2("NGS variant distribution (chicken) on mouse knockout genes"),
      DT::dataTableOutput("mytablemouse")	  
    ),
    tabItem(tabName = "chickendashboard",
      h2("NGS variant distribution (chicken) on genes related with chicken phenotype"),
      DT::dataTableOutput("mytablechicken")
    ),	
    tabItem(tabName = "genedashboard",
      h2("Chicken Gene Information Hub") ,
      DT::dataTableOutput("mytablegene")  #,
	#  h6("Gene information : https://www.ncbi.nlm.nih.gov/gene/ID")
	  # add a csv table. it is very small. search. 
	    # fluidRow(
			# column(4,
				# includeHTML("gene_atlas_galgal7.html")
				# )
			# )
    ),	
    tabItem(tabName = "svdashboard",
      h2("Chicken Structure Variant Hub (Under Development)") ,
      #DT::dataTableOutput("mytablegene")  
    ),		
    tabItem(tabName = "qtldashboard",
      h2("Chicken QTL Information Hub (Under Development)") 
      # DT::dataTableOutput("mytablegene")  #,
    ),		
	tabItem(tabName = "panGenomedashboard",
      h2("Chicken Pan Genome (Under Development)") #,
		# includeMarkdown("pan-genome/chicken-pan-genome.md")	  
    ),
	tabItem(tabName = "metaGenomicsdashboard",
      h2("Chicken MetaGenomics (Under Development)") #,
		# includeMarkdown("pan-genome/chicken-pan-genome.md")	  
    ),	
	tabItem(tabName = "GPTdashboard",
      h2("AI Unravels the Chicken's Hidden Secrets (Under Development)")
    ),	
      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
  )
)
# https://rstudio.github.io/DT/options.html
server <- function(input, output) {
  # set.seed(122)
  # histdata <- rnorm(500)

  # output$plot1 <- renderPlot({
    # data <- histdata[seq_len(input$slider)]
    # hist(data)
  # })
  samplesetChicken<- fread(file="/biotech/Jun/NGS/chicken.gene.subset.full.txt",head=TRUE,sep="\t")
  geneInfo<- fread(file="/biotech/Jun/NGS/gene_convertor.csv",head=TRUE,sep=",")
  samplesetMouse<- fread(file="/biotech/Jun/NGS/mouse.gene.subset.full.txt",head=TRUE,sep="\t")
  #  mouse data is out of memory   


	geneInfo = geneInfo %>%
	mutate( link =  paste0('<a  target=_blank href=', paste("https://www.ncbi.nlm.nih.gov/gene/",EntrezgeneID,sep=""), '>', EntrezgeneID,'</a>' ) )
    geneInfo$link[which(is.na(geneInfo$EntrezgeneID))]=""
# http://useast.ensembl.org/Gallus_gallus/Gene/Summary?db=core;g=ENSGALG00010000002;r=MT:1-69;t=ENSGALT00010000002
# paste("http://useast.ensembl.org/Gallus_gallus/Gene/Summary?db=core;g=","ENSGALG00010000002",";r=","MT",":","1","-","69",";t=","ENSGALT00010000002",sep="")

# colnmaes.
# Gene stable ID,Gene stable ID version,Gene description,Chromosome/scaffold name,Gene start (bp),Gene end (bp),Gene name,Source of gene name,Gene type,Source (gene),Version (gene),Gene Synonym,Phenotype description,EntrezgeneID

  output$mytablechicken <- DT::renderDataTable(server = TRUE, {

      # goodSampleplot=goodSample()
	   goodSampleplot=samplesetChicken
	   # goodSampleplot[,13:24]=round(goodSampleplot[,13:24],3)
	  DT::datatable(
	    # goodSampleplot[,c(1:12,)],
	    goodSampleplot,
		options = list(
		columnDefs = list(list(className = 'dt-center', targets = 5)),
		pageLength = 10,
		lengthMenu = c(5, 20, 50, 100),scrollX='400px',searchHighlight = TRUE
		),filter = 'top',rownames = F		
		)	   
	})

  output$mytablemouse <- DT::renderDataTable(server = TRUE, {

      # goodSampleplot=goodSample()
	   goodSampleplot=samplesetMouse
	   # goodSampleplot[,13:24]=round(goodSampleplot[,13:24],3)
	  DT::datatable(
	    # goodSampleplot[,c(1:12,)],
	    goodSampleplot,
		options = list(
		columnDefs = list(list(className = 'dt-center', targets = 5)),
		pageLength = 10,
		lengthMenu = c(5, 20, 50, 100),scrollX='400px',searchHighlight = TRUE
		),filter = 'top',rownames = F		
		)	   
	})
	
  output$mytablegene <- DT::renderDataTable(server = FALSE, {
	   goodSampleplot=geneInfo
	  DT::datatable(
	    goodSampleplot[,c(4,5,6,1,3,7,9,12,13,15)],
		options = list(
		columnDefs = list(list(className = 'dt-center', targets = 5)),
		pageLength = 10,
		lengthMenu = c(5, 20, 50, 100),scrollX='400px',searchHighlight = TRUE
		),filter = 'top',rownames = F,escape = F		
		)	   
	})	  # https://rpubs.com/erblast/369527 add link 
  
}

shinyApp(ui, server)

# change the color for text title. 
# https://rstudio.github.io/DT/
# library(markdown)
# https://stackoverflow.com/questions/30765338/how-to-make-the-horizontal-scrollbar-visible-in-dtdatatable
# fluidPage(

  # titlePanel("includeText, includeHTML, and includeMarkdown"),

  # fluidRow(
    # column(4,
      # includeText("include.txt"),
      # br(),
      # pre(includeText("include.txt"))
    # ),
    # column(4,
      # includeHTML("include.html")
    # ),
    # column(4,
      # includeMarkdown("include.md")
    # )
  # )
# )

