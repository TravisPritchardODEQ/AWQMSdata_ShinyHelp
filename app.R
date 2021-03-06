#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#




library(shiny)
library(AWQMSdata)


# Query out the valiid values ---------------------------------------------


if (!file.exists("query_cache.RData") |
    difftime(Sys.Date() , file.mtime("query_cache.RData") , units = c("days")) > 14) {
  
  if(!file.exists("query_cache.RData")){
    print("No initial query cache found.")
  } else {
    print(paste("Initial data queries ran", ceiling(difftime(Sys.Date() , file.mtime("query_cache.RData") , units = c("days"))), "days ago."))
  }
  
  
  
  print("Initial queires may take a few minutes ")
  
  chars <- AWQMS_Chars()
  chars <- chars$Char_Name
  chars <- sort(chars)
  
  station <- AWQMS_Stations()
  station <- station$MLocID
  station <- sort(station)
  
  projects <- AWQMS_Projects()
  projects <- projects$Project
  projects <- sort(projects)
  
  organization <- AWQMS_Orgs()
  organization <- organization$OrganizationID
  organization <- sort(organization)
  
  
  save(chars, station, projects, organization, file = "query_cache.RData")
} else {
  load("query_cache.RData")
  print(paste("Initial data queries ran", ceiling(difftime(Sys.Date() , file.mtime("query_cache.RData") , units = c("days"))), "days ago."))
}

stat_b <- c("30DADMean", "7DADM", "7DADmean", "7DADMin", "Delta", "Geometric Mean", "Maximum", "Mean",
            "Median", "Minimum", "MPN", "Standard", "Error", "Sum")

samp_media <- c('Water', 'Tissue', 'Soil', 'Sediment', 'Other', 'Habitat', 'Biological','Air' )

HUC8_Names <- c('Alsea', 'Alvord Lake', 'Applegate', 'Beaver-South Fork',
                'Brownlee Reservoir', 'Bully', 'Burnt', 'Chetco', 'Chief Joseph',
                'Clackamas', 'Coast Fork Willamette', 'Coos','Coquille',
                'Crooked-Rattlesnake',  'Donner und Blitzen',' Goose Lake',
                'Guano', 'Harney-Malheur Lakes', 'Illinois', 'Imnaha', 'Jordan',
                'Lake Abert', 'Little Deschutes','Lost', 'Lower Columbia', 'Lower Columbia-Clatskanie',
                'Lower Columbia-Sandy','Lower Crooked','Lower Deschutes', 'Lower Grande Ronde', 'Lower John Day',
                'Lower Malheur', 'Lower Owyhee', 'Lower Rogue', 'Lower Willamette', 'Mckenzie', 'Middle Columbia-Hood',
                'Middle Columbia-Lake Wallula', 'Middle Fork John Day', 'Middle Fork Willamette', 'Middle Owyhee',
                'Middle Rogue', 'Middle Snake-Payette', 'Middle Snake-Succor', 'Middle Willamette', 'Molalla-Pudding',
                'Necanicum', 'Nehalem', 'North Fork John Day', 'North Santiam', 'North Umpqua', 'Not Loaded', 'Powder',
                'Siletz-Yaquina', 'Siltcoos', 'Silver', 'Silvies', 'Siuslaw', 'Sixes', 'Smith', 'South Fork Owyhee', 'South Santiam',
                'South Umpqua', 'Sprague', 'Summer Lake', 'Trout', 'Tualatin', 'Umatilla', 'Umpqua', 'Upper Columbia-Entiat',
                'Upper Columbia-Priest Rapids', 'Upper Crooked', 'Upper Deschutes', 'Upper Grande Ronde,Upper John Day',
                'Upper Klamath', 'Upper Klamath Lake', 'Upper Malheur', 'Upper Quinn', 'Upper Rogue', 'Upper Willamette',
                'Walla Walla', 'Wallowa', 'Warner Lakes', 'Williamson', 'Willow', 'Wilson-Trusk-Nestuccu', 'Yamhill')


# Define UI 
ui <- fluidPage(

   # Application title
   titlePanel("ORDEQ AWQMS data retrieval function"),

   # Sidebar with parameter inputs
   sidebarLayout(
      sidebarPanel(
        # Start Date
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = '1949-09-15'
                  ),

        # End date
        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),

       #characteristics
         selectizeInput("characteristics",
                     "Select parameters",
                     choices = chars,
                     multiple = TRUE),

       # Monitoring locations 
        selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = station,
                        multiple = TRUE),


       # Projects 
       selectizeInput("projs",
                       "Select Projects",
                       choices = projects,
                       multiple = TRUE),

        selectizeInput("stat_basis",
                       "Select Statistical Basis",
                       choices = stat_b,
                       multiple = TRUE),

       # Sample media
        selectizeInput("samp_med",
                       "Select Sample Media",
                       choices = samp_media,
                       multiple = TRUE),

       #add warning
       tags$em("Warning: HUC8 may not include all stations"),
       
       # huc8 names 
       selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = HUC8_Names,
                       multiple = TRUE),
        
        
       #Orgs
       selectizeInput("orgs",
                       "Select organization",
                       choices = organization,
                       multiple = TRUE),
       
       #standard codes filter 
       checkboxInput("QCfilter",
                     label = "Filter out QC data",
                     value = TRUE),
       
       
       #QC filter 
       checkboxInput("strdcodes",
                      label = "Include standard codes?",
                      value = FALSE)


        ),


     # Setup main panel
       mainPanel(
        h1("AWQMS_Data() Builder"),
        h5("Select parameters on left to build data retrieval function"),
        #tags$br(),
        h5("Copy and paste function below into a different R session"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        textOutput("selected_chars")
   )
))

# Define server logic required to display query
server <- function(input, output) {

   output$selected_chars <- renderText({

     # Convert all field entries to strings of vectors - This allows their use in the query
     stats <- toString(sprintf("'%s'", input$monlocs))
     vals <- toString(sprintf("'%s'", input$characteristics))
     sbasis <- toString(sprintf("'%s'", input$stat_basis))
     proj_select <- toString(sprintf("'%s'", input$projs))
     samp_m <- toString(sprintf("'%s'", input$samp_med))
     huc8s <- toString(sprintf("'%s'", input$huc8_nms))
     organiz <-  toString(sprintf("'%s'", input$orgs))

     # Begin the query 
     qry <- "AWQMS_Data("

  
  # Add parameters to query - 
     #General format - If field is not blank, and fields above are not blank add a comma and the paramter
        # If above fields are all blank, do not add the comma
        # If field is empty, do notheing
     
  #Start date
      if(length(input$startd) > 0){
       qry <- paste0(qry, "startdate = '", input$startd,"'" )
     }

  #enddate
     if(length(input$endd) > 0){

       if(length(input$startd) > 0){
         qry <- paste0(qry, ", ")}

       qry <- paste0(qry, "enddate = '", input$endd,"'" )
     }

  #monlocs
       if(length(input$monlocs) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0){
         qry <- paste0(qry, ", ")
         }

        qry <- paste0(qry,"station = c(",stats,")"  )


       }

  #chars
     if(length(input$characteristics) > 0){


       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"char = c(",vals,") "  )

     }




  #stat base
     if(length(input$stat_basis) > 0){


       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"stat_base = c(",sbasis,") "  )

     }

  #projects
     if(length(input$projs) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"project = c(",proj_select,") "  )

     }

  #sample media
     if(length(input$samp_med) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"media = c(",samp_m,") "  )

     }

 #HUC8s
     if(length(input$huc8_nms) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0|
          length(input$samp_med) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"HUC8_Name = c(",huc8s,") "  )

     }
     
 #orgs
     if(length(input$orgs) > 0){
       
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0|
          length(input$samp_med) > 0|
          length(input$huc8_nms) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"org = c(",organiz,") "  )
       
     }
     
#standard codes
     
     if( input$strdcodes) {
       
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0|
          length(input$samp_med) > 0|
          length(input$huc8_nms) > 0|
          length(input$orgs) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"crit_codes  = TRUE")  
       
     }
     
#QC data
     
     if( !input$QCfilter) {
       
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0|
          length(input$samp_med) > 0|
          length(input$huc8_nms) > 0|
          length(input$orgs) > 0 |
          input$strdcodes == TRUE){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"filterQC = FALSE")  
       
     }
     qry <- paste0(qry, ")")
     qry


})


}
# Run the application
shinyApp(ui = ui, server = server)

