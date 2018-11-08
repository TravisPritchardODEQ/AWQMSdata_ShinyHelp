#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(AWQMSdata)

#Query out the valid values
chars <- AWQMS_Chars()
chars <- chars$Char_Name
chars <- sort(chars)

station <- AWQMS_Station()
station <- station$MLocID
station <- sort(station)

projects <- AWQMS_Projects()
projects <- projects$Project
projects <- sort(projects)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)


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
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = '1949-09-15'
                  ),

        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),

        selectizeInput("characteristics",
                     "Select parameters",
                     choices = chars,
                     multiple = TRUE),

         selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = station,
                        multiple = TRUE),


        selectizeInput("projs",
                       "Select Projects",
                       choices = projects,
                       multiple = TRUE),

        selectizeInput("stat_basis",
                       "Select Statistical Basis",
                       choices = stat_b,
                       multiple = TRUE),

        selectizeInput("samp_med",
                       "Select Sample Media",
                       choices = samp_media,
                       multiple = TRUE),

        selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = HUC8_Names,
                       multiple = TRUE),
        
        selectizeInput("orgs",
                       "Select organization",
                       choices = organization,
                       multiple = TRUE),
       
         checkboxInput("QCfilter",
                      label = "Filter out QC data",
                      value = TRUE)


        ),


      mainPanel(
        h1("Text to paste into R script"),
        tags$hr(),
        tags$br(),
        textOutput("selected_chars")
   )
))

# Define server logic required to display query
server <- function(input, output) {

   output$selected_chars <- renderText({

     stats <- toString(sprintf("'%s'", input$monlocs))
     vals <- toString(sprintf("'%s'", input$characteristics))
     sbasis <- toString(sprintf("'%s'", input$stat_basis))
     proj_select <- toString(sprintf("'%s'", input$projs))
     samp_m <- toString(sprintf("'%s'", input$samp_med))
     huc8s <- toString(sprintf("'%s'", input$huc8_nms))
     organiz <-  toString(sprintf("'%s'", input$orgs))

     qry <- "AWQMS_Data("

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
          length(input$orgs) > 0){
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

