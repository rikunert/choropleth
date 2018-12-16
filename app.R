#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# (c) Richard Kunert (RiKunert@gmail.com) December 2018

# install libraries to get this shit published on shiny server
if(!require(BH)){install.packages('BH')} # 
library(BH)

if(!require(DBI)){install.packages('DBI')} # 
library(DBI)

if(!require(R6)){install.packages('R6')} # 
library(R6)

if(!require(RColorBrewer)){install.packages('RColorBrewer')} # 
library(RColorBrewer)

#load libraries
library(shiny)

if(!require(rgdal)){install.packages('rgdal')} # read in polygon shape data
library(rgdal)

if(!require(rhandsontable)){install.packages('rhandsontable')} # tabular input
library(rhandsontable)

if(!require(colourpicker)){install.packages('colourpicker')} #colour choice button
library(colourpicker)

if(!require(cartogram)){install.packages('cartogram')} #
library(cartogram)

if(!require(tmap)){install.packages('tmap')} #
library(tmap)    # for static and interactive maps

###############################################################
# Default data

# shape variable
load(url('https://github.com/rikunert/choropleth/raw/master/German_state_shapes.RData'))

DF <- data.frame(name_cartogram = c('Mecklenburg-Vorpommern',
                                    'Bayern',
                                    'Thueringen',
                                    'Niedersachsen',
                                    'Rheinland-Pfalz',
                                    'Saarland',
                                    'Nordrhein-Westfalen',
                                    'Hamburg',
                                    'Hessen',
                                    'Berlin',
                                    'Brandenburg',
                                    'Baden-Wuerttemberg',
                                    'Schleswig-Holstein',
                                    'Bremen',
                                    'Sachsen-Anhalt',
                                    'Sachsen'),
                 State = c('Baden-Wuerttemberg',
                           'Bayern',
                           'Berlin',
                           'Brandenburg',
                           'Bremen',
                           'Hamburg',
                           'Mecklenburg-Vorpommern',
                           'Niedersachsen',
                           'Nordrhein-Westfalen',
                           'Rheinland-Pfalz',
                           'Saarland',
                           'Sachsen',
                           'Sachsen-Anhalt',
                           'Schleswig-Holstein',
                           'Thueringen',
                           'Hessen'), 
                 Size = c(10755000,
                          12542000,
                          3469000,
                          2500000,
                          661000,
                          1788000,
                          1639000,
                          7914000,
                          17837000,
                          4052803,
                          1018000,
                          4143000,
                          2331000,
                          2833000,
                          2231000,
                          6066000),# population 
                 Colour = c(1:16), 
                 stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  #NON-INTERACTIVE###############
  titlePanel("Creat data maps yourself!"),
  
  #INTERACTIVE#######################
  # left column
  column(6,
         
         br(),#line break
         'Use your own data to create four different kinds of maps of German federal states.',
         br(),#line break
         'Choose map type, colours and the title yourself.',
  
         wellPanel(tags$h3('Data Input'),
                   br(),#line break
                   'Default Size-column: state population.',
                   rHandsontableOutput("hot")
                   ),
         wellPanel(radioButtons(inputId = 'mapSelect', label = 'Map type',
                                c('Choropleth map' = 'choro',
                                  'Cartogram: continuous' = 'cont',
                                  'Cartogram: non-continuous' = 'ncont',
                                  'Cartogram: bubble' = 'dorling'),
                                selected = 'choro')
                   )
         ),
  
  # right column
  #place to display map
  column(6,
    plotOutput(outputId = 'map'),
    
    downloadButton(outputId = 'mapSave'),
    
    # Some text
    br(),#line break
    'This shinyApp was made by Richard Kunert (see code on ',a('github', href='https://github.com/rikunert/choropleth'), ').',
    br(),
    ' Licence: ',a('CC-BY-NC', href="https://creativecommons.org/licenses/by-nc/2.0/"),'.',
    br(),
    'Version: 0.1. December 2018.',
    br(),
    
    inputPanel(tags$h4('Labels'),
               textInput(inputId = 'titleText', label = 'Title',
                         value = '', placeholder = 'Insert figure title here'),
               
               textInput(inputId = 'legendText', label = 'Legend',
                         value = '', placeholder = 'Insert legend title here'),
               
               radioButtons(inputId = 'stateLabelSelect', label = 'Show state labels',
                            choices = c('None' = 'none',
                                        'Full name' = 'full'))
    ),
    
    inputPanel(tags$h4('State areas'),
               colourInput(inputId = 'areaHighColourSelect', label = 'High',
                           showColour = 'background',#'both',#
                           '#008B00', palette = 'limited'),#for changing colour of areas (high)
               colourInput(inputId = 'areaMidColourSelect', label = 'Mid',
                           showColour = 'background',#'both',#
                           '#FF7F00', palette = 'limited'),#for changing colour of areas (mid)
               colourInput(inputId = 'areaLowColourSelect', label = 'Low',
                           showColour = 'background',#'both',#
                           '#FF0000', palette = 'limited')#for changing colour of areas (low)
    ),
    
    inputPanel(tags$h4('Borders'),
               colourInput(inputId = 'borderColourSelect', label = 'Colour',
                           showColour = 'background',#'both',#
                           '#666666', palette = 'limited'),#for changing colour of borders
               sliderInput(inputId = 'borderLineSelect', label = 'Width', step = 0.25,
                           value = 1, min = 0, max = 5)#for changing line thickness
               )
    )
)

# Define server logic
server <- function(input, output) {
  
  # define table properties
  output$hot <- renderRHandsontable({
    
    rhandsontable(DF, height = 400, selectCallback = TRUE, readOnly = FALSE) %>%
      hot_col("State", readOnly = TRUE) %>%
      hot_col("name_cartogram", readOnly = TRUE, colWidths = 0.00001)
    
  })
  
  map_reactive = reactive({
    
    # Data
    #DF2 = hot_to_r(input$hot)#data()
    shape@data <- merge(shape@data, 
                        hot_to_r(input$hot),
                        by.x="name", 
                        by.y="name_cartogram")
    
    # Map type selection and drawing
    if (input$mapSelect == 'choro') {#if choropleth map chosen
      
      #plot(shape)
      map = tm_shape(shape)
      
    } else if (input$mapSelect == 'cont') {
      
      shape_map <- cartogram_cont(shape, "Size", itermax=10)
      map = tm_shape(shape_map)
      
    } else if (input$mapSelect == 'ncont') {
      
      shape_map <- cartogram_ncont(shape, "Size", k=1.5, inplace=T)  
      
      map = tm_shape(shape_map)
      
    } else if (input$mapSelect == 'dorling') {
      
      shape_map <- cartogram_dorling(shape, "Size", k=5, m_weight=0.1, itermax=10)
      
      map = tm_shape(shape) + 
        tm_borders(col=input$borderColourSelect,
                   lwd=input$borderLineSelect) +
        tm_shape(shape_map)
    }
    
    # polish map
    map_publish = map +
      tm_fill(col='Colour', title=input$legendText, 
              style='cont', palette=c(input$areaHighColourSelect,
                                      input$areaMidColourSelect,
                                      input$areaLowColourSelect)) +
      tm_borders(col=input$borderColourSelect,
                 lwd=input$borderLineSelect) +
      tm_layout(main.title = input$titleText, 
                main.title.position = 'center',
                frame=F,
                legend.outside = T)+
      tm_credits('@rikunert', size=1, 
                 col='grey', 
                 align='right', position=c('right', 'bottom'))
    
    if (input$stateLabelSelect=='full'){
      map_publish = map_publish +
        tm_text("State",
                col='grey30',
                print.tiny = T,
                auto.placement = F)  
    }
    
    
    return(map_publish)
    
  })
  
  # prepare and draw map
  output$map = renderPlot({
     
     # display map
    knit_print.tmap(map_reactive())
    
  })
  
  #save map
  output$mapSave = downloadHandler(
    filename = 'test.png',
    content = function(file){
      tmap_save(map_reactive(), filename = file,
                dpi=1000)}
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)