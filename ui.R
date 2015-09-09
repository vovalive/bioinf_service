
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Random data heatmap tree and pca"),
  
  ###file
  
  ####
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Set random seed:",
                min = 1,
                max = 10,
                value = 5),
    sliderInput("nsamples",
                "Samples number:",
                min = 5,
                max = 25,
                value = 15),
    sliderInput("nfeatures",
                "Features number:",
                min = 5,
                max = 25,
                value = 15),
    tags$hr(),
    fileInput('file1', 'You can choose file to upload',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"')
  ),
 
  
  # Show a plot of the generated distribution
  mainPanel(
    p("Изначально все графики строятся на случайных данных, но можно загрузить свои собственные - главное чтобы в левом верхнем углу было слово name, под ним названия генов или белков"),
    img(src='helper_sm.jpg', align = "center"),
    tags$hr(),
    tableOutput('contents'),
    tags$hr(),
    plotOutput("heatmap"),
    downloadButton('downloadHeatmap.png', 'Скачать в формате png'),
    downloadButton('downloadHeatmap.pdf', 'Скачать в формате pdf'),
    tags$hr(),
    plotOutput("tree"),
    downloadButton('downloadTree.png', 'Скачать в формате png'),
    downloadButton('downloadTree.pdf', 'Скачать в формате pdf'),
    tags$hr(),
    plotOutput("pca"),
    downloadButton('downloadPca.png', 'Скачать в формате png'),
    downloadButton('downloadPca.pdf', 'Скачать в формате pdf'),
    tableOutput('pcatable')
  )
))
