
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
shinyUI(navbarPage(title="Биоинформатический сервис",
  tabPanel(title="PCA, heatmap, кластеризация" ,                
  pageWithSidebar(
  
  # Application title
  headerPanel("Random data heatmap tree and pca"),
  
  ###file
  
  ####
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Генератор случайных данных",
                min = 1,
                max = 10,
                value = 5),
    sliderInput("nsamples",
                "Количество образцов\n случайных данных",
                min = 5,
                max = 25,
                value = 15),
    sliderInput("nfeatures",
                "Количество признаков случайных данных",
                min = 5,
                max = 25,
                value = 15),
    fileInput('file1', 'Выберите файл для загрузки',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),
    checkboxInput('header', 'Header', TRUE),
    selectInput('sep2', 'Разделитель',
                c(Запятая=',',
                         'Точка с запятой'=';',
                         Табуляция='\t'),
                ','),
    radioButtons('quote', 'Кавычки текста',
                 c(Отсутствуют='',
                   'Двойные(")'='"',
                   "Одинарные(')"="'"),
                 '"')
  )#close sidebar
  ,
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
  )#close panel
)#close page
)#close tab
,tabPanel(title="Оценка диагностических тестов"
,
sidebarPanel(
  checkboxInput(label="Рассчитать доверительные интервалы",inputId="diagci",value = T),
  selectInput(inputId="diagclevel",label="Уровень значимости",choices = c("99%","95%","90%"),selected = "95%"),
  selectInput(inputId="methodCI",label="Метод расчета дов. инт.",choices = c("Клоппера-Пирсона","Агрести-Коула","Уилсона","Асимптотика","Logit","Probit","cloglog"),selected = "Клоппера-Пирсона"),
  fileInput('file2', 'Выберите файл для загрузки',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  ),
  checkboxInput('header2', 'Наличие имен колонок', TRUE),
  selectInput('sep2', 'Разделитель',
               c(Запятая=',',
                 'Точка с запятой'=';',
                  Табуляция='\t'),
               ','),
  p("Файл для расчета ROC кривой должен быть сформирован в две колонки, при этом дискретная переменная(значения теста) должна быть в первой колонке, а бинарная(отклик, исход) - во второй;\n По вашим данным будет построена ROC кривая, рассчитана AUC и даны рекомендации по значению диагностической отсечки")
  
)#close sidebar
,
# Show a plot of the generated distribution
mainPanel(p("Для получения расчетных значений диагностических возможностей метода заполните таблицу  следующим образом: Ячейки со значением \n'Признак + Тест +' - количество образцов с интересующей характеристикой(наличием заболевания, наличием устойчивости к АБ препаратам и т.п.) у которых диагностический тест показал положительный результат; Ячейки со значением 'Признак - Тест +' - количество образцов без интересующей характеристики(наличия заболевания, наличия устойчивости к АБ препаратам и т.п.) у которых диагностический тест показал положительный результат и т.д."),
  column(width=6,
                 numericInput(label="Признак + \nТест +",value=1,inputId = "++",min = 0,step = 1,width = "100%"),
                  numericInput(label="Признак + \nТест -",value=1,inputId = "+-",min = 0,step = 1,width = "100%")),
         column(width=6,
                numericInput(label="Признак - \nТест +",value=1,inputId = "-+",min = 0,step = 1,width = "100%"
                 ),
         numericInput(label="Признак - \nТест -",value=1,inputId = "--",min = 0,step = 1,width = "100%")
) , 
tableOutput("crosstable")   ,
textOutput("sens"),
textOutput("spec"),
textOutput(outputId="accuracy"),
textOutput("progpos"),
textOutput("progneg"),
textOutput("diagOR"),
textOutput("likelihoodpos"),
textOutput("likelihoodneg"),
plotOutput(outputId="roc"),
textOutput(outputId="auc"),
plotOutput(outputId="cutoff"),
# sliderInput("cutoffline",
#             "Установите ползунком уровень отсечки с оптимальными чувствительностью, специфичностью и точностью",
#             min = -5,
#             max = 5,
#             value = 0,
#             step=0.01),
checkboxInput(inputId="legcut",label="Отображать легенду",value=T),
numericInput("cutoffline",value=0,label="Порог отсечки",step=0.05)
)


)#close tab
)#close navbarPage
)#close shinyUI
