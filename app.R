library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(ggplot2)
# library(dygraphs)
library(lubridate)
library(tidyverse)
library(DT)
library(readxl)
library(hydroGOF)
# UI ----
ui <- fluidPage(
  title = "Инструменты многомерной калибровки", 
  # theme = bs_theme(preset = "minty"),
  fluid = T, windowTitle = "ИМК", lang = "ru",
  useShinyjs(),
  titlePanel(title = 'Инструменты многомерной калибровки'),
  sidebarLayout(
    sidebarPanel(width = 3,
                 # выбор файла ----
      fileInput(inputId = "file1", label = 'Выбрать файл',
                multiple = FALSE, 
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"), 
                buttonLabel = "Выбрать...", 
                placeholder = "Файл не выбран"),
      
      # br() element to introduce extra vertical spacing
      br(),
      
      # выбор диапазона лет ----
      uiOutput(outputId = 'ui_minY'),
      uiOutput(outputId = 'ui_maxY'),
      actionButton("process", "Обработка"),
      tags$hr(),
      tags$h4('NSE взвешенный'),
      tags$h4(style = "display:inline-block; vertical-align:top", 
              htmlOutput('nseWeight')),
      tags$h4('NSE PC1-PC2'),
      tags$h4(style = "display:inline-block; vertical-align:top", 
              htmlOutput('nsePC')),
      tags$h4('NSE очищенный'),
      tags$h4(style = "display:inline-block; vertical-align:top", 
              htmlOutput('nseClean')),
      tags$footer(includeHTML("footer.html"))
    ),
    
    # Main panel  ----
    mainPanel(
      
      # footer = div(class = "footer", includeHTML("footer.html")), 
      # Tabset  ----
      tabsetPanel(type = "tabs", id = "tabSet",
                  # панель загрузки ----
                  tabPanel(value = 'load', title = "Загрузка", 
                           dataTableOutput("contents"),
                           br()),
                  # панель графика ----
                  tabPanel(value = 'plot', title = "График",
                           plotOutput("plot1")),
                  # панель обработки ----
                  tabPanel(value = 'proc', title = "Обработка", 
                           p('NSE по плотной матрице'),
                           tableOutput("dense_nse_df"),
                           p('Статистика по плотной матрице'), 
                           tableOutput("dense_df_stat"),
                           p('МГК'),
                           tableOutput('prcomp'),
                           p('umatrix'),
                           tableOutput('umatrix')
                           ),
                  # панель результата ----
                  tabPanel(value = 'res', title = "Результат", 
                           downloadButton('download',"Скачать таблицу"),
                           dataTableOutput("table"))
      )
      
    )
  )
)

# Server  ----
server <- function(input, output, session) {
  
  # Таблица данных расчета ----
  input_df <- reactive({
    req(input$file1)
    output$weather_warning <- validate(need(tools::file_ext(input$file1$datapath) == c("csv", "txt", "asc"), 
                                            "Пожалуйста, загрузите текстовый файл (txt, csv, asc)")
    )
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath, 
                       check.names = F, stringsAsFactors = F, na.strings = '-99.000')
        df <- df %>%
          mutate(Date = as.Date(strptime(as.character(Date), format = '%Y%m%d'))) %>%
          pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
          mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
                 calc = sub('.*(?=.$)', '', var, perl=T)) %>%
          select(Date, post, calc, val) 
      },
      error = function(e) stop(safeError(e))
    )
    return(df)
  })
  
  # выбор лет ----
  output$ui_minY <- renderUI({
    req(input$file1)
    selectInput(inputId = 'ymin', choices = unique(year(input_df()$Date)), 
                selected = min(unique(year(input_df()$Date))), multiple = F, 
                label = 'Начало', selectize = F, width = '50%')
  })
  output$ui_maxY <- renderUI({
    req(input$file1)
    selectInput(inputId = 'ymax', choices = unique(year(input_df()$Date)), 
                selected = max(unique(year(input_df()$Date))), multiple = F, 
                label = 'Конец', selectize = F, width = '50%')
  })
  
  # график
  output$plot1 <- renderPlot({
    req(input$file1)
    input_df() %>%
      filter(year(Date) >= input$ymin & year(Date) <= input$ymax) %>%
      ggplot(aes(x=Date, y = val, col = calc)) + 
      geom_line() + 
      facet_wrap(post~., ncol = 1, scales = 'free_y', 
                 strip.position = 'right') + 
      theme_light(base_size = 8)
  })
  # NSE таблица ----
  nse_df <- reactive({
    req(input$file1)
    df <- input_df() %>%
      filter(year(Date) >= input$ymin & year(Date) <= input$ymax) %>%
      pivot_wider(id_cols = c(Date, post), 
                  names_from = calc, values_from = val) %>%
      group_by(post) %>%
      summarise(nse = NSE(obs = m, sim = s), 
                rmse = rmse(obs = m, sim = s),
                r2 = cor(x = m, y = s, use = 'pairwise.complete.obs') ^ 2,
                ftest = var.test(m, s)$p.value,
                ttest = t.test(m, s)$p.value) %>%
      mutate(calc = 'm')
    return(df)
  })
  
  # таблица количества данных ----
  count_df <- reactive({
    # req(input$file1)
    df <- input_df() %>%
      filter(year(Date) >= input$ymin & year(Date) <= input$ymax) %>%
      group_by(post, calc) %>% 
      filter(!is.na(val)) %>%
      summarise(N = n(),
                mean = mean(val, na.rm = T),
                var = var(val, na.rm = T)) %>%
      left_join(nse_df(), by = c('post', 'calc')) %>%
      arrange(calc, desc(var)) %>%
      mutate_if(is.numeric, ~round(., 3))
    return(df)
  })
  
  # Вывод таблицы с загруженным файлом для просмотра ----
  output$contents <- renderDT(
    count_df(),
    selection = 'multiple',
    colnames = c('Индекс поста', 'Ряд*', 'Длина', 'Среднее', 'Дисперсия', 'NSE', 'RMSE', 'R2'),
    options = list(pageLength = 100, 
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # NSEw ----
  nsew <- reactive({
    # req(input$file1)
    
    df <- count_df() %>%
      filter(calc == 'm' & post %in% unlist(count_df()[input$contents_rows_selected,]$post)) %>%
      ungroup() %>%
      summarise(nse = sum(nse * var, na.rm = T) / 
                  sum(var, na.rm = T)) %>%
      pull(nse)
  })
  
  output$nseWeight <- renderText(
    return(case_when(nsew() > 0.8 ~ paste('<div style=\"background-color:green\">', round(nsew(), 4),"</div>"),
                     nsew() <= 0.8 ~ paste('<div style=\"background-color:yellow\">', round(nsew(), 4),"</div>"),
                     nsew() <= 0.5 ~ paste('<div style=\"background-color:red\">', round(nsew(), 4),"</div>"),
                     .default ="<div style=\"background-color:red\">Выберите хотя бы одну запись!</div>")
    ))
  

  # Обработка ----
  # плотная матрица ----
  calib_df <- reactive({
    df <- input_df() %>%
      filter(year(Date) >= input$ymin & year(Date) <= input$ymax & post %in% unlist(count_df()[input$contents_rows_selected,]$post)) %>%
      pivot_wider(id_cols = 'Date', names_from = c('post', 'calc'),  values_from = 'val') %>%
      na.exclude()
  })
  
  # статистика по плотной матрице ----
  dense_nse_df <- reactive({
    dense_nse_df <- calib_df() %>%
      pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
      mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
             calc = sub('.*(?=.$)', '', var, perl = T)) %>%
      pivot_wider(id_cols = c(Date, post), 
                  names_from = calc, values_from = val) %>%
      group_by(post) %>%
      summarise(nse = NSE(obs = m, sim = s), 
                rmse = rmse(obs = m, sim = s),
                r2 = cor(m, s, use = 'complete.obs') ^ 2,
                Pvalue1 = var.test(m, s)$p.value,
                Pvalue2 = t.test(m, s)$p.value) %>%
      mutate(calc = 'm')  
    })
  
  dense_df_stat <- reactive({
    dense_df_stat <- calib_df() %>%
      pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
      mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
             calc = sub('.*(?=.$)', '', var, perl=T)) %>%
      select(Date, post, calc, val) %>%
      group_by(post, calc) %>% 
      filter(!is.na(val)) %>%
      summarise(N = n(),
                mean = mean(val, na.rm = T),
                var = var(val, na.rm = T)) %>%
      left_join(dense_nse_df(), by = c('post', 'calc')) %>%
      arrange(calc, var) %>%
      mutate_if(is.numeric, ~round(., 4))
    
  })
  
  
  # МГК-преобразование плотной матрицы ----
  lm <- reactive({
    calib_df() %>%
    select(contains('_m')) %>%
    prcomp(scale = F)
  })
  # матрица трансформации ----
  loadings <- reactive({
    lm()$rotation
  })
  
  
  fact_matrix <- reactive({
    calib_df() %>%
      select(contains(c('_m')))
  })
  sim_matrix <- reactive({
    calib_df() %>%
      select(contains(c('_s')))
  })
  # преобразование в U-пространство ----
  fact_m <- reactive({
    as.matrix(fact_matrix())  %*% loadings()
  })
  sim_m <- reactive({
    as.matrix(sim_matrix())  %*% loadings()
  })  
  # очистка всех ГК кроме первых двух ----
  clean_df <- reactive({
    df <- data.frame(fact_m(), sim_m()) %>%
      mutate_at(vars(!contains(c('PC1', 'PC2', 'PC1.1', 'PC1.2'))), function(x){x = 0}) 
    return(df)
  })
  
  # nsePC по двум первым ГК ----
  nsePC <- reactive({
    var <- summary(lm())$importance[1,1:2] ^ 2 
    nsepc <- clean_df() %>%
      select(contains(c('PC1', 'PC2', 'PC1.1', 'PC2.1'))) %>%
      summarise(nse1 = NSE(obs = PC1, sim = PC1.1), 
                nse2 = NSE(obs = PC2, sim = PC2.1)) %>%
      unlist()
    
    return(sum(nsepc * var) / sum(var))
      
  })

  
  # преобразование рассчитанных расходов в U-пространство ----
  # с последующей очисткой и обратной трансформацией
  
  clean_sim1 <- reactive({
    df <- input_df() %>%
    filter(calc == 's' & post %in% unlist(count_df()[input$contents_rows_selected,]$post)) %>%
    pivot_wider(id_cols = Date, names_from = post, values_from = val)
  
    df <- data.frame(as.matrix(df[,-1]) %*% loadings()) %>%
      mutate_at(vars(!contains(c('PC1', 'PC2'))), function(x){x = 0}) 
  
    df <- as.data.frame(as.matrix(df) %*% t(loadings()))
    # меняем _m на _s в названии столбцов
    df <- df %>%
      rename_with(.fn = ~paste0(gsub(pattern = "\\_.+", 
                                     replacement = "", 
                                     x = .x), '_s'))
    return(df)
    })
  
  clean_fact <- reactive({
    clean_fact <- input_df() %>%
    filter(calc == 'm' & post %in% unlist(count_df()[input$contents_rows_selected,]$post)) %>%
    pivot_wider(id_cols = Date, names_from = post, names_glue = '{post}_m', 
                values_from = val)
  })
  
  clean_sim2 <- reactive({
    cbind(clean_fact(), clean_sim1())
  })
  
  nse_clean <- reactive({
    df <- clean_sim2() %>%
    pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
    mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
           calc = sub('.*(?=.$)', '', var, perl=T)) %>%
    select(Date, post, calc, val) %>%
    pivot_wider(id_cols = c(Date, post), 
                names_from = calc, values_from = val) %>%
    group_by(post) %>%
    summarise(nse = NSE(obs = m, sim = s), 
              rmse = rmse(obs = m, sim = s),
              r2 = cor(x = m, y = s, use = 'complete.obs') ^ 2 ) %>%
    mutate(calc = 'm')
    return(df)
  })
  
  count_clean <- reactive({
    df <- clean_sim2() %>%
    pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
    mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
           calc = sub('.*(?=.$)', '', var, perl=T)) %>%
    select(Date, post, calc, val) %>%
    group_by(post, calc) %>% 
    filter(!is.na(val)) %>%
    summarise(N = n(),
              mean = mean(val, na.rm = T),
              var = var(val, na.rm = T)) %>%
    left_join(nse_clean(), by = c('post', 'calc')) %>%
    arrange(calc, var) %>%
    mutate_if(is.numeric, ~round(., 3))
    return(df)
  })
  
  nsew_clean <- reactive({
    df <- count_clean() %>%
    filter(calc == 'm') %>%
    ungroup() %>%
    summarise(nse = sum(nse * var, na.rm = T) / 
                sum(var, na.rm = T)) %>%
    pull(nse)
    return(df)
  })
    
  # вывод результатов по нажатию кнопки ----
  observeEvent(input$process, {
    updateTabsetPanel(session, "tabSet",
                      selected = 'proc')
    output$summary <- reactive({
      summary(calib_df())
    })
    output$dense_nse_df <- renderTable(dense_nse_df())
    output$dense_df_stat <- renderTable(dense_df_stat())
    output$prcomp <- renderTable(summary(lm())$importance,
                                 rownames = T, digits = 3)
    output$loadings <- renderTable(loadings(), 
                                   rownames = T, digits = 3)
    output$nsePC <- renderText(
      return(case_when(nsePC() > 0.8 ~ paste('<div style=\"background-color:green\">', round(nsePC(), 3),"</div>"),
                       nsePC() <= 0.8 ~ paste('<div style=\"background-color:yellow\">', round(nsePC(), 3),"</div>"),
                       nsePC() <= 0.5 ~ paste('<div style=\"background-color:red\">', round(nsePC(), 3),"</div>"),
                       .default ="<div style=\"background-color:red\">Выберите хотя бы одну запись!</div>")
      ))
    
    output$umatrix <- renderTable(head(clean_df()))
    output$nseClean <- renderText(
      return(case_when(nsew_clean() > 0.8 ~ paste('<div style=\"background-color:green\">', round(nsew_clean(), 3),"</div>"),
                       nsew_clean() <= 0.8 ~ paste('<div style=\"background-color:yellow\">', round(nsew_clean(), 3),"</div>"),
                       nsew_clean() <= 0.5 ~ paste('<div style=\"background-color:red\">', round(nsew_clean(), 3),"</div>"),
                       .default ="<div style=\"background-color:red\">Выберите хотя бы одну запись!</div>")
      ))
    output$table <- renderDT(clean_sim2() %>% mutate_if(is.numeric, ~round(., 3)),
                             # colnames = c('Индекс поста', 'Ряд*', 'Длина', 'Среднее', 'Дисперсия', 'NSE', 'RMSE', 'R2'),
                             options = list(pageLength = 100,  
                                            language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
                             )
  })
  # Файл для скачивания ----
  output$download <- downloadHandler(
    filename = function(){"mdcalib_output.csv"}, 
    content = function(fname){
      write.csv2(clean_sim2(), 
                fname, quote = F, row.names = F, na = '-32968')
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)