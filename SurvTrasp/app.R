# Cargar librerías necesarias
library(readxl)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(shiny)

# UI
ui <- fluidPage(
    titlePanel("Curva de Sobrevida - Trasplante Hepático"),
    sidebarLayout(
        sidebarPanel(
            selectInput("sexo", "Sexo:", choices = c("Todos", "MASCULINO", "FEMENINO")),
            selectInput("tipo_donante", "Tipo de Donante:", choices = c("Todos", "CADAV\u00c9RICO", "VIVO")),
            sliderInput("ano_trasplante", "A\u00f1o de Trasplante:", min = 2011, max = 2025, value = c(2011, 2025), sep = ""),
            selectInput("up", "Centro de Trasplante:", 
                        choices = c("Todos", 
                                    "UP2104 - HOSPITAL M\u00c9XICO", 
                                    "UP2101 - HOSPITAL RAFAEL A. CALDER\u00d3N GUARDIA", 
                                    "UP2103 - HOSPITAL NACIONAL DE NI\u00d1OS")),
            sliderInput("tiempo", "Sobrevida m\u00ednima (meses):", min = 0, max = 120, value = 0, step = 1),
            selectInput("grupo_comparacion", "Variable de Comparaci\u00f3n:", 
                        choices = c("Ninguna", "SEXO", "TIPO_DONANTE", "ORGANO", "ANO_TRASPLANTE"))
        ),
        mainPanel(
            plotOutput("km_plot")
        )
    )
)

# Server
server <- function(input, output) {
    datos <- reactive({
        df <- read_excel("data/TRASPL_HEP.xlsx", sheet = "HIGADO") |> 
            mutate(
                FECHA_TRASPLANTE = as.Date(FECHA_TRASPLANTE),
                FECHA_DEFUNCION = as.Date(`FECHA DE DEFUNCI\u00d3N`),
                fallecido = as.numeric(FALLECIDO_COD),
                tiempo_meses = if_else(fallecido == 1,
                                       interval(FECHA_TRASPLANTE, FECHA_DEFUNCION) %/% months(1),
                                       interval(FECHA_TRASPLANTE, Sys.Date()) %/% months(1)),
                evento = fallecido
            ) |> 
            filter(!is.na(tiempo_meses) & tiempo_meses >= input$tiempo)
        
        if (input$sexo != "Todos") {
            df <- df |> filter(SEXO == input$sexo)
        }
        if (input$tipo_donante != "Todos") {
            df <- df |> filter(`TIPO_DONANTE` == input$tipo_donante)
        }
        if (input$up != "Todos") {
            df <- df |> filter(UP_TRASPLANTA == input$up)
        }
        df <- df |> filter(ANO_TRASPLANTE >= input$ano_trasplante[1], ANO_TRASPLANTE <= input$ano_trasplante[2])
        df
    })
    
    output$km_plot <- renderPlot({
        df <- datos()
        
        if (input$grupo_comparacion == "Ninguna") {
            fit <- survfit(Surv(tiempo_meses, evento) ~ 1, data = df)
            ggsurvplot(fit, data = df, conf.int = TRUE, risk.table = TRUE)
        } else {
            var_grp <- sym(input$grupo_comparacion)
            fit <- survfit(Surv(tiempo_meses, evento) ~ !!var_grp, data = df)
            ggsurvplot(fit, data = df, conf.int = TRUE, risk.table = TRUE, pval = TRUE)
        }
    })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)