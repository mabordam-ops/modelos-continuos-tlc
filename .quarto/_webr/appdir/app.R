library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  titlePanel("Simulación del Teorema Central del Límite (TCL)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribución:",
                  choices = c("Uniforme", "Binomial", "Poisson",
                              "Hipergeométrica", "Exponencial")),
      
      # Parámetros dinámicos
      conditionalPanel(
        condition = "input.dist == 'Uniforme'",
        numericInput("a", "Mínimo (a):", 0),
        numericInput("b", "Máximo (b):", 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("size", "Ensayos (n):", 10, min = 1),
        sliderInput("p", "Probabilidad (p):", value = 0.3, min = 0, max = 1, step = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda_pois", "Lambda:", 4, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Hipergeométrica'",
        numericInput("Npop", "Tamaño población (N):", 1000, min = 10),
        numericInput("K", "Éxitos en población (K):", 300, min = 1),
        numericInput("m", "Tamaño de muestra (m):", 20, min = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Exponencial'",
        numericInput("lambda_exp", "Lambda:", 1.5, min = 0.1)
      ),
      
      hr(),
      
      numericInput("n", "Tamaño de muestra (TCL):", 30, min = 1),
      numericInput("Nsim", "Número de simulaciones:", 10000, min = 1000),
      
      actionButton("simular", "Simular")
    ),
    
    mainPanel(
      plotOutput("plots", height = "450px")
    )
  )
)

server <- function(input, output) {
  
  sim_data <- eventReactive(input$simular, {
    
    set.seed(123)
    
    N <- 100000
    
    # --------------------------
    # GENERAR SEGÚN DISTRIBUCIÓN
    # --------------------------
    
    if (input$dist == "Uniforme") {
      poblacion <- runif(N, input$a, input$b)
      medias <- replicate(input$Nsim,
                          mean(runif(input$n, input$a, input$b)))
      
      mu <- (input$a + input$b)/2
      sigma <- (input$b - input$a)/sqrt(12)
    }
    
    else if (input$dist == "Binomial") {
      poblacion <- rbinom(N, input$size, input$p)
      medias <- replicate(input$Nsim,
                          mean(rbinom(input$n, input$size, input$p)))
      
      mu <- input$size * input$p
      sigma <- sqrt(input$size * input$p * (1 - input$p))
    }
    
    else if (input$dist == "Poisson") {
      poblacion <- rpois(N, input$lambda_pois)
      medias <- replicate(input$Nsim,
                          mean(rpois(input$n, input$lambda_pois)))
      
      mu <- input$lambda_pois
      sigma <- sqrt(input$lambda_pois)
    }
    
    else if (input$dist == "Hipergeométrica") {
      poblacion <- rhyper(N,
                          m = input$K,
                          n = input$Npop - input$K,
                          k = input$m)
      
      medias <- replicate(input$Nsim,
                          mean(rhyper(input$n,
                                      m = input$K,
                                      n = input$Npop - input$K,
                                      k = input$m)))
      
      mu <- input$m * (input$K / input$Npop)
      sigma <- sqrt(input$m * (input$K / input$Npop) *
                      (1 - input$K / input$Npop) *
                      ((input$Npop - input$m)/(input$Npop - 1)))
    }
    
    else if (input$dist == "Exponencial") {
      poblacion <- rexp(N, input$lambda_exp)
      medias <- replicate(input$Nsim,
                          mean(rexp(input$n, input$lambda_exp)))
      
      mu <- 1 / input$lambda_exp
      sigma <- 1 / input$lambda_exp
    }
    
    list(poblacion = poblacion,
         medias = medias,
         mu = mu,
         sigma = sigma)
  })
  
  output$plots <- renderPlot({
    
    data <- sim_data()
    
    df_pob <- data.frame(x = data$poblacion)
    df_med <- data.frame(x = data$medias)
    
    # Gráfico población
    g1 <- ggplot(df_pob, aes(x = x)) +
      geom_histogram(aes(y = ..density..),
                     bins = 50,
                     fill = "lightgreen",
                     color = "black") +
      stat_function(fun = dnorm,
                    args = list(mean = data$mu,
                                sd = data$sigma),
                    color = "darkgreen",
                    linewidth = 1) +
      labs(title = "Población",
           x = "Valores",
           y = "Densidad") +
      theme_minimal()
    
    # Gráfico TCL
    g2 <- ggplot(df_med, aes(x = x)) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "lightblue",
                     color = "black") +
      stat_function(fun = dnorm,
                    args = list(mean = data$mu,
                                sd = data$sigma/sqrt(input$n)),
                    color = "red",
                    linewidth = 1.2) +
      labs(title = "Distribución muestral de la media",
           x = "Media muestral",
           y = "Densidad") +
      theme_minimal()
    
    grid.arrange(g1, g2, ncol = 2)
  })
}

shinyApp(ui = ui, server = server)
