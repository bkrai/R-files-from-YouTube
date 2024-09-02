library(shiny)
library(ollamar)
library(bslib)

# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly", bg = 'white', fg = 'green'),
  titlePanel("App"),
  sidebarLayout(
    sidebarPanel(textInput('txt', 'Enter prompt', 'Tell a joke')),
    mainPanel(verbatimTextOutput("out"))))

# Server
server <- function(input, output, session) {
  output$out <- renderText({
    generate('llama3', input$txt, 'text', T) })
}

# App
shinyApp(ui = ui, server = server)
