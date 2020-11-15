library(shiny)
library(plotly)

df <- read.csv("df.csv", stringsAsFactors = FALSE)
df2 <- read.csv("df2.csv", stringsAsFactors = FALSE)
df3 <- read.csv("df3.csv", stringsAsFactors = FALSE)
df5 <- read.csv("df5.csv", stringsAsFactors = FALSE)
df6 <- read.csv("df6.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
  navbarPage(
    "Crime Data Dashboard",
    tabPanel(
      "Map Quantiles",
      sidebarLayout(
        sidebarPanel(
          helpText(
            HTML(
              "Choose <b>quantile</b> to display the relevant frequency states.<br>
              1st quartile = 9,<br>
              2nd quartile = 29,<br>
              3rd quartile = 58 police murders"
            )
          ),
          checkboxGroupInput("quantiles", "Select Quantiles", unique(df6$q), unique(df6$q))
        ),
        mainPanel(
          plotlyOutput("map_plot", height = "90vh")
        )
      )
    ),
    tabPanel(
      "Victim characteristics",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "characteristics", "Select Characteristics",
            c("Victim Age", "Mode of Killing", "Victim gender", "Mental health status"),
            "Victim Age")
          ),
        mainPanel(
          plotlyOutput("dynamic_plot", height = "85vh")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$map_plot <- renderPlotly({
    req(input$quantiles)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = "#d9d9d9",
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = "#ffffff",
      countrycolor = "#ffffff"
    )
    plot_data <- df6 %>% filter(q %in% input$quantiles)
    fig <- plot_geo(plot_data, locationmode = 'USA-states', sizes = c(1, 250))
    fig <- fig %>% add_markers(
      x = ~lon, y = ~lat, size = ~Freq, color = ~ q, hoverinfo = "text",
      text = ~ paste(plot_data$state, plot_data$Freq)
    )
    fig <- fig %>% layout(title = 'Number of murders of POC by police, by state', geo = g)
    fig
  })
  observeEvent(input$characteristics, {
    if (input$characteristics == "Victim Age") {
      dynamic_plot_object <- ggplot(df, aes(x=dataBlack.vage))+
          geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+
          labs(title="Ages of Black victims of police brutality", x="Age", y = "Count")+
          theme_classic()
      dynamic_plot_object <- ggplotly(dynamic_plot_object)
    } else if (input$characteristics == "Mode of Killing") {
      dynamic_plot_object <- ggplot(data=df2, aes(x=mode, y=proportion)) +
        geom_bar(stat="identity", fill = "steelblue") +
        labs(title="Mode of killing") +
        theme_minimal()
      dynamic_plot_object <- ggplotly(dynamic_plot_object)
    } else if (input$characteristics == "Victim gender") {
      dynamic_plot_object <- ggplot(data=df3, aes(x=gender, y=proportion)) +
        geom_bar(stat="identity", fill = "black") +
        labs(title="Gender of victims") +
        theme_minimal()
      dynamic_plot_object <- ggplotly(dynamic_plot_object)
    } else if (input$characteristics == "Mental health status") {
      dynamic_plot_object <- ggplot(data=df5, aes(x=mental_illness, y=proportion)) +
          geom_bar(stat="identity", fill = "grey2") +
          labs(title="Mental health status of the victims") +
          theme_minimal()
      dynamic_plot_object <- ggplotly(dynamic_plot_object)
    }
    output$dynamic_plot <- renderPlotly({
      dynamic_plot_object
    })
  })
}


shinyApp(ui, server)