# Covid data viewer, a simple shiny app
# The data is from assignment 3

# Load R packages
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)

# Load data and define variables
covid_data <- fread("masked_path/covid_data.csv")

state_map <- setDT(map_data("state"))
state_map[, region := gsub("Of", "of", str_to_title(region))]
setnames(state_map, "region", "state_name")

state_names <- fread("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
names(state_names) <- c("state_name", "state")

covid_data <- merge(covid_data, state_names, by = "state")
data_heat_map <- covid_data[, lapply(.SD, mean),
                            by = "state_name", .SDcols = variables_heat_map]
data_heat_map <- merge(data_heat_map, state_map, by = "state_name")

states <- unique(covid_data$state_name)
variables_time_series <- c("deaths", "cases", "new_deaths", "new_cases",
                           "ifr", "mr")
variables_heat_map <- c("ifr", "mr")



# Define UI
ui <- fluidPage(
  navbarPage(
    "COVID Data Viewer",
    tabPanel(
      "Time series",
      sidebarPanel(
        selectInput(
          inputId = "state",
          label = "State",
          choices = states,
          selected = "Washington",
          multiple = TRUE
        ),
        selectInput(
          inputId = "variable_time_series",
          label = "Variable",
          choices = variables_time_series,
          selected = variables_time_series[1]
        )
      ),
      mainPanel(
        plotOutput("plot_time_series")
      )
    ),
    tabPanel(
      "Heat map",
      sidebarPanel(
        selectInput(
          inputId = "variable_heat_map",
          label = "Variable",
          choices = variables_heat_map,
          selected = variables_heat_map[1]
        )
      ),
      mainPanel(
        plotOutput("plot_heat_map")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  get_time_series_info <- reactive({
    data <- subset(covid_data, state_name %in% input$state)
    if (startsWith(input$variable_time_series, "new")) {
      data <- subset(data, date != min(covid_data$date))
    }
    list(
      data = data,
      state = paste(input$state, collapse = " & "),
      variable = input$variable_time_series
    )
  })
  get_heat_map_info <- reactive({
    list(
      variable = input$variable_heat_map
    )
  })
  
  output$plot_time_series <- renderPlot({
    info <- get_time_series_info() 
    ggplot(info$data) +
      geom_line(aes_string(x = "date",
                           y = info$variable,
                           color = "state")) +
      ggtitle(info$state)
  })
  output$plot_heat_map <- renderPlot({
    info <- get_heat_map_info()
    ggplot(data_heat_map) +
      geom_polygon(aes_string(x = "long",
                              y = "lat",
                              group = "group",
                              fill = info$variable))
  })
}

# Create shiny app
shinyApp(ui = ui, server = server)
