library(shiny)
library(shinyjs)
library(magrittr)
library(ggplot2)
library(png)


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Otolith Analysis"),
  sidebarLayout(
    sidebarPanel(fileInput(
      "otolithImage",
      "Upload Otolith Image",
      accept = c("image/png", "image/jpeg")
    )),
    mainPanel(
      plotOutput(
        "otolithPlot",
        click = "plot_click",
        dblclick = "plot_dblclick",
        hover = "plot_hover"
      ),
      tableOutput("pointTable")
    )
  )
)

server <- function(input, output, session) {
  # Variables to store the state of the app
  appState <-
    reactiveValues(
      center = NULL,
      endPoint = NULL,
      points = data.frame(x = numeric(), y = numeric())
    )
  
  otolithImage <- reactiveVal(NULL)
  
  observeEvent(input$otolithImage, {
    otolithImage(readPNG(input$otolithImage$datapath))
  })
  
  plotCoordinates <- function(appState, otolithImage) {
    inFile <- input$otolithImage
    if (is.null(inFile)) {
      return(NULL)
    }
    
    img <- otolithImage()
    basePlot <-
      ggplot() + annotation_custom(grid::rasterGrob(
        img,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      -Inf,
      Inf,
      -Inf,
      Inf) +
      theme_void() +
      coord_cartesian(xlim = c(0, dim(img)[2]), ylim = c(0, dim(img)[1]))
    
    if (!is.null(appState$center)) {
      basePlot <-
        basePlot + geom_point(
          aes(x = appState$center[1], y = appState$center[2]),
          color = "blue",
          size = 3
        )
    }
    
    if (!is.null(appState$endPoint)) {
      basePlot <-
        basePlot + geom_point(
          aes(x = appState$endPoint[1], y = appState$endPoint[2]),
          color = "red",
          size = 3
        ) +
        geom_segment(
          aes(
            x = appState$center[1],
            y = appState$center[2],
            xend = appState$endPoint[1],
            yend = appState$endPoint[2]
          ),
          color = "black"
        )
    }
    
    points_df <-
      reactive({
        data.frame(x = appState$points$x, y = appState$points$y)
      })
    if (nrow(points_df()) > 0) {
      basePlot <-
        basePlot + geom_point(data = points_df(),
                              aes(x, y),
                              color = "green",
                              size = 3)
    }
    
    
    return(basePlot)
  }
  
  output$otolithPlot <- renderPlot({
    plotCoordinates(appState, otolithImage)
  }, bg = "transparent")
  
  observeEvent(input$plot_dblclick, {
    if (is.null(appState$center)) {
      appState$center <- c(input$plot_dblclick$x, input$plot_dblclick$y)
    } else if (is.null(appState$endPoint)) {
      appState$endPoint <- c(input$plot_dblclick$x, input$plot_dblclick$y)
    }
    output$otolithPlot <- renderPlot({
      plotCoordinates(appState, otolithImage)
    }, bg = "transparent")
  })
  
  observeEvent(input$plot_click, {
    if (!is.null(appState$center) && !is.null(appState$endPoint)) {
      clickPoint <- c(input$plot_click$x, input$plot_click$y)
      lineVector <- appState$endPoint - appState$center
      pointVector <- clickPoint - appState$center
      projectionFactor <-
        sum(lineVector * pointVector) / sum(lineVector * lineVector)
      
      if (projectionFactor >= 0 && projectionFactor <= 1) {
        newPoint <- appState$center + projectionFactor * lineVector
        appState$points <- rbind(appState$points, newPoint)
        names(appState$points) <- c("x", "y")
      }
    }
    output$otolithPlot <- renderPlot({
      plotCoordinates(appState, otolithImage)
    }, bg = "transparent")
  })
  
  # Render the table with point coordinates
  output$pointTable <- renderTable({
    appState$points
  })
  
  # Hover line
  observeEvent(input$plot_hover, {
    if (!is.null(appState$center) && is.null(appState$endPoint)) {
      hoverPoint <- c(input$plot_hover$x, input$plot_hover$y)
      isolate({
        output$otolithPlot <- renderPlot({
          basePlot <- plotCoordinates(appState, otolithImage)
          
          if (!is.null(hoverPoint) &&
              !is.null(appState$center) && is.null(appState$endPoint)) {
            basePlot <-
              basePlot + geom_segment(
                aes(
                  x = appState$center[1],
                  y = appState$center[2],
                  xend = hoverPoint[1],
                  yend = hoverPoint[2]
                ),
                color = "grey",
                linetype = "dashed"
              )
          }
          
          basePlot
        }, bg = "transparent")
      })
    }
  })
}

shinyApp(ui = ui, server = server)

