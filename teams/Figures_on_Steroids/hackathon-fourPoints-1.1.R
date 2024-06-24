library(plotly)
library(purrr)
library(shiny)

#parameters
curve_x_values <- seq(-10, 10, 0.01)
U.fit <- -0.644925
L.fit <- -2.347539
WT_value <- -0.8658


g_of_x <- function(x, L = L.fit, R = U.fit - L.fit) {
  F_value = L + (R / (1 + exp(-x)))
  return(F_value)
}


g_inverse_of_F <- function(F_value, L = L.fit, R = U.fit - L.fit) {
  x = -1*(log((R/(F_value-L)) - 1))
  return(x)
}

ui <- fluidPage(
  fluidRow(
    column(5, verbatimTextOutput("summary")),
    column(7, plotlyOutput("p")),
    column(3, numericInput(inputId = 'single_mutant_1', label = 'F (single mutant 1)', value = -1, min = round(L.fit, 2), max = round(U.fit, 2), step = 0.01)),
    column(3, numericInput(inputId = 'single_mutant_2', label = 'F (single mutant 2)', value = -1.5, min = round(L.fit, 2), max = round(U.fit, 2), step = 0.01)),
    column(3, numericInput(inputId = 'double_mutant', label = 'F (double mutant)', value = -2, min = round(L.fit, 2), max = round(U.fit, 2), step = 0.01))
  )
)

server <- function(input, output, session) {
  
  single_mutant_1 <- list()
  
  single_mutant_1$y <- reactive({
    return(input$single_mutant_1)
  })
  
  single_mutant_2 <- list()
  
  single_mutant_2$y <- reactive({
    return(input$single_mutant_2)
  })
  
  double_mutant <- list()
  
  double_mutant$y <- reactive({
    return(input$double_mutant)
  })
  
  output$p <- renderPlotly({
    
    
    # creates a list of circle shapes from x/y data
    single_mutant_1_circle <- map(single_mutant_1$y(),
                                  ~list(
                                    type = "circle",
                                    # anchor circles at curve
                                    xanchor = g_inverse_of_F(.x),
                                    yanchor = .x,
                                    # give each circle a 2 pixel diameter
                                    x0 = -4, x1 = 4,
                                    y0 = -4, y1 = 4,
                                    xsizemode = "pixel",
                                    ysizemode = "pixel",
                                    # other visual properties
                                    fillcolor = "blue",
                                    line = list(color = "transparent")
                                  )
    )
    
    single_mutant_2_circle <- map(single_mutant_2$y(),
                                  ~list(
                                    type = "circle",
                                    # anchor circles at curve
                                    xanchor = g_inverse_of_F(.x),
                                    yanchor = .x,
                                    # give each circle a 2 pixel diameter
                                    x0 = -4, x1 = 4,
                                    y0 = -4, y1 = 4,
                                    xsizemode = "pixel",
                                    ysizemode = "pixel",
                                    # other visual properties
                                    fillcolor = "blue",
                                    line = list(color = "transparent")
                                  )
    )
    
    double_mutant_circle <- map(double_mutant$y(),
                                ~list(
                                  type = "circle",
                                  # anchor circles at curve
                                  xanchor = g_inverse_of_F(.x),
                                  yanchor = .x,
                                  # give each circle a 2 pixel diameter
                                  x0 = -4, x1 = 4,
                                  y0 = -4, y1 = 4,
                                  xsizemode = "pixel",
                                  ysizemode = "pixel",
                                  # other visual properties
                                  fillcolor = "purple",
                                  line = list(color = "transparent")
                                )
    )    
    
    
    single_mutant_1_rectangle <- map(single_mutant_1$y(),
                                     ~list(
                                       type = "rect",
                                       xanchor = g_inverse_of_F(.x),
                                       yanchor = .x,
                                       # give each circle a 2 pixel diameter
                                       x0 = -1, x1 = 1,
                                       y1 = 20, y0 = -20,
                                       xsizemode = "pixel",
                                       ysizemode = "pixel",
                                       # other visual properties
                                       fillcolor = "blue",
                                       line = list(color = "transparent")
                                     )
    )
    
    single_mutant_2_rectangle <- map(single_mutant_2$y(),
                                     ~list(
                                       type = "rect",
                                       xanchor = g_inverse_of_F(.x),
                                       yanchor = .x,
                                       # give each circle a 2 pixel diameter
                                       x0 = -1, x1 = 1,
                                       y1 = 20, y0 = -20,
                                       xsizemode = "pixel",
                                       ysizemode = "pixel",
                                       # other visual properties
                                       fillcolor = "blue",
                                       line = list(color = "transparent")
                                     )
    )
    
    double_mutant_rectangle <- map(double_mutant$y(),
                                   ~list(
                                     type = "rect",
                                     xanchor = g_inverse_of_F(.x),
                                     yanchor = .x,
                                     # give each circle a 2 pixel diameter
                                     x0 = -1, x1 = 1,
                                     y1 = 20, y0 = -20,
                                     xsizemode = "pixel",
                                     ysizemode = "pixel",
                                     # other visual properties
                                     fillcolor = "purple",
                                     line = list(color = "transparent")
                                   )
    )  
    
    # plot the shapes and fitted line
    plot_ly() %>%
      add_lines(x = curve_x_values, y = g_of_x(curve_x_values), color = I("red")) %>%
      add_markers(x = g_inverse_of_F(WT_value), y = WT_value, type = "scatter", mode = "markers", color = I("black"), marker=list(size = 8, symbol = 'square')) %>%
      add_markers(x = signif((g_inverse_of_F(single_mutant_1$y()) - g_inverse_of_F(WT_value)) + (g_inverse_of_F(single_mutant_2$y()) - g_inverse_of_F(WT_value)) + g_inverse_of_F(WT_value), digits = 5),
                  y = signif(g_of_x(((g_inverse_of_F(single_mutant_1$y()) - g_inverse_of_F(WT_value)) + (g_inverse_of_F(single_mutant_2$y()) - g_inverse_of_F(WT_value)) + g_inverse_of_F(WT_value))), digits = 5),
                  type = "scatter", mode = "markers", color = I("green"), marker=list(size=8, sizemode="pixel")) %>%
      layout(shapes = c(single_mutant_1_circle, single_mutant_2_circle, double_mutant_circle, single_mutant_1_rectangle, single_mutant_2_rectangle, double_mutant_rectangle),
             xaxis = list(title = 'Genetic Score (x)'),
             yaxis = list(title = 'Fluorescence (F)')
             #autosize = F, width = 500, height = 500, margin = list(l = 100, r = 100, b = 100, t = 100, pad = 4 )
      ) %>%
      config(edits = list(shapePosition = TRUE))
  })
  
  # Output text "summary"
  output$summary <- renderText({
    paste0("Summary (x, F): ", 
           "\nWT: (", signif(g_inverse_of_F(WT_value), digits = 5), ", ", signif(WT_value, digits = 5), ")",
           "\nSingle 1: (", signif(g_inverse_of_F(single_mutant_1$y()), digits = 5), ", ", signif(single_mutant_1$y(), digits = 5), ")",
           "\nSingle 2: (", signif(g_inverse_of_F(single_mutant_2$y()), digits = 5), ", ", signif(single_mutant_2$y(), digits = 5), ")",
           "\nDouble: (", signif(g_inverse_of_F(double_mutant$y()), digits = 5), ", ", signif(double_mutant$y(), digits = 5), ")",
           "\nExpected double mutant: (", 
           signif((g_inverse_of_F(single_mutant_1$y()) - g_inverse_of_F(WT_value)) + (g_inverse_of_F(single_mutant_2$y()) - g_inverse_of_F(WT_value)) + g_inverse_of_F(WT_value), digits = 5),
           ", ", signif(g_of_x(((g_inverse_of_F(single_mutant_1$y()) - g_inverse_of_F(WT_value)) + (g_inverse_of_F(single_mutant_2$y()) - g_inverse_of_F(WT_value)) + g_inverse_of_F(WT_value))), digits = 5),
           ")"
    )
  })
  
  
  
  # update x/y reactive values in response to changes in shape anchors
  observe({
    ed <- event_data("plotly_relayout")
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    if (length(shape_anchors) != 2) return()
    row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
    print(row_index)
    pts <- as.numeric(shape_anchors)
    
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      'single_mutant_1',
      value = ifelse(row_index %in% c(1, 4), round(pts[2], 2), single_mutant_1$y()),
    )
    
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      'single_mutant_2',
      value = ifelse(row_index %in% c(2, 5), round(pts[2], 2), single_mutant_2$y()),
    )
    
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      'double_mutant',
      value = ifelse(row_index %in% c(3, 6), round(pts[2], 2), double_mutant$y()),
    )
  })
}

shinyApp(ui, server)