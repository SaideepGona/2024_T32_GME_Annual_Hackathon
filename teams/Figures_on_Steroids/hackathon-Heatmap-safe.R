
library('shiny')
library('ggplot2')
library('dplyr')
library(colourpicker)
library(shinyWidgets)


interactive = TRUE


# Set the working directory
setwd()

## START
safe_data <- read.csv("safe_data.csv")



toPlot <- 1
bkgd_nick <- "bkgd1"
org_name <- "AncSR"


se_plot1.data <- safe_data


# Only run these examples in interactive R sessions
if (interactive()) {
  # Demo of clicking, hovering, brushing with imageOutput
  # Note that coordinates are in pixels
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(colourInput("col1", "Lower colour", "#A3A1A1"), colourInput("col2", "Upper colour", "#FA0F0F"),
               width = 4,
               plotOutput("image", height=400,
                          width = 600,
                          click = "image_click",
                          hover = hoverOpts(
                            id = "image_hover",
                            delay = 500,
                            delayType = "throttle"
                          ),
                          brush = brushOpts(id = "image_brush")
               )
        ),
        column(noUiSliderInput(inputId = "Slider", label = "Bin Number",
                           min = 1, max = 30, value = 10, step = 1, orientation = "vertical",
                           width = "50px", height = "200px", direction = "rtl"),
               width = 2),
        
        
        column(width = 4,
               plotOutput("image_clickinfo")
        ),
        column(width = 2,
               verbatimTextOutput("image_clickinfo2")
        )
      )
    ),
    server = function(input, output, session) {
      
      output$image <- renderPlot({
        strata_2nd_base <- ggplot(data = filter(se_plot1.data, set_group == "2nd"), aes(x = n_total, y = avgF)) + 
          geom_bin2d(bins = input$Slider) + scale_x_log10() +
          scale_fill_gradient2(mid=input$col1, high=input$col2)  + xlab("Read depth") + ylab("Fluorescence")
        
        strata_2nd_base
        
        
        #return(strata_2nd_base)
      })
      
      output$image_clickinfo <- renderPlot({
        if (is.null(input$image_click)){
          ggplot()
        }else{
          bin <- ggplot_build(ggplot(data = filter(se_plot1.data, set_group == "2nd"), aes(x = n_total, y = avgF)) + 
                                geom_bin2d(bins = input$Slider) + scale_x_log10() +
                                scale_fill_gradient2(mid=input$col1, high=input$col2))$data[[1]] %>% filter(xmin < log10(input$image_click$x[1]) & xmax > log10(input$image_click$x[1]) & ymin < input$image_click$y[1] & ymax > input$image_click$y[1])
          sub_plot <- filter(safe_data, avgF > bin[1,'ymin'], avgF < bin[1,'ymax'], n_total > 10^bin[1,'xmin'], n_total < 10^bin[1,'xmax'])
          #print(sub_plot)
          n_variants <- nrow(sub_plot)
          ggplot(data = sub_plot, aes(x = SE)) + geom_density(fill = bin$fill, alpha = 0.5) +
            ggtitle(label = "", subtitle = paste0(n_variants, " variants")) +
            xlim(0, max(se_plot1.data$SE)) + xlab("Standard Error")
        }
      }, res = 96)
      output$image_clickinfo2 <- renderPrint({
        cat("Clicked:\n")
        str(input$image_click)
      })
    }
  )
}




