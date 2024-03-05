library(shiny)
library(ggplot2)
library(patchwork)
library(dplyr)






# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Simple random sampling"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # sliderInput(inputId = "sample_n",
      #             label = "Sample size:",
      #             min = 0,
      #             max = 1000,
      #             value = 0, step = 1, ticks = T),
      
      numericInput(inputId = 'sample_n', label = 'Sample size', value = 0, min = 0, max = 10000, step = NA)  
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      p("Population = 100,000"),
      textOutput("sample_n_text"),
      
      # Output: Histogram ----
      plotOutput(outputId = "samplePlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$sample_n_text <- renderText({ 
    paste("Sample size =", input$sample_n)
  })
  
  
  
  output$samplePlot <- renderPlot({
  
  set.seed(2017)  
  groups <- letters[1:5] |> toupper()
  pop <- sample(groups, 100000, replace = T, prob = c(.2, .3, .05, .1, .4))
  pop_df <- table(pop) |> as.data.frame()
  
  pop_plot <- pop_df |> 
  ggplot(aes(x = pop, y = Freq, fill = factor(pop))) +
  geom_col() +
    theme_minimal() +
    scale_fill_manual(values = c('#E69F00','#56B4E9','#CC79A7','#009E73','#D55E00')) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(size = 16)) +
    guides(fill = 'none') +
    labs(x = 'Group', y = 'Count', title = 'Population')
  ###
  sample_n <- input$sample_n
  if(is.na(sample_n)){
    sample_n <- 0
  }
  
  
  sample <- sample(pop, sample_n)
  sample_df <- table(sample) |> as.data.frame()
  
  
  sample_fill <- data.frame(sample = groups, Freq = 0)
  sample_expand <- sample_fill[which(!groups %in% sample_df$sample),]
  
  if(dim(sample_expand)[1] > 0){
     sample_df <- rbind(sample_df, sample_expand)  
  }
  
  sample_df$sample <- factor(sample_df$sample, levels = letters[1:5] |> toupper())
  
  
  
  
  sample_plot <- sample_df |> 
    ggplot(aes(x = sample, y = Freq, fill = factor(sample))) +
    geom_col() +
    theme_minimal() +
    scale_fill_manual(values = c('#E69F00','#56B4E9','#CC79A7','#009E73','#D55E00')) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(size = 16)) +
    guides(fill = 'none') +
    labs(x = 'Group', y = 'Count', title = 'Sample')
  
  
  


  
  if(sample_n == 0){
    sample_plot <- sample_plot + theme_void()
  } 
  
  pop_plot + sample_plot
  
  
  
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
