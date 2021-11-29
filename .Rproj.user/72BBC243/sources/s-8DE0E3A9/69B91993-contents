library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

hotel_information <- read.csv('../hotel_bookings.csv')
#hotel_information <- read.csv('./hotel_bookings.csv')

hotel_information <- hotel_information
#hotel_leadtime <- hotel_information %>% select(`lead_time`,`is_canceled`)
#group by
hotel_information

mtcars <- hotel_information %>% group_by(lead_time, arrival_date_year) %>%  dplyr ::summarise(count = n(),
                                                                   canceled = sum(is_canceled)) %>% mutate(per = canceled/count) %>% as.data.frame()

ui <- fluidPage(fluidRow(
    column(
        6,
        checkboxGroupInput(
            "checkGroup",
            label = h3("Checkbox group"),
            choices = list(2015, 2016, 2017),
            selected = 2015
        ),
    ),
    column(
        width = 6,
        plotOutput(
            "plot1",
            height = 350,
            click = "plot1_click",
            brush = brushOpts(id = "plot1_brush")
        ),
        actionButton("exclude_toggle", "Toggle points"),
        actionButton("exclude_reset", "Reset")
    )
))

server <- function(input, output) {
    # For storing which rows have been excluded
    
    # hotel_information <- hotel_information %>% filter(arrival_date_year == input$checkGroup)
    # hotel_leadtime <- hotel_information %>% select(`lead_time`,`is_canceled`)
    # #group by
    # 
    # mtcars <- hotel_leadtime %>% group_by(lead_time) %>%  dplyr ::summarise(count = n(),
    #                                                                         canceled = sum(is_canceled)) %>% mutate(per = canceled/count) %>% as.data.frame()
    
    
    vals <- reactiveValues(
        keeprows = rep(TRUE, nrow(mtcars))
    )
    
    output$plot1 <- renderPlot({
        print(input$checkGroup)
        # Plot the kept and excluded points as two separate data sets
        #mtcars <- mtcars %>% filter(arrival_date_year == input$checkGroup)
        keep    <- mtcars[ vals$keeprows, , drop = FALSE]
        exclude <- mtcars[!vals$keeprows, , drop = FALSE]
        
        if (!is.null(input$checkGroup)) {
            keep  <- keep %>% filter(arrival_date_year == input$checkGroup)
            exclude  <- exclude %>% filter(arrival_date_year == input$checkGroup)
        }
        
        
        ggplot(keep, aes(lead_time, per)) + geom_point() +
            geom_smooth(method = lm, fullrange = TRUE, color = "black") +
            geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25)
    })
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
        res <- nearPoints(mtcars, input$plot1_click, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
        res <- brushedPoints(mtcars, input$plot1_brush, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, nrow(mtcars))
    })
    
}

shinyApp(ui, server)
