#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(extrafont)
library(shinythemes)

shinyApp(
  ui = fluidPage(
    theme = shinythemes::shinytheme("darkly"),
    navbarPage(
      "Recommendation System",
      tabPanel("Overview",
               #mainPanel(
                   h1("Music Recommendation System ",align = "center"),
               br(),
               p("The Last.FM dataset contains social networking, tagging, and music artist listening information 
    from a set of 2K users from Last.fm online music system.",align = "center"),
               p("Dataset: Last.FM",align = "center"),
               p("1892 users",align = "center"),
               p("17632 artists",align = "center"),
               p("11946 tags",align = "center"),
               p("92834 relationsship [user, tag, artist]",align = "center"),
               br(),
                   h4("Yemariam Mamo",align = "center"),
                   h5("Capstone Project",align = "center"),
                   h5("12/16/2016",align = "center")
              
               #)
      ),
      
      tabPanel("Collaborative Filtering",
          h1("Item-based Collaborative Filtering",align = "center"),
          h5("Methods used: Matrix Similarity"),
          
          br(),
          
          
          fluidRow(
            selectInput("artist", "Select an Artist:", selected = "Brandy",
                        choices = colnames(sim_item_mx))),
          br(),
          fluidRow(
            tableOutput("table"))

          ),
      tabPanel("Content-Based Filtering",
               h1("Content-Based Filtering", align = "center"),
               h5("Methods used: Tag Frequency "),
               fluidRow(
                 selectInput('tag', 'Type a tag (ex: hip hop)', alltags, multiple=TRUE, selectize=TRUE, selected="dance")),
               fluidRow(
                 tableOutput("artist.list"))
               
               ))
      
    

  ),
  
  
  server = function(input, output) {
    
    df <- as.data.frame(0, ncol = 10, nrow = 2)
    #output$table <- renderDataTable({df})
    
    output$table <- renderTable({{df}
      data1 <- artists_recomm %>%
        filter(artist == input$artist) %>%
        sort(decreasing = TRUE) %>%
        top_n(10)
       
    }
    )
  
    output$artist.list <- renderTable({
      
      data <- artist_tags %>%
        filter(tagValue %in% unlist(input$tag)) %>%
        select(name, n) %>%
        ungroup() %>%
        arrange(desc(n))  %>% 
        top_n(10)
        data
    })
    
  }
  ,options = list(height = 500)
)




