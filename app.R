library(shiny)
library(tidyverse)
library(ggpattern)

mug = png::readPNG("C://Users//jking//OneDrive//Documents//R//sandbox//trivia//mugs3.png")
foam_filename = "C://Users//jking//OneDrive//Documents//R//sandbox//trivia//foam.png"
qs = readxl::read_xlsx("C://Users//jking//OneDrive//Documents//R//sandbox//trivia//questions.xlsx")

score = tibble(
    Team = c("A-C", "D-L", "M-Z"), 
    Score = c(10, 10, 10))


ui <- navbarPage("TRAC Bar Trivia!",
                 theme = shinythemes::shinytheme("darkly"),
                 
                 tabPanel("Rules",
                          includeMarkdown("C://Users//jking//OneDrive//Documents//R//sandbox//trivia//rules.md")),
                 
                 tabPanel("Game",
                          
                          sidebarLayout(
                              sidebarPanel(width = 2,
                                  selectInput("category", "Choose a category:",
                                              choices = c("Arts", "Entertainment", "Geography",
                                                          "History", "Operations Research", "Science")),
                                  
                                  actionButton("get_question", "Get Question"),
                                  hr(),
                                  h3("Timer:"), 
                                  h3(textOutput("eventTimeRemaining")),
                                  hr(),
                                  selectInput("winner", "Select the winner:",
                                              choices = c("A-C", "D-L", "M-Z", "None")),
                                  actionButton("award_points", label = "Award Points")
                              ),
                              mainPanel(
                                  h4("Question: "),
                                  h2(textOutput("qText")),
                                  tags$head(tags$style("#qText{color: yellow;font-style: italic;}")),
                                  hr(),
                                  h4("Questions Remaining"),
                                  tableOutput('tbl'),
                                  plotOutput('plot')
                                  
                              )
                          )
                 ),
                 
                 tabPanel("Team Prize", includeMarkdown("C://Users//jking//OneDrive//Documents//R//sandbox//trivia//duck.md")),
                 
                 tabPanel("MVP", includeMarkdown("C://Users//jking//OneDrive//Documents//R//sandbox//trivia//cert.md"))
)

# Define server logic
server <- function(input, output, session) {
    
    questions <- eventReactive(input$get_question, {
        q = qs %>% filter(Category == input$category & Available==TRUE) %>% slice(1) %>% .$Question
        qs <<- qs %>% mutate(
            Order = if_else(Question==q, max(qs$Order)+1, Order),
            Available = if_else(Question==q, FALSE, Available))
        qs
    }, ignoreNULL = FALSE)

    output$qText = renderText({questions() %>% filter(Order==max(Order)) %>% .$Question})
    
    output$tbl <- renderTable({ questions() %>%
            group_by(Category) %>%
            summarize(Remaining = sum(Available), .groups = "drop_last") 
    })
    
    EventTime <- reactiveVal()
    
    observeEvent(input$get_question,{
        EventTime(Sys.time() + 30)
    })
    
    output$eventTimeRemaining <- renderText({
        req(input$get_question)
        timeLeft <- round(difftime(EventTime(), Sys.time(), units='secs'))
        if(timeLeft > 0){
            invalidateLater(1000, session)
            msg <- timeLeft
        }else{msg <- "0"}
        msg
    })
    
    scores <- eventReactive(input$award_points, {
        score <<- score %>% mutate(Score = if_else(Team == input$winner, Score-1, Score))
    })
    
    output$plot <- renderPlot({
        ggplot(scores(), aes(x=Team, y=Score, fill=Team)) + 
            annotation_custom(grid::rasterGrob(mug, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
            geom_col_pattern(
                pattern_gravity = 'North', 
                pattern_scale = 1,
                pattern = 'image',
                pattern_filename = foam_filename,
                pattern_type = 'fit',
                width=0.45, position = position_dodge(width=0.5)) + 
            scale_fill_manual(values = c('gold1', 'orange3', 'tomato4')) +
            scale_y_continuous(breaks=seq(0,10,1), limits=c(0,10)) +
            theme_dark() + 
            ggtitle("Team Score") +
            ylab("Remaining") +
            theme(plot.title = element_text(color = 'white', hjust = 0.5, size=rel(2)),
                  plot.background = element_rect(color="#222222", fill="#222222"),
                  panel.background = element_rect(fill = "black"),
                  legend.position = "none",
                  axis.line.x = element_line(color = "lightgray"),
                  axis.line.y = element_line(color = "lightgray"),
                  axis.title.x = element_text(color="white", size=rel(1.5)),
                  axis.title.y = element_text(color="white", size=rel(1.5)),
                  axis.text = element_text(color="white", size=rel(1.5)))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
