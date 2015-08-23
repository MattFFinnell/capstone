library(shiny)

fluidPage(
        titlePanel("Word Predicting with N-grams"),
        h4("Coursera Data Science Capstone by Matt Finnell", style="color:gray"),
        h3("Please allow a few seconds for the prediction engine to fully load."),
        hr(),
        
        # Supress Error messages
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        sidebarLayout(
                sidebarPanel(
                        textInput("text",label=h4("Enter Your Text Below:")
                ))
                ,mainPanel(
                        h2("Predicted next word:"),
                        h1(textOutput("prediction"))
                )
        )
)