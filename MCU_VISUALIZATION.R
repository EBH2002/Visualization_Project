library(shiny)
library(rsconnect)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
MCU_Data <- read.csv("mcu dataset.csv")
Duration_column <- MCU_Data$Duration
split_values <- strsplit(Duration_column, " ")
converted_values <- lapply(split_values, function(x) {
  x_parts <- unlist(strsplit(x, "m "))  
  hours <- as.numeric(gsub("h", "", x_parts[1]))  
  minutes <- as.numeric(gsub("min", "", x_parts[2]))  
  total_hours <- hours + minutes/60 
  return(total_hours)
})
MCU_Data$Duration <- unlist(converted_values)

Dollars_Col=c("Budget", "Domestic.Gross","Total.Gross","Opening.Gross")
options(scipen = 999)
for (column_name in Dollars_Col){
  MCU_Data[[column_name]] <- as.numeric(gsub("[\\$,]", "", MCU_Data[[column_name]]))
}
MCU_Data[22, "Total.Gross"] <- as.numeric(2797501328)

Cols_to_Rename=c("US.release.Date","IMDB.rating","Domestic.Gross","Total.Gross","Opening.Gross","Oscar.Nomination","Oscar.won")
New_Col_Names=c("US Release Date","IMDB Rating","Domestic Gross","Total Gross","Opening Gross","Oscar Nomination","Oscar won")
Col_index=match(Cols_to_Rename,colnames(MCU_Data))
colnames(MCU_Data)[Col_index] <- New_Col_Names
MCU_Data$ROI = ((MCU_Data$'Total Gross' - MCU_Data$Budget) / MCU_Data$Budget) * 100
colnames(MCU_Data)[colnames(MCU_Data) == "metascore"] <- "Metascore"

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "MARVEL CINEMAS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ABOUT THE PROJECT", tabName = "introduction"),
      menuItem("SUCCESS METRICS", tabName = "success_track"),
      menuItem("BEST DIRECTOR AND LEAD ACTOR", tabName = "best_movies_directors"),
      menuItem("CORRELATION ANALYSIS", tabName = "correlations"),
      menuItem("CONCLUSION", tabName = "conclusion")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h3(HTML("<h1 style='text-align: center; text-decoration: underline; font-size: 22px; font-weight: bold; color: black;'>MARVEL CINEMATIC UNIVERSE FROM PHASE-1 TO PHASE-3</h1>
    <p style='text-align: center;color: black;font-size: 12px'>ESHA BHATTACHARYA<br>ROLL NO.: MDS202324<br>esha.mds2023@cmi.ac.in</p>
")),
        tags$p(style = "font-size: 18px; font-weight: bold;text-decoration: underline;", "ABOUT THE PROJECT"),
        fluidRow(
          tabBox(
            id = "introduction", height = "400px", width = 12,
            tabPanel(strong("INTRODUCTION"), uiOutput("introduction_text")),
            tabPanel(
              strong("DATA DESCRIPTION"),
              uiOutput("d_d_text"),
              div(
                style = "height: 300px; overflow-y: scroll;",
                dataTableOutput("data_table")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "success_track",
        h3("Performance of the movies till 2019"),
        p("We will see how has the marvel movies performed based on IMDB ratings and ROI:"),
        fluidRow(
          tabBox(
            id = "success_track", height = "400px", width = 12,
            tabPanel(
              strong("ROI of movies"),
              plotOutput("roi_plot"),
              "Commercially Marvel has seen good success rate with most of its movies having ROI of more than 300%. The mean Return on Investment has increased significantly after phase 1."
            ),
            tabPanel(
              strong("IMDB Ratings"),
              plotOutput("imdb_plot"),
              "Marvel movies have balanced critical reception, the IMDB ratings of movies have been consistently stable through the three phases."
            )
          )
        )
      ),
      tabItem(
        tabName = "best_movies_directors",
        h3("Best lead actor and director based on average gross and average IMDB ratings"),
        p("We will look at the average IMDB rating and average gross of all the movies of each director and lead actor:"),
        fluidRow(
          tabBox(
            id = "best_movies_directors", height = "400px", width = 12,
            selectInput("best_category", "Select Category", choices = c("Director", "Lead Actor")),
            selectInput("best_metric", "Select Metric", choices = c("Average IMDB Rating", "Average Gross")),
            tabPanel(
              "",
              plotOutput("best_movies_directors_plot"),
              textOutput("b_m_text")
            )
          )
        )
      ),
      tabItem(
        tabName = "correlations",
        h3("Correlation between some parameters of success and other factors"),
        p("We will check if there is any relation between the several parameters of success with other factors such as budget, metascore, etc., and see how they are correlated:"),
        fluidRow(
          tabBox(
            id = "correlations", height = "400px", width = 12,
            selectInput("correlation_var1", "Variable 1", choices = c("IMDB Rating", "ROI", "Budget", "Oscar Nomination", "Metascore", "Opening Gross", "Total Gross")),
            selectInput("correlation_var2", "Variable 2", choices = c("IMDB Rating", "ROI", "Budget", "Oscar Nomination", "Metascore", "Opening Gross", "Total Gross")),
            tabPanel(
              "",
              plotOutput("correlations_plot"),
              textOutput("c_text")
            )
          )
        )
      ),
      tabItem(
        tabName = "conclusion",
        uiOutput("conclusion_text")
      )
    )
  )
)


server <- function(input, output,session) {
  output$introduction_text <- renderUI({
    intro_text <-HTML("<h2 style='text-decoration: underline; font-size: 18px; font-weight: bold; color: black;'>INTRODUCTION</h2>

                <p style='font-size: 16px'>The Marvel Cinematic Universe is one of the largest and highest-earning film franchises of all time with a revenue of $21.4 billion, surpassing classics such as Star Wars and Harry Potter. The 22nd movie in this franchise, Avengers: Endgame — a thrilling culmination of the buildup and hype created over the infinity stones in the last decade — became the highest-grossing film of all time at $2.79 billion. We will try to gain some insights into the performance of each movie, how a director's or actor's work was received by the audience and critics, and if there is any relation between the parameters of success, budget, etc.</p>

                ")
    return(intro_text)
  })
  output$d_d_text<-renderUI({
    dt_text<-HTML("
    <h2 style='text-decoration: underline; font-size: 18px; font-weight: bold; color: black;'>DATA DESCRIPTION</h2>

                <p>The Marvel Cinematic Universe (MCU) dataset comprises information about 23 movies of the Marvel Cinematic Universe, in chronological order. Below is a list of the column names:</p>

                <table style='width:100%'>
                  <tr>
                    <th style='text-align: left;'>COLUMN NAMES</th>
                    <th></th>
                  </tr>
                  <tr>
                    <td>Name</td>
                    <td>Budget</td>
                  </tr>
                  <tr>
                    <td>US Release Date</td>
                    <td>Domestic Gross</td>
                  </tr>
                  <tr>
                    <td>Director</td>
                    <td>Total Gross</td>
                  </tr>
                  <tr>
                    <td>Producer</td>
                    <td>Opening Gross</td>
                  </tr>
                  <tr>
                    <td>Duration</td>
                    <td>Oscar Nomination</td>
                  </tr>
                  <tr>
                    <td>Genre</td>
                    <td>Oscar Won</td>
                  </tr>
                  <tr>
                    <td>IMDB Rating</td>
                    <td>Phase</td>
                  </tr>
                  <tr>
                    <td>Metascore</td>
                    <td>ROI (Return On Investment)</td>
                  </tr>
                  <tr>
                    <td>Cast</td>
                    <td></td>
                  </tr>
                </table>

                <p>The data contain all this information for all the Marvel movies from phase 1 to phase 3. Here is a glimpse of the dataset:")
    return(dt_text)
  })
  output$data_table <- renderDT({
    colnames(MCU_Data) <- as.character(colnames(MCU_Data))
    
    datatable(head(MCU_Data), options = list(pageLength = 6, lengthChange=FALSE, searching = FALSE))
  })
  
  
  movie_order <- factor(MCU_Data$Name, levels = MCU_Data$Name)
  
  output$imdb_plot <- renderPlot({
    ggplot(MCU_Data, aes(x = movie_order, y = `IMDB Rating`, fill = as.factor(Phase))) +
      geom_bar(stat = "identity") +
      labs(title = "Movies vs IMDb Ratings",
           x = "Movies",
           y = "IMDb Rating") +
      scale_fill_manual(values = c("blue", "maroon", "gold"),
                        labels = c("Phase 1", "Phase 2", "Phase 3"),
                        name = "Phases") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$roi_plot <- renderPlot({
    ggplot(MCU_Data, aes(x = factor(Phase, labels = c("Phase 1", "Phase 2", "Phase 3")), y = ROI)) +
      geom_boxplot(col = "black", fill = "lavender") +
      geom_jitter(width = 0.2, alpha = 0.7, color = "black", size = 1) +
      labs(
        title = "Box Plots of ROI VS. Movies in each phase",
        x = "Phase",
        y = "ROI"
      ) +
      theme_minimal()
  })
  
  output$best_movies_directors_plot <- renderPlot({
    category <- input$best_category
    metric <- input$best_metric
    MCU_Data <- MCU_Data %>%
      mutate(MainActor = sub(",.*", "", Cast))

    if (category == "Director" && metric == "Average IMDB Rating") {
      director_ratings <- MCU_Data %>%
        group_by(Director) %>%
        summarise(Avg_IMDB_Rating = mean(`IMDB Rating`, na.rm = TRUE))
      ggplot(data = director_ratings, aes(x = reorder(Director, Avg_IMDB_Rating), y = Avg_IMDB_Rating, group = 1)) +
        geom_bar(stat = "identity", fill = "olivedrab4") +
        labs(title = "Average IMDb Ratings VS. Director",
             x = "Director",
             y = "Average IMDb Ratings") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(limits = director_ratings$Director)
    } else if (category == "Director" && metric == "Average Gross") {
      director_avg_gross <- MCU_Data %>%
        group_by(Director) %>%
        summarise(Avg_Gross = mean(`Total Gross`, na.rm = TRUE))
      director_avg_gross['Avg_Gross'] = director_avg_gross['Avg_Gross']/10^9
      ggplot(data = director_avg_gross, aes(x = reorder(Director, Avg_Gross), y = Avg_Gross, group = 1)) +
        geom_bar(stat = "identity", fill = "gold3") +
        labs(title = "Average Gross VS. Director",
             x = "Director",
             y = "Average Gross in Billions") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(limits = director_avg_gross$Director)
    } else if (category == "Lead Actor" && metric == "Average IMDB Rating") {
      actor_avg_imdb <- MCU_Data %>%
        group_by(MainActor) %>%
        summarise(AvgIMDb = mean(`IMDB Rating`, na.rm = TRUE))
      ggplot(actor_avg_imdb, aes(x = reorder(MainActor, -AvgIMDb), y = AvgIMDb)) +
        geom_bar(stat = "identity", fill = "firebrick4") +
        labs(
          title = "Main Actor vs. Average IMDb ratings",
          x = "Main Actor",
          y = "Average IMDb ratings"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } else if (category == "Lead Actor" && metric == "Average Gross") {
      actor_avg_gross <- MCU_Data %>%
        group_by(MainActor) %>%
        summarise(AverageGross = mean(`Total Gross`))
      actor_avg_gross['AverageGross'] = actor_avg_gross['AverageGross']/10^9
      ggplot(actor_avg_gross, aes(x = reorder(MainActor, -AverageGross), y = AverageGross)) +
        geom_bar(stat = "identity", fill = "turquoise4") +
        labs(
          title = "Main Actor vs. Average Gross",
          x = "Main Actor",
          y = "Average Gross in billions"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    }
  })
  output$b_m_text<-renderText({
    t2<-"Chris Patt has the highest average IMDB ratings and Robert Downey Jr. has the highest average gross.The Russo brothers, that is Anthony and Joe Russo have the highest average IMDB and Average Gross."
    return(t2)
    })
  output$correlations_plot <- renderPlot({
    correlation_vars <- c(input$correlation_var1, input$correlation_var2)
    plot_data <- MCU_Data[, correlation_vars]
    ggplot(plot_data, aes(x = !!sym(correlation_vars[1]), y = !!sym(correlation_vars[2]))) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE,col="blue") +
      labs(title = paste("Correlation Plot: ", correlation_vars[1], " vs ", correlation_vars[2]),
           x = correlation_vars[1],
           y = correlation_vars[2])
  })
  output$c_text<-renderText({
    "All the variables show positive correlation, though all are not very highly correlated compared to a few pairs."
  })
  output$conclusion_text <- renderUI({
    con_text<-HTML("<h2 style='text-decoration: underline; font-size: 18px; font-weight: bold; color: black;'>CONCLUSION</h2>

    <p>The bar graphs of actors and directors against average IMDB ratings and average gross show how the lead cast and the directors have contributed to the success of the franchise. Through the correlation plots, we explore the correlation between ratings, award nominations, and other financial parameters, where all of the variables are found to have positive correlation, though several pairs of variables have low correlation. It is evident that the Marvel Cinematic Universe has seen substantial growth in commercial success, with Avengers: Endgame being the highest grosser, and the movies have consistently received favorable ratings.")
  return(con_text)
    })
}

# Run the application
shinyApp(ui, server)
