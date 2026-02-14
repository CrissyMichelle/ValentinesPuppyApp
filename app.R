#
# Toy web app for displaying images of puppies based
# on the breed selected by user input.
#
# The bottom half allows the user to interactively produce
# various tables and plots based on Old Faithful geyser data and click events.
#

library(shiny)
library(ggplot2)
library(munsell)
library(glue)
library(bslib)
library(tibble)
library(dplyr)

# Puppy photos from Unsplash
puppies <- tibble::tribble(
  ~breed, ~id, ~author,
  "bernese mountain dog","yngXHs2eS_M", "PetPonder",
  "golden retriever", "9LkqymZFLrE", "billstephan",
  "labrador", "czHm-pEraKM", "garrettkaroski",
  "child_playing-w/sheep, I mean, Great Pyrenees","Ziuo9zxhTog", "lauraohlman",
  "rottweiler", "R0y2bRvuDoc", "shanarajpoot",
  "saint-bernard", "veUSWM5CTbM", "marshall-public-library"
)

# User Interface
ui <- fluidPage(
  # Create theme with Bootstrap library
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("SN Pro"),
    heading_font = font_google("Poppins")
  ),
  # Add styles.css for gift energy
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # HERO
  div(
    class = "hero",
    h1(
      HTML("Happy Valentine's Janey! 🐶🐶🐶")
    ),
    div(
      class = "sub",
      HTML("Peruse puppy pics plus a sprig of stats. Click around! 💫")
    )
  ),

  # Let's try one on top of the other
    card(
      class = "cardish",
      card_header(
        HTML("🐾 Pupps <span class='accent' style='font-weight:600;'>- pick a breed</span>")
      ),
      card_body(selectInput("id", NULL,
                            choices = setNames(puppies$id, puppies$breed)),
      div(class = "source-line", uiOutput("source")),
      div(class = "photo-frame", uiOutput("photo"))
      )
    ),
  
  div(class = "soft-hr"),

    card(
      class = "cardish",
      card_header(
        HTML("Ol' Faithful Never Wavers <span class='accent' style='font-weight:600;'>- click the plot</span>")
      ),
      card_body(
        radioButtons(
          "view_mode", NULL,
          choices = c(
            "Histogram: waiting time" = "hist",
            "Scatter: eruptions vs waiting" = "scatter"
          ),
          inline = TRUE
        ),
        sliderInput("bins",
                    "Number of bins:",
                    min = 5,
                    max = 60,
                    value = 25),
        # A little stats strip
        uiOutput("faith_stats"),
        
        plotOutput("distPlot", click = "plot_click", height = "380px"),
        
        uiOutput("info_pretty"),
        
        # a table output under the click
        tableOutput("near_tbl")
      )
    ),

  div(class = "soft-hr"),
  # Footer note
  div(
    style = "opacity:.9, text-align:center; padding-bottom: 10px;",
    HTML("Made with care while thinking of you. Barfbarf 🦦")
  )
)

# Server component
server <- function(input, output) {
  output$photo <- renderUI({
    req(input$id)
    tags$img(
      src = paste0("puppy-pics/", input$id, ".jpg"),
      alt = "Puppy photo",
      style = "width:100%; height:auto;"
    )
  })
  
  output$source <- renderUI({
    info <- puppies |> filter(id == input$id)
    HTML(glue::glue("
      <p style='margin: 6px 0 12px 0;'>
        <a target='_blank' href='https://unsplash.com/photos/{info$id}'>original</a>
        <span style='opacity:.85;'>by {info$author}</span>
      </p>
    "))
  })

  faith_df <- reactive({
    faithful |>
      as.data.frame() |>
      rename(eruptions = eruptions, waiting = waiting)
  })
  
  output$faith_stats <- renderUI({
    df <- faith_df()
    m <- mean(df$waiting)
    md <- median(df$waiting)
    s <- sd(df$waiting)
    n <- nrow(df)
    
    HTML(glue("
      <div style='margin: 8px 0 10px 0; display: flex; gap: 10px, flex-wrap: wrap;'>
        <span class='pill'>n = {n}</span>
        <span class='pill'>μ = {round(m, 2)} min</span>
        <span class='pill'>median = {round(md,2)} min</span>
        <span class='pill'>σ = {round(s, 2)} min</span>
      </div>
    "))
  })
  
  output$distPlot <- renderPlot({
    df <- faith_df()
    
    if (input$view_mode == "hist") {
      ggplot(df, aes(x = waiting)) +
        geom_histogram(bins = input$bins) +
        labs(
          title = "Geyser waiting time distribution",
          subtitle = "Click on a waiting time to see nearby eruptions (table below)",
          x = "Waiting time (minutes)",
          y = "Count"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    } else {
      ggplot(df, aes(x = waiting, y = eruptions)) +
        geom_point(alpha = 0.7) +
        labs(
          title = "Eruptions versus waiting time",
          subtitle = "Click in plot to see the closes observations (table below)",
          x = "Waiting time (minutes)",
          y = "Eruption duration (minutes)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    }
  }, res = 96)
  
  nearest_rows <- reactive({
    req(input$plot_click)
    df <- faith_df()
    
    x <- input$plot_click$x
    y <- input$plot_click$y
    
    if (input$view_mode == "hist") {
      # nearest by waiting time
      df |>
        mutate(dist = abs(waiting - x)) |>
        arrange(dist) |>
        head(8) |>
        select(waiting, eruptions, dist)
    } else {
      #nearest by 20 distance to clicked point
      df |>
        mutate(dist = sqrt((waiting - x)^2 + (eruptions - y)^2)) |>
        arrange(dist) |>
        head(8) |>
        select(waiting, eruptions, dist)
    }
  })
  
  output$info_pretty <- renderUI({
    if (is.null(input$plot_click)) {
      HTML("
        <div style='margin-top:10px; opacity:.85;'>Click plot to reveal nearby data ✧✨</div>
          ")
    } else {
      x <- round(input$plot_click$x, 2)
      y <- round(input$plot_click$y, 2)
      
      if (input$view_mode == "hist") {
        HTML(
          glue(
            "<div style='margin-top:10px;'>You clicked waiting ≈ <span class='pill'>{x} min</span></div>"
          )
        )
      } else {
        HTML(
          glue(
            "<div style='margin-top:10px;'>You clicked near <span class='pill'>({x},{y})</span></div>"
          )
        )
      }
    }
  })
  
  output$near_tbl <- renderTable({
    tbl <- nearest_rows()
    
    # Round and rename columns
    tbl |>
      mutate(
        waiting = round(waiting, 2),
        eruptions = round(eruptions, 2),
        dist = round(dist, 3)
      ) |>
      rename(
        'Waiting (min)' = waiting,
        'Eruption (min)' = eruptions,
        'Closeness' = dist
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

# Run the application 
shinyApp(ui = ui, server = server)
