library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(stringr)
library(tidyverse)
library(bslib)
library(janitor)

# Internal API key (replace with your actual API key)
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")


ui <- page_fluid(
  theme = bs_theme(version = 5),
  titlePanel("Fake Dataset Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("description", "Describe the dataset you want", 
                placeholder = "e.g., health data for a family of 4"),
      actionButton("generate", "Generate Dataset"),
      downloadButton("download", "Download CSV")
    ),
    mainPanel(
      navset_tab(
        nav_panel("Data Table", DTOutput("dataset")),
        nav_panel("Visualizations",
                  selectInput("variable", "Select Variable", choices = NULL),
                  plotOutput("plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  preprocess_csv <- function(csv_string) {
    # Extract only the CSV part
    csv_pattern <- "(?s)(.+?\\n(?:[^,\n]+(?:,[^,\n]+)*\n){2,})"
    csv_match <- str_extract(csv_string, csv_pattern)
    
    if (is.na(csv_match)) {
      stop("No valid CSV data found in the response")
    }
    
    lines <- str_split(csv_match, "\n")[[1]]
    lines <- lines[lines != ""]  # Remove empty lines
    
    # Get the number of columns from the header
    header <- str_split(lines[1], ",")[[1]]
    num_cols <- length(header)
    
    # Ensure all rows have the same number of columns
    processed_lines <- sapply(lines[-1], function(line) {  # Skip header
      cols <- str_split(line, ",")[[1]]
      if (length(cols) < num_cols) {
        cols <- c(cols, rep("", num_cols - length(cols)))
      } else if (length(cols) > num_cols) {
        cols <- cols[1:num_cols]
      }
      cols
    })
    
    # Create a tibble
    tibble(!!!setNames(as.list(as.data.frame(t(processed_lines))), header))
  }
  
  observeEvent(input$generate, {
    req(input$description)
    
    prompt <- paste("Generate a fake dataset as a CSV string based on this description:",
                    input$description, "Include a header row. Limit to 25 rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations.")
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
      content_type_json(),
      body = toJSON(list(
        model = "gpt-3.5-turbo-0125",
        messages = list(
          list(role = "system", content = "You are a helpful assistant that generates fake datasets."),
          list(role = "user", content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      content <- content(response)
      csv_string <- content$choices[[1]]$message$content
      
      tryCatch({
        # Preprocess the CSV string and create a tibble
        df <- preprocess_csv(csv_string) %>% clean_names() %>% 
          mutate(across(everything(), ~ ifelse(suppressWarnings(!is.na(as.numeric(.))), as.numeric(.), as.character(.))))
        dataset(df)
        updateSelectInput(session, "variable", choices = names(df))
      }, error = function(e) {
        showNotification(paste("Error parsing CSV:", e$message), type = "error")
      })
    } else {
      showNotification("Error generating dataset. Please try again later.", type = "error")
    }
  })
  
  output$dataset <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 10))
  })
  
  output$plot <- renderPlot({
    req(dataset(), input$variable)
    df <- dataset()
    var <- input$variable
    
    if (is.numeric(df[[var]])) {
      ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(binwidth = function(x) diff(range(x)) / 30, fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of", var), x = NULL, y = NULL) +
        theme_minimal()
    } else {
      
      df2 <- df %>%
        count(.data[[var]]) %>%
        arrange(desc(n)) %>%
        slice(1:10)
      
      ggplot(df2, aes_string(x = var, y = "n")) +
        geom_bar(stat = "identity") +
        labs(title = "Column Chart", caption = "Up to 10 records shown", x = NULL, y = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "generated_dataset.csv"
    },
    content = function(file) {
      req(dataset())
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
