library(shiny)
library(DT)

# Fonction pour générer une grille de Picross aléatoire
generate_picross_grid <- function(size) {
  picross_grid <- matrix(sample(c(0, 1), size^2, replace = TRUE), nrow = size, ncol = size)
  row_indices <- sapply(1:size, function(i) paste(sum(picross_grid[i,] == 1), collapse = " "))
  col_indices <- sapply(1:size, function(j) paste(sum(picross_grid[,j] == 1), collapse = " "))
  list(grid = picross_grid, row_indices = row_indices, col_indices = col_indices)
}

ui <- fluidPage(
  titlePanel("Picross"),
  sidebarLayout(
    sidebarPanel(
      numericInput("grid_size", "Taille de la Grille", value = 5, min = 3, max = 10),
      actionButton("new_puzzle", "Nouveau Puzzle"),
      actionButton("check_puzzle", "Vérifier"),
      p("Utilisez les boutons ci-dessous pour remplir ou effacer les cases de la grille.")
    ),
    mainPanel(
      fluidRow(
        column(6, DTOutput("row_indices")),
        column(6, DTOutput("col_indices"))
      ),
      br(),
      fluidRow(
        column(12, DTOutput("grid"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialiser la grille de Picross
  picross_data <- reactiveVal(NULL)
  
  # Observer pour générer un nouveau puzzle
  observeEvent(input$new_puzzle, {
    picross_data(generate_picross_grid(input$grid_size))
  })
  
  # Observer pour afficher la grille de Picross
  output$grid <- renderDT({
    if (!is.null(picross_data())) {
      grid <- picross_data()$grid
      datatable(grid, options = list(dom = 't', paging = FALSE, ordering = FALSE, searching = FALSE, 
                                     rownames = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })
  
  # Observer pour afficher les indices des lignes
  output$row_indices <- renderDT({
    if (!is.null(picross_data())) {
      data.frame(Lignes = picross_data()$row_indices)
    }
  })
  
  # Observer pour afficher les indices des colonnes
  output$col_indices <- renderDT({
    if (!is.null(picross_data())) {
      data.frame(Colonnes = picross_data()$col_indices)
    }
  })
  
  # Fonction pour vérifier si la grille actuelle correspond à la solution
  check_solution <- function() {
    if (!is.null(picross_data())) {
      input_grid <- as.matrix(sapply(1:input$grid_size^2, function(i) input[[paste0("cell_", i)]]))
      solution <- picross_data()$grid
      identical(input_grid, solution)
    } else {
      FALSE
    }
  }
  
  # Observer pour vérifier la solution
  observeEvent(input$check_puzzle, {
    if (check_solution()) {
      showModal(modalDialog(
        title = "Bravo!",
        "Vous avez résolu le puzzle avec succès."
      ))
    } else {
      showModal(modalDialog(
        title = "Désolé!",
        "La grille n'est pas correcte. Veuillez réessayer."
      ))
    }
  })
}

shinyApp(ui, server)

