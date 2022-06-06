# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)
library(shinyalert)

GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Time Series Game"
APP_DESCP  <<- paste(
  "Time Series Game",
  "Interactive tic tac toe game to test knowledge of various time series concepts."
)
# End App Meta Data------------------------------------------------------------

# Define UI for App
ui <- list(
  ## Create the app page
  dashboardPage(
    skin = "purple",
    ### Create the app header
  
    dashboardHeader(
      title = "Time Series Game",
      titleWidth = 250,
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
        href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=[app_repo_name]"
        )
      )
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
        #### Set up the Overview Page
      tabItems(
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Time Series Tic Tac Toe"), # This should be the full name.
          p("The goal of this app is to test your knowledge of time series 
          analysis. You should use the  time series concepts reviewed in this 
            chapter to complete the game and win against the computer."),
          h2("Instructions"),
          p("To play the game:"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the",
                    actionLink(
                      inputId = "linkPreq",
                      label = "Prerequisites tab.",
                      class = "bodylinks"
                    )),
            tags$li("Click the GO! button to go the game page."),
            tags$li("Select whether you'll play as the O's or the X's."),
            tags$li("Select the square that you want to place your marker."),
            tags$li("Answer the question that is given. If you're correct, you 
                    get that square. If not, the computer will."),
            tags$li("Win by filling a row, a column, or a main diagonal with 
                    your mark (X's or O's).")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was originally developed by Ryan Voyack. The most recent 
            update was done by Chenese Gray using the Tic Tac Toe game concept 
            developed by Neil J. Hatfield and Robert Carey.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/23/2020 by CAG.")
          )
        ),
        
          #### Set up the Prerequisites Page
          tabItem(
            tabName = "Prerequisites",
            withMathJax(),
            h2("Prerequisites"),
            p("In order to get the most out of this app, please review the
            following:"),
            tags$ul(
                tags$li(strong("Stationarity")),
                tags$li("Diagnostics for stationarity include looking for constant mean 
                  (or, trend) and variance over time"
                ),
                tags$li("Constant mean is associated with data that does not have any 
                  sort of vertical (typically linear) trend over time."
                ),
                tags$li("Seasonality could also be apparent in the mean structure. Recall 
                  that seasonal ARIMA cannot explain a seasonal trend, only seasonal 
                  correlations (ARIMA models work to explain correlation structure 
                  of a time series AFTER the mean and variance are constant)."
                ),
                tags$li("Constant variance is associated with data whose vertical 
                spread (in the valleys and peaks) is constant over the duration 
                  of the time series."
                ),
                tags$li(strong("Autocorrelation Functions of Stationary 
                               Time Series")),
                tags$li("We typically trust the dashed lines in the autocorrelation 
                function (ACF) plots to be the significance cut-off bounds for 
                  any lag's correlation"
                ),
                tags$li("In a model with non-zero autoregressive (AR) and moving 
                average (MA) parts, there is no logical interpretation for both 
                  ACFS cutting off, thus,"
                ),
                  tags$ul(
                    tags$li("For AR(p) models, the ACF will tail off and the 
                            PACF will cut off after lag p."),
                    tags$li("For MA(q) models, the ACF will cut off after lag q, 
                            and the PACF will tail off."),
                    tags$li("For ARMA(p, q) models, both the ACF and the PACF 
                            will both tail off.")
                  ),
                tags$li("The ARMA subsets plot is not the best tool for determining 
                ARMA(p,q) orders, and thus will only be used as a tie breaker or 
                  guide after the ACF and PACF plots have been thoroughly inspected."),
                strong(tags$li("Model Diagnostics")),
                tags$li("The ARIMA model aims to forecast future values of a stationary 
                  time series by estimating a mathematical function to explain 
                  the underlying correlation structure. For this reason, the ACF 
                  and PACF of the residuals of the ARIMA model that has been 
                  fitted should not contain any significant 
                  remaining correlation."
                ),
                tags$li("Though forecasting is the purpose for fitting an ARIMA model, 
                looking at the forecast itself (against future values that have 
                been reserved) isnt the best way to assess the goodness of the 
                model's fit, this is why we look at the AIC and the ACF plots of 
                  the residuals of the model."
                )
              )
          ),

        #### Set up a Game Page
        tabItem(
          tabName = "game",
          withMathJax(),
          useShinyalert(),
          h2("Time Series Tic Tac Toe"),
          p(
            "To play, click on any one of the buttons that have a question mark. 
      A question will appear to the right with possible answers. If you answer 
      correctly, you will take the square; if not, the computer will take the 
      square. Try your best to win the game!"
          ),
          h3(uiOutput("player")),
          fluidRow(
            div(
              class = "col-sm-12 col-md-4",
              h3("Game Board"),
              br(),
              uiOutput("gameBoard", class = "game-board")
            ),
            div(
              class = "col-sm-12 col-md-8",
              h3("Question"),
              withMathJax(uiOutput("question")),
              uiOutput("extraOutput"),
              h3("Answer"),
              uiOutput("answer"),
              actionButton(
                inputId = "submit",
                label = "Submit",
                color = "primary",
                size = "large",
                style = "bordered",
                disabled = TRUE
              ),
              actionButton(
                inputId = "reset",
                label = "Reset Game",
                color = "primary",
                size = "large",
                style = "bordered"
              ),
              br(),
              #These two triggers help with MathJax re-rendering
              uiOutput("trigger1"),
              uiOutput("trigger2")
            )
          ), br(),br(),br(),
          fluidRow(
            column(12,uiOutput("Feedback"))
          )
        ),
      
        #### Set up the References Page
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D. and Edwards, T. (2020). shinyalert: Easily 
            Create Pretty Popup Messages (Modals) in 'Shiny'. R package version 1.1. 
            Available from https://CRAN.R-project.org/package=shinyalert"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny. R package version 0.5.2. 
            Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(session, "pages", "game")
  })
  observeEvent(input$linkPreq, {
    updateTabItems(session, "pages", "Prerequisites")
  })
  
  
  ## Listen for any inputs
  nMTC <- reactive({
    return(input$mtcTests)
  })
  aMTC <- reactive({
    return(input$mtcAlpha)
  })

  ## Code for any outputs

  activeBtn <- NA
  activeQuestion <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- FALSE
  
  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    row <- -1
    col <- -1
    
    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo
      
      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }
    
    coordinates <- list("row" = row,
                        "col" = col)
    
    return(coordinates)
  }
  
  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)
    
    index = GRID_SIZE * (coords$row - 1) + coords$col
    
    return(index)
  }
  
  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("grid-", coords$row, "-", coords$col)
    updateButton(
      session = session,
      inputId = id,
      label = "?",
      disabled = FALSE
    )
  }
  
  .score <- function(score, tile, value) {
    i <- .tileCoordinates(tile)
    
    score[i$row, i$col] <- value
    
    return(score)
  }
  
  .gameCheck <- function(mat) {
    rows <- rowSums(mat)
    cols <- colSums(mat)
    
    if (GRID_SIZE > 1) {
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))
      
      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols ||
          mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } else if (any(mat == 0)) {
        return("continue")
      } else {
        return("draw")
      }
    } else {
      ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
    }
  }
  
  # Read in data and generate the first subset
  questionBank <-
    read.csv("questions1.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  qSelected <-
    sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  gameSet <- questionBank[qSelected,]
  
  
  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)
    activeQuestion <<- gameSet[index, "id"]
    
    output$question <- renderUI({
      withMathJax()
      return(gameSet[index, "question"])
    })
    
    output$answer <- .ansFunc(index, gameSet)
    
    if (gameSet[index, "extraOutput"] != "") {
      output$extraOutput <- renderText({
        gameSet[index, "extraOutput"]
      })
    } else {
      output$extraOutput <- NULL
    }
    
    #Retrigger MathJax processing
    output$trigger1 <- renderUI({
      withMathJax()
    })
    output$trigger2 <- renderUI({
      withMathJax()
    })
    
    #Enable Submit Button
    updateButton(session = session,
                 inputId = "submit",
                 disabled = FALSE)
  }
  
  
  
  .ansFunc <- function(index, df) {
    if (df[index, "format"] == "two") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical",
        )
      })
    } else if (df[index, "format"] == "three") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    } else if (df[index, "format"] == "four"){
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    } else if(df[index, "format"] == "five"){
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"],
                         df[index, "E"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    }
  }
  
  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    qSelected <<-
      sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
    gameSet <<- questionBank[qSelected,]
    
    output$question <-
      renderUI({
        return("Click a button on the game board to get started on your new game.")
      })
    output$answer <- renderUI({
      ""
    })
    output$extraOutput <- renderUI({
      ""
    })
    scoreMatrix <<-
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA
    
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  }
  
  
  
 
  # Program the Reset Button
  observeEvent(input$reset, {
    
    .gameReset()
  })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        
        board[[index]] <<- tags$li(
          actionButton(
            inputId = paste0("grid-", row, "-", column),
            label = "?",
            color = "primary",
            style = "bordered",
            class = "grid-fill"
          ),
          class = "grid-tile"
        )
        
        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
        
        })
        
        index <<- index + 1
      })
    })
    
    tags$ol(board, class = paste(
      "grid-board",
      "grid-fill",
      paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
    ))
  })
  
  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    answer <- ""
    
    
    if (gameSet[index, "format"] == "numeric") {
      answer <- gameSet[index, "answer"]
    } else {
      answer <- gameSet[index, gameSet[index, "answer"]]
    }
    
    success <- input$ans == answer
    
    if (success) {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = player,
        disabled = TRUE
      )
      output$Feedback <- NULL
      scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
    } else {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = opponent,
        disabled = TRUE
      )
      output$Feedback <- renderUI({
        withMathJax(
          h4(sprintf(gameSet[index, "feedback"]
          ))
        )
      })
      scoreMatrix <<- .score(scoreMatrix, activeBtn,-1)
    }
    
    # Check for game over states
    .gameState <- .gameCheck(scoreMatrix)
    completion <- ifelse(.gameState == "continue", FALSE, TRUE)
    interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric", "choice")
    
    
    
    if (.gameState == "win") {
      
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You Win!",
        text = "You've filled either a row, a column, or a main diagonal. 
        Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "lose") {
      
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "draw") {
      
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    }
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  })
  
  observeEvent(input$pages, {
    if (input$pages == "game") {
      if (!gameProgress) {
        shinyalert(
          title = "Player Select",
          text = "Select whether you want to play as O or X.",
          showConfirmButton = TRUE,
          confirmButtonText = "Play as X",
          showCancelButton = TRUE,
          cancelButtonText = "Play as O"
        )
        gameProgress <<- TRUE
      }
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$endGame, {
    
    .gameReset()
  })
  
  observeEvent(input$shinyalert, {
    if (input$shinyalert == TRUE) {
      player <<- "X"
      opponent <<- "O"
    }
    if (input$shinyalert == FALSE) {
      player <<- "O"
      opponent <<- "X"
    }
    
    
    
    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })

  
  # End of Neil Hatfield's code-------------------------------------------------
}

# Create Shiny App using BOAST App template
boastApp(ui = ui, server = server)
