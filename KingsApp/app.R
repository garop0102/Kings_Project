# app.R - Revised Shiny app for Kings scouting (fixed distribution grid + EDA alignment)
# Saves: place this file in the project root and run via Run App in RStudio

# Load packages
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, shinythemes, tidyverse, plotly, DT, lubridate, here, readr, scales, htmltools)

# Robust path: try here() Data folder, fallback to explicit path used in your Rmd
data_path <- here::here("Data", "cleaned_kings_player_data.csv")
if(!file.exists(data_path)){
  data_path <- "~/Documents/R-Projects/Kings_Project/Data/cleaned_kings_player_data.csv"
}
if(!file.exists(data_path)) stop("cleaned_kings_player_data.csv not found. Please put it in Data/ or update data_path in app.R")

# Read data
full_data_raw <- readr::read_csv(data_path, show_col_types = FALSE)

# Preprocess to ensure columns exist / types match your EDA ingestion
full_data <- full_data_raw |>
  mutate(
    minutes = as.numeric(minutes),
    games = as.integer(games),
    three_attempt_rate = as.numeric(three_attempt_rate),
    three_pct = as.numeric(three_pct),
    free_throw_rate = as.numeric(free_throw_rate),
    internal_box_plus_minus = as.numeric(internal_box_plus_minus),
    points_per_36 = as.numeric(points_per_36),
    steals = as.numeric(steals),
    blocked_shots = as.numeric(blocked_shots),
    deflections = as.numeric(deflections),
    birth_date = lubridate::ymd(birth_date),
    season = as.integer(season)
  ) |>
  mutate(
    age = if_else(!is.na(birth_date) & !is.na(season), as.integer(season - year(birth_date)), NA_integer_),
    def_actions_per_36 = if_else(!is.na(minutes) & minutes > 0, (coalesce(steals,0) + coalesce(blocked_shots,0) + coalesce(deflections,0)) / minutes * 36, NA_real_),
    points_per_36 = if_else(is.na(points_per_36) & !is.na(points) & !is.na(minutes) & minutes>0, as.numeric(points) / minutes * 36, points_per_36)
  )

# Variables used in composite
feature_vars <- c("three_pct","three_attempt_rate","free_throw_rate","internal_box_plus_minus","def_actions_per_36")

# helper: plot generator (returns ggplot)
plot_var <- function(df, var, transform = c("none","log","percent"), bins = 40){
  transform <- match.arg(transform)
  p <- ggplot(df, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = bins, alpha = 0.6, fill = "#2b8cbe", color = "white") +
    geom_density(alpha = 0.2) +
    theme_minimal() + labs(title = var, x = var, y = "density")
  if(transform == "log") p <- p + scale_x_continuous(trans = "log10")
  if(transform == "percent") p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  p
}

# League-normalization helper (z-scores within league)
league_norm <- function(df, vars){
  df %>% group_by(league) |>
    mutate(across(all_of(vars), ~ (. - mean(.x, na.rm=TRUE)) / (sd(.x, na.rm=TRUE) + 1e-9), .names = "ln_{col}")) %>%
    ungroup()
}

# UI -----------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Kings — International Player Scouting (EDA → Shortlist)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("mins", "Minimum season minutes:", min = 0, max = 2000, value = 400, step = 50),
      sliderInput("top_pct", "Shortlist top percentile (composite):", min = 0.5, max = 0.99, value = 0.90, step = 0.01),
      numericInput("min_games", "Minimum games in season:", value = 8, min = 0, max = 200),
      checkboxInput("use_league_norm", "League-normalize features", value = TRUE),
      hr(),
      h5("Composite weights (adjust):"),
      sliderInput("w_3ppct", "3P% weight", min = 0, max = 1, value = 0.25, step = 0.05),
      sliderInput("w_3prate", "3PA rate weight", min = 0, max = 1, value = 0.25, step = 0.05),
      sliderInput("w_ftr", "FTr weight", min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("w_ibpm", "IBPM weight", min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("w_def", "Defense weight", min = 0, max = 1, value = 0.1, step = 0.05),
      actionButton("apply", "Apply filters & score", class = "btn-primary"),
      hr(),
      downloadButton("dl_shortlist", "Download shortlist CSV")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Distributions",
                 br(),
                 plotlyOutput("dist_grid", height = "700px")
        ),
        tabPanel("Shortlist Table",
                 br(),
                 DT::dataTableOutput("players_table")
        ),
        tabPanel("Player Profile",
                 br(),
                 fluidRow(
                   column(6,
                          htmltools::div(style = "border: 1px solid #eee; padding: 10px; border-radius: 6px;",
                                         h4(textOutput("profile_name")),
                                         p(textOutput("profile_meta")),
                                         DT::dataTableOutput("profile_stats")
                          )
                   ),
                   column(6,
                          plotlyOutput("radar_plot", height = "350px"),
                          br(),
                          plotlyOutput("sparkline_plot", height = "250px")
                   )
                 )
        )
      )
    )
  )
)

# Server -------------------------------------------------------------------
server <- function(input, output, session){
  
  # base filtered DF reactive
  df_base <- reactive({
    full_data |> filter(!is.na(minutes) & !is.na(games) & games >= input$min_games)
  })
  
  # Scoring reactive triggered by Apply
  df_scored <- eventReactive(input$apply, {
    df <- df_base() |> filter(minutes >= input$mins)
    # apply league normalization if asked
    if(input$use_league_norm){
      df <- league_norm(df, feature_vars)
      # copy ln_ to z_
      for(v in feature_vars) df[[paste0('z_', v)]] <- df[[paste0('ln_', v)]]
    } else {
      # global z
      for(v in feature_vars) df[[paste0('z_', v)]] <- (df[[v]] - mean(df[[v]], na.rm=TRUE)) / (sd(df[[v]], na.rm=TRUE)+1e-9)
    }
    
    # normalize weights
    w <- c(input$w_3ppct, input$w_3prate, input$w_ftr, input$w_ibpm, input$w_def)
    if(sum(w) == 0) w <- rep(1/length(w), length(w)) else w <- w / sum(w)
    
    # ensure NA z_ replaced with 0
    for(zv in paste0('z_', feature_vars)){
      if(!zv %in% names(df)) df[[zv]] <- 0
      df[[zv]][is.na(df[[zv]])] <- 0
    }
    
    df <- df |> mutate(
      composite = w[1]*z_three_pct + w[2]*z_three_attempt_rate + w[3]*z_free_throw_rate + w[4]*z_internal_box_plus_minus + w[5]*z_def_actions_per_36
    ) |> mutate(composite_pct = percent_rank(composite))
    
    cutoff <- input$top_pct
    shortlist <- df |> filter(composite_pct >= cutoff)
    df_out <- df |> mutate(shortlist_flag = ifelse(player_id %in% shortlist$player_id, TRUE, FALSE))
    
    list(all = df_out, shortlist = shortlist)
  }, ignoreNULL = FALSE)
  
  # Distribution grid: create 6 ggplots, convert each to ggplotly and then combine with subplot
  output$dist_grid <- renderPlotly({
    df <- if(isTruthy(df_scored())) df_scored()$all else df_base()
    req(df)
    p1 <- plot_var(df, 'three_attempt_rate', 'percent')
    p2 <- plot_var(df, 'three_pct', 'percent')
    p3 <- plot_var(df, 'free_throw_rate', 'percent')
    p4 <- plot_var(df, 'internal_box_plus_minus', 'none')
    p5 <- plot_var(df, 'age', 'none')
    p6 <- plot_var(df, 'points_per_36', 'none')
    
    # convert to plotly
    pl1 <- ggplotly(p1, tooltip = 'x')
    pl2 <- ggplotly(p2, tooltip = 'x')
    pl3 <- ggplotly(p3, tooltip = 'x')
    pl4 <- ggplotly(p4, tooltip = 'x')
    pl5 <- ggplotly(p5, tooltip = 'x')
    pl6 <- ggplotly(p6, tooltip = 'x')
    
    # combine via subplot (2 rows)
    sp <- subplot(list(pl1, pl2, pl3, pl4, pl5, pl6), nrows = 2, margin = 0.03, shareX = FALSE, titleX = TRUE, titleY = TRUE) %>%
      layout(title = list(text = 'Distribution Grid — key metrics', x = 0.5))
    sp
  })
  
  # Players table
  output$players_table <- DT::renderDataTable({
    df <- if(isTruthy(df_scored())) df_scored()$all else df_base()
    dt <- df |> mutate(name = paste(first_name, last_name)) |>
      select(player_id, name, team, league, season, minutes, games, three_pct, three_attempt_rate, free_throw_rate, internal_box_plus_minus, def_actions_per_36, composite, composite_pct, shortlist_flag) |>
      arrange(desc(composite))
    DT::datatable(dt, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, selection = list(mode = 'single', target = 'row'))
  })
  
  # Selected player reactive
  selected_player <- reactive({
    s <- input$players_table_rows_selected
    if(length(s) == 0) return(NULL)
    df <- if(isTruthy(df_scored())) df_scored()$all else df_base()
    dt <- df |> mutate(name = paste(first_name, last_name)) |> arrange(desc(composite))
    dt[s, ]
  })
  
  output$profile_name <- renderText({
    sp <- selected_player(); if(is.null(sp)) return('No player selected')
    paste(sp$first_name, sp$last_name)
  })
  output$profile_meta <- renderText({
    sp <- selected_player(); if(is.null(sp)) return('')
    paste0(sp$team, ' | ', sp$league, ' | Season: ', sp$season, ' | Minutes: ', sp$minutes, ' | Age: ', sp$age)
  })
  
  output$profile_stats <- DT::renderDataTable({
    sp <- selected_player(); if(is.null(sp)) return(NULL)
    metrics <- tibble(metric = c('3P%','3PA_rate','FTr','IBPM','DefActions/36','Points/36','Composite'),
                      value = c(sp$three_pct, sp$three_attempt_rate, sp$free_throw_rate, sp$internal_box_plus_minus, sp$def_actions_per_36, sp$points_per_36, sp$composite))
    DT::datatable(metrics, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Radar plot
  output$radar_plot <- renderPlotly({
    sp <- selected_player(); if(is.null(sp)) return(NULL)
    df <- if(isTruthy(df_scored())) df_scored()$all else df_base()
    vars_plot <- c('three_pct','three_attempt_rate','free_throw_rate','internal_box_plus_minus','def_actions_per_36')
    stats <- map_dfr(vars_plot, function(v) tibble(var = v, min = min(df[[v]], na.rm = TRUE), max = max(df[[v]], na.rm = TRUE)))
    pvals <- map_dbl(vars_plot, function(v){ val <- sp[[v]]; if(is.na(val)) return(0); minv <- stats$min[stats$var==v]; maxv <- stats$max[stats$var==v]; if(is.na(minv) || is.na(maxv) || (maxv-minv)==0) return(0); (val-minv)/(maxv-minv) })
    rad_df <- tibble(theta = vars_plot, r = pvals)
    plot_ly(type='scatterpolar', r = c(rad_df$r, rad_df$r[1]), theta = c(rad_df$theta, rad_df$theta[1]), fill='toself') |> layout(polar = list(radialaxis = list(visible = TRUE, range = c(0,1))), showlegend = FALSE)
  })
  
  # Sparkline: season trend for selected player (3P% & FTr)
  output$sparkline_plot <- renderPlotly({
    sp <- selected_player(); if(is.null(sp)) return(NULL)
    pid <- sp$player_id
    dfp <- full_data |> filter(player_id == pid) |> arrange(season)
    if(nrow(dfp) == 0) return(NULL)
    p <- plot_ly(dfp, x = ~season) |> add_lines(y = ~three_pct, name = '3P%') |> add_markers(y = ~three_pct, showlegend = FALSE) %>% add_lines(y = ~free_throw_rate, name = 'FTr', yaxis = 'y2') |> layout(yaxis = list(title = '3P%'), yaxis2 = list(overlaying = 'y', side = 'right', title = 'FTr'), legend = list(orientation = 'h'))
    p
  })
  
  # Download shortlist
  output$dl_shortlist <- downloadHandler(filename = function() paste0('kings_shortlist_', Sys.Date(), '.csv'), content = function(file){ df <- if(isTruthy(df_scored())) df_scored()$shortlist else tibble(); readr::write_csv(df, file) })
  
}

# Run app
shinyApp(ui, server)
