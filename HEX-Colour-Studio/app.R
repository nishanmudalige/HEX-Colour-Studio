# app.R — HEX Colour Studio ----------------------------------------------------------
# A polished Shiny app for selecting HEX colors, checking WCAG contrast,
# generating shades/tints, copying HEX codes, and exporting palettes.

# ---- Dependencies -----------------------------------------------------------
packages <- c("shiny", "bslib", "colourpicker", "rclipboard", "htmltools")
new <- setdiff(packages, rownames(installed.packages()))
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(packages, library, character.only = TRUE))

# ---- Helpers: color math + WCAG contrast -----------------------------------
hex_to_srgb <- function(hex) {
  m <- grDevices::col2rgb(hex) / 255
  matrix(m, nrow = 3, dimnames = list(c("r","g","b"), NULL))
}
srgb_to_linear <- function(c) ifelse(c <= 0.04045, c/12.92, ((c + 0.055)/1.055)^2.4)
rel_luminance <- function(hex) {
  s <- hex_to_srgb(hex); l <- apply(s, 1, srgb_to_linear)
  drop(0.2126*l[1] + 0.7152*l[2] + 0.0722*l[3])
}
contrast_ratio <- function(fg, bg) {
  L1 <- rel_luminance(fg); L2 <- rel_luminance(bg)
  (pmax(L1, L2) + 0.05) / (pmin(L1, L2) + 0.05)
}
wcag_badges <- function(ratio) {
  tags$div(
    class = "d-flex gap-2",
    tags$span(class = paste0("badge ", if (ratio >= 7)  "bg-success" else "bg-secondary"),
              sprintf("AAA %.2f:1", ratio)),
    tags$span(class = paste0("badge ", if (ratio >= 4.5) "bg-success" else "bg-warning"),
              sprintf("AA  %.2f:1", ratio)),
    tags$span(class = paste0("badge ", if (ratio >= 3)  "bg-success" else "bg-warning"),
              "AA Large")
  )
}
mix_hex <- function(hex, with = "#ffffff", t = 0.2) {
  a <- hex_to_srgb(hex); b <- hex_to_srgb(with)
  o <- (1 - t) * a + t * b
  grDevices::rgb(o[1], o[2], o[3])
}
shade_series <- function(hex, n = 9) {
  idx <- seq(-0.4, 0.4, length.out = n)
  vapply(idx, function(t) {
    if (t < 0) mix_hex(hex, "#000000", -t)
    else if (t > 0) mix_hex(hex, "#ffffff", t)
    else hex
  }, character(1))
}

# ---- Theme ------------------------------------------------------------------
thm <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#1F4E8C",
  base_font  = font_google("Inter", local = FALSE),
  code_font  = font_google("JetBrains Mono", local = FALSE),
  bg         = "#f8fafc",
  fg         = "#0f172a"
)

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "HEX Colour Studio",
  theme = thm,
  id = "page",
  
  nav_panel(
    "Picker",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        h4("Choose Colors"),
        colourpicker::colourInput("bg", "Background", value = "#FFFFFF",
                                  returnName = FALSE, allowTransparent = FALSE,
                                  showColour = "both"), #, palette = "limited"),
        colourpicker::colourInput("fg", "Text / Foreground", value = "#1F4E8C",
                                  returnName = FALSE, allowTransparent = FALSE,
                                  showColour = "both"), # palette = "limited"),
        hr(),
        rclipboard::rclipboardSetup(),  # must appear once in the UI
        
        # copy buttons rendered server-side so clipText is a string value
        uiOutput("copy_fg_btn"),
        uiOutput("copy_bg_btn"),
        
        hr(),
        actionButton("add_fg", "Add FG to Palette", icon = icon("plus"), class = "btn-primary"),
        actionButton("add_bg", "Add BG to Palette", icon = icon("plus")),
        br(), br(),
        h5("Shades & Tints"),
        sliderInput("n_shades", "Swatches", min = 5, max = 13, value = 9, step = 2),
        radioButtons("shade_source", NULL, inline = TRUE,
                     choices = c("From FG" = "fg", "From BG" = "bg"))
      ),
      card(
        class = "p-4",
        card_header(h5("Live Preview & Contrast")),
        card_body(
          uiOutput("preview_block")
        )
      ),
      card(
        class = "p-4",
        card_header(h5("Shades/Tints")),
        card_body(uiOutput("shade_grid"))
      )
    )
  ),
  
  nav_panel(
    "Palette",
    layout_columns(
      col_widths = c(12),
      card(
        class = "p-3",
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              h5("Your Palette"),
              div(
                uiOutput("copy_palette_btn"),
                downloadButton("dl_palette", "Download CSV", class = "btn btn-success ms-2")
              )
          )
        ),
        card_body(uiOutput("palette_grid"))
      )
    )
  ),
  
  nav_panel(
    "About",
    card(
      class = "p-4",
      h4("About HEX Colour Studio"),
      p("Pick colors, evaluate accessibility, generate shades/tints, and export palettes."),
      p("Powered by ", code("colourpicker"), " for the picker, ",
        code("bslib"), " for theming and layout, and ",
        code("rclipboard"), " for one-click copying.")
    )
  ),
  
  footer = div(class = "text-center text-muted small my-3",
               HTML("&copy; 2025 • HEX Colour Studio for students • Developed by Nishan Mudalige"))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Copy buttons (IMPORTANT: label includes icon; clipText is a character)
  output$copy_fg_btn <- renderUI({
    rclipboard::rclipButton(
      inputId  = "copy_fg",
      label    = tagList(icon("clipboard"), " Copy Foreground HEX"),
      clipText = input$fg,
      tooltip  = "Copy foreground HEX",
      placement = "top",
      class    = "btn btn-outline-secondary w-100"
    )
  })
  output$copy_bg_btn <- renderUI({
    rclipboard::rclipButton(
      inputId  = "copy_bg",
      label    = tagList(icon("clipboard"), " Copy Background HEX"),
      clipText = input$bg,
      tooltip  = "Copy background HEX",
      placement = "top",
      class    = "btn btn-outline-secondary w-100"
    )
  })
  
  # Live preview with WCAG badges
  output$preview_block <- renderUI({
    ratio <- contrast_ratio(input$fg, input$bg)
    div(
      class = "rounded p-4 border",
      style = paste0("background:", input$bg, "; color:", input$fg, ";"),
      h3("Preview"),
      p(HTML("The quick brown fox jumps over the lazy dog 123 !@#")),
      p(tags$code(htmlEscape(input$fg)), " on ",
        tags$code(htmlEscape(input$bg))),
      wcag_badges(ratio)
    )
  })
  
  # Shades/tints grid with copy buttons
  output$shade_grid <- renderUI({
    src  <- if (input$shade_source == "fg") input$fg else input$bg
    cols <- shade_series(src, n = input$n_shades)
    fluidRow(
      lapply(seq_along(cols), function(i) {
        div(class = "col-6 col-md-3 col-lg-2 mb-3",
            div(class = "card shadow-sm",
                div(style = paste0("height:80px; background:", cols[i], ";")),
                div(class = "card-body p-2 d-flex justify-content-between align-items-center",
                    code(cols[i]),
                    rclipboard::rclipButton(
                      inputId  = paste0("copy_shade_", i),
                      label    = tagList(icon("clipboard"), ""),
                      clipText = cols[i],
                      tooltip  = "Copy HEX",
                      placement = "left",
                      class    = "btn btn-outline-secondary btn-sm"
                    )
                )
            )
        )
      })
    )
  })
  
  # Palette store & controls
  pal <- reactiveVal(character(0))
  observeEvent(input$add_fg, pal(unique(c(pal(), input$fg))))
  observeEvent(input$add_bg, pal(unique(c(pal(), input$bg))))
  
  output$palette_grid <- renderUI({
    cols <- pal()
    if (!length(cols)) {
      return(div(class = "text-muted", "No colors yet. Add from the Picker page."))
    }
    fluidRow(
      lapply(seq_along(cols), function(i) {
        col <- cols[i]
        on_bg <- ifelse(contrast_ratio("#000000", col) >= contrast_ratio("#ffffff", col),
                        "#000000", "#ffffff")
        # card
        div(class = "col-6 col-md-4 col-lg-3 mb-3",
            div(class = "card border-0 shadow-sm",
                div(class = "card-img-top d-flex align-items-end justify-content-between p-2",
                    style = paste0("height:120px; background:", col, "; color:", on_bg, ";"),
                    strong(col),
                    div(class = "btn-group btn-group-sm",
                        rclipboard::rclipButton(
                          inputId  = paste0("copy_pal_", i),
                          label    = tagList(icon("clipboard"), ""),
                          clipText = col,
                          tooltip  = "Copy HEX",
                          placement = "left",
                          class    = "btn btn-light"
                        ),
                        actionButton(paste0("remove_", i), NULL, icon = icon("trash"),
                                     class = "btn btn-light")
                    )
                ),
                div(class = "card-body p-2",
                    small("Contrast vs white: ",
                          sprintf("%.2f:1", contrast_ratio(col, "#FFFFFF")),
                          " • vs black: ",
                          sprintf("%.2f:1", contrast_ratio(col, "#000000")))
                )
            )
        )
      })
    )
  })
  
  # Remove handlers (generate observers dynamically)
  observe({
    cols <- pal()
    lapply(seq_along(cols), function(i) {
      local({
        ii <- i
        observeEvent(input[[paste0("remove_", ii)]], ignoreInit = TRUE, {
          current <- pal()
          if (ii <= length(current)) pal(current[-ii])
        })
      })
    })
  })
  
  # Copy entire palette & download CSV
  output$copy_palette_btn <- renderUI({
    rclipboard::rclipButton(
      inputId  = "copy_palette",
      label    = tagList(icon("clipboard"), " Copy as HEX list"),
      clipText = paste(pal(), collapse = ", "),
      tooltip  = "Copy palette (comma-separated)",
      placement = "left",
      class    = "btn btn-outline-secondary"
    )
  })
  output$dl_palette <- downloadHandler(
    filename = function() paste0("palette-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content  = function(file) utils::write.csv(data.frame(hex = pal()), file, row.names = FALSE)
  )
  
  # Optional interactive theme editor
  bs_themer()
}

# ---- Run --------------------------------------------------------------------
shinyApp(ui, server)
