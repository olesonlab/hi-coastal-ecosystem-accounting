# app/view/tabs/home/home_page.R

# Purpose
# - Page-level UI for the "Home" landing page in the dashboard.
# - Displays project overview, team information, and navigation guidance.

# Rules
# - UI only in this file (no data loading, no preprocessing, no spatial ops).
# - Do not create or store global state here.
# - IDs must be namespaced via NS(id); do not hardcode global IDs.

# Current status
# - Full home page with About section and Team Profile cards.
# - Routing from main.R is implemented.

# Imports
box::use(
  shiny[
    NS,
    moduleServer,
    fluidRow,
    column,
    strong,
    div,
    h2,
    h4,
    h5,
    p,
    hr,
    tagList,
    icon,
    tags,
    img,
    a
  ],
  bs4Dash[
    box
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    #–––––––––––––––––––––––––––––––––––––––––––––––––
    # About Our Project Section
    #–––––––––––––––––––––––––––––––––––––––––––––––––
    fluidRow(
      column(
        width = 12,
        box(
          title = tagList(icon("home"), strong("About Our Project")),
          status = "olive",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          maximizable = FALSE,
          div(
            class = "about-our-project-section",

            # Dashboard Introduction Section
            div(
              class = "about-our-project-intro",
              icon(
                "globe-americas",
                class = "about-our-project-icon"
              ),
              h2(
                class = "about-our-project-title",
                "Coastal Ecosystem Accounting Dashboard"
              ),
              p(
                class = "about-our-project-text",
                "Welcome! This dashboard provides interactive tools and visualizations for exploring the state,
                trends, and value of coastal ecosystems across the Main Hawaiian Islands."
              )
            ),

            hr(),

            h4("Key Features"),
            div(
              class = "metric-row",
              style = "display: flex; gap: 16px; flex-wrap: wrap; margin-top: 12px; margin-bottom: 12px;",
              div(
                style = "flex: 1 1 320px;",
                div(
                  class = "metric-box moku-metric-box",
                  style = "min-height: 110px; padding: 14px 18px;",
                  tags$span(
                    class = "info-icon",
                    icon("info-circle"),
                    `data-toggle` = "tooltip",
                    `data-placement` = "top",
                    title = paste(
                      "The Moku system (districts) is a biocultural resource",
                      "management system based on Hawaiian social-ecological",
                      "regions and communities. Their boundaries are used as",
                      "Ecosystem Accounting Areas (EAA), accounting units for the",
                      "SEEA EA framework."
                    )
                  ),
                  div(class = "metric-value", style = "font-size: 40px; line-height: 1.1;", "43"),
                  div(class = "metric-label", "Mokus Analyzed")
                )
              ),
              div(
                style = "flex: 1 1 320px;",
                div(
                  class = "metric-box et-metric-box",
                  style = "min-height: 110px; padding: 14px 18px;",
                  tags$span(
                    class = "info-icon",
                    icon("info-circle"),
                    `data-toggle` = "tooltip",
                    `data-placement` = "top",
                    title = paste(
                      "An ecosystem type is a distinct ecosystem functional group",
                      "with its unique combination of abiotic and biotic components",
                      "and interactions. Marine types include coral, seagrass, and",
                      "open ocean. Terrestrial types include forest, shrubland, and",
                      "developed areas."
                    )
                  ),
                  div(class = "metric-value", style = "font-size: 40px; line-height: 1.1;", "14"),
                  div(class = "metric-label", "Ecosystem Types")
                )
              )
            ),

            tags$ul(
              tags$li(
                strong("Ecosystem Extents:"),
                " Track changes in the size and distribution of major coastal ecosystem types."
              ),
              tags$li(
                strong("Condition Indicators:"),
                " Monitor environmental conditions like rainfall and vegetation health."
              )
            ),

            h4("How to Use This Dashboard"),
            tags$ol(
              tags$li(
                "Use the navigation menu on the left to select a topic area."
              ),
              tags$li(
                "Apply filters in the right sidebar to customize maps, tables, and charts by island, moku, ecosystem, year, and more."
              ),
              tags$li(
                "Download filtered datasets and export tables for further analysis."
              ),
              tags$li(
                "Keep the Help toggle (top right) switched on to enable tooltips and info-on-hover. Whenever you see an ",
                icon("info-circle"),
                " icon within a value box, hover over it to learn more information."
              )
            ),
          )
        )
      )
    ),

    #–––––––––––––––––––––––––––––––––––––––––––––––––
    # Team Profile Cards Section (hidden for now)
    #–––––––––––––––––––––––––––––––––––––––––––––––––
    # div(
    #   class = "team-profiles-section",

    #   # Kirsten's Card
    #   div(
    #     class = "team-profiles-single-card",
    #     img(
    #       class = "team-profiles-headshot",
    #       src = "static/img/2023_kirsten_oleson_headshot.png"
    #     ),
    #     h5("Kirsten L.L. Oleson"),
    #     p(
    #       class = "team-profiles-role",
    #       "Title"
    #     ),
    #     p(
    #       class = "team-profiles-affiliation",
    #       "University of Hawai\u02bbi at M\u0101noa"
    #     ),
    #     a(
    #       class = "team-profiles-email",
    #       href = "mailto:koleson@hawaii.edu",
    #       icon("envelope"),
    #       " Email"
    #     )
    #   ),

    #   # Louis's Card
    #   div(
    #     class = "team-profiles-single-card",
    #     img(
    #       class = "team-profiles-headshot",
    #       src = "static/img/2023_louis_chua_heashot.png"
    #     ),
    #     h5("Louis Chua"),
    #     p(
    #       class = "team-profiles-role",
    #       "Title"
    #     ),
    #     p(
    #       class = "team-profiles-affiliation",
    #       "University of Hawai\u02bbi at M\u0101noa"
    #     ),
    #     a(
    #       class = "team-profiles-email",
    #       href = "mailto:",
    #       icon("envelope"),
    #       " Email"
    #     )
    #   ),

    #   # Ela's Card
    #   div(
    #     class = "team-profiles-single-card",
    #     img(
    #       class = "team-profiles-headshot",
    #       src = "static/img/2023_ela_ural_headshot.png"
    #     ),
    #     h5("Ela Ural"),
    #     p(
    #       class = "team-profiles-role",
    #       "Title"
    #     ),
    #     p(
    #       class = "team-profiles-affiliation",
    #       "University of Hawai\u02bbi at M\u0101noa"
    #     ),
    #     a(
    #       class = "team-profiles-email",
    #       href = "mailto:",
    #       icon("envelope"),
    #       " Email"
    #     )
    #   ),

    #   # Ashley's Card
    #   div(
    #     class = "team-profiles-single-card",
    #     img(
    #       class = "team-profiles-headshot",
    #       src = "static/img/2025_ashley_lowe_mackenzie_headshot.png"
    #     ),
    #     h5("Ashley Lowe Mackenzie"),
    #     p(
    #       class = "team-profiles-role",
    #       "Title"
    #     ),
    #     p(
    #       class = "team-profiles-affiliation",
    #       "University of Hawai\u02bbi at M\u0101noa"
    #     ),
    #     a(
    #       class = "team-profiles-email",
    #       href = "mailto:",
    #       icon("envelope"),
    #       " Email"
    #     )
    #   ),

    #   # Alemarie's Card
    #   div(
    #     class = "team-profiles-single-card",
    #     img(
    #       class = "team-profiles-headshot",
    #       src = "static/img/2023_alemarie_ceria_headshot.png"
    #     ),
    #     h5("Alemarie Ceria"),
    #     p(
    #       class = "team-profiles-role",
    #       "Title"
    #     ),
    #     p(
    #       class = "team-profiles-affiliation",
    #       "University of Hawai\u02bbi at M\u0101noa"
    #     ),
    #     a(
    #       class = "team-profiles-email",
    #       href = "mailto:",
    #       icon("envelope"),
    #       " Email"
    #     )
    #   )
    # )
  )
}

# #' @export
# server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#   })
# }
