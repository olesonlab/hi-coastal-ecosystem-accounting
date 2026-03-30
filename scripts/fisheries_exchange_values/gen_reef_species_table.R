## Reef-associated fish species comparison table
## Outputs: HTML + PNG to outputs/tables/fisheries_exchange_values/
## † = species appearing in both commercial and non-commercial accounts

library(gt)
library(dplyr)
library(fs)

out_dir <- "outputs/tables/fisheries_exchange_values"
fs::dir_create(out_dir)

# ── Species lists ─────────────────────────────────────────────────────────────
# † marks species that appear in both accounts

noncomm <- c(
  "Convict tang \u2020",
  "Bluespine unicornfish",
  "Goldring surgeonfish",
  "Spectacled parrotfish",
  "Stareye parrotfish \u2020",
  "Redlip parrotfish",
  "Palenose parrotfish \u2020",
  "Uhu (all other parrotfish)"
)

comm <- c(
  # Parrotfish / surgeonfish / tang
  "Convict tang \u2020",          "Stareye parrotfish \u2020",     "Palenose parrotfish \u2020",  "Parrotfish (misc.)",
  "Unicornfish",                  "Sleek unicornfish",             "Naso tang / Orangespine unicornfish", "Brown surgeonfish",
  "Lavender tang",                "Blueline surgeonfish",          "Whitebar surgeonfish",        "Olive tang",
  "Orangeband surgeonfish",       "Yellow tang",                   "Achilles tang",               "Eyestripe surgeonfish",
  "Ringtail surgeonfish",         "Yellowfin surgeonfish",         "Whitespotted surgeonfish",    "Black surgeonfish",
  # Wrasse / hogfish
  "Hawaiian hogfish",             "Tableboss",                     "Wrasse (misc.)",              "Blackstripe coris wrasse",
  "Wrasse (Thalassoma spp.)",     "Cigar wrasse",                  "Mongoose fish",               "Peacock razorfish",
  "Pearl wrasse",                 "Humphead wrasse",               "Napoleon wrasse",             "Ringtail wrasse",
  # Goatfish
  "Orange goatfish",              "Pfluger's goatfish",            "White saddle goatfish",       "Goatfish (misc.)",
  "Sidespot goatfish",            "Doublebar goatfish",            "Bandtail goatfish",           "Yellowfin goatfish",
  "Square-spot goatfish",         "Yellowstripe goatfish",         "Blue goatfish",               "Yellowsaddle goatfish",
  # Squirrelfish
  "Hawaiian squirrelfish",        "Indianfish",                    "Squirrelfish (Holocentridae)","Squirrelfish (Myripristis spp.)",
  "Longjaw squirrelfish",
  # Snapper / grouper / sea bass
  "Forktail snapper",             "Bluestripe snapper",            "Blacktail snapper",           "Golden perch",
  "Peacock grouper",              "Royal sea bass",                "Glasseye snapper",            "Hawaiian bigeye",
  # Barracuda / eel
  "Great barracuda",              "Heller's barracuda",            "Japanese barracuda",
  "Moray eel (misc.)",            "Moray eel (Gymnothorax spp.)",  "Conger eel",                  "Garden eel",
  "White eel",
  # Mullet / scad / mackerel
  "Flathead grey mullet",         "Striped mullet",                "Australian mullet",           "Summer mullet",
  "False mullet",                 "Sharpnose mullet",              "Bigeye scad",                 "Bigeye scad (juvenile)",
  "Mackerel scad",                "Mackerel",                      "Japanese mackerel",           "Butternose",
  # Ray / misc. reef
  "Eagle ray",                    "Sting ray",                     "Hage (Big Island)",           "Triggerfish",
  "Moorish idol",                 "Hawkfish",                      "Damselfish",                  "Blackspot sergeant",
  "Millet butterflyfish",         "Hawaiian sergeant",             "Sergeant major",              "Filefish",
  "Bonefish",                     "Scorpionfish",                  "Cornetfish",                  "Stickfish",
  "Trumpetfish",                  "Balloonfish",                   "Porcupinefish",               "Pufferfish",
  # Misc.
  "Milkfish",                     "Hawaiian ladyfish",             "Hawaiian flagtail",           "Mountainbass",
  "Hawaiian silverside",          "Perch",                         "Ballyhoo",                    "Halfbeak",
  "Garfish",                      "Needlefish",                    "Sardine",                     "Threadfin",
  "Bigeye emperor",               "Anchovy",                       "Chub",                        "Rudderfish",
  "Flatfish",                     "Flounder",                      "Delicate roundherring",       "Cardinalfish",
  "Ocean sunfish",                "Slender sunfish",               "Tilapia",                     "Spotted knifejaw",
  "Barred knifejaw",              "Gold-spot herring"
)

n_comm    <- length(comm)
n_noncomm <- length(noncomm)

# ── Build non-commercial block (2 columns × 4 rows) ──────────────────────────
nc_df <- data.frame(
  col1 = noncomm[1:4],
  col2 = noncomm[5:8],
  stringsAsFactors = FALSE
)

# ── Build commercial block (4 columns × n_rows rows) ─────────────────────────
n_rows_c <- ceiling(n_comm / 4)
comm_pad  <- c(comm, rep(NA_character_, n_rows_c * 4 - n_comm))

comm_df <- data.frame(
  col1 = comm_pad[seq(1,              n_rows_c)],
  col2 = comm_pad[seq(n_rows_c+1,    2*n_rows_c)],
  col3 = comm_pad[seq(2*n_rows_c+1,  3*n_rows_c)],
  col4 = comm_pad[seq(3*n_rows_c+1,  4*n_rows_c)],
  stringsAsFactors = FALSE
)

# ── Shared tab_options (no font color args — set via tab_style instead) ───────
.tbl_opts <- function(tbl) {
  tbl |>
    tab_options(
      table.font.names                 = "Arial",
      table.font.size                  = px(13),
      heading.title.font.size          = px(15),
      heading.title.font.weight        = "bold",
      heading.subtitle.font.size       = px(12),
      column_labels.hidden             = TRUE,
      table.border.top.color           = "#2C5F8A",
      table.border.top.width           = px(3),
      heading.background.color         = "#2C5F8A",
      row.striping.include_table_body  = FALSE
    ) |>
    # White heading text (title + subtitle) via tab_style
    tab_style(
      style     = cell_text(color = "white"),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style     = cell_text(color = "#D0E8F5"),
      locations = cells_title(groups = "subtitle")
    )
}

# ── Non-commercial gt table ───────────────────────────────────────────────────
tbl_noncomm <- nc_df |>
  gt() |>
  tab_header(
    title    = md("Non-commercial reef-associated species"),
    subtitle = md(glue::glue("{n_noncomm} species included in this analysis"))
  ) |>
  tab_style(
    style     = cell_text(size = px(13)),
    locations = cells_body()
  ) |>
  .tbl_opts() |>
  tab_footnote(
    footnote = "\u2020 Also included in the commercial account."
  )

# ── Commercial gt table ───────────────────────────────────────────────────────
tbl_comm <- comm_df |>
  gt() |>
  tab_header(
    title    = md("Commercial reef-associated species"),
    subtitle = md(glue::glue("{n_comm} species/species groups included in this analysis"))
  ) |>
  tab_style(
    style     = cell_text(size = px(13)),
    locations = cells_body()
  ) |>
  sub_missing(missing_text = "") |>
  .tbl_opts() |>
  tab_footnote(
    footnote = "\u2020 Also included in the non-commercial account."
  )

# ── Save ─────────────────────────────────────────────────────────────────────
gtsave(tbl_noncomm, file.path(out_dir, "reef_species_noncomm.html"))
gtsave(tbl_comm,    file.path(out_dir, "reef_species_comm.html"))

# PNG (requires webshot2 / chromote)
tryCatch({
  gtsave(tbl_noncomm, file.path(out_dir, "reef_species_noncomm.png"), expand = 10)
  gtsave(tbl_comm,    file.path(out_dir, "reef_species_comm.png"),    expand = 10)
  message("PNG exports complete.")
}, error = function(e) {
  message("PNG export skipped (webshot2 not available): ", conditionMessage(e))
  message("HTML files written — open in browser to screenshot or print.")
})

message("\nDone. Files written to: ", out_dir)
message("  reef_species_noncomm.html / .png")
message("  reef_species_comm.html / .png")
