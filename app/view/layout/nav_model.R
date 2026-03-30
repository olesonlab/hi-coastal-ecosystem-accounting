# app/view/layout/nav_model.R

# Purpose
# - Single source of truth for dashboard navigation "scope keys".
# - These keys drive routing in main.R (which page + which controlbar controls are shown).

# Rules
# - Keep this file data-only: constants/vectors only.
# - No Shiny code, no bs4Dash code, no reactivity, no data loading.
# - Update only when adding a new page
#   - If new page added, update SCOPES and SCOPE_LABELS together.

HOME = "home"
EXTENTS = "extents"
CONDITIONS = "conditions"

SCOPES = c(EXTENTS, CONDITIONS)

SCOPE_LABELS = c(
  home = "Home",
  extents = "Extents",
  conditions = "Conditions"
)
