# Fisheries Exchange Value Accounts — `data/03_processed/fisheries_exchange_values/`

Exchange values (USD) for commercial and non-commercial fisheries in the Main Hawaiian Islands, following the UN SEEA-EA framework. Exchange value represents the economic contribution of fisheries to national income:

> **Exchange Value = Gross Revenue − Marginal Costs**

---

## Files

| File | Format | Rows | Spatial unit | Temporal coverage |
|---|---|---|---|---|
| `comm_ev.csv` | CSV | 3,334 | DAR catch area (310 areas) | 1997–2021 |
| `noncomm_ev.csv` | CSV | 108 | Island (6 islands) | 2005–2022 |

---

## `comm_ev.csv` — Commercial fisheries exchange values

### Coverage

| Dimension | Coverage |
|---|---|
| Spatial unit | HDAR commercial fishing catch areas (310 unique area codes across all years) |
| Temporal coverage | 1997–2021 |
| Species groups | Deep 7 Bottomfish, Shallow Bottomfish (sbf), Pelagics, Reef-Associated (reef) |
| Ecosystem attribution | Inshore-Reef (`REEFET`), Coastal-Open Ocean (`NONREEFET`), All ecosystems (`TOTAL`) |

### Data dictionary

#### Identifiers

| Column | Type | Unit | Description |
|---|---|---|---|
| `year` | numeric | — | Calendar year (1997–2021) |
| `area` | numeric | — | DAR catch area identifier (integer code) |
| `county` | character | — | County in which the catch area falls (`"hawaii"`, `"maui"`, `"oahu"`, `"kauai"`). Note: `"oahu"` represents Honolulu County and is a known data issue to be corrected. |

#### Physical landings

| Column | Type | Unit | Description |
|---|---|---|---|
| `landings.deep7` | numeric | lbs | Commercial landings of Deep 7 bottomfish species |
| `landings.sbf` | numeric | lbs | Commercial landings of shallow bottomfish species |
| `landings.reef` | numeric | lbs | Commercial landings of reef-associated species |
| `landings.pelagic` | numeric | lbs | Commercial landings of pelagic species |

#### Labor inputs

| Column | Type | Unit | Description |
|---|---|---|---|
| `labor.deep7` | numeric | full-time equivalent (FTE) | Labor input for Deep 7 bottomfish fishing |
| `labor.sbf` | numeric | FTE | Labor input for shallow bottomfish fishing |
| `labor.reef` | numeric | FTE | Labor input for reef-associated fishing |
| `labor.pelagic` | numeric | FTE | Labor input for pelagic fishing |

#### Market wholesale prices

| Column | Type | Unit | Description |
|---|---|---|---|
| `mwp.deep7` | numeric | USD/lb | Nominal market wholesale price, Deep 7 |
| `mwp.sbf` | numeric | USD/lb | Nominal market wholesale price, shallow bottomfish |
| `mwp.reef` | numeric | USD/lb | Nominal market wholesale price, reef-associated |
| `mwp.pelagic` | numeric | USD/lb | Nominal market wholesale price, pelagics |
| `mwp.deep7.adj` | numeric | USD/lb | CPI-adjusted market wholesale price, Deep 7 |
| `mwp.sbf.adj` | numeric | USD/lb | CPI-adjusted market wholesale price, shallow bottomfish |
| `mwp.reef.adj` | numeric | USD/lb | CPI-adjusted market wholesale price, reef-associated |
| `mwp.pelagic.adj` | numeric | USD/lb | CPI-adjusted market wholesale price, pelagics |

#### Gross revenue

| Column | Type | Unit | Description |
|---|---|---|---|
| `rev.deep7` | numeric | USD | Gross revenue, Deep 7 bottomfish (`landings.deep7 × mwp.deep7.adj`) |
| `rev.sbf` | numeric | USD | Gross revenue, shallow bottomfish |
| `rev.reef` | numeric | USD | Gross revenue, reef-associated |
| `rev.pelagic` | numeric | USD | Gross revenue, pelagics |

#### Total costs (all species)

| Column | Type | Unit | Description |
|---|---|---|---|
| `cost.all.deep7` | numeric | USD/lb | All-in marginal cost per pound, Deep 7 (labor + other operating costs, CPI-adjusted) |
| `cost.all.sbf` | numeric | USD/lb | All-in marginal cost per pound, shallow bottomfish |
| `cost.all.reef` | numeric | USD/lb | All-in marginal cost per pound, reef-associated |
| `cost.all.pelagic` | numeric | USD/lb | All-in marginal cost per pound, pelagics |

#### Species-level exchange values

| Column | Type | Unit | Description |
|---|---|---|---|
| `ev.deep7` | numeric | USD | Exchange value, Deep 7 bottomfish (`rev.deep7 − cost.all.deep7 × landings.deep7`) |
| `ev.sbf` | numeric | USD | Exchange value, shallow bottomfish |
| `ev.reef` | numeric | USD | Exchange value, reef-associated species |
| `ev.pelagic` | numeric | USD | Exchange value, pelagics |

#### Ecosystem attribution — Shallow bottomfish split

Shallow bottomfish EV is split between reef and non-reef ecosystems using a reef-association ratio derived from the FSOET dataset.

| Column | Type | Unit | Description |
|---|---|---|---|
| `sbf.ratio.REEFET` | numeric | proportion (0–1) | Fraction of shallow bottomfish EV attributed to the inshore-reef ecosystem. `NA` where landings are zero. |
| `ev.sbf.REEFET` | numeric | USD | Shallow bottomfish EV attributed to inshore-reef ecosystem (`ev.sbf × sbf.ratio.REEFET`) |
| `ev.sbf.NONREEFET` | numeric | USD | Shallow bottomfish EV attributed to coastal-open ocean ecosystem |

#### Ecosystem-aggregated exchange values

| Column | Type | Unit | Description |
|---|---|---|---|
| `ev.REEFET` | numeric | USD | Total EV attributed to the **inshore-reef** ecosystem (reef-associated + sbf reef fraction) |
| `ev.NONREEFET` | numeric | USD | Total EV attributed to the **coastal-open ocean** ecosystem (pelagic + sbf non-reef fraction) |
| `ev.TOTAL` | numeric | USD | Total EV across all species and ecosystems (`ev.REEFET + ev.NONREEFET`) |

---

## `noncomm_ev.csv` — Non-commercial fisheries exchange values

### Coverage

| Dimension | Coverage |
|---|---|
| Spatial unit | 6 islands (Hawaiʻi, Maui, Molokaʻi, Lānaʻi, Oʻahu, Kauaʻi) |
| Temporal coverage | 2005–2022 |
| Species group | Herbivores (100% reef-attributed) |
| Ecosystem attribution | Inshore-Reef only; Coastal-Open Ocean = $0 in current data |

Niʻihau and Kahoʻolawe lack MRIP survey data and are excluded.

### Data dictionary

| Column | Type | Unit | Description |
|---|---|---|---|
| `year` | numeric | — | Calendar year (2005–2022) |
| `island` | character | — | Island name lowercase slug (`"hawaii"`, `"maui"`, `"molokai"`, `"lanai"`, `"oahu"`, `"kauai"`) |
| `lbs.total` | numeric | lbs | Estimated total non-commercial catch for the island and year (MRIP-derived) |
| `reef.price.adj` | numeric | USD/lb | CPI-adjusted market price of reef fish used as the shadow price for non-commercial catch |
| `rev.total` | numeric | USD | Imputed gross revenue (`lbs.total × reef.price.adj`) |
| `costs.other.perlb.adj` | numeric | USD/lb | CPI-adjusted non-labor operating costs per pound (fuel, gear, etc.) |
| `costs.labor.perlb.adj` | numeric | USD/lb | CPI-adjusted opportunity cost of labor per pound |
| `ev.total` | numeric | USD | Total exchange value across all ecosystem types (`rev.total − (costs.other.perlb.adj + costs.labor.perlb.adj) × lbs.total`) |
| `ev.total.REEF` | numeric | USD | EV attributed to the **inshore-reef** ecosystem. Currently equals `ev.total` (100% reef assumption for herbivores). |
| `ev.total.OO` | numeric | USD | EV attributed to the **coastal-open ocean** ecosystem. Currently `0` for all records. |

---

## Ecosystem attribution framework

Both datasets attribute exchange value to two ecosystem types consistent with the extent and condition accounts:

| Label | Ecosystem type | Corresponds to extent type |
|---|---|---|
| `REEFET` | Inshore — Reef | `Coral Dominated Hard Bottom`, `Other Hard Bottom`, `Pavement`, `Rock/Boulder`, `Soft Bottom` |
| `NONREEFET` / `OO` | Coastal — Open Ocean | `Open Ocean` |

Attribution is derived from the **Fish Species over Ecosystem Type (FSOET)** dataset, which assigns each species group a probability of occurring over reef vs. non-reef habitat.

---

## Notes

- All monetary values are in **nominal USD** adjusted to a reference year using the Consumer Price Index (CPI). Check `R/prep_uses_fisheries.R` for the reference year used.
- `NA` in exchange value columns indicates a catch area had zero reported landings for that species group in that year; the ratio cannot be computed and EV components are undefined.
- Negative exchange values (`ev.pelagic`, `ev.NONREEFET`) can occur when the pelagic fishery operates at a loss relative to marginal costs in that area-year.
- The `_archive/` subfolder contains earlier pipeline versions with slightly different schemas. Do not use these for analysis.
- The `_reference/` subfolder contains intermediate calculation tables used to validate the pipeline against original workbook outputs.

---

## Source datasets (Appendix A cross-reference)

| Source | Agency | Format | Temporal coverage |
|---|---|---|---|
| Commercial Marine Landings (CML) | Hawaiʻi Division of Aquatic Resources (HDAR) | Tabular | 1997–2021 |
| Fish Species over Ecosystem Type (FSOET) | HDAR | Tabular | — |
| MRIP Hawaiʻi Marine Recreational Fishing Survey | NOAA Fisheries | Tabular | 2005–2022 |
| DAR commercial fishing catch area polygons | HDAR | Vector | Current |
| Island boundaries | Derived from moku polygons | Vector | Current |
