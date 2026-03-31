# Ecosystem Condition Accounts — `data/03_processed/conditions/`

Condition indicator values aggregated to the moku level for three accounting years. Indicators follow the [SEEA Ecosystem Condition Typology (ECT)](https://seea.un.org/ecosystem-accounting) and cover both terrestrial and marine realms.

---

## Files

| File | Format | Rows | Description |
|---|---|---|---|
| `mokus_conditions.csv` | CSV | 850 | Tabular data — use for charts, tables, and time-series |
| `mokus_conditions.gpkg` | GeoPackage | 850 | Same data with moku polygon geometry attached — use for maps |

Both files contain identical attribute data. The `.gpkg` adds one geometry column (`geom`, MULTIPOLYGON, EPSG:4326).

---

## Coverage

| Dimension | Coverage |
|---|---|
| Spatial unit | 43 moku, Main Hawaiian Islands |
| Realm | Terrestrial and Marine |
| Accounting years | 2013, 2016, 2019 |
| CRS (spatial file) | EPSG:4326 — WGS 84 |

---

## Data dictionary

| Column | Type | Description |
|---|---|---|
| `name2` | character | Moku identifier code (uppercase). Join key to other account datasets. |
| `year` | integer | Accounting year: `2013`, `2016`, or `2019`. |
| `indicator` | character | Condition indicator code. See indicator table below. |
| `value` | numeric | Indicator value for the moku-year. Units vary by indicator — see table below. |
| `ci_lower` | numeric | Lower bound of the 95% confidence interval (or equivalent uncertainty estimate). Same units as `value`. `NA` where CIs are not available for the source data. |
| `ci_upper` | numeric | Upper bound of the 95% confidence interval. Same units as `value`. `NA` where CIs are not available. |
| `category` | character | ECT category label. See controlled vocabulary below. |
| `realm` | character | `"Terrestrial"` or `"Marine"`. |
| `moku` | character | Moku lowercase slug. |
| `moku_olelo` | character | Moku display name in ʻŌlelo Hawaiʻi. For display only. |
| `island` | character | Island lowercase slug. |
| `island_olelo` | character | Island display name in ʻŌlelo Hawaiʻi. For display only. |

---

## `category` vocabulary

Values in the `category` column map to ECT groups:

| `category` value | Realm | ECT group |
|---|---|---|
| `Marine Abiotic` | Marine | Group A — Abiotic Ecosystem Characteristics |
| `Marine Biotic` | Marine | Group B — Biotic Ecosystem Characteristics |
| `Terrestrial` | Terrestrial | Groups A, B, C — all terrestrial indicators share a single category label |

---

## Indicator table

Each row in `mokus_conditions.csv` represents one moku × year × indicator combination. The `value` column carries the indicator's measurement in the units listed below.

### Marine — Abiotic (Group A)

| `indicator` | Description | Unit | Temporal aggregation | Source |
|---|---|---|---|---|
| `kd490` | Diffuse attenuation coefficient at 490 nm. Higher values indicate more turbid water with reduced light penetration. | m⁻¹ | Annual mean | NOAA NCRMP / Coral Reef Watch |
| `par` | Photosynthetically active radiation at the seafloor. Indicator of light availability for coral and algae. | mol photons m⁻² day⁻¹ | Annual mean | NOAA NCRMP / Coral Reef Watch |
| `sst` | Sea surface temperature. | °C | Annual mean | NOAA NCRMP / Coral Reef Watch |

### Marine — Biotic (Group B)

| `indicator` | Description | Unit | Temporal aggregation | Source |
|---|---|---|---|---|
| `coral_cover` | Percent live coral cover on the reef substrate. | % | Survey mean | NOAA National Coral Reef Monitoring Program (NCRMP) |
| `adult_coral_density` | Density of adult coral colonies. | colonies per m² | Survey mean | NOAA NCRMP |
| `juvenile_coral_density` | Density of juvenile coral recruits (<4 cm diameter). | colonies per m² | Survey mean | NOAA NCRMP |
| `coral_diversity` | Coral species diversity index. | index | Survey mean | NOAA NCRMP |
| `primary_consumer_biomass` | Biomass of primary consumer fish (herbivores, detritivores). | g per m² | Survey mean | NOAA NCRMP |
| `planktivore_biomass` | Biomass of planktivore fish. | g per m² | Survey mean | NOAA NCRMP |
| `piscivore_biomass` | Biomass of piscivore (fish-eating) fish. | g per m² | Survey mean | NOAA NCRMP |
| `disease_prevalence` | Proportion of coral colonies showing signs of disease. | % | Survey mean | NOAA NCRMP |

### Terrestrial — Abiotic (Group A)

| `indicator` | Description | Unit | Temporal aggregation | Source |
|---|---|---|---|---|
| `mean_rainfall` | Mean monthly rainfall within the moku, averaged across all terrestrial pixels. | mm | Annual mean of monthly means | Hawaiʻi Climate Data Portal (HCDP) |
| `mean_temperature` | Mean monthly temperature within the moku. | °C | Annual mean of monthly means | Hawaiʻi Climate Data Portal (HCDP) |
| `burnt_area` | Total area within the moku affected by wildfire during the accounting year. | km² | Annual cumulative | Hawaiʻi Wildfire Management Organization (HWMO) |

### Terrestrial — Biotic / Landscape (Groups B & C)

| `indicator` | Description | Unit | Temporal aggregation | Source |
|---|---|---|---|---|
| `ndvi` | Normalized Difference Vegetation Index. Mean NDVI within the moku, averaged across ecosystem type pixels. Values range −1 to +1; higher values indicate denser, healthier vegetation. | dimensionless (−1 to 1) | Annual mean | Hawaiʻi Climate Data Portal (HCDP) |

> **Note:** Indicator codes in the `indicator` column are defined in `R/prep_conditions.R`. The table above lists the primary indicators included in the current pipeline outputs. Refer to [`documentation/DATA_SOURCES.md`](../../documentation/DATA_SOURCES.md) for the full inventory of condition data sources considered under the ECT framework.

---

## Uncertainty columns

`ci_lower` and `ci_upper` represent the uncertainty bounds on `value`. Their precise meaning depends on the source data:

- **NOAA NCRMP survey data** (coral, fish): 95% confidence intervals derived from the survey's stratified random design.
- **Raster-derived indicators** (mean_rainfall, mean_temperature, ndvi, kd490, par, sst): bounds represent the 5th and 95th percentile of pixel values within the moku boundary for the accounting year.
- `NA` in both columns indicates the source provided a point estimate only.

---

## Notes

- The dataset is in **long format** — one row per moku × year × indicator. To analyze multiple indicators simultaneously, pivot wider on `indicator`.
- Not all indicators are available for all moku. Survey-based indicators (NCRMP) depend on survey site coverage, which may not include every moku in every year.
- Terrestrial indicators are computed over ecosystem-type-specific areas within each moku (e.g., rainfall for tree cover, NDVI for tree cover and freshwater wetlands) and then aggregated to the moku level. Values represent conditions experienced by the ecosystem, not the moku as a whole.

---

## Source datasets (Appendix A cross-reference)

| Source | Agency | Format | Resolution | Years |
|---|---|---|---|---|
| NOAA National Coral Reef Monitoring Program (NCRMP) | NOAA | Vector (survey sites) | Site-level | 2013, 2016, 2019 |
| Coral Reef Watch (CRW) | NOAA | Raster | 5 km | 2013, 2016, 2019 |
| Hawaiʻi Climate Data Portal (HCDP) | UH Mānoa | Raster | 2 m (rainfall/temp); 240 m (NDVI) | 1990–present |
| Hawaiʻi Wildfire Management Organization (HWMO) | HWMO | Vector (burn perimeters) | — | 1999–2022 |
| Moku boundaries | HIMARC | Vector (polygons) | — | Current |
