# Ecosystem Extent Accounts — `data/03_processed/extents/`

Area (km²) of each ecosystem type within each moku for three accounting years. Covers both terrestrial and marine realms across all 43 moku in the Main Hawaiian Islands.

---

## Files

| File | Format | Rows | Description |
|---|---|---|---|
| `mokus_extents.csv` | CSV | 1,745 | Tabular data — use for charts, tables, and summaries |
| `mokus_extents.gpkg` | GeoPackage | 1,745 | Same data with moku polygon geometry attached — use for maps |

Both files contain identical attribute data. The `.gpkg` adds one geometry column (`geom`, MULTIPOLYGON, EPSG:4326).

---

## Coverage

| Dimension | Coverage |
|---|---|
| Spatial unit | 43 moku, Main Hawaiian Islands |
| Realm | Terrestrial (9 ecosystem types) and Marine (6 ecosystem types) |
| Accounting years | 2013, 2016, 2019 |
| CRS (spatial file) | EPSG:4326 — WGS 84 |

---

## Data dictionary

| Column | Type | Unit | Description |
|---|---|---|---|
| `realm` | character | — | `"Terrestrial"` or `"Marine"` |
| `name2` | character | — | Moku identifier code (uppercase). Join key to other account datasets. |
| `moku_olelo` | character | — | Moku name in ʻŌlelo Hawaiʻi. For display only. |
| `moku` | character | — | Moku lowercase slug. |
| `island_olelo` | character | — | Island name in ʻŌlelo Hawaiʻi. For display only. |
| `island` | character | — | Island lowercase slug (`"hawaii"`, `"maui"`, `"oahu"`, `"kauai"`, `"molokai"`, `"lanai"`, `"niihau"`, `"kahoolawe"`). |
| `ecosystem_type` | character | — | Ecosystem type label. See controlled vocabulary below. |
| `area_km2` | numeric | km² | Area of the ecosystem type within the moku boundary. |
| `year` | numeric | — | Accounting year: `2013`, `2016`, or `2019`. |

---

## Ecosystem type vocabulary

### Terrestrial (source: SEEA-EA land cover mosaic)

| `ecosystem_type` | Description | Primary data source | Source years |
|---|---|---|---|
| `Developed` | Impervious surfaces, built-up areas | USGS LCMAP (class 1) | 2013, 2016, 2019 |
| `Grass/Shrub` | Grassland and shrubland; includes former cropland not claimed by ag census | USGS LCMAP (class 3; residual class 2 reclassified) | 2013, 2016, 2019 |
| `Tree Cover` | Forest and woodland | USGS LCMAP (class 4) | 2013, 2016, 2019 |
| `Estuarine Wetlands` | Brackish and coastal wetlands; LCMAP class 6 pixels not identified as freshwater by NWI | USGS LCMAP (class 6, residual) | 2013, 2016, 2019 |
| `Barren` | Exposed rock, bare soil, lava | USGS LCMAP (class 8) | 2013, 2016, 2019 |
| `Cropland` | Active agricultural cropland (non-pasture) | State of Hawaiʻi Agriculture Land Use Census | 2015 (→ 2013 & 2016), 2020 (→ 2019) |
| `Pasture` | Agricultural pasture land | State of Hawaiʻi Agriculture Land Use Census | 2015 (→ 2013 & 2016), 2020 (→ 2019) |
| `Freshwater Wetlands` | Freshwater emergent and forested/shrub wetlands | USFWS National Wetland Inventory (NWI) | 2018 (applied to all years) |
| `Beaches/Dunes` | Unconsolidated shore — beaches and sand dunes | NOAA Coastal Change Analysis Program (C-CAP), class 19 | 2010–2011 (applied to all years) |

### Marine (source: HIMARC benthic habitat model)

| `ecosystem_type` | Description | Primary data source | Source years |
|---|---|---|---|
| `Coral Dominated Hard Bottom` | Substrates where living coral cover is the primary surface feature | HIMARC benthic habitat raster (class 5) | 2004–2014 model |
| `Other Hard Bottom` | Consolidated rocky substrates with low or absent coral cover | HIMARC benthic habitat raster (class 2) | 2004–2014 model |
| `Pavement` | Flat, smooth consolidated substrate with little rugosity | HIMARC benthic habitat raster (class 4) | 2004–2014 model |
| `Rock/Boulder` | Structurally complex substrates of large individual rocks or boulder fields | HIMARC benthic habitat raster (class 3) | 2004–2014 model |
| `Soft Bottom` | Unconsolidated sediment — sand, mud, and rubble | HIMARC benthic habitat raster (class 1) | 2004–2014 model |
| `Open Ocean` | Nearshore area within moku marine boundary not classified as one of the five benthic types | HIMARC benthic habitat raster (residual/unclassified) | 2004–2014 model |

---

## Area calculation methods

**Terrestrial:** Pixel count × pixel area. LCMAP raster resolution is 30 m × 30 m = 900 m² per pixel. Areas summed per class per moku and converted to km² (`count × 900 / 1,000,000`). Classification priority order: Agriculture Census > NWI > C-CAP > LCMAP.

**Marine:** Pixel count × pixel area. HIMARC benthic raster resolution is 100 m × 100 m = 10,000 m² per pixel. Areas summed per class per moku and converted to km² (`count × 10,000 / 1,000,000`). Marine coverage extends from the shoreline to approximately 30 m depth.

**CRS for area calculation:** NAD83 (HARN) / Hawaiʻi State Plane Zone 3, EPSG 2784. Final outputs stored in EPSG:4326.

---

## Notes

- Marine extent data is static — the HIMARC benthic model has a single baseline (2004–2014). The same values are repeated for all three accounting years so the dataset structure is consistent with terrestrial.
- Terrestrial area for a moku should approximately equal the moku's total land area. Small discrepancies arise from water pixels (LCMAP class 5) set to NA and excluded from the accounts.
- The agriculture census years (2015, 2020) are the closest available to the accounting periods. The 2015 vintage is used for both the 2013 and 2016 accounts; the 2020 vintage is used for 2019.

---

## Source datasets (Appendix A cross-reference)

| Source | Agency | Format | Resolution | Years |
|---|---|---|---|---|
| USGS LCMAP | USGS | Raster | 30 m | 2013, 2016, 2019 |
| Agriculture Land Use Census | State of Hawaiʻi / UH Hilo SDAV Lab | Vector (polygons) | Parcel | 2015, 2020 |
| National Wetland Inventory (NWI) | USFWS | Vector (polygons) | — | 2018 |
| Coastal Change Analysis Program (C-CAP) | NOAA | Raster | 3 m | 2010–2011 |
| HIMARC benthic habitat | Hawaiʻi Monitoring and Reporting Collaborative | Raster (modeled) | 100 m | 2004–2014 |
| Moku boundaries | HIMARC | Vector (polygons) | — | Current |
