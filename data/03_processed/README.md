# Processed Data — `data/03_processed/`

This folder contains the pipeline's final analysis-ready outputs, one subfolder per account type. Each subfolder has its own README with a full data dictionary.

All datasets follow the [UN System of Environmental-Economic Accounting – Ecosystem Accounting (SEEA-EA)](https://seea.un.org/ecosystem-accounting) framework. Spatial units are the 43 traditional Hawaiian *moku* (land divisions), which serve as Ecosystem Accounting Areas (EAAs) for the Main Hawaiian Islands.

---

## Subfolders

| Folder | Contents | Rows | Temporal coverage |
|---|---|---|---|
| [`extents/`](extents/) | Ecosystem type areas by moku | 1,745 | 2013, 2016, 2019 |
| [`conditions/`](conditions/) | Ecosystem condition indicators by moku | 850 | 2013, 2016, 2019 |
| [`fisheries_exchange_values/`](fisheries_exchange_values/) | Commercial and non-commercial fisheries exchange values | 3,334 + 108 | 1997–2022 |

---

## Shared identifier fields

All three account datasets share a common set of geographic identifier fields. Definitions are consistent across files.

| Field | Type | Description | Example |
|---|---|---|---|
| `name2` | character | Moku code — uppercase, used as the join key across all datasets | `"HALELEA"` |
| `moku` | character | Moku lowercase slug | `"halelea"` |
| `moku_olelo` | character | Moku display name in ʻŌlelo Hawaiʻi | `"Haleleʻa"` |
| `island` | character | Island lowercase slug | `"kauai"` |
| `island_olelo` | character | Island display name in ʻŌlelo Hawaiʻi | `"Kauaʻi"` |
| `realm` | character | `"Terrestrial"` or `"Marine"` | `"Marine"` |
| `year` | integer | Accounting year | `2013` |

---

## Source inventory

For the full inventory of input datasets, data formats, agencies, and temporal coverage, see [`documentation/DATA_SOURCES.md`](../../documentation/DATA_SOURCES.md), which corresponds to Appendix A of the project report.
