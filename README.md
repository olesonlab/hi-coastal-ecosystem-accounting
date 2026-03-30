# Hawaiʻi Coastal Ecosystem Accounting

Natural capital accounts for the Main Hawaiian Islands under the UN SEEA-EA framework—ecosystem extents, conditions, fisheries exchange values, and links to related recreation economics work.

## Links

| | |
|--|--|
| **Interactive dashboard** | [olesonlab-hi-coastal-ecosystem-accounting.share.connect.posit.cloud](https://olesonlab-hi-coastal-ecosystem-accounting.share.connect.posit.cloud) |
| **Project website** | [olesonlab-hawaii-coastal-ecosystem-accounting-website.share.connect.posit.cloud](https://olesonlab-hawaii-coastal-ecosystem-accounting-website.share.connect.posit.cloud) |
| **Recreation account (coral reef welfare)** | [loweas.github.io/CR](https://loweas.github.io/CR/) |

## Repository contents

- **Shiny dashboard** ([Rhino](https://appsilon.github.io/rhino/)) — explores extents, conditions, and fisheries valuation from processed outputs in `data/`.
- **Data pipeline** — [`targets`](https://books.ropensci.org/targets/) in `_targets.R`; R helpers in `R/`.
- **Quarto project website** — source in `website/` (documentation-style pages; may be deployed separately).

Further technical notes: [documentation/](documentation/) (e.g. [DATA_SOURCES](documentation/DATA_SOURCES.md)).

## Quick start

```bash
git clone https://github.com/olesonlab/hi-coastal-ecosystem-accounting.git
cd hi-coastal-ecosystem-accounting

Rscript -e "renv::restore()"
Rscript -e "targets::tar_make()"   # rebuild processed data (optional if data is present)
Rscript -e "rhino::app()"          # run dashboard locally
```

Requires **R 4.5+** (see `renv.lock`). Large raw inputs live under `data/01_raw/`; not all paths may be public—see documentation.

## License

[MIT](LICENSE)

## Acknowledgments

[Oleson Lab](https://www.olesonlab.org/), Department of Natural Resources and Environmental Management, University of Hawaiʻi at Mānoa.
