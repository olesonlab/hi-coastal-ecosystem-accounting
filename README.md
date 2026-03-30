# Hawaiʻi Coastal Ecosystem Accounting

Natural capital accounts for the Main Hawaiian Islands (MHI) following the UN SEEA-EA framework. The work covers ecosystem extent and condition accounts (ecosystem type areas and biophysical condition indicators by moku; accounting years 2013, 2016, and 2019 where applicable) and ecosystem services or use accounts: commercial and non-commercial fisheries exchange values by species group, county, and island (1997–2022), and recreation (non-market values for marine recreational activities).

## Links

- **[HCEA Extents and Conditions Dashboard](https://olesonlab-hi-coastal-ecosystem-accounting.share.connect.posit.cloud)**
- **[HCEA Project Website](https://olesonlab-hawaii-coastal-ecosystem-accounting-website.share.connect.posit.cloud)**
- **[Recreation Account Website](https://loweas.github.io/CR/)**

## Repository contents

- **Shiny dashboard** ([Rhino](https://appsilon.github.io/rhino/)): extent and condition views plus the fisheries exchange value account, from processed outputs in `data/`.
- **Data pipeline**: [`targets`](https://books.ropensci.org/targets/) in `_targets.R`, R helpers in `R/`.
- **Quarto project website**: source in `website/` (documentation-style pages; may be deployed separately).

Further technical notes: [documentation/](documentation/) (e.g. [DATA_SOURCES](documentation/DATA_SOURCES.md)).

## Quick start

```bash
git clone https://github.com/olesonlab/hi-coastal-ecosystem-accounting.git
cd hi-coastal-ecosystem-accounting

Rscript -e "renv::restore()"
Rscript -e "targets::tar_make()"   # rebuild processed data (optional if data is present)
Rscript -e "rhino::app()"          # run dashboard locally
```

Requires **R 4.5+** (see `renv.lock`). Large raw inputs live under `data/01_raw/`; not every path may be public. See [documentation/](documentation/).

## License

[MIT](LICENSE)

## Acknowledgments

[Oleson Lab](https://www.olesonlab.org/), Department of Natural Resources and Environmental Management, University of Hawaiʻi at Mānoa.
