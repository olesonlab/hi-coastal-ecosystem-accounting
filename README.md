# Hawaiʻi Coastal Ecosystem Accounting

Natural capital accounts for the Main Hawaiian Islands (MHI) following the UN SEEA-EA framework. The work covers ecosystem extent and condition accounts (ecosystem type areas and biophysical condition indicators by moku; accounting years 2013, 2016, and 2019 where applicable) and ecosystem services or use accounts: commercial and non-commercial fisheries exchange values by species group, county, and island (1997–2022), and recreation (non-market values for marine recreational activities).

## Links

- **[HCEA Extents and Conditions Dashboard Prototype](https://olesonlab-hcea-extents-and-conditions-dashboard.share.connect.posit.cloud)**
- **[HCEA Fisheries Exchange Values Dashboard GitHub Repo](https://github.com/CTAHR-Dashboard/CTAHR-Dashboard)**
  - **[Dashboard Prototype Demo Video](https://drive.google.com/file/d/1-YyCopTKhXcHii7TXUnpemuc2fOPesJN/view?usp=sharing)**
- **[HCEA Project Website](https://olesonlab-hcea-project-website.share.connect.posit.cloud)**
- **HCEA Recreation Account Resources**
  - [The economic impact of climate change on coral reef in the Main Hawaiian Islands project website](https://loweas.github.io/moku_cr/)
  - [Recreation by moku website](https://loweas.github.io/moku_cr/)

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
