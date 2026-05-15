# Capital Natural Mexico Shiny

This repository contains the code and data for the Capital Natural Mexico Shiny application. The application is built using R and Shiny to visualize and analyze environmental data.

## Folder Structure

- **server.R**: Contains the server-side logic for the Shiny app.
- **ui.R**: Contains the user interface definition for the Shiny app.
- **data/**: Contains the data files used in the application.
  - `master_hex_10km.gpkg`: Geopackage file for 10km hexagonal grid.
  - `master_hex_2km.gpkg`: Geopackage file for 2km hexagonal grid (ignored in version control).
  - `indicadores/`: Folder containing indicator CSV files.
- **scripts/**: Contains R scripts for data processing and preparation.
  - `build_master_hex_10km.R`: Script to build the 10km hexagonal grid.
  - `build_master_hex.R`: Script to build hexagonal grids.
  - `create_example_xlsx.R`: Script to create example Excel files.
  - `create_template_excel.R`: Script to create Excel templates.
- **www/**: Contains static assets such as CSS files.
  - `style.css`: Custom styles for the Shiny app.

## Getting Started

1. Clone the repository:
   ```bash
   git clone <repository-url>
   ```

2. Open the R project in RStudio.

3. Install the required R packages:
   ```R
   install.packages(c("shiny", "sf", "tidyverse"))
   ```

4. Run the Shiny app:
   ```R
   shiny::runApp()
   ```

## Notes

- The `data/master_hex_2km.gpkg` file is ignored in version control to reduce repository size.
- Ensure that all required data files are present in the `data/` folder before running the app.

## License

This project is licensed under the MIT License. See the LICENSE file for details.