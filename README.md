<a id="devex-badge" rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

# Air Zone Report Templates

Creation of Air Zone reports for reporting achievement of Canadian Ambient Air Quality Standards

### Usage

These reports depend on outputs from the [Ozone CAAQS Analysis](https://github.com/bcgov/ozone-caaqs-indicator/) and [Fine Particulate Matter CAAQS Analysis](https://github.com/bcgov/pm25-caaqs-indicator). To get started:

* Clone these repositories
* Check out the `update-2018` branch
* Run the analysis scripts in each repository (or at least number 01-04)

Then, in a *clean* R session, run the `render_air_zone_reports.R` script in this
repository. It is important to run this script in a clean R session, and not the
one in which you ran the ozone and PM2.5 analyses. Those analyses create some
objects with the same names, which will create problems when rendering the
reports. `render_air_zone_reports.R` loads only the objects it needs.

#### Example

Rendering the reports is a two-stage process; first rendering to
an intermediate markdown file, editing the text in this, then
rendering that to a final pdf:

1. First load the necessary objects from the ozone and PM2.5 analyses

```r 
library("purrr")
library("rmarkdown")


ozone_dir <- "../ozone-caaqs-indicator/"
pm25_dir  <- "../pm25-caaqs-indicator/"

load(file.path(ozone_dir, "tmp/ozone_clean.RData"))
load(file.path(ozone_dir, "tmp/analysed.RData"))
load(file.path(ozone_dir, "tmp/plots.RData"))

load(file.path(pm25_dir, "tmp/pm25_clean.rda"))
load(file.path(pm25_dir, "tmp/analysed.RData"))
load(file.path(pm25_dir, "tmp/plots.RData"))
```

2. Render intermediate rerports to `.md` format:

```r
render_intermediate("00_summary_report.Rmd", summary_report = TRUE)

## Render intermediate reports, named 01_northeast.Rmd, 02_central_interior.Rmd, etc.
## using default parameters
render_intermediate("01_northeast.Rmd")
render_intermediate("02_central_interior.Rmd")
render_intermediate("03_southern_interior.Rmd")
render_intermediate("04_lower_fraser_valley.Rmd")
render_intermediate("05_georgia_strait.Rmd")
render_intermediate("06_coastal.Rmd")
```

To render a report and specify a subset of stations to include in plots, 
provide a vector of the ems_ids to the `ozone_ems_ids` and `pm25_ems_ids` 
arguments. E.g.:

```r
render_intermediate("01_northeast.Rmd", 
                    ozone_ems_ids = c("E227431", "E229797", "E231866"))
```

3. Edit the text (or figure paths etc) in the `.md` files that are generated in the 
`intermediate_reports` folder

4. Finally run `render_final()` on those markdown files:

```r
render_final("intermediate_reports/00_summary_report_intermediate.md")
render_final("intermediate_reports/01_northeast_intermediate.md")
render_final("intermediate_reports/02_central_interior_intermediate.md")
render_final("intermediate_reports/03_southern_interior_intermediate.md")
render_final("intermediate_reports/04_lower_fraser_valley_intermediate.md")
render_final("intermediate_reports/05_georgia_strait_intermediate.md")
render_final("intermediate_reports/06_coastal_intermediate.md")
```

### Project Status

This project is under active development

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/air-zone-reports/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2019 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
