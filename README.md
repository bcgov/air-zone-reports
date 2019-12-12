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

```r
source("render_air_zone_reports.R")
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
