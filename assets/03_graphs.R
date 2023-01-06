# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Graphs that are available
#' 
source('./assets/00_setup.R')
dataDirectory <- './data/out'
list.files(dataDirectory)

require(dplyr)
require(ggplot2)
#create seasonal graphs
df_seasonal <- readRDS(paste(dataDirectory,'exceed_seasonal.Rds',sep='/'))

param <- 'PM25'
station <- 'Prince George Plaza 400'


#create 
