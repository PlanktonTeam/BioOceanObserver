# BioOceanObserver Dataset Summary

This table summarises all datasets saved to `R/sysdata.rda` via [`data-raw/All_RData.R`](data-raw/All_RData.R).

Two primary mechanisms are used to retrieve data from the `planktonr` package:

- **`pr_get_Raw()`** — downloads CSV data from the IMOS AODN GeoServer WFS endpoint:  
  `http://geoserver-123.aodn.org.au/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=imos:LAYER_NAME&outputFormat=csv-with-metadata-header`

- **`pr_get_s3()`** — downloads CSV data from the IMOS S3 bucket:  
  `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/FILENAME.csv`  
  (browseable at `http://imos-data.s3-website-ap-southeast-2.amazonaws.com/?prefix=IMOS/BGC_DB/harvested_from_CSIRO/`)

---

## Core Colour / Metadata

| Dataset | Description | Source |
|---------|-------------|--------|
| `col12` | A 12-colour palette (RColorBrewer "Paired") used for consistent plot colouring throughout the app. | Derived in-app from the R package `RColorBrewer`; no external download. |
| `modified_time` | Timestamp recording when the sysdata file was last rebuilt. | Generated at build time via `lubridate::now()`; no external download. |
| `ParamDef` | A lookup table of parameter names, display labels, units, and descriptions used to annotate plots and UI elements. | Manually curated CSV file at [`data-raw/ParameterDefn.csv`](data-raw/ParameterDefn.csv); no external download. |
| `AusStatesSimple` | A simplified spatial polygon object of Australian state and territory boundaries used for background mapping. | Pre-processed and saved as [`data-raw/aus_states_simplified.rds`](data-raw/aus_states_simplified.rds); original source is the Australian Bureau of Statistics (ABS) ASGS boundaries. |
| `imcra` | Polygon boundaries for the Integrated Marine and Coastal Regionalisation of Australia (IMCRA v4.0) Provincial Bioregions, used for spatial context on maps. | Australian Government Department of Climate Change, Energy, the Environment and Water — IMCRA v4.0; read from the local file [`data-raw/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.geojson`](data-raw/Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.geojson). |

---

## Station / Survey Metadata

| Dataset | Description | Source |
|---------|-------------|--------|
| `NRSStation` | Station-level metadata (name, code, latitude, longitude, region, start date, operational status) for the IMOS National Reference Station (NRS) network. | IMOS BGC Station Info CSV bundled with the `planktonr` package at `inst/extdata/BGC_StationInfo.csv`; accessed via `planktonr::pr_get_info(Source = "NRS")`. No external URL — file is shipped with the package. |
| `SotsStation` | Station-level metadata for the Southern Ocean Time Series (SOTS) mooring site south of Tasmania. | Same `inst/extdata/BGC_StationInfo.csv` file as `NRSStation`, filtered to the SOTS project; accessed via `planktonr::pr_get_info(Source = "SOTS")`. |
| `NRSinfo` | Policy-relevant metadata for the NRS network including bioregion descriptions and oceanographic features, used in the Policy tab. | Same `inst/extdata/BGC_StationInfo.csv` as above; accessed via `planktonr::pr_get_info(Source = "NRS")`. |
| `CPRinfo` | Policy-relevant metadata for the CPR survey bioregions, including sampling effort (nautical miles) and oceanographic feature descriptions. | Derived from CPR trip data; trip data downloaded from the IMOS AODN GeoServer WFS layer `imos:cpr_samp`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_samp`; accessed via `planktonr::pr_get_info(Source = "CPR")`. |
| `SOTSinfo` | Policy-relevant metadata for the SOTS mooring site. | Same `inst/extdata/BGC_StationInfo.csv` as `NRSStation`, filtered to SOTS; accessed via `planktonr::pr_get_info(Source = "SOTS")`. |

---

## Trip / Sampling Metadata

| Dataset | Description | Source |
|---------|-------------|--------|
| `datNRSTrip` | Year, month, and station name for each NRS sampling trip, used to display sampling calendars and effort summaries. | IMOS S3 bucket: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/bgc_trip.csv`; accessed via `planktonr::pr_get_trips(Survey = "NRS")`. |
| `datCPRTrip` | Latitude, year, month, bioregion, and trip code for each CPR sample segment, used for spatial and temporal effort displays. | IMOS AODN GeoServer WFS layer `imos:cpr_samp`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_samp`; accessed via `planktonr::pr_get_trips(Survey = "CPR")`. |
| `datCPRTripSO` | Per-trip summary (mean position, sample count, year, month) for Southern Ocean CPR tows collected by the Australian Antarctic Division (AAD). | AAD dataset AADC-00099 (Southern Ocean CPR survey, 1991–present); stored as a local CSV within the `planktonr` package source at `data-raw/AADC-00099_-_2025_data_update/AADC-00099_29August2025.csv` and compiled into the internal object `cpr_AAD`. Original data available from the Australian Antarctic Data Centre: `https://data.aad.gov.au/metadata/records/AADC-00099`. |
| `datHABTrip` | Mean latitude, longitude, and date range for each HAB monitoring station with sufficient data (>15 non-zero records for at least one taxon). | State-based and seafood-industry HAB monitoring programs; data stored as internal CSVs within the `planktonr` package source (`data-raw/HAB_data_temp/HAB_Sites.csv`, `HAB_Samples.csv`); accessed via `planktonr::pr_get_trips(Survey = "HAB")`. AODN hosting is not yet available. Data owners include NSW Food Authority, Hornsby Council, TAS EPA, and others. |
| `datHABdataTable` | Summary table of HAB monitoring effort by state, data owner, and analyst, including date ranges and sample counts. | Same HAB monitoring program data as `datHABTrip` (`data-raw/HAB_data_temp/HAB_Sites.csv` and `HAB_Samples.csv` within `planktonr`). |

---

## Progress Map Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `PMapData` | A named list (`NRS`, `CPR`) of sampling location records used to render the interactive progress map showing where and when samples have been collected. | NRS component: IMOS S3 bucket `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/bgc_trip.csv`. CPR component: IMOS AODN GeoServer WFS layer `imos:cpr_samp`. Both accessed via `planktonr::pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE, near_dist_km = 250)`. |

---

## NRS Ecological Indices

| Dataset | Description | Source |
|---------|-------------|--------|
| `datNRSz` | Time series of derived zooplankton community indices (biomass, abundance, copepod diversity, evenness, mean length, etc.) at NRS stations. | IMOS S3 bucket: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/nrs_derived_indices_data.csv`; accessed via `planktonr::pr_get_Indices(Survey = "NRS", Type = "Zooplankton")`. |
| `datNRSp` | Time series of derived phytoplankton community indices (abundance, carbon biomass, diversity, diatom:dinoflagellate ratio, etc.) at NRS stations. | Same IMOS S3 file: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/nrs_derived_indices_data.csv`; accessed via `planktonr::pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")`. |
| `datNRSp_all` | Combined NRS + SOTS phytoplankton indices dataset (row-bind of `datNRSp` and `SOTSp`), pre-computed to avoid repeated joins inside reactive expressions. | Derived in-app from `datNRSp` and `SOTSp`; no additional download. |
| `datNRSw` | Time series of physical water-column indices at NRS stations including Secchi depth, mixed layer depth (temperature- and salinity-based), deep chlorophyll maximum depth, and CTD-derived surface temperature, salinity, and chlorophyll fluorescence. | Same IMOS S3 file `nrs_derived_indices_data.csv` as above; accessed via `planktonr::pr_get_Indices(Survey = "NRS", Type = "Water")`; mixed layer depth is computed in-app from the two MLD columns. |
| `SOTSp` | Time series of derived phytoplankton community indices for the SOTS station, calculated from raw NRS phytoplankton microscopy data filtered to SOTS samples at depths <50 m using the LM method. | IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_raw_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_phytoplankton_abundance_raw_data`; accessed via `planktonr::pr_get_Indices(Survey = "SOTS", Type = "Phytoplankton")`. |

---

## CPR Ecological Indices

| Dataset | Description | Source |
|---------|-------------|--------|
| `datCPRz` | Time series of derived zooplankton community indices (biomass index, abundance, copepod diversity, etc.) aggregated by CPR bioregion. | Two sources combined: (1) IMOS AODN GeoServer WFS layer `imos:cpr_derived_indices_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_derived_indices_data`; (2) Southern Ocean CPR zooplankton counts from AAD dataset AADC-00099 (internal `cpr_AAD` object). Accessed via `planktonr::pr_get_Indices(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250)`. |
| `datCPRp` | Time series of derived phytoplankton community indices (Phytoplankton Colour Index, abundance, carbon biomass, diversity, etc.) aggregated by CPR bioregion. | IMOS AODN GeoServer WFS layer `imos:cpr_derived_indices_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_derived_indices_data`; accessed via `planktonr::pr_get_Indices(Survey = "CPR", Type = "Phytoplankton", near_dist_km = 250)`. |
| `PCI` | Seasonal spatial summaries of the Phytoplankton Colour Index (PCI) — a visual estimate of phytoplankton biomass recorded on each CPR silk — averaged to 1° grid cells by season and bioregion. | IMOS AODN GeoServer WFS layer `imos:cpr_derived_indices_data` (same URL as `datCPRp`); accessed via `planktonr::pr_get_PCIData()`. |

---

## HAB (Harmful Algal Bloom) Monitoring Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `datHABg` | Time series of phytoplankton genus-level abundance indices (cells/L, biovolume, carbon biomass) at HAB monitoring stations with sufficient data (>15 non-zero records). | State-based and seafood-industry HAB monitoring programs (NSW Food Authority, Hornsby Council, TAS EPA, and others); data stored as internal CSVs within the `planktonr` package source (`data-raw/HAB_data_temp/HAB_data.csv`, `HAB_Samples.csv`, `HAB_Sites.csv`); accessed via `planktonr::pr_get_Indices(Survey = "HAB", Type = "Phytoplankton", Subset = "Genus")`. AODN hosting is not yet available. |
| `datHABs` | Time series of phytoplankton species-level abundance indices at HAB monitoring stations with sufficient data. | Same HAB monitoring program data as `datHABg`; accessed via `planktonr::pr_get_Indices(Survey = "HAB", Type = "Phytoplankton", Subset = "Species")`. |

---

## Functional Group Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `NRSfgz` | Time series of zooplankton higher taxonomic group (HTG) abundances at NRS stations, aggregated into functional groups (copepods, appendicularians, molluscs, cladocerans, chaetognaths, thaliaceans, other). | IMOS AODN GeoServer WFS layer `imos:bgc_zooplankton_abundance_htg_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_zooplankton_abundance_htg_data`; accessed via `planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton")`. |
| `NRSfgp` | Time series of phytoplankton higher taxonomic group abundances at NRS stations, aggregated into functional groups (centric diatoms, pennate diatoms, dinoflagellates, cyanobacteria, other). | IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_htg_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_phytoplankton_abundance_htg_data`; accessed via `planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton")`. |
| `SOTSfgp` | Time series of phytoplankton functional group abundances at the SOTS station, derived from NRS phytoplankton HTG data filtered to SOTS samples. | Same IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_htg_data` as `NRSfgp`; accessed via `planktonr::pr_get_FuncGroups(Survey = "SOTS", Type = "Phytoplankton")`. |
| `CPRfgz` | Time series of zooplankton functional group abundances aggregated by CPR bioregion. | IMOS AODN GeoServer WFS layer `imos:cpr_zooplankton_abundance_htg_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_zooplankton_abundance_htg_data`; accessed via `planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250)`. |
| `CPRfgp` | Time series of phytoplankton functional group abundances aggregated by CPR bioregion. | IMOS AODN GeoServer WFS layer `imos:cpr_phytoplankton_abundance_htg_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_phytoplankton_abundance_htg_data`; accessed via `planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton", near_dist_km = 250)`. |

---

## BGC (Biogeochemical) Environmental Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `Nuts` | Depth-resolved water chemistry time series at NRS stations including macronutrients (nitrate, nitrite, ammonium, phosphate, silicate), dissolved oxygen, salinity, and derived indices (NOx, DIN, Redfield ratio). | IMOS AODN GeoServer WFS layer `imos:bgc_chemistry_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_chemistry_data`; accessed via `planktonr::pr_get_data(Survey = "NRS", Type = "Chemistry")`. |
| `Pigs` | Depth-resolved HPLC photosynthetic pigment concentrations at NRS stations, binned into functional pigment classes (total chlorophyll a, photoprotective carotenoids, photosynthetic carotenoids, etc.). | IMOS AODN GeoServer WFS layer `imos:bgc_pigments_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_pigments_data`; accessed via `planktonr::pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")`. |
| `Pico` | Depth-resolved flow cytometry cell counts of picophytoplankton groups (Prochlorococcus, Synechococcus, picoeukaryotes) at NRS stations. | IMOS AODN GeoServer WFS layer `imos:bgc_picoplankton_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_picoplankton_data`; accessed via `planktonr::pr_get_data(Survey = "NRS", Type = "Pico")`. |
| `ctd` | Depth-binned CTD profiles (salinity and temperature) at NRS stations, filtered to depths matching microbial sample depths. | IMOS AODN GeoServer WFS layer `imos:nrs_depth_binned_ctd_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:nrs_depth_binned_ctd_data`; accessed via `planktonr::pr_get_data(Survey = "NRS", Type = "CTD")`. |
| `CSChem` | Surface water chemistry (chlorophyll a, temperature, salinity) at coastal microbial monitoring sites. | Australian Microbiome Initiative ocean visualisation dataset; downloaded directly from: `https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv`; accessed via `planktonr::pr_get_data(Survey = "Coastal", Type = "Chemistry")`. |

---

## Microbial Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `datNRSm` | Long-format time series of microbial community indices (bacterial, archaeal, and eukaryote diversity, salinity/nutrient/temperature niche indices, picophytoplankton counts, and Trichodesmium abundance) at NRS stations. | Two sources combined: (1) Australian Microbiome Initiative ocean visualisation dataset: `https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv`, joined to NRS trip metadata from IMOS S3 `bgc_trip.csv`; (2) Trichodesmium abundance from IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_genus_data`. Accessed via `planktonr::pr_get_data(Survey = "NRS", Type = "Microbes")`. |
| `datCSm` | Long-format time series of microbial community indices at coastal monitoring sites (same variables as `datNRSm`). | Australian Microbiome Initiative ocean visualisation dataset: `https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv`; accessed via `planktonr::pr_get_data(Survey = "Coastal", Type = "Microbes")`. |
| `datGSm` | Long-format microbial community indices from GO-SHIP (Global Ocean Ship-based Hydrographic Investigations Program) transect samples. | Australian Microbiome Initiative ocean visualisation dataset: `https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv`, filtered to GO-SHIP sample IDs (rows 34369–34678 and 36916–37650 of the `code` column); accessed via `planktonr::pr_get_data(Survey = "GO-SHIP", Type = "Microbes")`. |

---

## Policy / Essential Ocean Variables (EOVs)

| Dataset | Description | Source |
|---------|-------------|--------|
| `PolNRS` | Standardised anomaly time series of key Essential Ocean Variables (EOVs) at NRS stations: zooplankton biomass, phytoplankton carbon biomass, temperature, copepod diversity, phytoplankton diversity, salinity, chlorophyll a, and macronutrients. | IMOS S3 bucket: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/nrs_derived_indices_data.csv`; accessed via `planktonr::pr_get_EOVs(Survey = "NRS")`. |
| `PolCPR` | Standardised anomaly time series of EOVs by CPR bioregion: zooplankton biomass index, phytoplankton carbon biomass, copepod diversity, phytoplankton diversity, SST, and satellite chlorophyll. | Two sources: (1) IMOS AODN GeoServer WFS layer `imos:cpr_derived_indices_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_derived_indices_data`; (2) satellite SST and chlorophyll matched to CPR sample locations, stored in the `planktonr` package at `inst/extdata/CPR_SatData.csv`. Accessed via `planktonr::pr_get_EOVs(Survey = "CPR", near_dist_km = 250)`. |
| `PolLTM` | Standardised anomaly time series of long-term macronutrient and physical water-column variables at Maria Island, Rottnest Island, and Port Hacking, extending records back to the 1940s. | Three sources combined: (1) IMOS Long-Term Monitoring (LTM) dataset bundled with the `planktonr` package at `inst/extdata/LongTermMonitoringData.csv` — this contains historical nutrient and hydrographic data from the CSIRO Coastal Monitoring Programme (1942–present) at Port Hacking (CS-PHB), Rottnest Island (CS-ROT), and Maria Island (CS-MAI); (2) NRS chemistry data from IMOS AODN GeoServer WFS layer `imos:bgc_chemistry_data`; (3) NRS CTD data from IMOS AODN GeoServer WFS layer `imos:nrs_depth_binned_ctd_data`. Accessed via `planktonr::pr_get_EOVs(Survey = "LTM")`. |
| `PolSOTS` | Standardised anomaly time series of EOVs at the SOTS mooring site: phytoplankton indices, physical mooring data (temperature, salinity, chlorophyll fluorescence), and nutrient data. | Three sources combined: (1) Phytoplankton indices from IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_raw_data` (filtered to SOTS); (2) Physical mooring data (temperature, salinity, chlorophyll) from IMOS AODN THREDDS server via OPeNDAP — catalog at `https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/derived_products/gridded/catalog.xml`, files accessed via `https://thredds.aodn.org.au/thredds/dodsC/IMOS/DWM/SOTS/derived_products/gridded/`; (3) Nutrient data (from SOTS Remote Access Sampler, RAS) from IMOS AODN THREDDS server — catalogs at `https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/{year}/catalog.xml` (RAS files, 1997–present). Accessed via `planktonr::pr_get_EOVs(Survey = "SOTS")`. |

---

## Mooring Climatology Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `MooringTS` | Day-of-year temperature climatology time series at three depth levels (surface, mean mixed layer depth, and maximum sampling depth) for each NRS station, derived from long-term mooring records. | IMOS NRS Long-Term Mooring Temperature Series (LMTS) climatology NetCDF files stored locally at [`data-raw/LMTS/`](data-raw/LMTS/) (e.g., `NRSNSI_CLIM.nc`, `NRSMAI_CLIM.nc`, `PH100_CLIM.nc`). These are IMOS-produced climatology products from the NRS mooring time series. Not yet accessible directly from AODN; files are stored locally pending public hosting. |
| `MooringClim` | Full depth × day-of-year temperature climatology grids for each NRS station, used to render depth-time contour climatology plots. | Same IMOS NRS LMTS climatology NetCDF files as `MooringTS` in [`data-raw/LMTS/`](data-raw/LMTS/). |

---

## Species Distribution Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `fMapDataz` | Presence/absence frequency of zooplankton species across a 0.5° spatial grid by season, combining NRS and CPR survey records, used to render species distribution maps. | Two sources combined: (1) NRS zooplankton species data from IMOS AODN GeoServer WFS layers `imos:bgc_zooplankton_abundance_copepods_data` and `imos:bgc_zooplankton_abundance_non_copepods_data`; (2) CPR zooplankton species data from IMOS AODN GeoServer WFS layers `imos:cpr_zooplankton_abundance_copepods_data` and `imos:cpr_zooplankton_abundance_non_copepods_data`. Accessed via `planktonr::pr_get_FreqMap(Type = "Zooplankton")`. |
| `fMapDatap` | Presence/absence frequency of phytoplankton species across a 0.5° spatial grid by season, combining NRS and CPR survey records. | Two sources combined: (1) NRS phytoplankton species data from IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_species_data`; (2) CPR phytoplankton species data from IMOS AODN GeoServer WFS layer `imos:cpr_phytoplankton_abundance_species_data`. Accessed via `planktonr::pr_get_FreqMap(Type = "Phytoplankton")`. |

---

## Species Thermal Index (STI) Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `stiz` | Copepod species abundance paired with satellite sea surface temperature (SST) from both NRS and CPR surveys, used to calculate Species Thermal Indices (STI) and Community Temperature Indices (CTI). | Four sources combined: (1) NRS copepod data from IMOS AODN GeoServer WFS layers `imos:bgc_zooplankton_abundance_copepods_data` and `imos:bgc_zooplankton_abundance_non_copepods_data`; (2) CPR copepod data from IMOS AODN GeoServer WFS layers `imos:cpr_zooplankton_abundance_copepods_data` and `imos:cpr_zooplankton_abundance_non_copepods_data`; (3) NRS satellite SST from `planktonr` package file `inst/extdata/NRS_SatData.csv`; (4) CPR satellite SST from `planktonr` package file `inst/extdata/CPR_SatData.csv`. Accessed via `planktonr::pr_get_STIdata(Type = "Zooplankton")`. |
| `stip` | Phytoplankton species abundance paired with satellite SST from both NRS and CPR surveys, used to calculate phytoplankton STI and CTI. | Four sources combined: (1) NRS phytoplankton species data from IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_species_data`; (2) CPR phytoplankton species data from IMOS AODN GeoServer WFS layer `imos:cpr_phytoplankton_abundance_species_data`; (3) NRS satellite SST from `inst/extdata/NRS_SatData.csv`; (4) CPR satellite SST from `inst/extdata/CPR_SatData.csv`. Accessed via `planktonr::pr_get_STIdata(Type = "Phytoplankton")`. |

---

## Day/Night Abundance Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `daynightz` | Monthly mean copepod species abundances from CPR samples, split by day vs. night sampling time (determined from sunrise/sunset calculations at each sample location), used to assess diel vertical migration. | CPR copepod data from IMOS AODN GeoServer WFS layers `imos:cpr_zooplankton_abundance_copepods_data` and `imos:cpr_zooplankton_abundance_non_copepods_data`; day/night classification computed using the `suncalc` R package applied to sample UTC time and coordinates. Accessed via `planktonr::pr_get_DayNight(Type = "Zooplankton")`. |
| `daynightp` | Monthly mean phytoplankton species abundances from CPR samples, split by day vs. night sampling time. | CPR phytoplankton species data from IMOS AODN GeoServer WFS layer `imos:cpr_phytoplankton_abundance_species_data`; day/night classification computed using the `suncalc` R package. Accessed via `planktonr::pr_get_DayNight(Type = "Phytoplankton")`. |

---

## Taxa Accumulation Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `PSpNRSAccum` | Cumulative phytoplankton species discovery curve for the NRS survey — date of first observation for each species in chronological order. | IMOS AODN GeoServer WFS layer `imos:bgc_phytoplankton_abundance_species_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_phytoplankton_abundance_species_data`; accessed via `planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "Phytoplankton")`. |
| `PSpCPRAccum` | Cumulative phytoplankton species discovery curve for the CPR survey. | IMOS AODN GeoServer WFS layer `imos:cpr_phytoplankton_abundance_species_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:cpr_phytoplankton_abundance_species_data`; accessed via `planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "Phytoplankton")`. |
| `ZSpNRSAccum` | Cumulative zooplankton species discovery curve for the NRS survey. | IMOS AODN GeoServer WFS layers `imos:bgc_zooplankton_abundance_copepods_data` and `imos:bgc_zooplankton_abundance_non_copepods_data`; accessed via `planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "Zooplankton")`. |
| `ZSpCPRAccum` | Cumulative zooplankton species discovery curve for the CPR survey. | IMOS AODN GeoServer WFS layers `imos:cpr_zooplankton_abundance_copepods_data` and `imos:cpr_zooplankton_abundance_non_copepods_data`; accessed via `planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "Zooplankton")`. |

---

## Species Information / Trait Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `SpInfoP` | Taxonomic and trait reference table for phytoplankton species including functional group, cell biovolume, carbon content, WoRMS AphiaID, and HAB status. | IMOS S3 bucket: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/phytoinfo.csv`; accessed via `planktonr::pr_get_info(Source = "Phytoplankton")`. |
| `SpInfoZ` | Taxonomic and trait reference table for zooplankton species including functional group, diet, mean body length, and WoRMS AphiaID. | IMOS S3 bucket: `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/zoopinfo.csv`; accessed via `planktonr::pr_get_info(Source = "Zooplankton")`. |

---

## Larval Fish Data

| Dataset | Description | Source |
|---------|-------------|--------|
| `LFData` | Records of larval fish catches (count > 0) from vertical net tows at NRS stations, including species (family-level WoRMS codes), abundance standardised to 1000 m³, environmental data (temperature, salinity), and gear metadata (mesh size, tow type, volume filtered). | IMOS AODN GeoServer WFS layer `imos:bgc_larval_fish_count_raw_data`: `http://geoserver-123.aodn.org.au/geoserver/ows?...&typeName=imos:bgc_larval_fish_count_raw_data`; accessed via `planktonr::pr_get_LFData()`. |
| `LFDataAbs` | Unique sampling events (one row per tow) from the larval fish dataset, used to display sampling effort independent of species composition. | Same IMOS AODN GeoServer WFS layer `imos:bgc_larval_fish_count_raw_data` as `LFData`, deduplicated by location and time. |

---

## Pre-computed UI Choice Lists

| Dataset | Description | Source |
|---------|-------------|--------|
| `choicespNRSp` | Named character vector of formatted parameter labels for NRS phytoplankton indices, used to populate sidebar dropdown menus. | Derived in-app from `datNRSp_all` via `planktonr:::pr_relabel()`; no external download. |
| `choicespNRSz` | Named character vector of formatted parameter labels for NRS zooplankton indices. | Derived in-app from `datNRSz` via `planktonr:::pr_relabel()`; no external download. |
| `choicespNRSm` | Named character vector of formatted parameter labels for NRS microbial indices. | Derived in-app from `datNRSm` via `planktonr:::pr_relabel()`; no external download. |
| `choicespCPRp` | Named character vector of formatted parameter labels for CPR phytoplankton indices. | Derived in-app from `datCPRp` via `planktonr:::pr_relabel()`; no external download. |
| `choicespCPRz` | Named character vector of formatted parameter labels for CPR zooplankton indices. | Derived in-app from `datCPRz` via `planktonr:::pr_relabel()`; no external download. |
| `choicespCSm` | Named character vector of formatted parameter labels for coastal microbial indices. | Derived in-app from `datCSm` via `planktonr:::pr_relabel()`; no external download. |
| `choicespHAB` | Named character vector of formatted parameter labels for HAB phytoplankton indices (abundance, biovolume, carbon biomass, species richness). | Hardcoded set of four HAB parameters, formatted via `planktonr:::pr_relabel()`; no external download. |

---

## Animal Tracking Data

All animal tracking datasets are derived from the IMOS Animal Tracking Facility (ATF), which is part of the Australian Animal Tracking and Monitoring System (AATAMS). Acoustic detection data are downloaded from the IMOS AODN public S3 bucket and receiver deployment data from the IMOS Animal Tracking portal API. Data are pre-processed by [`data-raw/AnimalTracking/AT_DataProcessing.R`](data-raw/AnimalTracking/AT_DataProcessing.R) and saved as intermediate RDS files in [`data-raw/AnimalTracking/Output/`](data-raw/AnimalTracking/Output/).

| Dataset | Description | Source |
|---------|-------------|--------|
| `AT_receivers` | Spatial point dataset (sf object) of acoustic receiver installation locations enriched with detection summary statistics (number of species, individuals, and total detections), including popup HTML for interactive maps. | IMOS ATF receiver deployment API: `https://animaltracking.aodn.org.au/api/receiver/deployment/deployments.zip`; saved as [`data-raw/AnimalTracking/Output/ReceiverSummary.rds`](data-raw/AnimalTracking/Output/ReceiverSummary.rds). |
| `AT_animal_summary` | Per-animal, per-installation, per-day summary of acoustic detections including species identity, sex, body length, and total detection counts. | IMOS AODN public S3 bucket: `https://data.aodn.org.au/IMOS/AATAMS/acoustic_detections_QC_summary/` (file listing via `https://data.aodn.org.au/?prefix=IMOS/AATAMS/acoustic_detections_QC_summary/`); saved as [`data-raw/AnimalTracking/Output/AnimalSummary.rds`](data-raw/AnimalTracking/Output/AnimalSummary.rds). |
| `AT_species_summary` | Monthly detection totals aggregated by installation, species (common and scientific name), and WoRMS AphiaID. | Derived from `AT_animal_summary` (same IMOS AATAMS S3 source); saved as [`data-raw/AnimalTracking/Output/SpeciesSummary.rds`](data-raw/AnimalTracking/Output/SpeciesSummary.rds). |
| `AT_station_species` | Per-installation breakdown of species detected, including number of unique individuals and total detections, used to populate station detail tables. | Derived in-app from `AT_animal_summary` (same IMOS AATAMS S3 source); no additional download. |
| `AT_individual_data` | One row per unique animal per installation, recording species, sex, and body length measurement, used for individual-level summaries. | Derived in-app from `AT_animal_summary` (same IMOS AATAMS S3 source); no additional download. |
| `AT_all_species` | Sorted character vector of all unique species common names detected across all installations, used to populate the species filter dropdown. | Derived in-app from `AT_animal_summary` (same IMOS AATAMS S3 source); no additional download. |
| `AT_daily_summary` | Daily time series of unique individual counts and total detections per installation and species, used for time-series plots in the Animal Tracking module. | Derived in-app from `AT_animal_summary` (same IMOS AATAMS S3 source); no additional download. |

---

## Summary of External Data Sources

| Source | URL | Datasets |
|--------|-----|----------|
| IMOS AODN GeoServer WFS | `http://geoserver-123.aodn.org.au/geoserver/ows` | `Nuts`, `Pigs`, `Pico`, `ctd`, `SOTSp`, `NRSfgz`, `NRSfgp`, `SOTSfgp`, `CPRfgz`, `CPRfgp`, `datCPRz`, `datCPRp`, `PCI`, `PolCPR`, `fMapDataz`, `fMapDatap`, `stiz`, `stip`, `daynightz`, `daynightp`, `PSpNRSAccum`, `PSpCPRAccum`, `ZSpNRSAccum`, `ZSpCPRAccum`, `LFData`, `LFDataAbs`, `datNRSm` (Trichodesmium), `CPRinfo`, `datCPRTrip`, `PMapData` (CPR) |
| IMOS AODN S3 bucket | `https://s3-ap-southeast-2.amazonaws.com/imos-data/IMOS/BGC_DB/harvested_from_CSIRO/` | `datNRSTrip`, `datNRSz`, `datNRSp`, `datNRSw`, `PolNRS`, `SpInfoP`, `SpInfoZ`, `PMapData` (NRS) |
| IMOS AODN THREDDS (OPeNDAP) | `https://thredds.aodn.org.au/thredds/` | `PolSOTS` (physical mooring + nutrients) |
| IMOS ATF receiver API | `https://animaltracking.aodn.org.au/api/receiver/deployment/deployments.zip` | `AT_receivers` |
| IMOS AATAMS S3 | `https://data.aodn.org.au/IMOS/AATAMS/acoustic_detections_QC_summary/` | `AT_animal_summary`, `AT_species_summary`, `AT_station_species`, `AT_individual_data`, `AT_all_species`, `AT_daily_summary` |
| Australian Microbiome Initiative (GitHub) | `https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv` | `datNRSm`, `datCSm`, `datGSm`, `CSChem` |
| AAD / Australian Antarctic Data Centre | `https://data.aad.gov.au/metadata/records/AADC-00099` | `datCPRTripSO`, `datCPRz` (Southern Ocean component) |
| `planktonr` package bundled files (`inst/extdata/`) | Shipped with package; no URL | `NRSStation`, `SotsStation`, `NRSinfo`, `SOTSinfo`, `PolLTM` (LTM component), `stiz`, `stip`, `PolCPR` (satellite component) |
| HAB monitoring programs (local CSVs in `planktonr`) | Not yet on AODN; stored in `data-raw/HAB_data_temp/` within `planktonr` | `datHABg`, `datHABs`, `datHABTrip`, `datHABdataTable` |
| IMOS NRS LMTS mooring NetCDF (local) | Stored in [`data-raw/LMTS/`](data-raw/LMTS/); not yet on AODN | `MooringTS`, `MooringClim` |
| Local / in-app derived | No download | `col12`, `modified_time`, `ParamDef`, `AusStatesSimple`, `imcra`, `datNRSp_all`, `choicespNRSp`, `choicespNRSz`, `choicespNRSm`, `choicespCPRp`, `choicespCPRz`, `choicespCSm`, `choicespHAB`, `AT_station_species`, `AT_individual_data`, `AT_all_species`, `AT_daily_summary` |