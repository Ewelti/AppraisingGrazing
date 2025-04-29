## AppraisingGrazing
This repository contains data and code relating to the study:

### **Herbivore effects on plant quality and quantity in a shortgrass prairie** 

#### Authors:
Rehb, J. & E.A.R. Welti

#### Study investigation of:
bison, cattle, prairie dog, and insecticide effects on plant biomass and plant and soil chemistry in an eastern Montana shortgrass prairie during the summer of 2022


## rawdata file:

#### ***AP_propertySizes_grazeDens.csv*** :
* Contains project data including Metadata on site properties and grazer densities

#### ***ClipBiomass.csv*** :
* Contains data on g/m^2 dry biomass of grasses, forbs, and woody plants

#### ***PastureMeter.csv*** :
* Contains data on pasture meter measurements and conversions to g/m^2 dry plant biomass

#### ***PastureMeterCalibration.xlsx*** :
* Contains regression equation calculation to convert pasture meter measurements into units of dry plant biomass

#### ***PD_PooCount.csv*** :
* Contains data on counts of prairie dog dungs/m^2

#### ***PlantChem.csv*** :
* Contains data on chemistry of plant (grass, forb, woody when available) including %C, %N, ppm Al, ppm B, ppm Ca, ppm Cu, ppm Fe, ppm K, ppm Li, ppm Mg, ppm Mn, ppm Na, ppm Ni, ppm P, ppm 	Pb, ppm S, ppm Si, ppm Sr, ppm Ti, ppm V, & ppm Zn

#### ***Rainfall.csv*** :
* Contains data on rainfall in mm collected across the growing season in a local rain gauge

#### ***RingCounts.csv*** :
* Contains data on counts of grasshoppers per grasshopper ring (/0.2m^2)

#### ***SiteCorners.csv*** :
* Contains data on latitude and longitude of site boundaries

#### ***SoilChem.csv*** :
* Contains data on site soil elemental chemistry including %C, %N, ppm Al, ppm Ca, ppm Co, ppm Fe, ppm K, ppm Mg, ppm Mn, ppm Na, ppm P, ppm S, & ppm Zn

#### ***stocking.csv*** :
* Contains data on bison and cattle stock rates, and presence of prairie dogs and insecticide use per site


## outputs file:

#### ***ElementalResponses_grazingGradient.csv*** :
* statistical results for responses of plant and soil chemistry responses to herbivore densities and month

#### ***ElementalResponses_grazingTrt.csv*** :
* statistical results for responses of plant and soil chemistry responses to herbivore regime and month

#### ***ForbNutrients_ResponsesToMonth_WithinSites.csv*** :
* statistical results for responses of forb chemistry responses to month

#### ***GrassNutrients_ResponsesToMonth_WithinSites.csv*** :
* statistical results for responses of grass chemistry responses to month

#### ***SiteMonthLevel.csv*** :
* all data from raw data folder summarized at the level of each site and month; created by R/calculate_estimates.R

#### ***SoilNutrients_ResponsesToMonth_WithinSites.csv*** :
* statistical results for responses of grass chemistry responses to month

#### ***SupplementalTables.xlsx*** :
* statistical results including all paper's Supplemental Tables for models testing herbivore effects on plant quality, quantity, and soil elemental chemistry


## R file:

#### ***calculate_estimates.R*** :
* R script containing code to summarize data from the rawdata folder into a combined file with all data provided as one value for each site and month. The result of running this script is provided in outputs/SiteMonthLevel.csv

#### ***DriverModels_H1_lmm_GrazingGradient.R*** :
* R script containing code for models examining effects of herbivore densities to test H1: Grazing will increase plant quality with smaller herbivores having larger effects

#### ***DriverModels_H1_lmm_GrazingTrt.R*** :
* R script containing code for models examining effects of herbivore regimes to test H1: Grazing will increase plant quality with smaller herbivores having larger effects

#### ***DriverModels_H2_GrazingGradient.R*** :
* R script containing code for models examining effects of herbivore densities to test H2: All grazers will decrease aboveground plant biomass, and insect herbivory will equal that of large grazer herbivory

#### ***DriverModels_H2_GrazingTrt.R*** :
* R script containing code for models examining effects of herbivore regimes to test H2: All grazers will decrease aboveground plant biomass, and insect herbivory will equal that of large grazer herbivory

#### ***DriverModels_H3_abovegroundbiomass.R*** :
* R script containing code for models to test plant biomass component of H3: Plant nutrient content will be greatest in the early season while biomass will peak mid-season

#### ***DriverModels_H3_nutrients.R*** :
* R script containing code for models to test plant nutrient component of H3: Plant nutrient content will be greatest in the early season while biomass will peak mid-season

#### ***GhopRings.R*** :
* R script containing code to summarize data of grasshopper densities from ring counts and examine their variation over grazing regimes

#### ***PDtown_ttests.R*** :
* R script containing code to conduct t tests to examine prairie dog effects on soil within properties as a robustness analysis


## plots file:

#### ***ForbChem_individualPlots.tif*** :
*main results figure (Fig. 2) showing responses of forb concentrations of %N, ppm P, ppm K, ppm Mg, ppm Na, and ppm Si to different grazing regimes and over the four sampling months (June-September)

#### ***ForbChem_plots.R*** :
*R script to make the figure ForbChem_individualPlots.tif

#### ***GrassChem_individualPlots.tif*** :
*main results figure (Fig. 1) showing responses of grass concentrations of %N, ppm P, ppm K, ppm Mg, ppm Na, and ppm Si to different grazing regimes and over the four sampling months (June-September)

#### ***GrassChem_plots.R*** :
*R script to make the figure GrassChem_individualPlots.tif

#### ***PastureMeter.tif*** :
*main results figure (Fig. 4) showing responses of aboveground plant biomass to different grazing regimes and over the four sampling months (June-September)

#### ***PastureMeterEsts_Plot.R*** :
*R script to make the figure PastureMeter.tif

#### ***sites2022_Rebh.tiff*** :
*site map

#### ***SoilChem_individualPlots.tif*** :
*main results figure (Fig. 4) showing responses of soil concentrations of %C, %N, ppm P, ppm K, ppm Mg, and ppm Na to different grazing regimes and over the four sampling months (June-September)

#### ***SoilChem_plots.R*** :
*R script to make the figure SoilChem_individualPlots.tif