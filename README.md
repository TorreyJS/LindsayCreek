Data & code for: Microbial and biogeochemical responses to stream water fecal and nutrient contamination
Torrey Stephenson, University of Idaho Department of Soil and Water Systems, Moscow, ID
Josie Brown, University of Idaho Department of Soil and Water Systems, Moscow, ID
Jason Williams, The Cadmus Group LLC, Waltham, MA
Jane Lucas, Cary Institute of Ecosystem Studies, Millbrook, NY
Sujata Connell, Idaho Department of Environmental Quality, Lewiston, ID
David McIntyre, Idaho Department of Environmental Quality, Lewiston, ID
Alan Kolok, University of Idaho Department of Fish and Wildlife Sciences, Moscow, ID
Jenna Fortier, Idaho Department of Environmental Quality, Lewiston, ID
Laurel Lynch, University of Idaho Department of Soil and Water Systems, Moscow, ID

Description of the data
Grab samples were collected monthly from six sites along Lindsay Creek from May to October 2022 (n = 36 samples) to capture seasonal variation in hydrology, biogeochemistry, and potential sources of nitrate and fecal contamination. Four sites were located on the main stem (Sites 1, 2, 4, and 6) and two on small tributaries (Sites 3 and 5) to capture multiple land use types.
Files and variables
File: LC_site_coords.csv
Description: Coordinates and elevation for the sample sites.
Variables
•	site_abbrev: site name for lab use only
•	site_num: site number for use in manuscript (1 = headwaters, 6 = mouth at Clearwater River)
•	latitude: 
•	longitude:
•	elev_ft: elevation of site in feet
•	elev_m: elevation of site in meters
File: LC_FullData.xlsx
Description: 
Variables
•	month: month sample was collected, from May (5) to October (10)
•	position: equivalent to site_num. (1 = headwaters, 6 = mouth at Clearwater River)
•	season: categorical variable for lab use only
•	location: categorical variable for lab use only
•	discharge: Units = feet per second. estimated using an FH950 Handheld Flow Meter (Hach, CO, USA)
•	turbidity: Units = FNU. Measured using an EXO1 Multiparameter Sonde (YSI, Inc., Ohio, USA)
•	temp: water temperature. Units = degrees Celsius. Measured using an EXO1 Multiparameter Sonde (YSI, Inc., Ohio, USA)
•	DOsat: dissolved oxygen saturation. Units = percent. Measured using an HQ40d Multi-Parameter Meter
•	cond: specific conductivity. Units = µS cm^-1^. Measured using an EXO1 Multiparameter Sonde (YSI, Inc., Ohio, USA)
•	ph: pH of water sample
•	DOC: Dissolved organic carbon. Units = μg L^-1^. Quantified using a TOC-L TNM-L analyzer.
•	TN: Total dissolved nitrogen. Units = μg L^-1^. Quantified using a TOC-L TNM-L analyzer.
•	NO3: Nitrate. Units = mg L^-1^. Quantified using a SpectraMax M2 Microplate Reader (Molecular Devices LLC, CA, USA) with vanadium reduction method.
•	PO4: Phosphate. Units = mg L^-1^. Quantified using a SpectraMax M2 Microplate Reader (Molecular Devices LLC, CA, USA) with AMP-Malachite Green method.
•	coliform: Units = most probable number (MPN). Estimated using the Colilert®-18 test and Quanti-Tray/2000 system (IDEXX Laboratories Inc., Maine, USA)
•	Ecoli: Units = most probable number (MPN). Estimated using the Colilert®-18 test and Quanti-Tray/2000 system (IDEXX Laboratories Inc., Maine, USA)
•	Actinobacteria (column R) through Verrucomicrobiota (column AB): relative abundance of bacterial phyla in decimal format.
•	rk_ratio: ratio of r-selected () to K-selected bacterial phyla. No units.
•	HF183: number of copies of HF183 gene specific to human digestive tract (human fecal marker). Blank cells = no data (treated as 0).
•	CowM2: number of copies of CowM2 gene specific to bovine digestive tract (bovine fecal marker). Blank cells = no data (treated as 0).
•	site_id: for within-lab use only
•	year: year of sample collection
•	precip_mm: Units = mm. Monthly precipitation total, obtained from 4-km resolution PRISM data.
•	mean_temp: Units = degrees Celsius. Average air temperature, obtained from 4-km resolution PRISM data.
•	ShanDiv: Shannon Diversity of bacterial community. 
Access information
Other publicly accessible locations of the data:
•	https://github.com/TorreyJS/LindsayCreek

