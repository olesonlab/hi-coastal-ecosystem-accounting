README

This README describes the steps to getting the final raster output

Land Cover in this layer includes-
Class 1. Developed
Class 3. Grass/ Shrubs
Class 4. Tree Cover
Class 6: Estuarine Wetlands
Class 8. Barren

Arrived at the final ouput from first erasing Ag land use (2015/2020), 
then reclassifying remaining cropland in LCMAP as Grass/ Shrub, and Water as NoData
Then I erased branches and dunes using CCAP and wetlands from this layer using the NWI (Freshwater Emergent and Freshwater Forested/ Shrubwetland), whatever remains in the LCMAP that is classified as wetlands are assumed to be Estuarine Wetlands.


* Note validation in Molokaʻi study showed we missed only 3 wetlands- one was designated in NWI as a Freshwater Pond, while another was a Loʻi so was arguably correctly omitted. Which meant only one site was essentially completely missed with our datasets 

* Freshwater wetlands class is a separate vector layer (from NWI)
* Beaches and dunes is a separate raster layer from (CCAP)- class 19 (unconsolidated shore).


