__Complete transect data (trantab.csv)__: Minimally altered raw transect data for every transect and site (quadrat) along each transect.  

* *Date*: Sample date in YYYY-MM-DD
* *Transect*: Transect name
* *Site*: Quadrat location along each transect, meters
* *Depth*: Site depth, meters. Should be negative values below the water surface.
* *Savspecies*: Relevant seagrass genus, also includes AA (attached algae, multiple types including Caulerpa), DA (drift algae), and no cover
* *SeagrassEdge*: Depth of seagrass edge, cm
* *var*: Measured variable, one of abundance, blade length, or short shoot density
* *aveval*: Average value for the measured variable (multiple quadrats at a site depending on variable), unitless if abundance, cm if blade length, shoots per square meter if short shoot density
* *sdval*: Standard deviation for the measured variable, same units as in aveval

__Summarized frequency occurrence data (tranocctab.csv)__: Summarized data at each transect and date, aggregating information across sites (quadrats).

* *Date*: Sample date in YYYY-MM-DD
* *Transect*: Transect name
* *Savspecies*: Relevant seagrass genus, also includes AA (attached algae), DA (drift algae), and no cover
* *nsites*: Number of sites (quadrats) at each transect/date, repeated across rows  
* *foest*: Frequency occurrence estimate for the whole transect for the species, from 0 - 1
* *bbest*: Abundance (Braun-Blaunquet) average across sites (quadrats) for the species, from 0 - 5
