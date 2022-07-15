<img src="www/hexSticker_scrm.png" width="120" alt = "sCRM logo" height="135" align = "right">

## Welcome to sCRM!

__sCRM__ is a web-based application designed to interact with the
[`{stochLAB}`](https://www.github.com/HiDef-Aerial-Surveying/stochLAB) package.

Collision Risk Models (CRMs) estimate the number of in-flight collisions of
seabirds with offshore wind turbines, playing a vital role in the assessment of
impacts of offshore wind farms on seabird populations. The original CRM was
developed by [Band
(2012)](https://www.bto.org/sites/default/files/u28/downloads/Projects/Final_Report_SOSS02_Band1ModelGuidance.pdf),
and subsequently extended by [Masden
(2015)](https://data.marine.gov.scot/dataset/developing-avian-collision-risk-model-incorporate-variability-and-uncertainty)
to include stochasticity in the calculations.

__sCRM__ provides a user-friendly interface to specify CRM input parameters for
multiple impact scenarios, to calculate collision risk at different levels of
aggregation (i.e. per annum, by season or month) and to export outputs in
convenient formats.

Furthermore, users can opt between running CRM on a deterministic context (akin
to the original Band model) or under a stochastic approach.

Collision risk estimates are calculated for two types of models:
- Basic model - assumes a uniform distribution of bird flights at collision
risk height
- Extended model - takes into account the distribution of bird flight heights
at collision risk height

These two modelling approaches are then applied to produce estimates under 4 Options:
- Option 1 - Basic model with collision risk height derived from a site survey.
- Option 2 - Basic model with collision risk height obtained from flight height distribution data.
- Option 3 - Extended model using a generic flight height distribution (FHD).
- Option 4 - Extended model using a site-specific FHD.

__sCRM__ produces estimates for Option 1 and Option 2 by default, while users are
required to provide FHD data for Options 3 or/and 4. 

__sCRM__ is purposed to replace the
[stochCRM](https://github.com/dmpstats/stochCRM) tool, offering the following additional
features:
- Multiple wind farm scenarios
- Seasonal outputs
- Deterministic Vs. Stochastic modes
- Option 4 estimates
- Automated report generation
- Faster simulations, thanks to `{stochLAB}`


*__sCRM__ was developed by DMP Statistical Solutions Ltd and HiDef Aerial
Surveying Ltd. This is a BETA version and the app and is currently undergoing
testing.*
