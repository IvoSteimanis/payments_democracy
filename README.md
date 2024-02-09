# Conditional Payments for Democracy to Local Leaders Managing Natural Resources in Rural Namibia
 
This repository contains the data and code that replicates tables and figures for the following paper:

Title: Conditional Payments for Democracy to Local Leaders Managing Natural Resources in Rural Namibia

Authors: IVO STEIMANIS<sup> 1</sup> , ESTHER BLANCO<sup> 2,3,*</sup> & BJÖRN VOLLAN<sup>1</sup> <br>
Affiliations: <sup>1</sup> Sustainable Use of Natural Resources, Universität Marburg, Am Plan 2, 35032 Marburg, Germany.
<sup>2</sup> University of Innsbruck, Department of Public Finance, Universitätsstrasse 15, Innsbruck 6020, Austria.
<sup>3</sup> The Ostrom Workshop, Indiana University, Bloomington, IN 47408, USA.

*Correspondence to: Esther Blanco, University of Innsbruck, Department of Public Finance, Universitätsstrasse 15,
Innsbruck 6020, Austria. Email: esther.blanco@uibk.ac.at
JEL Codes: D7, Q2, Q5, C9.
Keywords: climate change, extreme weather events, natural disasters, adaptation, pro-social behavior, online experiment

# License

The data and code are licensed under a Creative Commons Attribution 4.0 International Public License. See
LICENSE.txt for details.

# Software requirements

All analysis were done in Stata version 16 :

```
‒ Add-on packages are included in libraries and do not need to be installed by user. The names, installation
sources, and installation dates of these packages are available in scripts/libraries/stata.trk.
```
# Instructions

1. Save the folder ‘replication_package’ to your local drive.
2. Open the master script ‘run.do **’** and change the global pointing to the working direction (line 20) to the location
    where you save the folder on your local drive.
3. Run the master script ‘run **.do’** to replicate the analysis and generate all tables and figures reported in the paper
    and supplementary online materials.

# Dataset

```
‒ The experimental and survey data are stored in data/leader_raw.dta for leaders and data/villagers_raw.dta for
villagers.
‒ The Round 6 Afrobarometer data is stored in data/afro_r6.dta
```
# Descriptions of scripts

scripts/ 01 _cleaning.do

This script processes the raw experimental and survey data from all participants and prepares it for analysis.

scripts/0 2 _generate.do

This script creates additional variables used in the analysis.

scripts/ 03 _generate.do

This script creates figures and tables shown in the paper and saves them to results/Figures and results/Tables


