# trait-threshold-phyto
Code for trait &amp; threshold based phytoplankton group classification

[![Project Status: Active – The project is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)


<br>

This code is used to analyse phytoplankton taxonomic data for the purposes of creating classified groups. The method is described in the paper by Dang et al (2025):

*Dang, H.V., Stephanie, K., Huang, P., Carey, C.C. and Hipsey, M.R., Phytoplankton Group Classification by Integrating Trait Information and Observed Environmental Thresholds. Available at SSRN 4939909.* (Currently under consideration for publication in Ecological Informatics).

<br>

## Citing this code

[![DOI](https://zenodo.org/badge/XXXXXXX.svg)](https://zenodo.org/doi/10.5281/zenodo.XXXXXXX)

These model input files are citable, and associated with the manuscript by Dang et al, submitted to Ecological Informatics.

When citing a specific model version bundle, please use the appropriate DOI.


## Workflow summary

The code consists of both data preprocessing and analysis, structured into four stages within the Hawkesbury Nepean Phytoplankton Trait Threshold Based Classification Framework (Dang et al., 2025). Stage 1 involves MATLAB and R scripts to prepare data ready for Multi Correlation  Analysis (MCA), Principal Component Analysis (PCA), and Taxa Indicator Threshold Analysis (TITAN). Stage 2 focuses on R scripts for performing MCA and PCA, providing the key environmental variables for TITAN analysis. Stage 3 includes R scripts for conducting TITAN along with generating output and visualizations to interpret the results. Stage 4 involves R code for K-prototype classification used to classify phytoplankton species and evaluate classified groups, and sensitivity analysis for assessing how the classification responds to changes in environmental variables. 
Please refer to the accompanying instruction document on how to apply the framework to analyze the datasets.

[<img src="images/workflow.png" alt="AED" width="100"/>](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4939909)

## Contact

For further information please contact Hoang Dang Vuong @ hoangvuong.dang@research.uwa.edu.au
