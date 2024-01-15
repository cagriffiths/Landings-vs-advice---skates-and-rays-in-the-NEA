# Landings vs advice - skates and rays in the Northeast Atlantic
This repo contains all the data and code needed to reproduce the analysis and figures from Batsleer et al. 2024. 

The repo contains 2 primary folders:

*Data* - contains 5 .csv files:

(1) `landings_2009-2022` - ICES estimated landings (in tonnes) by stock, fishing area and year. 

(2) `advice` - ICES advice (in tonnes) by stock and year.  

(3) `life_history_traits` - species-specific life history traits (Linf, Lmat and fecundity), listed in Table 2 in the main MS. 

(4) `landings_vs_advice` - final dataset used in the analysis, product of running the R script `collate_landings_and_advice`. 

(5) `TAC_2009-2022` - lists TAC values (in tonnes) by ICES area and year. 

*Code* - contains 3 files:

(1) `collate_landings_and_advice` - code to collate and compare ICES landings and ICES advice. Also includes the calculation to split ICES advice based on landing proportions by stock and year.  

(2) `generate_figures` - code to generate Figures 1-3 from the main MS as well as Figure S1. 

(3) `generate_tables` - code to generate SI tables S1 and S3. 

There is an additional *Output* folder which contains the all the generated datafiles and figures used in the MS and SIs. 

# Raw data
The raw data used in this work are collated as part the ICES Working Group on Elasmobranch Fishes (WGEF - https://www.ices.dk/community/groups/pages/wgef.aspx) and are updated on an annual and biennial basis. The WGEF landings and discards tables can be accessed on request from ICES. Data on ICES scientific advice and TAC values are freely available online and can be accessed in the ICES stock advice library or via the EU Council Regulations, respectively. 

# Citations
Batsleer, J., Griffiths, C.A., Bleeker, K., Johnston, G., Cardinale, M. and Lorance, P. (2024). Comparisons of landings to scientific advice indicates overshooting within the common TAC for skates and rays in the Northeast Atlantic. *ICES Journal of Marine Science*, accepted. 


