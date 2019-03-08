# Gates-Mydlarz-bleaching-recovery
<a href="https://doi.org/10.5281/zenodo.1175034"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.1175034.svg" alt="DOI"></a>  

Wall CB, Ricci CA, Foulds GE, Mydlarz LD, Gates RD, Putnam HM. 2018. The effects of environmental history and thermal stress on coral physiology and immunity. _Marine Biology_ 165:56-71

This project examines the response of coral physiology and immunity to repeated bleaching and subsequent recovery during the 2014 - 2016 global bleaching events in Hawai'i. The coral Montipira capitata was collected from two reef locations within Kāne'ohe Bay, O'ahu, Hawai'i: Lilipuna Reef and Reef 14 in southern and central Kāne'ohe Bay, respectively. immunity.Marine Biology165:56-71

### Folder: gitR_analysis
Within this folder is a directory of...
- data
- output
- scripts

#### data: 
  - Gates_Mydlarz_20142016_ALL_DATA.csv
      - the master datasheet of physiology, immunity, color scores, and Symbiodinium genotypes
  - Gates_Mydlarz_20142016_physimmun.csv
      - this is just the physiology and immunity data
  - Gates_Mydlarz_20142016_qPCR.csv
      - this is qPCR data including the data from a previous experiment, subset to be included in th 2014-2016 dataset
      
  - environmental
      - this folder has PAR and temperature data collected in situ at the two reefs (Lilipuna and Reef 14)
      - Lilipuna PAR all.csv
      - Lilipuna temp all.csv
      - Reef14 PAR all.csv
      - Reef14 temp all.csv
      - (folder) HIMB station -- this has discrete periods of temperature at HIMB weather station where loggers failed
      
   - qPCR
      - this folder has the 12 raw .txt files used in script to determine symbiont community
     
   - Mydlarz_bleach recover.proj -- R project for R studio
   - Mydlarz_bleach recover.Rmd -- an R studio Rmarkdown file
      - R-markdown for figure generation and  analysis of all responses
      
#### output: 
  - figures and products from analyses

#### scripts:
  - Environ_data_Mydlarz.R
      - R script to make figures for environmental data
  - qPCR_Mydlarz-2014-2016.R 
      - R script to run qPCR plates, linked to R. Cunning github



