# Repeat bleaching effects on coral physiotypes 
This project examines the response of coral physiology and immunity to repeated bleaching and subsequent recovery during repeat bleaching events in 2014 and 2015 in Hawai'i. The coral *Montipira capitata* was collected from two reef locations within Kāne'ohe Bay, O'ahu, Hawai'i: Lilipuna Reef and Reef 14 in southern and central Kāne'ohe Bay, respectively (reefs previously described in, Wall et al. (2018) Marine Biology 165:56-71).


### Folders
- data
- figures
- manuscript
- output

#### R Markdown files
 - Mydlarz_bleach recover.proj -- R project for R studio
 - Mydlarz_bleach recover.Rmd -- an R studio Rmarkdown file
     - R-markdown for figure generation and analysis of all responses
 - Mydlarz_bleach-recover.html
     - html output from data analysis in Rmd. Download this file and open in browser to view html (can't open with GitHub).
      
#### data: 
  - Gates_Mydlarz_20142016_ALL_DATA.csv
      - the master datasheet of physiology, immunity, color scores, and Symbiodiniaceae community
  - Gates_Mydlarz_20142016_physimmun.csv
      - this is just the physiology and immunity data
  - Gates_Mydlarz_20142016_qPCR.csv
      - this is qPCR data including the data from a previous experiment, subset to be included in th 2014-2016 dataset
      
  - ecology
      - benthic data from reef surveys 
      
  - environmental
      - this folder has PAR and temperature data collected in situ at the two reefs (Lilipuna and Reef 14)
      - Lilipuna PAR all.csv
      - Lilipuna temp all.csv
      - Reef14 PAR all.csv
      - Reef14 temp all.csv
      - (folder) HIMB station -- this has discrete periods of temperature at HIMB weather station where loggers failed
      
   - qPCR
      - this folder has the 12 raw .txt files used in script to determine symbiont community
     
#### figures
   - exported figures during code execution
   - 'pau published' = final versions formatted for journal
   
#### manuscript
   - Online Supplement.pdf for manuscript 
   - Table 1_Permanova.pdf is formatted table for manuscript
      
#### output: 
  - additional figures and products exported during analyses (primarily in degree heating weeks calculations).


