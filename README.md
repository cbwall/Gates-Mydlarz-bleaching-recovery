# Repeat bleaching effects on coral physiotypes 
This project examines the response of coral physiology and immunity to repeated bleaching and subsequent recovery during repeat bleaching events in 2014 and 2015 in Hawai'i. The coral *Montipira capitata* was collected from two reef locations within Kāne'ohe Bay, O'ahu, Hawai'i: Lilipuna and Reef 14 in southern and central Kāne'ohe Bay, respectively (reefs previously described in, Wall et al. (2018) Marine Biology 165:56-71).


### Folders
- data
- figures
- output
- tables

#### R Markdown files 
 - Mydlarz_bleach recover.proj -- R project for R studio
 - Mydlarz_bleach recover.Rmd -- an R studio Rmarkdown file  
     - R-markdown for figure generation and analysis of all responses  
 - Mydlarz_bleach-recover.html  
     - html output from data analysis in Rmd. Download this file and open in browser to view html (can't open with GitHub).
      
#### data  <.csvs>
  - Gates_Mydlarz_20142016.nolab.recal_physimmun.csv  
      - the physiology and immunity data, recalibrated and using only field samples
  - Gates_Mydlarz_20142016_nolab_qPCR.csv  
      - qPCR data for symbiont community  
  - Gates_Mydlarz_20142016.nolab.recal_ALLDATA.csv  
      - the master datasheet of physiology, immunity, and Symbiodiniaceae community  
      - this data file is the amalgamation of the two .csv files above  
      
 **metadata for data as column-by-column**
  1. **Gates_Mydlarz_20142016_nolab_ALL_DATA.csv** and also **Gates_Mydlarz_20142016_nolab_physimmun.csv**    
      *Species* = Montipora capitata   
      *Period* = 2014 October, 2015 February, 2015 October, 2016 February 
      *Status* = either bleaching or recovery periods  
      *Site* = Lilipuna or Reef 14  
      *Status_Site* = interaction of Status and Site columns  
      *ID* = arbitrary sample ID  
      *total.blastate.ml* = total tissue extract (blastate) in ml   
      *surface.area.cm2* = colony surface area in cm2  
      *mg.prot..ml* = mg total protein per ml blastate  
      *cells..ml* = symbiont densities in cells per ml blastate  
      *ug.chla..ml* = μg of chlorophyll a per ml blastate    
      *g.AFDW..ml* = mg of host biomass per ml blastate  
      *CAT* = μmol H2O2 scavenged mg protein–1 min–1  
         - (slope of change in Abs at 240 nm (0 - 8 min) against μmol H2O2 standard curve)  
      *POX* = Δ Abs470 nm mg protein-1 min-1  
         - (slope of change in Abs at 470 nm (0-10 min), normalized to mg protein)  
      *SOD* = SOD activity units mg protein-1  
         - (% Inhibition of Abs at 450 nm, normalized to mg protein) (1 SOD Unit = 50% inhibition)    
      *PPO* = Δ Abs490 nm mg protein-1 min-1  
         - (slope of change in Abs 490 (0-15 min normalized to mg protein and time)  
      *MEL* = mg melanin mg tissue-1  
         - (Abs 490 nm, the concentration of melanin determined using a standard curve)  
      *propC* = proportion of community as percentage *Cladocopium*  
      *propD* = proportion of community as percentage *Durusdinium*    
      *syms* = 4 categories of symbiont community: all D, all C, D>C or D<C  
      *dom* = dominant symbiont at > 50% community  
      
  2. **Gates_Mydlarz_20142016_nolab_qPCR.csv**  
      *Status* = either bleaching or recovery periods  
      *Year* = 2014, 2015, 2016  
      *Event* = 2014 Bleach, 2015 Bleach, 2015 Recovery, 2016 Recovery  
      *Sample.ID* = arbitrary ID for colony at each reef location   
      *Sample.Name* = amalgamation of previous columns (i.e., event, year, ID)  
      *File.Name* = the qPCR file where data originated  
      *C.CT.mean* = mean CT value for 2 reps for *Cladocopium*    
      *D.CT.mean* = mean CT value for 2 reps for *Durusdinium*  
      *C.CT.sd* = standard deviation CT value for 2 reps for *Cladocopium*  
      *D.CT.sd* = standard deviation CT value for 2 reps for *Durusdinium*  
      *C.reps* = number of replicate wells C found in (of 2)  
      *D.reps* = number of replicate wells D found in (of 2)  
      *C.D* = C to D ratio normalized to actin gene copy number  
      *propC* = proportion of community as percentage *Cladocopium*  
      *propD* = proportion of community as percentage *Durusdinium*  
      *syms* = mixture of symbionts as all C, all D, D>C or C>D  
      *dom* = dominant symbiont at > 50%  
        
      
  - ecology <subfolder>
      - benthic data from reef surveys from 2014-2016
      
  - environmental <subfolder>
      - this folder has PAR and temperature data collected in situ at the two reefs (Lilipuna and Reef 14)
      - Lilipuna PAR all.csv
      - Lilipuna temp all.csv
      - Reef14 PAR all.csv
      - Reef14 temp all.csv
      - (folder) HIMB station -- this has discrete periods of temperature at HIMB weather station where loggers failed
      
   - qPCR <subfolder>
      - this folder has the 12 raw .txt files used in script to determine symbiont community
 
   - recal immunity <subfolder>
      - raw data for immunity and antioxidant calculations
 
   - unmodified dataframes <subfolder>
      - old dataframes previously used in 1st round of review
     
#### figures <folder>  
   - exported figures during code execution  
   - 'Main text figures' = final versions of main text figures formatted for journal  
   - 'Suppl figures' = final versions of supplemental text figures formatted for journal  
      
#### output
  - additional figures and products exported during analyses (primarily in degree heating weeks calculations)  
  - 4.panel.NMDS.stress.pdf  = 2D stress plots for 4 panel NMDS used in Figure 4  
  - annual.temps.csv = annual temps for degree heating week (DHW) calculations  
  - Gates_Mydlarz_20142016_standardized.DF.csv = standardized dataframe used in figure generation (code for standardization in Rmd)  
  - KBay.DHW.csv = DHW datframe by half-week dates  
  - KBay.DHWplot.png = plot of DHW 
  - Trajectory.NMDS.stressplot.pdf = 2D stress plot for NMDS used in Figure 3  
      
#### tables
  - formatted stats tables for the manuscript


