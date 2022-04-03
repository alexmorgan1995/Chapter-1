# Chapter1# Chapter 1 Analysis - Systematic scoping review: Current state of AMR transmission modelling at the livestock/human interface 

## Systematic Scoping Review Details

This repository is to store the analysis scripts and data files I have for chapter 1 of my PhD. 

Chapter 1 aims to carry out a systematic scoping review to assess the current state of AMR transmission modelling between livestock and human populations. This was carried out covering a wide range of potential modelling studies, including:
- Predictive Models
- Mechanistic Models
- Risk Assessments
- Source Attribution Studies 

A large excel sheet was created as part of the data-extraction protocol of the systematic scoping review. This can be found in the repository as ```Review Sheet Full Sheet.xlsx``` there is also an import version of this sheet as an ```.csv``` file format. An R script was created to analyse this excel data extraction sheet, this can be found as ```Lit Review Analysis.R```. 

**Currently there is still 1 paper that needs to be integrated into the analysis**
- Carmo et al, (2014) - Exposure assessment of extended-spectrum beta-lactamases/AmpC beta-lactamases-producing Escherichia coli in meat in Denmark

## Search Terms
The search terms used in this analysis for the PubMed search are: 
```
(((mathematic*[Title/Abstract] OR simulation[Title/Abstract] OR determinist*[Title/Abstract] OR stochast*[Title/Abstract] OR compartment*[Title/Abstract]) AND model*[Title/Abstract])
OR (risk analysis[Title/Abstract] OR risk assessment[Title/Abstract] OR hazard assessment[Title/Abstract] OR cost benefit[Title/Abstract] OR risk model[Title/Abstract] OR exposure assessment[Title/Abstract] OR risk management[Title/Abstract]))
AND (((antimicrobial[Title/Abstract] OR drug[Title/Abstract] OR antibiotic[Title/Abstract] OR microbial drug[Title/Abstract] OR multi-drug[Title/Abstract] OR penicillin[Title/Abstract] OR amoxicillin[Title/Abstract] OR flucloxacillin[Title/Abstract] OR co-amoxiclav[Title/Abstract] OR cephalosporins[Title/Abstract] OR cephalexin[Title/Abstract] OR cefuroxime[Title/Abstract] OR ceftriaxone[Title/Abstract] OR tetracycline[Title/Abstract ] OR doxycycline[Title/Abstract] OR lymecycline[Title/Abstract] OR oxytetracycline[Title/Abstract] OR quinolones[Title/Abstract] OR ciprofloxacin[Title/Abstract] OR levofloxacin[Title/Abstract] OR ofloxacin[Title/Abstract] OR macrolides[Title/Abstract] OR clarithromycin[Title/Abstract] OR erythromycin[Title/Abstract] OR azithromycin[Title/Abstract] OR telithromycin[Title/Abstract] OR nitrofurantoin[Title/Abstract] OR aminoglycosides[Title/Abstract] OR glycopeptide[Title/Abstract] OR daptomycin[Title/Abstract] OR vancomycin[Title/Abstract] OR teicoplanin[Title/Abstract] OR colistin[Title/Abstract] OR methicillin[Title/Abstract] OR carbapenem[Title/Abstract] OR meropenem[Title/Abstract] OR ertapenem[Title/Abstract] OR imipenem-cilastin[Title/Abstract]) AND resis*[Title/Abstract]) OR extended-spectrum*[Title/Abstract])
NOT (malaria[Title/Abstract] OR virus[Title/Abstract] OR HIV[Title/Abstract] OR cancer[Title/Abstract] OR tumor[Title/Abstract] OR tumour[Title/Abstract])
```

The other search terms for the Web of Science search, as well as more details can be found in the ```Criteria.docx``` and ```Lit Review Search Terms v2.docx``` files.