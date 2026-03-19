##Necessary Packages 
rm(list = ls())
library(readxl)
library(tidyverse)##necessary packages to eliminate NA rows in datasets
library(tidyr)##necessary packages to eliminate NA rows in datasets

## read in proteomic files ->  Protein files only
## WT protein data load

Protein_df_WT_plex1 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex1/WT-Plex1-Proteins.xlsx"
  )

Protein_df_WT_plex2 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex2/WT-Plex2-Proteins.xlsx"
  )
Protein_df_WT_plex3 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex3/WT-Plex3-Proteins.xlsx"
  )

Protein_df_WT_plex4 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex4/WT-Plex4-Proteins.xlsx"
  )

Protein_df_WT_plex5 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex5/WT-Plex5-Proteins.xlsx"
  )

## WT peptide data load
## read in proteomic files ->  peptide files only
Peptides_dataf_WT_plex1 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex1/WT-Plex1-Peptides.xlsx"
  )
Peptides_dataf_WT_plex2 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex2/WT-Plex2-Peptides.xlsx"
  )
Peptides_dataf_WT_plex3 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex3/WT-Plex3-Peptides.xlsx"
  )
Peptides_dataf_WT_plex4 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex4/WT-Plex4-Peptides.xlsx"
  )
Peptides_dataf_WT_plex5 <-
  read_excel(
    "/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_Plex5/WT-Plex5-Peptides.xlsx"
  )


## NaHS Protein data load
Protein_df_NaHS_plex1 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex1/NaHS_Plex1_Proteins.xlsx"
  )

Protein_df_NaHS_plex2 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex2/NaHS_Plex2_Proteins.xlsx"
  )

Protein_df_NaHS_plex3 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex3/NaHS_Plex3_Proteins.xlsx"
  )

Protein_df_NaHS_plex4 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex4/NaHS_Plex4_Proteins.xlsx"
  )

Protein_df_NaHS_plex5 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex5/NaHS_Plex5_Proteins.xlsx"
  )

## NaHS peptide data load
## read in proteomic files ->  peptide files only
Peptides_dataf_NaHS_plex1 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex1/NaHS_Plex1_Peptides.xlsx"
  )
Peptides_dataf_NaHS_plex2 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex2/NaHS_Plex2_Peptides.xlsx"
  )
Peptides_dataf_NaHS_plex3 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex3/NaHS_Plex3_Peptides.xlsx"
  )
Peptides_dataf_NaHS_plex4 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex4/NaHS_Plex4_Peptides.xlsx"
  )
Peptides_dataf_NaHS_plex5 <-
  read_excel(
    "/Users/User/Desktop/mdx_ECC_NaHS_R_Files/NaHS_Plex5/NaHS_Plex5_Peptides.xlsx"
  )

## Find protein overlap for WT

protein_accession_WT_plex1 <- data.frame('WT_Plex1 Accesssion' = Protein_df_WT_plex1$Accession, check.names = FALSE)
protein_accession_WT_plex2 <- data.frame('WT_Plex2 Accesssion' = Protein_df_WT_plex2$Accession, check.names = FALSE)
protein_accession_WT_plex3 <- data.frame('WT_Plex3 Accesssion' = Protein_df_WT_plex3$Accession, check.names = FALSE)
protein_accession_WT_plex4 <- data.frame('WT_Plex4 Accesssion' = Protein_df_WT_plex4$Accession, check.names = FALSE)
protein_accession_WT_plex5 <- data.frame('WT_Plex5 Accesssion' = Protein_df_WT_plex5$Accession, check.names = FALSE)
protein_accession_NaHS_plex1 <- data.frame('NaHS_Plex1 Accesssion' = Protein_df_NaHS_plex1$Accession, check.names = FALSE)
protein_accession_NaHS_plex2 <- data.frame('NaHS_Plex2 Accesssion' = Protein_df_NaHS_plex2$Accession, check.names = FALSE)
protein_accession_NaHS_plex3 <- data.frame('NaHS_Plex3 Accesssion' = Protein_df_NaHS_plex3$Accession, check.names = FALSE)
protein_accession_NaHS_plex4 <- data.frame('NaHS_Plex4 Accesssion' = Protein_df_NaHS_plex4$Accession, check.names = FALSE)
protein_accession_NaHS_plex5 <- data.frame('NaHS_Plex5 Accesssion' = Protein_df_NaHS_plex5$Accession, check.names = FALSE)

## calculate overlap across all combinations of 6plex 
overlap_accession <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_Plex1 = protein_accession_WT_plex1$`WT_Plex1 Accesssion`,
      WT_Plex2 = protein_accession_WT_plex2$`WT_Plex2 Accesssion`,
      WT_Plex3 = protein_accession_WT_plex3$`WT_Plex3 Accesssion`,
      WT_Plex4 = protein_accession_WT_plex4$`WT_Plex4 Accesssion`,
      WT_Plex5 = protein_accession_WT_plex5$`WT_Plex5 Accesssion`,
      NaHS_Plex1 = protein_accession_NaHS_plex1$`NaHS_Plex1 Accesssion`,
      NaHS_Plex2 = protein_accession_NaHS_plex2$`NaHS_Plex2 Accesssion`,
      NaHS_Plex3 = protein_accession_NaHS_plex3$`NaHS_Plex3 Accesssion`,
      NaHS_Plex4 = protein_accession_NaHS_plex4$`NaHS_Plex4 Accesssion`,
      NaHS_Plex5 = protein_accession_NaHS_plex5$`NaHS_Plex5 Accesssion`
    )
  ))


## Begin dataframe clean up 
## find common peptides by selecting those associated with proteins shared across all plex
## Clean up dataframe by dropping any rows that contain NA in modifications
## Clean up dataframe by dropping any rows that contain NA in Abundance Ratio:(ECC_ox)/(ECC/Total)
## Clean up dataframe by identifying and subsequently dropping any rows that contain non iodoTMT mods. This was accomplished
##    by identifying elements in Modfications column with a '[;'. This character sequence was 
##    unique to elements with Modications non-iododTMT mods
## Reorder up dataframe in alphabetical order by Accession

library(tidyverse)##necessary packages to eliminate NA rows in datasets
library(tidyr)##necessary packages to eliminate NA rows in datasets
library(plyr)

##WT plex 1
## first filtering only peptides with common protein accessions across all plexes
## then cleaning by eliminating all peptides with no quan values
## then create a new dataframe using numcolwise(sum) that only returns Sequence (or specified column) and numerical value columns. All column values of duplicate sequences are summed and consolidated into a single row  
## peptide data needs to be extracted from the unconsolidated file and merged back with the consolidated file
##
common_peptides_by_accession_WT_plex1 <-
  Peptides_dataf_WT_plex1 %>% filter(Peptides_dataf_WT_plex1$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_WT_plex1 <- common_peptides_by_accession_WT_plex1 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_WT_plex1 <- subset(common_pep_clean_WT_plex1, select = -c(4:14, 21:25))

pep_string_data_WT_plex1 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_WT_plex1$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_WT_plex1$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_WT_plex1$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_WT_plex1$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_WT_plex1$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_WT_plex1$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_WT_plex1$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_WT_plex1$Checked,
  Confidence = common_peptides_by_accession_WT_plex1$Confidence,
  Modifications = common_peptides_by_accession_WT_plex1$Modifications,
  `Quan Info` = common_peptides_by_accession_WT_plex1$`Quan Info`,
  check.names =FALSE
)
pep_string_data_WT_plex1 <- pep_string_data_WT_plex1[!duplicated(pep_string_data_WT_plex1[c('Sequence')]),]
common_pep_clean_WT_plex1 <- merge(pep_string_data_WT_plex1, common_pep_clean_WT_plex1, by='Sequence')


##WT plex 2
common_peptides_by_accession_WT_plex2 <-
  Peptides_dataf_WT_plex2 %>% filter(Peptides_dataf_WT_plex2$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_WT_plex2 <- common_peptides_by_accession_WT_plex2 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_WT_plex2 <- subset(common_pep_clean_WT_plex2, select = -c(4:14, 21:25))

pep_string_data_WT_plex2 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_WT_plex2$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_WT_plex2$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_WT_plex2$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_WT_plex2$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_WT_plex2$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_WT_plex2$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_WT_plex2$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_WT_plex2$Checked,
  Confidence = common_peptides_by_accession_WT_plex2$Confidence,
  Modifications = common_peptides_by_accession_WT_plex2$Modifications,
  `Quan Info` = common_peptides_by_accession_WT_plex2$`Quan Info`,
  check.names =FALSE
)
pep_string_data_WT_plex2 <- pep_string_data_WT_plex2[!duplicated(pep_string_data_WT_plex2[c('Sequence')]),]
common_pep_clean_WT_plex2 <- merge(pep_string_data_WT_plex2, common_pep_clean_WT_plex2, by='Sequence')

##WT plex 3
common_peptides_by_accession_WT_plex3 <-
  Peptides_dataf_WT_plex3 %>% filter(Peptides_dataf_WT_plex3$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_WT_plex3 <- common_peptides_by_accession_WT_plex3 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_WT_plex3 <- subset(common_pep_clean_WT_plex3, select = -c(4:14, 21:25))

pep_string_data_WT_plex3 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_WT_plex3$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_WT_plex3$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_WT_plex3$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_WT_plex3$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_WT_plex3$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_WT_plex3$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_WT_plex3$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_WT_plex3$Checked,
  Confidence = common_peptides_by_accession_WT_plex3$Confidence,
  Modifications = common_peptides_by_accession_WT_plex3$Modifications,
  `Quan Info` = common_peptides_by_accession_WT_plex3$`Quan Info`,
  check.names =FALSE
)
pep_string_data_WT_plex3 <- pep_string_data_WT_plex3[!duplicated(pep_string_data_WT_plex3[c('Sequence')]),]
common_pep_clean_WT_plex3 <- merge(pep_string_data_WT_plex3, common_pep_clean_WT_plex3, by='Sequence')

##WT plex 4
common_peptides_by_accession_WT_plex4 <-
  Peptides_dataf_WT_plex4 %>% filter(Peptides_dataf_WT_plex4$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_WT_plex4 <- common_peptides_by_accession_WT_plex4 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_WT_plex4 <- subset(common_pep_clean_WT_plex4, select = -c(4:14, 21:25))

pep_string_data_WT_plex4 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_WT_plex4$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_WT_plex4$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_WT_plex4$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_WT_plex4$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_WT_plex4$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_WT_plex4$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_WT_plex4$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_WT_plex4$Checked,
  Confidence = common_peptides_by_accession_WT_plex4$Confidence,
  Modifications = common_peptides_by_accession_WT_plex4$Modifications,
  `Quan Info` = common_peptides_by_accession_WT_plex4$`Quan Info`,
  check.names =FALSE
)
pep_string_data_WT_plex4 <- pep_string_data_WT_plex4[!duplicated(pep_string_data_WT_plex4[c('Sequence')]),]
common_pep_clean_WT_plex4 <- merge(pep_string_data_WT_plex4, common_pep_clean_WT_plex4, by='Sequence')

##WT plex 5
common_peptides_by_accession_WT_plex5 <-
  Peptides_dataf_WT_plex5 %>% filter(Peptides_dataf_WT_plex5$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_WT_plex5 <- common_peptides_by_accession_WT_plex5 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_WT_plex5 <- subset(common_pep_clean_WT_plex5, select = -c(4:14, 21:25))

pep_string_data_WT_plex5 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_WT_plex5$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_WT_plex5$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_WT_plex5$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_WT_plex5$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_WT_plex5$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_WT_plex5$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_WT_plex5$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_WT_plex5$Checked,
  Confidence = common_peptides_by_accession_WT_plex5$Confidence,
  Modifications = common_peptides_by_accession_WT_plex5$Modifications,
  `Quan Info` = common_peptides_by_accession_WT_plex5$`Quan Info`,
  check.names =FALSE
)
pep_string_data_WT_plex5 <- pep_string_data_WT_plex5[!duplicated(pep_string_data_WT_plex5[c('Sequence')]),]
common_pep_clean_WT_plex5 <- merge(pep_string_data_WT_plex5, common_pep_clean_WT_plex5, by='Sequence')


## NaHS Screen
## NaHS plex1
common_peptides_by_accession_NaHS_plex1 <-
  Peptides_dataf_NaHS_plex1 %>% filter(Peptides_dataf_NaHS_plex1$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_NaHS_plex1 <- common_peptides_by_accession_NaHS_plex1 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_NaHS_plex1 <- subset(common_pep_clean_NaHS_plex1, select = -c(3:13, 20:24))

pep_string_data_NaHS_plex1 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_NaHS_plex1$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_NaHS_plex1$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_NaHS_plex1$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_NaHS_plex1$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_NaHS_plex1$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_NaHS_plex1$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_NaHS_plex1$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_NaHS_plex1$Checked,
  Confidence = common_peptides_by_accession_NaHS_plex1$Confidence,
  Modifications = common_peptides_by_accession_NaHS_plex1$Modifications,
  `Quan Info` = common_peptides_by_accession_NaHS_plex1$`Quan Info`,
  check.names =FALSE
)
pep_string_data_NaHS_plex1 <- pep_string_data_NaHS_plex1[!duplicated(pep_string_data_NaHS_plex1[c('Sequence')]),]
common_pep_clean_NaHS_plex1 <- merge(pep_string_data_NaHS_plex1, common_pep_clean_NaHS_plex1, by='Sequence')

##NaHS plex 2
common_peptides_by_accession_NaHS_plex2 <-
  Peptides_dataf_NaHS_plex2 %>% filter(Peptides_dataf_NaHS_plex2$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_NaHS_plex2 <- common_peptides_by_accession_NaHS_plex2 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_NaHS_plex2 <- subset(common_pep_clean_NaHS_plex2, select = -c(3:13, 20:24))

pep_string_data_NaHS_plex2 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_NaHS_plex2$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_NaHS_plex2$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_NaHS_plex2$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_NaHS_plex2$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_NaHS_plex2$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_NaHS_plex2$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_NaHS_plex2$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_NaHS_plex2$Checked,
  Confidence = common_peptides_by_accession_NaHS_plex2$Confidence,
  Modifications = common_peptides_by_accession_NaHS_plex2$Modifications,
  `Quan Info` = common_peptides_by_accession_NaHS_plex2$`Quan Info`,
  check.names =FALSE
)
pep_string_data_NaHS_plex2 <- pep_string_data_NaHS_plex2[!duplicated(pep_string_data_NaHS_plex2[c('Sequence')]),]
common_pep_clean_NaHS_plex2 <- merge(pep_string_data_NaHS_plex2, common_pep_clean_NaHS_plex2, by='Sequence')

##NaHS plex 3
common_peptides_by_accession_NaHS_plex3 <-
  Peptides_dataf_NaHS_plex3 %>% filter(Peptides_dataf_NaHS_plex3$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_NaHS_plex3 <- common_peptides_by_accession_NaHS_plex3 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_NaHS_plex3 <- subset(common_pep_clean_NaHS_plex3, select = -c(3:13, 20:24))

pep_string_data_NaHS_plex3 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_NaHS_plex3$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_NaHS_plex3$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_NaHS_plex3$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_NaHS_plex3$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_NaHS_plex3$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_NaHS_plex3$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_NaHS_plex3$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_NaHS_plex3$Checked,
  Confidence = common_peptides_by_accession_NaHS_plex3$Confidence,
  Modifications = common_peptides_by_accession_NaHS_plex3$Modifications,
  `Quan Info` = common_peptides_by_accession_NaHS_plex3$`Quan Info`,
  check.names =FALSE
)
pep_string_data_NaHS_plex3 <- pep_string_data_NaHS_plex3[!duplicated(pep_string_data_NaHS_plex3[c('Sequence')]),]
common_pep_clean_NaHS_plex3 <- merge(pep_string_data_NaHS_plex3, common_pep_clean_NaHS_plex3, by='Sequence')

##NaHS plex 4
common_peptides_by_accession_NaHS_plex4 <-
  Peptides_dataf_NaHS_plex4 %>% filter(Peptides_dataf_NaHS_plex4$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_NaHS_plex4 <- common_peptides_by_accession_NaHS_plex4 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_NaHS_plex4 <- subset(common_pep_clean_NaHS_plex4, select = -c(3:13, 20:24))

pep_string_data_NaHS_plex4 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_NaHS_plex4$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_NaHS_plex4$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_NaHS_plex4$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_NaHS_plex4$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_NaHS_plex4$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_NaHS_plex4$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_NaHS_plex4$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_NaHS_plex4$Checked,
  Confidence = common_peptides_by_accession_NaHS_plex4$Confidence,
  Modifications = common_peptides_by_accession_NaHS_plex4$Modifications,
  `Quan Info` = common_peptides_by_accession_NaHS_plex4$`Quan Info`,
  check.names =FALSE
)
pep_string_data_NaHS_plex4 <- pep_string_data_NaHS_plex4[!duplicated(pep_string_data_NaHS_plex4[c('Sequence')]),]
common_pep_clean_NaHS_plex4 <- merge(pep_string_data_NaHS_plex4, common_pep_clean_NaHS_plex4, by='Sequence')

##NaHS plex 5
common_peptides_by_accession_NaHS_plex5 <-
  Peptides_dataf_NaHS_plex5 %>% filter(Peptides_dataf_NaHS_plex5$`Master Protein Accessions` %in% overlap_accession$Overlap)%>% drop_na(`Abundance Ratio: (mdx_ox) / (mdx_total)`)
common_pep_clean_NaHS_plex5 <- common_peptides_by_accession_NaHS_plex5 %>% ddply("Sequence", numcolwise(sum)) 
common_pep_clean_NaHS_plex5 <- subset(common_pep_clean_NaHS_plex5, select = -c(3:13, 20:24))

pep_string_data_NaHS_plex5 <- data.frame(
  `Master Protein Descriptions`= common_peptides_by_accession_NaHS_plex5$`Master Protein Descriptions`,
  `Master Protein Accessions` = common_peptides_by_accession_NaHS_plex5$`Master Protein Accessions`,
  Sequence = common_peptides_by_accession_NaHS_plex5$Sequence,
  `Annotated Sequence` = common_peptides_by_accession_NaHS_plex5$`Annotated Sequence`,
  `Positions in Master Proteins` = common_peptides_by_accession_NaHS_plex5$`Positions in Master Proteins`,
  `Modifications in Master Proteins` = common_peptides_by_accession_NaHS_plex5$`Modifications in Master Proteins`,
  `Channel Occupancy [%]` = common_peptides_by_accession_NaHS_plex5$`Channel Occupancy [%]`,
  Checked = common_peptides_by_accession_NaHS_plex5$Checked,
  Confidence = common_peptides_by_accession_NaHS_plex5$Confidence,
  Modifications = common_peptides_by_accession_NaHS_plex5$Modifications,
  `Quan Info` = common_peptides_by_accession_NaHS_plex5$`Quan Info`,
  check.names =FALSE
)
pep_string_data_NaHS_plex5 <- pep_string_data_NaHS_plex5[!duplicated(pep_string_data_NaHS_plex5[c('Sequence')]),]
common_pep_clean_NaHS_plex5 <- merge(pep_string_data_NaHS_plex5, common_pep_clean_NaHS_plex5, by='Sequence')

## order datasets by alphabetical order
common_pep_clean_WT_plex1 <- common_pep_clean_WT_plex1[order(common_pep_clean_WT_plex1$`Master Protein Descriptions`),]
common_pep_clean_WT_plex2 <- common_pep_clean_WT_plex2[order(common_pep_clean_WT_plex2$`Master Protein Descriptions`),]
common_pep_clean_WT_plex3 <- common_pep_clean_WT_plex3[order(common_pep_clean_WT_plex3$`Master Protein Descriptions`),]
common_pep_clean_WT_plex4 <- common_pep_clean_WT_plex4[order(common_pep_clean_WT_plex4$`Master Protein Descriptions`),]
common_pep_clean_WT_plex5 <- common_pep_clean_WT_plex5[order(common_pep_clean_WT_plex5$`Master Protein Descriptions`),]

common_pep_clean_NaHS_plex1 <- common_pep_clean_NaHS_plex1[order(common_pep_clean_NaHS_plex1$`Master Protein Descriptions`),]
common_pep_clean_NaHS_plex2 <- common_pep_clean_NaHS_plex2[order(common_pep_clean_NaHS_plex2$`Master Protein Descriptions`),]
common_pep_clean_NaHS_plex3 <- common_pep_clean_NaHS_plex3[order(common_pep_clean_NaHS_plex3$`Master Protein Descriptions`),]
common_pep_clean_NaHS_plex4 <- common_pep_clean_NaHS_plex4[order(common_pep_clean_NaHS_plex4$`Master Protein Descriptions`),]
common_pep_clean_NaHS_plex5 <- common_pep_clean_NaHS_plex5[order(common_pep_clean_NaHS_plex5$`Master Protein Descriptions`),]

## Additional clean up step for ox/total threshold. When calculating ox/total values, some are >1 which indicates that the total channel does not represent 
## the actual total number of peptides. These rows/peptides should be excluded from analysis. That is what the following code works to do. 

## screen 1 clean up
peptides_WT_threshclean_plex1 <- subset(common_pep_clean_WT_plex1, !((common_pep_clean_WT_plex1$'Abundances (Grouped): WT_ox'/common_pep_clean_WT_plex1$'Abundances (Grouped): WT_total'>1)))
peptides_WT_threshclean_plex2 <- subset(common_pep_clean_WT_plex2, !((common_pep_clean_WT_plex2$'Abundances (Grouped): WT_ox'/common_pep_clean_WT_plex2$'Abundances (Grouped): WT_total'>1)))
peptides_WT_threshclean_plex3 <- subset(common_pep_clean_WT_plex3, !((common_pep_clean_WT_plex3$'Abundances (Grouped): WT_ox'/common_pep_clean_WT_plex3$'Abundances (Grouped): WT_total'>1)))
peptides_WT_threshclean_plex4 <- subset(common_pep_clean_WT_plex4, !((common_pep_clean_WT_plex4$'Abundances (Grouped): WT_ox'/common_pep_clean_WT_plex4$'Abundances (Grouped): WT_total'>1)))
peptides_WT_threshclean_plex5 <- subset(common_pep_clean_WT_plex5, !((common_pep_clean_WT_plex5$'Abundances (Grouped): WT_ox'/common_pep_clean_WT_plex5$'Abundances (Grouped): WT_total'>1)))

peptides_WTECC_threshclean_plex1 <- subset(common_pep_clean_WT_plex1, !((common_pep_clean_WT_plex1$`Abundances (Grouped): WT_ECC_ox`/common_pep_clean_WT_plex1$`Abundances (Grouped): WT_Ecc_total`>1)))
peptides_WTECC_threshclean_plex2 <- subset(common_pep_clean_WT_plex2, !((common_pep_clean_WT_plex2$'Abundances (Grouped): WT_ECC_ox'/common_pep_clean_WT_plex2$'Abundances (Grouped): WT_Ecc_total'>1)))
peptides_WTECC_threshclean_plex3 <- subset(common_pep_clean_WT_plex3, !((common_pep_clean_WT_plex3$'Abundances (Grouped): WT_ECC_ox'/common_pep_clean_WT_plex3$'Abundances (Grouped): WT_Ecc_total'>1)))
peptides_WTECC_threshclean_plex4 <- subset(common_pep_clean_WT_plex4, !((common_pep_clean_WT_plex4$'Abundances (Grouped): WT_ECC_ox'/common_pep_clean_WT_plex4$'Abundances (Grouped): WT_Ecc_total'>1)))
peptides_WTECC_threshclean_plex5 <- subset(common_pep_clean_WT_plex5, !((common_pep_clean_WT_plex5$'Abundances (Grouped): WT_ECC_ox'/common_pep_clean_WT_plex5$'Abundances (Grouped): WT_Ecc_total'>1)))

peptides_mdx_threshclean_plex1 <- subset(common_pep_clean_WT_plex1, !((common_pep_clean_WT_plex1$`Abundances (Grouped): mdx_ox`/common_pep_clean_WT_plex1$`Abundances (Grouped): mdx_total`>1)))
peptides_mdx_threshclean_plex2 <- subset(common_pep_clean_WT_plex2, !((common_pep_clean_WT_plex2$'Abundances (Grouped): mdx_ox'/common_pep_clean_WT_plex2$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex3 <- subset(common_pep_clean_WT_plex3, !((common_pep_clean_WT_plex3$'Abundances (Grouped): mdx_ox'/common_pep_clean_WT_plex3$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex4 <- subset(common_pep_clean_WT_plex4, !((common_pep_clean_WT_plex4$'Abundances (Grouped): mdx_ox'/common_pep_clean_WT_plex4$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex5 <- subset(common_pep_clean_WT_plex5, !((common_pep_clean_WT_plex5$'Abundances (Grouped): mdx_ox'/common_pep_clean_WT_plex5$'Abundances (Grouped): mdx_total'>1)))

overlap_fun <- function(x){Reduce(intersect, x)}
peptides_WTscreen_threshclean_plex1 <- list(
  peptides_WT_threshclean_plex1$Sequence,
  peptides_WTECC_threshclean_plex1$Sequence,
  peptides_mdx_threshclean_plex1$Sequence
)

peptides_WTscreen_threshclean_plex2 <- list(
  peptides_WT_threshclean_plex2$Sequence,
  peptides_WTECC_threshclean_plex2$Sequence,
  peptides_mdx_threshclean_plex2$Sequence
)
peptides_WTscreen_threshclean_plex3 <- list(
  peptides_WT_threshclean_plex3$Sequence,
  peptides_WTECC_threshclean_plex3$Sequence,
  peptides_mdx_threshclean_plex3$Sequence
)
peptides_WTscreen_threshclean_plex4 <- list(
  peptides_WT_threshclean_plex4$Sequence,
  peptides_WTECC_threshclean_plex4$Sequence,
  peptides_mdx_threshclean_plex4$Sequence
)
peptides_WTscreen_threshclean_plex5 <- list(
  peptides_WT_threshclean_plex5$Sequence,
  peptides_WTECC_threshclean_plex5$Sequence,
  peptides_mdx_threshclean_plex5$Sequence
)
pep_overlap_WTsc_threshclean_plex1 <-
  data.frame(sequence = overlap_fun(peptides_WTscreen_threshclean_plex1))
pep_overlap_WTsc_threshclean_plex2 <-
  data.frame(sequence = overlap_fun(peptides_WTscreen_threshclean_plex2))
pep_overlap_WTsc_threshclean_plex3 <-
  data.frame(sequence = overlap_fun(peptides_WTscreen_threshclean_plex3))
pep_overlap_WTsc_threshclean_plex4 <-
  data.frame(sequence = overlap_fun(peptides_WTscreen_threshclean_plex4))
pep_overlap_WTsc_threshclean_plex5 <-
  data.frame(sequence = overlap_fun(peptides_WTscreen_threshclean_plex5))

## screen 2 clean up
peptides_mdx_threshclean_plex1 <- subset(common_pep_clean_NaHS_plex1, !((common_pep_clean_NaHS_plex1$'Abundances (Grouped): mdx_ox'/common_pep_clean_NaHS_plex1$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex2 <- subset(common_pep_clean_NaHS_plex2, !((common_pep_clean_NaHS_plex2$'Abundances (Grouped): mdx_ox'/common_pep_clean_NaHS_plex2$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex3 <- subset(common_pep_clean_NaHS_plex3, !((common_pep_clean_NaHS_plex3$'Abundances (Grouped): mdx_ox'/common_pep_clean_NaHS_plex3$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex4 <- subset(common_pep_clean_NaHS_plex4, !((common_pep_clean_NaHS_plex4$'Abundances (Grouped): mdx_ox'/common_pep_clean_NaHS_plex4$'Abundances (Grouped): mdx_total'>1)))
peptides_mdx_threshclean_plex5 <- subset(common_pep_clean_NaHS_plex5, !((common_pep_clean_NaHS_plex5$'Abundances (Grouped): mdx_ox'/common_pep_clean_NaHS_plex5$'Abundances (Grouped): mdx_total'>1)))

peptides_mdxECC_threshclean_plex1 <- subset(common_pep_clean_NaHS_plex1, !((common_pep_clean_NaHS_plex1$'Abundances (Grouped): ECC_ox'/common_pep_clean_NaHS_plex1$'Abundances (Grouped): ECC_total'>1)))
peptides_mdxECC_threshclean_plex2 <- subset(common_pep_clean_NaHS_plex2, !((common_pep_clean_NaHS_plex2$'Abundances (Grouped): ECC_ox'/common_pep_clean_NaHS_plex2$'Abundances (Grouped): ECC_total'>1)))
peptides_mdxECC_threshclean_plex3 <- subset(common_pep_clean_NaHS_plex3, !((common_pep_clean_NaHS_plex3$'Abundances (Grouped): ECC_ox'/common_pep_clean_NaHS_plex3$'Abundances (Grouped): ECC_total'>1)))
peptides_mdxECC_threshclean_plex4 <- subset(common_pep_clean_NaHS_plex4, !((common_pep_clean_NaHS_plex4$'Abundances (Grouped): ECC_ox'/common_pep_clean_NaHS_plex4$'Abundances (Grouped): ECC_total'>1)))
peptides_mdxECC_threshclean_plex5 <- subset(common_pep_clean_NaHS_plex5, !((common_pep_clean_NaHS_plex5$'Abundances (Grouped): ECC_ox'/common_pep_clean_NaHS_plex5$'Abundances (Grouped): ECC_total'>1)))

peptides_NaHS_threshclean_plex1 <- subset(common_pep_clean_NaHS_plex1, !((common_pep_clean_NaHS_plex1$`Abundances (Grouped): NaHS_ox`/common_pep_clean_NaHS_plex1$`Abundances (Grouped): NaHS_total`>1)))
peptides_NaHS_threshclean_plex2 <- subset(common_pep_clean_NaHS_plex2, !((common_pep_clean_NaHS_plex2$'Abundances (Grouped): NaHS_ox'/common_pep_clean_NaHS_plex2$'Abundances (Grouped): NaHS_total'>1)))
peptides_NaHS_threshclean_plex3 <- subset(common_pep_clean_NaHS_plex3, !((common_pep_clean_NaHS_plex3$'Abundances (Grouped): NaHS_ox'/common_pep_clean_NaHS_plex3$'Abundances (Grouped): NaHS_total'>1)))
peptides_NaHS_threshclean_plex4 <- subset(common_pep_clean_NaHS_plex4, !((common_pep_clean_NaHS_plex4$'Abundances (Grouped): NaHS_ox'/common_pep_clean_NaHS_plex4$'Abundances (Grouped): NaHS_total'>1)))
peptides_NaHS_threshclean_plex5 <- subset(common_pep_clean_NaHS_plex5, !((common_pep_clean_NaHS_plex5$'Abundances (Grouped): NaHS_ox'/common_pep_clean_NaHS_plex5$'Abundances (Grouped): NaHS_total'>1)))


# overlap_fun <- function(x){Reduce(intersect, x)}
peptides_NaHSscreen_threshclean_plex1 <- list(
  peptides_mdx_threshclean_plex1$Sequence,
  peptides_mdxECC_threshclean_plex1$Sequence,
  peptides_NaHS_threshclean_plex1$Sequence
)
peptides_NaHSscreen_threshclean_plex2 <- list(
  peptides_mdx_threshclean_plex2$Sequence,
  peptides_mdxECC_threshclean_plex2$Sequence,
  peptides_NaHS_threshclean_plex2$Sequence
)
peptides_NaHSscreen_threshclean_plex3 <- list(
  peptides_mdx_threshclean_plex3$Sequence,
  peptides_mdxECC_threshclean_plex3$Sequence,
  peptides_NaHS_threshclean_plex3$Sequence
)
peptides_NaHSscreen_threshclean_plex4 <- list(
  peptides_mdx_threshclean_plex4$Sequence,
  peptides_mdxECC_threshclean_plex4$Sequence,
  peptides_NaHS_threshclean_plex4$Sequence
)
peptides_NaHSscreen_threshclean_plex5 <- list(
  peptides_mdx_threshclean_plex5$Sequence,
  peptides_mdxECC_threshclean_plex5$Sequence,
  peptides_NaHS_threshclean_plex5$Sequence
)
pep_overlap_NaHSsc_threshclean_plex1 <-
  data.frame(sequence = overlap_fun(peptides_NaHSscreen_threshclean_plex1))
pep_overlap_NaHSsc_threshclean_plex2 <-
  data.frame(sequence = overlap_fun(peptides_NaHSscreen_threshclean_plex2))
pep_overlap_NaHSsc_threshclean_plex3 <-
  data.frame(sequence = overlap_fun(peptides_NaHSscreen_threshclean_plex3))
pep_overlap_NaHSsc_threshclean_plex4 <-
  data.frame(sequence = overlap_fun(peptides_NaHSscreen_threshclean_plex4))
pep_overlap_NaHSsc_threshclean_plex5 <-
  data.frame(sequence = overlap_fun(peptides_NaHSscreen_threshclean_plex5))


############################################################################################################
######################################Find overlapping sequences

overlap_sequence <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_Plex1 = pep_overlap_WTsc_threshclean_plex1$sequence,
      WT_Plex2 = pep_overlap_WTsc_threshclean_plex2$sequence,
      WT_Plex3 = pep_overlap_WTsc_threshclean_plex3$sequence,
      WT_Plex4 = pep_overlap_WTsc_threshclean_plex4$sequence,
      WT_Plex5 = pep_overlap_WTsc_threshclean_plex5$sequence,
      NaHS_Plex1 = pep_overlap_NaHSsc_threshclean_plex1$sequence,
      NaHS_Plex2 = pep_overlap_NaHSsc_threshclean_plex2$sequence,
      NaHS_Plex3 = pep_overlap_NaHSsc_threshclean_plex3$sequence,
      NaHS_Plex4 = pep_overlap_NaHSsc_threshclean_plex4$sequence,
      NaHS_Plex5 = pep_overlap_NaHSsc_threshclean_plex5$sequence
    )
  ))

## Create dataframes that contain only contain data which match overlapping sequences   
common_peptides_WT_plex1 <-
  common_pep_clean_WT_plex1 %>% filter(
    common_pep_clean_WT_plex1$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_WT_plex2 <-
  common_pep_clean_WT_plex2 %>% filter(
    common_pep_clean_WT_plex2$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_WT_plex3 <-
  common_pep_clean_WT_plex3 %>% filter(
    common_pep_clean_WT_plex3$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_WT_plex4 <-
  common_pep_clean_WT_plex4 %>% filter(
    common_pep_clean_WT_plex4$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_WT_plex5 <-
  common_pep_clean_WT_plex5 %>% filter(
    common_pep_clean_WT_plex5$`Sequence` %in% overlap_sequence$Overlap
  )

## Create dataframes that contain only contain data which match overlapping sequences   
common_peptides_NaHS_plex1 <-
  common_pep_clean_NaHS_plex1 %>% filter(
    common_pep_clean_NaHS_plex1$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_NaHS_plex2 <-
  common_pep_clean_NaHS_plex2 %>% filter(
    common_pep_clean_NaHS_plex2$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_NaHS_plex3 <-
  common_pep_clean_NaHS_plex3 %>% filter(
    common_pep_clean_NaHS_plex3$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_NaHS_plex4 <-
  common_pep_clean_NaHS_plex4 %>% filter(
    common_pep_clean_NaHS_plex4$`Sequence` %in% overlap_sequence$Overlap
  )
common_peptides_NaHS_plex5 <-
  common_pep_clean_NaHS_plex5 %>% filter(
    common_pep_clean_NaHS_plex5$`Sequence` %in% overlap_sequence$Overlap
  )

############################################################################################################
###############################################################################
######### selecting protein info/function and merging it with peptide data
##############################################################################

## Find protein data for the common proteins 

Common_protein_info <- #protein info taken from Protein_df_NaHS_plex1, at this point all peptides are matched across plexes and screens so protein info will be the same for all dfs
  data.frame(
    'Master Protein Accessions' = Protein_df_NaHS_plex1$Accession,
    'Gene Symbol' = Protein_df_NaHS_plex1$'Gene Symbol',
    'Biological Process' = Protein_df_NaHS_plex1$`Biological Process`,
    'Cellular Component' = Protein_df_NaHS_plex1$`Cellular Component`,
    'Molecular Function' = Protein_df_NaHS_plex1$`Molecular Function`,
    'Gene ID' = Protein_df_NaHS_plex1$`Gene ID`,
    'Pfam IDs' = Protein_df_NaHS_plex1$`Pfam IDs`,
    'Reactome Pathway Accessions' = Protein_df_NaHS_plex1$`Reactome Pathway Accessions`,
    'Reactome Pathways' = Protein_df_NaHS_plex1$`Reactome Pathways`,
    'WikiPathway Accessions' = Protein_df_NaHS_plex1$`WikiPathway Accessions`,
    'WikiPathways' = Protein_df_NaHS_plex1$`WikiPathways`,
    check.names=FALSE #leaves spaces in place between word in column names
  )

######## Merge protein info with peptide data for plex1
##WT peptides plex 1
accession_overlap_WT_plex1 <-
  generics::intersect(common_peptides_WT_plex1$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_WT_plex1,]

peptides_WT_plex1 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_WT_plex1)

##WT peptides plex 2
accession_overlap_WT_plex2 <-
  generics::intersect(common_peptides_WT_plex2$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_WT_plex2,]

peptides_WT_plex2 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_WT_plex2)

##WT peptides plex 3
accession_overlap_WT_plex3 <-
  generics::intersect(common_peptides_WT_plex3$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_WT_plex3,]

peptides_WT_plex3 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_WT_plex3)

##WT peptides plex 4
accession_overlap_WT_plex4 <-
  generics::intersect(common_peptides_WT_plex4$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_WT_plex4,]

peptides_WT_plex4 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_WT_plex4)

##WT peptides plex 5
accession_overlap_WT_plex5 <-
  generics::intersect(common_peptides_WT_plex5$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_WT_plex5,]

peptides_WT_plex5 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_WT_plex5)

##NaHS peptides plex 1
accession_overlap_NaHS_plex1 <-
  generics::intersect(common_peptides_NaHS_plex1$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_NaHS_plex1,]

peptides_NaHS_plex1 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_NaHS_plex1)

##NaHS peptides plex 2
accession_overlap_NaHS_plex2 <-
  generics::intersect(common_peptides_NaHS_plex2$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_NaHS_plex2,]

peptides_NaHS_plex2 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_NaHS_plex2)

##NaHS peptides plex 3
accession_overlap_NaHS_plex3 <-
  generics::intersect(common_peptides_NaHS_plex3$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_NaHS_plex3,]

peptides_NaHS_plex3 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_NaHS_plex3)

##NaHS peptides plex 4
accession_overlap_NaHS_plex4 <-
  generics::intersect(common_peptides_NaHS_plex4$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_NaHS_plex4,]

peptides_NaHS_plex4 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_NaHS_plex4)

##NaHS peptides plex 5
accession_overlap_NaHS_plex5 <-
  generics::intersect(common_peptides_NaHS_plex5$`Master Protein Accessions`,
                      Common_protein_info$'Master Protein Accessions')
Common_protein_info <-
  Common_protein_info[Common_protein_info$`Master Protein Accessions` %in% accession_overlap_NaHS_plex5,]

peptides_NaHS_plex5 <-
  dplyr::inner_join(Common_protein_info, by = "Master Protein Accessions", common_peptides_NaHS_plex5)




###############################################################################
###############################################################################
# T-test function for multiple experiments
t_test <- function(dt,grp1,grp2){
  # Subset control group and convert to numeric
  x <- dt[grp1] %>% unlist %>% as.numeric()
  # Subset treatment group and convert to numeric
  y <- dt[grp2] %>% unlist %>% as.numeric()
  # Perform t-test using the mean of x and y
  result <- t.test(x, y, var.equal =TRUE)
  # # Extract p-values from the results
  p_vals <- tibble(p_val = result$p.value)
  # # Return p-values
  return(p_vals)
} 


#################################################################################
#################################################################################
#################################################################################
########### Manually calculate abundance ratios
#################################################################################


## IF YOU ADD ANY ADDITIONAL COLUMNS INTO THIS DATAFRAME BE SURE TO CHANGE REFERENCE COLUMNS FOR MEAN CACLUATIONS (SEE BELOW)
rm(
  WT_NaHS_comb_abundance_ratios_1,
  WT_NaHS_comb_abundance_ratios_2,
  WT_NaHS_comb_abundance_ratios_3,
  WT_NaHS_comb_abundance_ratios,
  mean_ox_total_abundance_ratios,
  mean_ox_total_abundance_ratios_1,
  mean_ox_total_abundance_ratios_2,
  mean_ox_only_abundance_ratios,
  mean_ox_only_abundance_ratios_1,
  mean_ox_only_abundance_ratios_2,
  mean_ox_total_norm_abundance_ratios,
  mean_ox_total_norm_abundance_ratios_1,
  mean_ox_total_norm_abundance_ratios_2,
  mean_ox_only_norm_abundance_ratios,
  mean_ox_only_norm_abundance_ratios_1,
  mean_ox_only_norm_abundance_ratios_2,
  abundance_ratios_normalized_mdx
)


WT_NaHS_comb_abundance_ratios_1 = 
data.frame(
  'Master Protein Accessions' = peptides_WT_plex1$`Master Protein Accessions`,
  'Gene_Symbol'= peptides_WT_plex1$'Gene Symbol',
  'Master Protein Descriptions' = peptides_WT_plex1$`Master Protein Descriptions`,
  'Peptide_Sequence' = peptides_WT_plex1$Sequence,
  'Annotated Sequence' = peptides_WT_plex1$`Annotated Sequence`,
  'Postitions in Master Proteins' = peptides_WT_plex1$`Positions in Master Proteins`,
  '# Missed Clevages' = peptides_WT_plex1$`# Missed Cleavages`,
  'Modifications' = peptides_WT_plex1$Modifications,
  'Abundances (Grouped): WT_ox Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_ox`,    
  'Abundances (Grouped): WT_ox Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_ox`, #row10
  'Abundances (Grouped): WT_ox Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_ox`,
  'Abundances (Grouped): WT_ox Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_ox`,
  'Abundances (Grouped): WT_ox Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_ox`,
  'Abundances (Grouped): WT_total Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_total`,
  'Abundances (Grouped): WT_total Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_total`,
  'Abundances (Grouped): WT_total Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_total`,
  'Abundances (Grouped): WT_total Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_total`,
  'Abundances (Grouped): WT_total Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_total`,
  'Abundances (Grouped): WT_ECC_ox Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_ECC_ox`,
  'Abundances (Grouped): WT_ECC_ox Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_ECC_ox`, #row 20
  'Abundances (Grouped): WT_ECC_ox Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_ECC_ox`,
  'Abundances (Grouped): WT_ECC_ox Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_ECC_ox`,
  'Abundances (Grouped): WT_ECC_ox Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_ECC_ox`,
  'Abundances (Grouped): WT_Ecc_total Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_Ecc_total`,
  'Abundances (Grouped): WT_Ecc_total Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_Ecc_total`,
  'Abundances (Grouped): WT_Ecc_total Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_Ecc_total`,
  'Abundances (Grouped): WT_Ecc_total Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_Ecc_total`,
  'Abundances (Grouped): WT_Ecc_total Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_Ecc_total`,
  'Abundances (Grouped): mdx_ox Plex1' = peptides_WT_plex1$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_ox Plex2' = peptides_WT_plex2$`Abundances (Grouped): mdx_ox`, #row 30
  'Abundances (Grouped): mdx_ox Plex3' = peptides_WT_plex3$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_ox Plex4' = peptides_WT_plex4$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_ox Plex5' = peptides_WT_plex5$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_total Plex1' = peptides_WT_plex1$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total Plex2' = peptides_WT_plex2$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total Plex3' = peptides_WT_plex3$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total Plex4' = peptides_WT_plex4$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total Plex5' = peptides_WT_plex5$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_ox(NaHS) Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): mdx_ox`,    
  'Abundances (Grouped): mdx_ox(NaHS) Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): mdx_ox`, #row40
  'Abundances (Grouped): mdx_ox(NaHS) Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_ox(NaHS) Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_ox(NaHS) Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): mdx_ox`,
  'Abundances (Grouped): mdx_total(NaHS) Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total(NaHS) Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total(NaHS) Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total(NaHS) Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): mdx_total(NaHS) Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): mdx_total`,
  'Abundances (Grouped): ECC_ox Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): ECC_ox`,
  'Abundances (Grouped): ECC_ox Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): ECC_ox`, #row 50
  'Abundances (Grouped): ECC_ox Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): ECC_ox`,
  'Abundances (Grouped): ECC_ox Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): ECC_ox`,
  'Abundances (Grouped): ECC_ox Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): ECC_ox`,
  'Abundances (Grouped): ECC_total Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): ECC_total`,
  'Abundances (Grouped): ECC_total Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): ECC_total`,
  'Abundances (Grouped): ECC_total Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): ECC_total`,
  'Abundances (Grouped): ECC_total Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): ECC_total`,
  'Abundances (Grouped): ECC_total Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): ECC_total`,
  'Abundances (Grouped): NaHS_ox Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): NaHS_ox`,
  'Abundances (Grouped): NaHS_ox Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): NaHS_ox`, #row 60
  'Abundances (Grouped): NaHS_ox Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): NaHS_ox`,
  'Abundances (Grouped): NaHS_ox Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): NaHS_ox`,
  'Abundances (Grouped): NaHS_ox Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): NaHS_ox`,
  'Abundances (Grouped): NaHS_total Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): NaHS_total`,
  'Abundances (Grouped): NaHS_total Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): NaHS_total`,
  'Abundances (Grouped): NaHS_total Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): NaHS_total`,
  'Abundances (Grouped): NaHS_total Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): NaHS_total`,
  'Abundances (Grouped): NaHS_total Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): NaHS_total`,
  'WT ox/total Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_ox` /peptides_WT_plex1$`Abundances (Grouped): WT_total`,
  'WT ox/total Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_ox` /peptides_WT_plex2$`Abundances (Grouped): WT_total`, #row 70
  'WT ox/total Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_ox` /peptides_WT_plex3$`Abundances (Grouped): WT_total`,
  'WT ox/total Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_ox` /peptides_WT_plex4$`Abundances (Grouped): WT_total`,
  'WT ox/total Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_ox` /peptides_WT_plex5$`Abundances (Grouped): WT_total`,
  'WT ECC ox/total Plex1' = peptides_WT_plex1$`Abundances (Grouped): WT_ECC_ox` /peptides_WT_plex1$`Abundances (Grouped): WT_Ecc_total`,
  'WT ECC ox/total Plex2' = peptides_WT_plex2$`Abundances (Grouped): WT_ECC_ox` /peptides_WT_plex2$`Abundances (Grouped): WT_Ecc_total`,
  'WT ECC ox/total Plex3' = peptides_WT_plex3$`Abundances (Grouped): WT_ECC_ox` /peptides_WT_plex3$`Abundances (Grouped): WT_Ecc_total`,
  'WT ECC ox/total Plex4' = peptides_WT_plex4$`Abundances (Grouped): WT_ECC_ox` /peptides_WT_plex4$`Abundances (Grouped): WT_Ecc_total`,
  'WT ECC ox/total Plex5' = peptides_WT_plex5$`Abundances (Grouped): WT_ECC_ox` /peptides_WT_plex5$`Abundances (Grouped): WT_Ecc_total`,
  'mdx ox/total Plex1' = peptides_WT_plex1$`Abundances (Grouped): mdx_ox`/peptides_WT_plex1$`Abundances (Grouped): mdx_total`,
  'mdx ox/total Plex2' = peptides_WT_plex2$`Abundances (Grouped): mdx_ox`/peptides_WT_plex2$`Abundances (Grouped): mdx_total`, #row 80
  'mdx ox/total Plex3' = peptides_WT_plex3$`Abundances (Grouped): mdx_ox`/peptides_WT_plex3$`Abundances (Grouped): mdx_total`,
  'mdx ox/total Plex4' = peptides_WT_plex4$`Abundances (Grouped): mdx_ox`/peptides_WT_plex4$`Abundances (Grouped): mdx_total`,
  'mdx ox/total Plex5' = peptides_WT_plex5$`Abundances (Grouped): mdx_ox`/peptides_WT_plex5$`Abundances (Grouped): mdx_total`,
  'mdx(NaHS) ox/total Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): mdx_ox` /peptides_NaHS_plex1$`Abundances (Grouped): mdx_total`,
  'mdx(NaHS) ox/total Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): mdx_ox` /peptides_NaHS_plex2$`Abundances (Grouped): mdx_total`, 
  'mdx(NaHS) ox/total Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): mdx_ox` /peptides_NaHS_plex3$`Abundances (Grouped): mdx_total`,
  'mdx(NaHS) ox/total Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): mdx_ox` /peptides_NaHS_plex4$`Abundances (Grouped): mdx_total`,
  'mdx(NaHS) ox/total Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): mdx_ox` /peptides_NaHS_plex5$`Abundances (Grouped): mdx_total`,
  'ECC ox/total Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): ECC_ox` /peptides_NaHS_plex1$`Abundances (Grouped): ECC_total`,
  'ECC ox/total Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): ECC_ox` /peptides_NaHS_plex2$`Abundances (Grouped): ECC_total`, #row 90
  'ECC ox/total Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): ECC_ox` /peptides_NaHS_plex3$`Abundances (Grouped): ECC_total`,
  'ECC ox/total Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): ECC_ox` /peptides_NaHS_plex4$`Abundances (Grouped): ECC_total`,
  'ECC ox/total Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): ECC_ox` /peptides_NaHS_plex5$`Abundances (Grouped): ECC_total`,
  'NaHS ox/total Plex1' = peptides_NaHS_plex1$`Abundances (Grouped): NaHS_ox`/peptides_NaHS_plex1$`Abundances (Grouped): NaHS_total`,
  'NaHS ox/total Plex2' = peptides_NaHS_plex2$`Abundances (Grouped): NaHS_ox`/peptides_NaHS_plex2$`Abundances (Grouped): NaHS_total`,
  'NaHS ox/total Plex3' = peptides_NaHS_plex3$`Abundances (Grouped): NaHS_ox`/peptides_NaHS_plex3$`Abundances (Grouped): NaHS_total`,
  'NaHS ox/total Plex4' = peptides_NaHS_plex4$`Abundances (Grouped): NaHS_ox`/peptides_NaHS_plex4$`Abundances (Grouped): NaHS_total`,
  'NaHS ox/total Plex5' = peptides_NaHS_plex5$`Abundances (Grouped): NaHS_ox`/peptides_NaHS_plex5$`Abundances (Grouped): NaHS_total`, #row 98
  check.names = FALSE
)



#Change 0 values in WT ox/total values to 0.01 to allow for fold change calulation (prevent dividing by 0)
WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex1`[WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex1`==0]<- 0.01
WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex2`[WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex2`==0]<- 0.01
WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex3`[WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex3`==0]<- 0.01
WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex4`[WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex4`==0]<- 0.01
WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex5`[WT_NaHS_comb_abundance_ratios_1$`WT ox/total Plex5`==0]<- 0.01



## IF YOU ADD ANY ADDITIONAL COLUMNS INTO ABOVE DATAFRAME BE SURE TO CHANGE REFERENCE COLUMNS FOR MEAN CACLUATIONS (SEE BELOW)


mean_ox_total_abundance_ratios_1 <-
  data.frame(
    'Abundances (Grouped): WT (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(69:73)],),
    'Abundances (Grouped): WT_ECC (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(74:78)],),
    'Abundances (Grouped): mdx(wt) (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(79:83)],),
    'Abundances (Grouped): mdx(NaHS) (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(84:88)],),
    'Abundances (Grouped): mdx(combined) (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(79:88)],),
    'Abundances (Grouped): mdx_ECC (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(89:93)],),
    'Abundances (Grouped): mdx_NaHS (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(94:98)],),
    check.names = FALSE
  )


  mean_ox_total_abundance_ratios_2 <-
    data.frame(
    'WT_ECC/WT (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',
    'WT_ECC/WT log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',2),
    'mdx(wt)/WT (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',
    'mdx(wt)/WT (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',2),
    'mdx(NaHS)/WT (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',
    'mdx(NaHS)/WT (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',2),
    'mdx(combined)/WT (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',
    'mdx(combined)/WT (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',2),
    'mdx_ECC/mdx(combined) (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean',
    'mdx_ECC/mdx(combined) (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean',2),
    'mdx_ECC/WT_ECC (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean',
    'mdx_ECC/WT_ECC (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean',2),
    'mdx(wt)/WT_ECC (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean',
    'mdx(wt)/WT_ECC (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox/total) mean',2),
    'mdx_ECC/WT (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',
    'mdx_ECC/WT (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): WT (ox/total) mean',2),
    'mdx_NaHS/mdx_ECC (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean',
    'mdx_NaHS/mdx_ECC (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox/total) mean',2),
    'mdx_NaHS/mdx(combined) (ox/total)' = mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean',
    'mdx_NaHS/mdx(combined) (ox/total) log2' = log(mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox/total) mean'/mean_ox_total_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox/total) mean',2),
    check.names = FALSE
  )

  mean_ox_total_abundance_ratios <- cbind(mean_ox_total_abundance_ratios_1,mean_ox_total_abundance_ratios_2)
  
  
  
  ########################################################################
  ## calculate averages for ox_only data and additional ratios using ox only data
  # rm(
  #   mean_ox_only_abundance_ratios,
  #   mean_ox_only_norm_abundance_ratios_1,
  #   mean_ox_only_abundance_ratios_2
  #   )
 
mean_ox_only_abundance_ratios_1 <-
  data.frame(
    'Abundances (Grouped): WT (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(9:13)],),
    'Abundances (Grouped): WT_ECC (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(19:23)],),
    'Abundances (Grouped): mdx(wt) (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(29:33)],),
    'Abundances (Grouped): mdx(NaHS) (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(39:43)],),
    'Abundances (Grouped): mdx(combined) (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(29:33,39:43)],),
    'Abundances (Grouped): mdx_ECC (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(49:53)],),
    'Abundances (Grouped): mdx_NaHS (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(59:63)],),
    check.names = FALSE
  )

mean_ox_only_abundance_ratios_2 <-
  data.frame(
    'WT_ECC/WT (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',
    'WT_ECC/WT (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',2),
    'mdx(wt)/WT (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',
    'mdx(wt)/WT (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',2),
    'mdx(NaHS)/WT (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',
    'mdx(NaHS)/WT (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',2),
    'mdx(combined)/WT (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',
    'mdx(combined)/WT (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',2),
    'mdx_ECC/mdx(combined) (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean',
    'mdx_ECC/mdx(combined) (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean',2),
    'mdx_ECC/WT_ECC (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean',
    'mdx_ECC/WT_ECC (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean',2),
    'mdx(wt)/WT_ECC (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean',
    'mdx(wt)/WT_ECC (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(wt) (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT_ECC (ox only) mean',2),
    'mdx_ECC/WT (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',
    'mdx_ECC/WT (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): WT (ox only) mean',2),
    'mdx_NaHS/mdx_ECC (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean',
    'mdx_NaHS/mdx_ECC (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_ECC (ox only) mean',2),
    'mdx_NaHS/mdx(combined) (ox only)' = mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean',
    'mdx_NaHS/mdx(combined) (ox only) log2' = log(mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS (ox only) mean'/mean_ox_only_abundance_ratios_1$'Abundances (Grouped): mdx(combined) (ox only) mean',2),
    check.names = FALSE
  )

mean_ox_only_abundance_ratios <- cbind(mean_ox_only_abundance_ratios_1,mean_ox_only_abundance_ratios_2)


mean_total_only_abundance_ratios <-
  data.frame(
    'Abundances (Grouped): WT (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(14:18)],),
    'Abundances (Grouped): WT_ECC (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(24:28)],),
    'Abundances (Grouped): mdx(wt) (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(34:38)],),
    'Abundances (Grouped): mdx(NaHS) (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(44:48)],),
    'Abundances (Grouped): mdx_ECC (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(54:58)],),
    'Abundances (Grouped): mdx_NaHS (total only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_1[c(64:68)],),
    check.names = FALSE
  )

## IF YOU ADD ANY ADDITIONAL COLUMNS INTO ABOVE DATAFRAME BE SURE TO CHANGE REFERENCE COLUMNS FOR MEAN CACLUATIONS (SEE BELOW)
WT_NaHS_comb_abundance_ratios_2 <-
  cbind(
    WT_NaHS_comb_abundance_ratios_1,
    mean_ox_total_abundance_ratios,
    mean_ox_only_abundance_ratios,
    mean_total_only_abundance_ratios
  )



## Correct values from NaHS screen based on the difference between mdx(WT) and mdx(NaHS) 
## First ox/total normalized values and then ox_only normalized values



abundance_ratios_normalized_mdx <-
  data.frame(
    'WT ox/total norm Plex1' = WT_NaHS_comb_abundance_ratios_2$'WT ox/total Plex1',
    'WT ox/total norm Plex2' = WT_NaHS_comb_abundance_ratios_2$'WT ox/total Plex2' ,
    'WT ox/total norm Plex3' = WT_NaHS_comb_abundance_ratios_2$'WT ox/total Plex3' ,
    'WT ox/total norm Plex4' = WT_NaHS_comb_abundance_ratios_2$'WT ox/total Plex4' ,
    'WT ox/total norm Plex5' = WT_NaHS_comb_abundance_ratios_2$'WT ox/total Plex5' ,
    'WT ECC norm ox/total Plex1' = WT_NaHS_comb_abundance_ratios_2$'WT ECC ox/total Plex1' ,
    'WT ECC norm ox/total Plex2' = WT_NaHS_comb_abundance_ratios_2$'WT ECC ox/total Plex2' ,
    'WT ECC norm ox/total Plex3' = WT_NaHS_comb_abundance_ratios_2$'WT ECC ox/total Plex3' ,
    'WT ECC norm ox/total Plex4' = WT_NaHS_comb_abundance_ratios_2$'WT ECC ox/total Plex4' ,
    'WT ECC norm ox/total Plex5' = WT_NaHS_comb_abundance_ratios_2$'WT ECC ox/total Plex5' ,
    'mdx norm ox/total Plex1' = WT_NaHS_comb_abundance_ratios_2$'mdx ox/total Plex1',
    'mdx norm ox/total Plex2' = WT_NaHS_comb_abundance_ratios_2$'mdx ox/total Plex2', 
    'mdx norm ox/total Plex3' = WT_NaHS_comb_abundance_ratios_2$'mdx ox/total Plex3',
    'mdx norm ox/total Plex4' = WT_NaHS_comb_abundance_ratios_2$'mdx ox/total Plex4',
    'mdx norm ox/total Plex5' = WT_NaHS_comb_abundance_ratios_2$'mdx ox/total Plex5',
    'mdx(NaHS) norm ox/total Plex1' = WT_NaHS_comb_abundance_ratios_2$'mdx(NaHS) ox/total Plex1' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'mdx(NaHS) norm ox/total Plex2' = WT_NaHS_comb_abundance_ratios_2$'mdx(NaHS) ox/total Plex2' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'mdx(NaHS) norm ox/total Plex3' = WT_NaHS_comb_abundance_ratios_2$'mdx(NaHS) ox/total Plex3' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'mdx(NaHS) norm ox/total Plex4' = WT_NaHS_comb_abundance_ratios_2$'mdx(NaHS) ox/total Plex4' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'mdx(NaHS) norm ox/total Plex5' = WT_NaHS_comb_abundance_ratios_2$'mdx(NaHS) ox/total Plex5' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'ECC norm ox/total Plex1' = WT_NaHS_comb_abundance_ratios_2$'ECC ox/total Plex1' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'ECC norm ox/total Plex2' = WT_NaHS_comb_abundance_ratios_2$'ECC ox/total Plex2' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')), 
    'ECC norm ox/total Plex3' = WT_NaHS_comb_abundance_ratios_2$'ECC ox/total Plex3' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'ECC norm ox/total Plex4' = WT_NaHS_comb_abundance_ratios_2$'ECC ox/total Plex4' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'ECC norm ox/total Plex5' = WT_NaHS_comb_abundance_ratios_2$'ECC ox/total Plex5' + ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'NaHS norm ox/total Plex1' = WT_NaHS_comb_abundance_ratios_2$'NaHS ox/total Plex1'+ ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'NaHS norm ox/total Plex2' = WT_NaHS_comb_abundance_ratios_2$'NaHS ox/total Plex2'+ ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'NaHS norm ox/total Plex3' = WT_NaHS_comb_abundance_ratios_2$'NaHS ox/total Plex3'+ ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'NaHS norm ox/total Plex4' = WT_NaHS_comb_abundance_ratios_2$'NaHS ox/total Plex4'+ ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    'NaHS norm ox/total Plex5' = WT_NaHS_comb_abundance_ratios_2$'NaHS ox/total Plex5'+ ((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox/total) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox/total) mean')),
    
    'WT ox_only norm Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ox Plex1' ,
    'WT ox_only norm Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ox Plex2' ,
    'WT ox_only norm Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ox Plex3',
    'WT ox_only norm Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ox Plex4',
    'WT ox_only norm Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ox Plex5',
    'WT ECC norm ox_only Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ECC_ox Plex1',
    'WT ECC norm ox_only Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ECC_ox Plex2',
    'WT ECC norm ox_only Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ECC_ox Plex3',
    'WT ECC norm ox_only Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ECC_ox Plex4',
    'WT ECC norm ox_only Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): WT_ECC_ox Plex5' ,
    'mdx norm ox_only Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox Plex1',
    'mdx norm ox_only Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox Plex2', 
    'mdx norm ox_only Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox Plex3',
    'mdx norm ox_only Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox Plex4',
    'mdx norm ox_only Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox Plex5',
    'mdx(NaHS) norm ox_only Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox(NaHS) Plex1' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'mdx(NaHS) norm ox_only Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox(NaHS) Plex2' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'mdx(NaHS) norm ox_only Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox(NaHS) Plex3' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'mdx(NaHS) norm ox_only Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox(NaHS) Plex4' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'mdx(NaHS) norm ox_only Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx_ox(NaHS) Plex5' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'ECC norm ox_only Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): ECC_ox Plex1' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'ECC norm ox_only Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): ECC_ox Plex2' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'ECC norm ox_only Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): ECC_ox Plex3' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'ECC norm ox_only Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): ECC_ox Plex4' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'ECC norm ox_only Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): ECC_ox Plex5' + abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'NaHS norm ox_only Plex1' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): NaHS_ox Plex1'+ abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'NaHS norm ox_only Plex2' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): NaHS_ox Plex2'+ abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'NaHS norm ox_only Plex3' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): NaHS_ox Plex3'+ abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'NaHS norm ox_only Plex4' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): NaHS_ox Plex4'+ abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    'NaHS norm ox_only Plex5' = WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): NaHS_ox Plex5'+ abs((WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(wt) (ox only) mean' - WT_NaHS_comb_abundance_ratios_2$'Abundances (Grouped): mdx(NaHS) (ox only) mean')),
    check.names = FALSE
  )

# rm(WT_NaHS_comb_abundance_ratios_2) ## remove variable and rerun code

mean_mdx_combined_ox_total_1 <-
  data.frame(
    'mdx(combined)_plex1 norm (ox/total) mean' = rowMeans(abundance_ratios_normalized_mdx[c(11,16)],),
    'mdx(combined)_plex2 norm (ox/total) mean' = rowMeans(abundance_ratios_normalized_mdx[c(12,17)],),
    'mdx(combined)_plex3 norm (ox/total) mean' = rowMeans(abundance_ratios_normalized_mdx[c(13,18)],),
    'mdx(combined)_plex4 norm (ox/total) mean' = rowMeans(abundance_ratios_normalized_mdx[c(14,19)],),
    'mdx(combined)_plex5 norm (ox/total) mean' = rowMeans(abundance_ratios_normalized_mdx[c(15,20)],),
    'mdx(combined)_plex1 norm (ox_only) mean' = rowMeans(abundance_ratios_normalized_mdx[c(41,46)],),
    'mdx(combined)_plex2 norm (ox_only) mean' = rowMeans(abundance_ratios_normalized_mdx[c(42,47)],),
    'mdx(combined)_plex3 norm (ox_only) mean' = rowMeans(abundance_ratios_normalized_mdx[c(43,48)],),
    'mdx(combined)_plex4 norm (ox_only) mean' = rowMeans(abundance_ratios_normalized_mdx[c(44,49)],),
    'mdx(combined)_plex5 norm (ox_only) mean' = rowMeans(abundance_ratios_normalized_mdx[c(45,50)],),
    check.names = FALSE
  )

WT_NaHS_comb_abundance_ratios_3 <-
  cbind(
    WT_NaHS_comb_abundance_ratios_2,
    abundance_ratios_normalized_mdx,
    mean_mdx_combined_ox_total_1
  )

##### Calculate means and ratios using normalized values

mean_ox_total_norm_abundance_ratios_1 <-
  data.frame(
    'Abundances (Grouped): WT norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(159:163)],),
    'Abundances (Grouped): WT_ECC norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(164:168)],),
    'Abundances (Grouped): mdx(wt) norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(169:173)],),
    'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(174:178)],),
    'Abundances (Grouped): mdx(combined) norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(219:223)],),
    'Abundances (Grouped): mdx_ECC norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(179:183)],),
    'Abundances (Grouped): mdx_NaHS norm (ox/total) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(184:188)],),
    check.names = FALSE
  )



mean_ox_total_norm_abundance_ratios_2 <-
  data.frame(
    'WT_ECC/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'WT_ECC/WT norm log2 (ox/total)' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    'mdx(wt)/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'mdx(wt)/WT norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    'mdx(NaHS)/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'mdx(NaHS)/WT norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    'mdx(combined)/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'mdx(combined)/WT norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    'mdx_ECC/mdx(combined) norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',
    'mdx_ECC/mdx(combined) norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',2),
    'mdx_ECC/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'mdx_ECC/WT norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    'mdx_NaHS/WT norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',
    'mdx_NaHS/WT norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox/total) mean',2),
    
    'mdx_ECC/WT_ECC norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean',
    'mdx_ECC/WT_ECC norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean',2),
    'mdx(combined)/WT_ECC norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean',
    'mdx(combined)/WT_ECC norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox/total) mean',2),
    'mdx_NaHS/mdx_ECC norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean',
    'mdx_NaHS/mdx_ECC norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox/total) mean',2),
    'mdx_NaHS/mdx(combined) norm (ox/total)' = mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',
    'mdx_NaHS/mdx(combined) norm (ox/total) log2' = log(mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean'/mean_ox_total_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',2),
    check.names = FALSE
  )



mean_ox_total_norm_abundance_ratios <- cbind(mean_ox_total_norm_abundance_ratios_1,mean_ox_total_norm_abundance_ratios_2)
##good

# rm(
#   mean_ox_only_abundance_ratios,
#   mean_ox_only_norm_abundance_ratios_1,
#   mean_ox_only_abundance_ratios_2
#   )


mean_ox_only_norm_abundance_ratios_1 <-
  data.frame(
    'Abundances (Grouped): WT norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(189:193)],),
    'Abundances (Grouped): WT_ECC norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(194:198)],),
    'Abundances (Grouped): mdx(wt) norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(199:203)],),
    'Abundances (Grouped): mdx(NaHS) norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(204:208)],),
    'Abundances (Grouped): mdx(combined) norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(224:228)],),
    'Abundances (Grouped): mdx_ECC norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(209:213)],),
    'Abundances (Grouped): mdx_NaHS norm (ox only) mean' = rowMeans(WT_NaHS_comb_abundance_ratios_3[c(214:218)],),
    check.names = FALSE
  )

mean_ox_only_norm_abundance_ratios_2 <-
  data.frame(
    
    'WT_ECC/WT norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',
    'WT_ECC/WT norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',2),
    'mdx(wt)/WT norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',
    'mdx(wt)/WT norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',2),
    'mdx(NaHS)/WT norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',
    'mdx(NaHS)/WT norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',2),
    'mdx(combined)/WT norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',
    'mdx(combined)/WT norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(combined) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',2),
    'mdx_ECC/mdx(NaHS) norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean',
    'mdx_ECC/mdx(NaHS) norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean',2),
    'mdx_ECC/WT_ECC norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean',
    'mdx_ECC/WT_ECC norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean',2),
    'mdx(wt)/WT_ECC norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean',
    'mdx(wt)/WT_ECC norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(wt) norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT_ECC norm (ox only) mean',2),
    'mdx_ECC/WT norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',
    'mdx_ECC/WT norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): WT norm (ox only) mean',2),
    'mdx_NaHS/mdx_ECC norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean',
    'mdx_NaHS/mdx_ECC norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_ECC norm (ox only) mean',2),
    'mdx_NaHS/mdx(NaHS) norm (ox only)' = mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean',
    'mdx_NaHS/mdx(NaHS) norm (ox only) log2' = log(mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx_NaHS norm (ox only) mean'/mean_ox_only_norm_abundance_ratios_1$'Abundances (Grouped): mdx(NaHS) norm (ox only) mean',2),
    check.names = FALSE
  )

mean_ox_only_norm_abundance_ratios <- cbind(mean_ox_only_norm_abundance_ratios_1,mean_ox_only_norm_abundance_ratios_2)





# rm(WT_NaHS_comb_abundance_ratios)
WT_NaHS_comb_abundance_ratios <-
  cbind(
    WT_NaHS_comb_abundance_ratios_3,
    mean_ox_total_norm_abundance_ratios,
    mean_ox_only_norm_abundance_ratios
  )
WT_NaHS_comb_abundance_ratios <- WT_NaHS_comb_abundance_ratios[order(WT_NaHS_comb_abundance_ratios$`Master Protein Descriptions`),]


WT_NaHS_comb_abundance_ratios["120",'Gene_Symbol'] <- 'Cacna1s'
WT_NaHS_comb_abundance_ratios["121",'Gene_Symbol'] <- 'Cacna1s'
WT_NaHS_comb_abundance_ratios["122",'Gene_Symbol'] <- 'Cacna1s'



# 
# library("writexl")
# write_xlsx(WT_NaHS_comb_abundance_ratios,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.11_WT_NaHS_comb_abundance_ratios.xlsx")
# # #
# #################################################################################

#######################################################################################
## Create function to generate pvals

pval_fun <- function(reference_df, output_df, numrow, grp1col1, grp1col2, grp2col1, grp2col2){
  output <-
    plyr::adply(
      reference_df,
      .margins = 1,
      .fun = t_test,
      grp1 = c(grp1col1:grp1col2),
      grp2 = c(grp2col1:grp2col2)
    ) %>% as.tibble()
  output_df = data.frame(
    'pvals' = output$p_val,
    'log_pval' = -1 * log10(output$`p_val`),
    'Adjusted pvalues' = p.adjust(output$`p_val`, method = "fdr", n = numrow), #adjust for changing datasets
    'Log10 Adjusted pvalues' = -1 * log10(p.adjust(output$`p_val`, method = "fdr", n = numrow))
  )
}

#######################################################################################
## Create function to generate volcano plots

volcano_fun_raw_pval <- function(volcano_df){
  volcano_df %>%
    # Add a threhold for significant observations
    mutate(threshold =
             if_else(`log_fc` >= 0 & `log_pval` >= 1.3, "A",
                     if_else(`log_fc` <= -0 & `log_pval` >= 1.3,"B","C"))) %>%
    # if_else(`log_fc` <= 1 & `log_pval` <= 1.3 | `log_fc` >= -1 & `log_pval` <= 1.3,"C")))) %>%
    # Plot with points coloured according to the threshold
    ggplot(aes(`log_fc`, `log_pval`, colour = threshold)) +
    geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
    # Add dotted lines to indicate the threshold, semi-transparent
    geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
    # x axis limit
    xlim(-12, 12) +
    # y axis limits
    ylim(0, 10) +
    # Set the colour of the points
    scale_colour_manual(values = c("A"= "green", "B"= "red", "C" = "black")) +
    xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
    theme_minimal() + # Set the theme
    theme(legend.position="none") # Hide the legendion="none") # Hide the legend
}

#######################################################################################
## Create function to generate data files from volcano plots

create_sig_files <-
  function(
    volcano_file_name,
    master_file_name,
    updated_master_file_name,
    sigUP_file_name,
    sigDOWN_file_name,
    allsig_file_name,
    nonsig_file_name
  )
  {
    combo_file_name <-
      volcano_file_name %>%
      mutate(
        threshold = 
          if_else(`log_fc` >= 0 & `log_pval` >= 1.3, "UP",
                  if_else(`log_fc` <= -0 & `log_pval` >= 1.3,"DOWN","NotSig")))
    
    
    updated_master_file <- cbind(master_file_name, combo_file_name)
    # file_name_var <- deparse(substitute(updated_master_file_name))
    write_xlsx(updated_master_file, paste0("/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/Contractile_Pep_volcano_data/", deparse(substitute(updated_master_file_name)), ".xlsx"))
    sigUP_file <- updated_master_file[updated_master_file$threshold == 'UP', ]
    write_xlsx(sigUP_file, paste0("/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/Contractile_Pep_volcano_data/", deparse(substitute(sigUP_file_name)), ".xlsx"))
    sigDOWN_file <- updated_master_file[updated_master_file$threshold == 'DOWN', ]
    write_xlsx(sigDOWN_file, paste0("/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/Contractile_Pep_volcano_data/", deparse(substitute(sigDOWN_file_name)), ".xlsx"))
    allsig_file <- updated_master_file[updated_master_file$threshold == 'DOWN' | updated_master_file$threshold == 'UP', ]
    write_xlsx(allsig_file, paste0("/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/Contractile_Pep_volcano_data/", deparse(substitute(allsig_file_name)), ".xlsx"))
    nonsig_file <- updated_master_file[updated_master_file$threshold == 'NotSig', ]
    write_xlsx(nonsig_file, paste0("/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/Contractile_Pep_volcano_data/", deparse(substitute(nonsig_file_name)), ".xlsx"))
  }
#################################################################################
## Calculate pvals from ttest between WT and WT ECC for all peptides 
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_WT_ECC <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(159:163),
    grp2 = c(164:168)
  ) %>% as_tibble()
pvals_man_ratio_WT_ECC <-
  data.frame('WT vs. WT ECC pvals' = pvals_man_ratio_WT_ECC$p_val, check.names = FALSE)
pvals_man_ratio_WT_ECC$'WT vs. WT ECC log_pval' <-
  -1 * log10(pvals_man_ratio_WT_ECC$`WT vs. WT ECC pvals`)
pvals_man_ratio_WT_ECC$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_WT_ECC$`WT vs. WT ECC pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_WT_ECC$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_WT_ECC$`Adjusted pvalues`)

## Create dataframe for volcano plot for WT/ECC data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_WT_ECC_raw_pval <-
  data.frame(
    'log_fc ECC_WT' = WT_NaHS_comb_abundance_ratios$'WT_ECC/WT norm log2 (ox/total)',
    'log_pval ECC_WT' = pvals_man_ratio_WT_ECC$`WT vs. WT ECC log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_WT_ECC_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc ECC_WT` >= 0 & `log_pval ECC_WT` >= 1.3 |
                               `log_fc ECC_WT` <= -0 & `log_pval ECC_WT` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc ECC_WT`, `log_pval ECC_WT`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_ECC_WT_1 <-
  volcano_data_WT_ECC_raw_pval %>% 
  mutate(
    threshold_ECC_WT = if_else(
      `log_fc ECC_WT` >= 0 & 
        `log_pval ECC_WT` >= 1.3 |
        `log_fc ECC_WT` <= -0 &
        `log_pval ECC_WT` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_WT_ECC <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_ECC_WT_1)
sig_pep_raw_mancalc_WT_ECC <-
  peptides_with_raw_sig_pep_mancalc_WT_ECC[peptides_with_raw_sig_pep_mancalc_WT_ECC$threshold_ECC_WT == 'A', ]
# library("writexl")
# write_xlsx(sig_pep_raw_mancalc_WT_ECC,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_WT_ECC.xlsx")
# write_xlsx(sig_pep_raw_mancalc_ECC_WT_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_ECC_WT_1.xlsx")



#################################################################################
#################################################################################
## Calculate pvals from ttest between mdx and mdx ECC for all peptides 
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_mdx_ECC <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(219:223),
    grp2 = c(179:183)
  ) %>% as.tibble()
pvals_man_ratio_mdx_ECC <-
  data.frame('mdx vs. mdx ECC pvals' = pvals_man_ratio_mdx_ECC$p_val, check.names = FALSE)
pvals_man_ratio_mdx_ECC$'mdx vs. mdx ECC log_pval' <-
  -1 * log10(pvals_man_ratio_mdx_ECC$`mdx vs. mdx ECC pvals`)
pvals_man_ratio_mdx_ECC$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_mdx_ECC$`mdx vs. mdx ECC pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_mdx_ECC$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_mdx_ECC$`Adjusted pvalues`)

## Create dataframe for volcano plot for WT/ECC data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_mdx_ECC_raw_pval <-
  data.frame(
    'log_fc ECC_mdx' = WT_NaHS_comb_abundance_ratios$'mdx_ECC/mdx(combined) norm (ox/total) log2',
    'log_pval ECC_mdx' = pvals_man_ratio_mdx_ECC$`mdx vs. mdx ECC log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_mdx_ECC_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc ECC_mdx` >= 0 & `log_pval ECC_mdx` >= 1.3 |
                               `log_fc ECC_mdx` <= -0 & `log_pval ECC_mdx` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc ECC_mdx`, `log_pval ECC_mdx`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_ECC_mdx_1 <-
  volcano_data_mdx_ECC_raw_pval %>% 
  mutate(
    threshold_ECC_mdx = if_else(
      `log_fc ECC_mdx` >= 0 & 
        `log_pval ECC_mdx` >= 1.3 |
        `log_fc ECC_mdx` <= -0 &
        `log_pval ECC_mdx` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_mdx_ECC <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_ECC_mdx_1)
sig_pep_raw_mancalc_mdx_ECC <-
  peptides_with_raw_sig_pep_mancalc_mdx_ECC[peptides_with_raw_sig_pep_mancalc_mdx_ECC$threshold_ECC_mdx == 'A', ]
sig_pep_raw_mancalc_mdx_ECC <- drop_na(sig_pep_raw_mancalc_mdx_ECC)

# library("writexl")
# # write_xlsx(pvals_man_ratio_mdx_ECC,"Desktop/mdx_ECC_NaHS_R_Files/mdx_ECC_pvals.xlsx")
# # # 
# write_xlsx(sig_pep_raw_mancalc_mdx_ECC,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_mdx_ECC.xlsx")
# write_xlsx(sig_pep_raw_mancalc_ECC_mdx_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-mdx_ECC_volcano_data.xlsx")





#################################################################################
## Calculate pvals from ttest between mdx ECC and mdx NaHS for all peptides 
## using ox_total values normalized to mdx(NaHS)

## Calculate pvals
pvals_man_ratio_NaHS_ECC_ox_total <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(184:188),
    grp2 = c(179:183)
  ) %>% as.tibble()
pvals_man_ratio_NaHS_ECC_ox_total <-
  data.frame('mdx NaHS vs. mdx ECC pvals' = pvals_man_ratio_NaHS_ECC_ox_total$p_val, check.names = FALSE)
pvals_man_ratio_NaHS_ECC_ox_total$'mdx NaHS vs. mdx ECC log_pval' <-
  -1 * log10(pvals_man_ratio_NaHS_ECC_ox_total$`mdx NaHS vs. mdx ECC pvals`)
pvals_man_ratio_NaHS_ECC_ox_total$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_NaHS_ECC_ox_total$`mdx NaHS vs. mdx ECC pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_NaHS_ECC_ox_total$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_NaHS_ECC_ox_total$`Adjusted pvalues`)

## Create dataframe for volcano plot for WT/ECC data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_NaHS_ECC_raw_pval_ox_total <-
  data.frame(
    'log_fc NaHS_ECC' = WT_NaHS_comb_abundance_ratios$'mdx_NaHS/mdx_ECC norm (ox/total) log2',
    'log_pval NaHS_ECC' = pvals_man_ratio_NaHS_ECC_ox_total$'mdx NaHS vs. mdx ECC log_pval', 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_NaHS_ECC_raw_pval_ox_total %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc NaHS_ECC` >= 0 & `log_pval NaHS_ECC` >= 1.3 |
                               `log_fc NaHS_ECC` <= -0 & `log_pval NaHS_ECC` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc NaHS_ECC`, `log_pval NaHS_ECC`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_NaHS_ECC_1 <-
  volcano_data_NaHS_ECC_raw_pval_ox_total %>% 
  mutate(
    threshold_NaHS_mdx = if_else(
      `log_fc NaHS_ECC` >= 0 & 
        `log_pval NaHS_ECC` >= 1.3 |
        `log_fc NaHS_ECC` <= -0 &
        `log_pval NaHS_ECC` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_NaHS_ECC <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_NaHS_ECC_1)
sig_pep_NaHS_ECC_ox_total <-
  peptides_with_raw_sig_pep_NaHS_ECC[peptides_with_raw_sig_pep_NaHS_ECC$threshold_NaHS_mdx == 'A', ]

# library("writexl")
# write_xlsx(sig_pep_NaHS_ECC_ox_total,"/Users/User/Desktop/WT_NaHS_combo_analysis/sig_pep_NaHS_ECC_ox_total.xlsx")
# write_xlsx(sig_pep_raw_NaHS_ECC_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2022.10.28-sig_pep_raw_NaHS_ECC_1.xlsx")
# 


#######################################################################################
#######################################################################################
## generate data for mdxNaHS vs mdxECC
#WTECC = 164:168
#mdxECC = 179:183
#mdxNaHS = 184:188
mdxNaHS_vs_mdxECC_abs_pvals <- pval_fun(WT_NaHS_comb_abundance_ratios, mdxNaHS_vs_mdxECC_pvals,  610, 184,188, 179,183 )

volcano_mdxNaHSvsmdxECC <-
  data.frame('log_fc' = WT_NaHS_comb_abundance_ratios$`mdx_NaHS/mdx_ECC norm (ox/total) log2`,
             'log_pval' = mdxNaHS_vs_mdxECC_abs_pvals$log_pval,
             'Log10_Adjusted_pvalues' = mdxNaHS_vs_mdxECC_abs_pvals$Log10.Adjusted.pvalues,
             check.names = FALSE
  )

volcano_fun_raw_pval(volcano_mdxNaHSvsmdxECC)

library("writexl")
create_sig_files(
  volcano_mdxNaHSvsmdxECC,
  WT_NaHS_comb_abundance_ratios,
  mdxNaHSvsmdxECC_Data_File_wSigpep,
  mdxNaHSvsmdxECC_sigUP_pep,
  mdxNaHSvsmdxECC_sigDOWN_pep,
  mdxNaHSvsmdxECC_allsig_pep,
  mdxNaHSvsmdxECC_nonsig_pep
)

#################################################################################
#################################################################################
## Calculate pvals from ttest between WT and mdx for all peptides from WT screen
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_mdx_WT <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(219:223),
    grp2 = c(159:163)
  ) %>% as.tibble()
pvals_man_ratio_mdx_WT <-
  data.frame('mdx vs. WT pvals' = pvals_man_ratio_mdx_WT$p_val, check.names = FALSE)
pvals_man_ratio_mdx_WT$'mdx vs. WT log_pval' <-
  -1 * log10(pvals_man_ratio_mdx_WT$`mdx vs. WT pvals`)
pvals_man_ratio_mdx_WT$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_mdx_WT$`mdx vs. WT pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_mdx_WT$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_mdx_WT$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_mdx_WT_raw_pval <-
  data.frame(
    'log_fc mdx_WT' = WT_NaHS_comb_abundance_ratios$'mdx(combined)/WT norm (ox/total) log2',
    'log_pval mdx_WT' = pvals_man_ratio_mdx_WT$`mdx vs. WT log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_mdx_WT_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc mdx_WT` >= 0 & `log_pval mdx_WT` >= 1.3 |
                               `log_fc mdx_WT` <= -0 & `log_pval mdx_WT` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc mdx_WT`, `log_pval mdx_WT`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_mdx_WT_1 <-
  volcano_data_mdx_WT_raw_pval %>% 
  mutate(
    threshold_mdx_WT = if_else(
      `log_fc mdx_WT` >= 0 & 
        `log_pval mdx_WT` >= 1.3 |
        `log_fc mdx_WT` <= -0 &
        `log_pval mdx_WT` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_mdx_WT <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_mdx_WT_1)
sig_pep_raw_mancalc_mdx_WT <-
  peptides_with_raw_sig_pep_mancalc_mdx_WT[peptides_with_raw_sig_pep_mancalc_mdx_WT$threshold_mdx_WT == 'A', ]

# library("writexl")
# write_xlsx(sig_pep_raw_mancalc_mdx_WT,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_mdx_WT.xlsx")
# write_xlsx(sig_pep_raw_mancalc_mdx_WT_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-WT_vs_mdx(com)_volcano_data.xlsx")

#################################################################################
#################################################################################
## Calculate pvals from ttest between WT ECC and mdx for all peptides from WT screen
## using ox/total values normalized to mdx

# ## Calculate pvals
pvals_man_ratio_mdx_WTECC <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(169:173),
    grp2 = c(164:168)
  ) %>% as.tibble()
pvals_man_ratio_mdx_WTECC <-
  data.frame('mdx vs. WT ECC pvals' = pvals_man_ratio_mdx_WTECC$p_val, check.names = FALSE)
pvals_man_ratio_mdx_WTECC$'mdx vs. WT ECC log_pval' <-
  -1 * log10(pvals_man_ratio_mdx_WTECC$`mdx vs. WT ECC pvals`)
pvals_man_ratio_mdx_WTECC$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_mdx_WTECC$`mdx vs. WT ECC pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_mdx_WTECC$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_mdx_WTECC$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_mdx_WTECC_raw_pval <-
  data.frame(
    'log_fc mdx_WT_ECC' = WT_NaHS_comb_abundance_ratios$'mdx(combined)/WT_ECC norm (ox/total) log2',
    'log_pval mdx_WT_ECC' = pvals_man_ratio_mdx_WTECC$`mdx vs. WT ECC log_pval`,
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_mdx_WTECC_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc mdx_WT_ECC` >= 0 & `log_pval mdx_WT_ECC` >= 1.3 |
                               `log_fc mdx_WT_ECC` <= -0 & `log_pval mdx_WT_ECC` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc mdx_WT_ECC`, `log_pval mdx_WT_ECC`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -1, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_mdx_WT_ECC_1 <-
  volcano_data_mdx_WTECC_raw_pval %>%
  mutate(
    threshold_mdx_WT_ECC = if_else(
      `log_fc mdx_WT_ECC` >= 0 &
        `log_pval mdx_WT_ECC` >= 1.3 |
        `log_fc mdx_WT_ECC` <= -0 &
        `log_pval mdx_WT_ECC` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_mdx_WT_ECC <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_mdx_WT_ECC_1)
sig_pep_raw_mancalc_mdx_WT_ECC <-
  peptides_with_raw_sig_pep_mancalc_mdx_WT_ECC[peptides_with_raw_sig_pep_mancalc_mdx_WT_ECC$threshold_mdx_WT_ECC == 'A', ]

# library("writexl")
# write_xlsx(sig_pep_raw_mancalc_mdx_WT_ECC,"Desktop/WT_ECC_mdx_R_Files/sig_pep_raw_mancalc_mdx_WTECC.xlsx")
# write_xlsx(sig_pep_raw_mancalc_mdx_WT_ECC_1,"Desktop/WT_ECC_mdx_R_Files/WT_ECC_vs_mdx(wt)_volcano_data.xlsx")


#################################################################################
#################################################################################
## Calculate pvals from ttest between mdx_ECC_NaHS and mdx for all peptides from WT screen
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_NaHS_mdx <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(184:188),
    grp2 = c(219:223)
  ) %>% as.tibble()
pvals_man_ratio_NaHS_mdx <-
  data.frame('NaHS vs. mdx pvals' = pvals_man_ratio_NaHS_mdx$p_val, check.names = FALSE)
pvals_man_ratio_NaHS_mdx$'NaHS vs. mdx log_pvals' <-
  -1 * log10(pvals_man_ratio_NaHS_mdx$'NaHS vs. mdx pvals')
pvals_man_ratio_NaHS_mdx$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_NaHS_mdx$'NaHS vs. mdx pvals',
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_NaHS_mdx$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_NaHS_mdx$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_NaHS_mdx_raw_pval <-
  data.frame(
    'log_fc NaHS_mdx' = WT_NaHS_comb_abundance_ratios$'mdx_NaHS/mdx(combined) norm (ox/total) log2', 
    'log_pval NaHS_mdx' = pvals_man_ratio_NaHS_mdx$`NaHS vs. mdx log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_NaHS_mdx_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc NaHS_mdx` >= 0 & `log_pval NaHS_mdx` >= 1.3 |
                               `log_fc NaHS_mdx` <= -0 & `log_pval NaHS_mdx` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc NaHS_mdx`, `log_pval NaHS_mdx`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_NaHS_mdx_1 <-
  volcano_data_NaHS_mdx_raw_pval %>% 
  mutate(
    threshold_NaHS_mdx = if_else(
      `log_fc NaHS_mdx` >= 0 & 
        `log_pval NaHS_mdx` >= 1.3 |
        `log_fc NaHS_mdx` <= -0 &
        `log_pval NaHS_mdx` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_NaHS_mdx <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_NaHS_mdx_1)
sig_pep_raw_mancalc_NaHS_mdx <-
 peptides_with_raw_sig_pep_mancalc_NaHS_mdx[peptides_with_raw_sig_pep_mancalc_NaHS_mdx$threshold_NaHS_mdx == 'A', ]

# # library("writexl")
# write_xlsx(sig_pep_raw_mancalc_NaHS_mdx,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_NaHS_mdx.xlsx")
# write_xlsx(sig_pep_raw_mancalc_NaHS_mdx_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-NaHS_vs_mdx(NaHS)_volcano_data.xlsx")


#################################################################################
#################################################################################
## Calculate pvals from ttest between WT and mdx_ECC for all peptides from WT screen
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_mdxECC_WT <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(179:183),
    grp2 = c(159:163)
  ) %>% as.tibble()
pvals_man_ratio_mdxECC_WT <-
  data.frame('mdxECC vs. WT pvals' = pvals_man_ratio_mdxECC_WT$p_val, check.names = FALSE)
pvals_man_ratio_mdxECC_WT$'mdxECC vs. WT log_pval' <-
  -1 * log10(pvals_man_ratio_mdxECC_WT$`mdxECC vs. WT pvals`)
pvals_man_ratio_mdxECC_WT$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_mdxECC_WT$`mdxECC vs. WT pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_mdxECC_WT$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_mdxECC_WT$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_mdxECC_WT_raw_pval <-
  data.frame(
    'log_fc mdxECC_WT' = WT_NaHS_comb_abundance_ratios$'mdx_ECC/WT norm (ox/total) log2',
    'log_pval mdxECC_WT' = pvals_man_ratio_mdxECC_WT$`mdxECC vs. WT log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_mdxECC_WT_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc mdxECC_WT` >= 0 & `log_pval mdxECC_WT` >= 1.3 |
                               `log_fc mdxECC_WT` <= -0 & `log_pval mdxECC_WT` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc mdxECC_WT`, `log_pval mdxECC_WT`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_mdxECC_WT_1 <-
  (volcano_data_mdxECC_WT_raw_pval %>% 
  mutate(
    threshold_mdxECC_WT = if_else(
      `log_fc mdxECC_WT` >= 0 & 
        `log_pval mdxECC_WT` >= 1.3 |
        `log_fc mdxECC_WT` <= -0 &
        `log_pval mdxECC_WT` >= 1.3,
      "A",
      "B"
    )
  ))
peptides_with_raw_sig_pep_mancalc_mdxECC_WT <-
  cbind(WT_NaHS_comb_abundance_ratios,
       (sig_pep_raw_mancalc_mdxECC_WT_1))
sig_pep_raw_mancalc_mdxECC_WT <-
  peptides_with_raw_sig_pep_mancalc_mdxECC_WT[peptides_with_raw_sig_pep_mancalc_mdxECC_WT$threshold_mdxECC_WT == 'A', ]

# # library("writexl")
# write_xlsx(sig_pep_raw_mancalc_mdxECC_WT,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_mdxECC_WT.xlsx")
# write_xlsx(sig_pep_raw_mancalc_mdxECC_WT_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-WT_vs_mdxECC_volcano_data.xlsx")

#################################################################################
#################################################################################
## Calculate pvals from ttest between WT and mdx_ECC_NaHS for all peptides from WT screen
## using ox/total values normalized to mdx

## Calculate pvals
pvals_man_ratio_NaHSECC_WT <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(159:163),
    grp2 = c(184:188)
  ) %>% as.tibble()
pvals_man_ratio_NaHSECC_WT <-
  data.frame('NaHSECC vs. WT pvals' = pvals_man_ratio_NaHSECC_WT$p_val, check.names = FALSE)
pvals_man_ratio_NaHSECC_WT$'NaHSECC vs. WT log_pval' <-
  -1 * log10(pvals_man_ratio_NaHSECC_WT$`NaHSECC vs. WT pvals`)
pvals_man_ratio_NaHSECC_WT$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_NaHSECC_WT$`NaHSECC vs. WT pvals`,
           method = "fdr",
           n = 610) #adjust for changing datasets
pvals_man_ratio_NaHSECC_WT$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_NaHSECC_WT$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_NaHSECC_WT_raw_pval <-
  data.frame(
    'log_fc NaHSECC_WT' = WT_NaHS_comb_abundance_ratios$'mdx_NaHS/WT norm (ox/total) log2',
    'log_pval NaHSECC_WT' = pvals_man_ratio_NaHSECC_WT$`NaHSECC vs. WT log_pval`, 
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_NaHSECC_WT_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc NaHSECC_WT` >= 0 & `log_pval NaHSECC_WT` >= 1.3 |
                               `log_fc NaHSECC_WT` <= -0 & `log_pval NaHSECC_WT` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc NaHSECC_WT`, `log_pval NaHSECC_WT`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_NaHSECC_WT_1 <-
  volcano_data_NaHSECC_WT_raw_pval %>% 
  mutate(
    threshold_NaHSECC_WT = if_else(
      `log_fc NaHSECC_WT` >= 0 & 
        `log_pval NaHSECC_WT` >= 1.3 |
        `log_fc NaHSECC_WT` <= -0 &
        `log_pval NaHSECC_WT` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_NaHSECC_WT <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_NaHSECC_WT_1)
sig_pep_raw_mancalc_NaHSECC_WT <-
  peptides_with_raw_sig_pep_mancalc_NaHSECC_WT[peptides_with_raw_sig_pep_mancalc_NaHSECC_WT$threshold_NaHSECC_WT == 'A', ]

# # library("writexl")
# write_xlsx(sig_pep_raw_mancalc_NaHSECC_WT,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-sig_pep_raw_mancalc_NaHSECC_WT.xlsx")
# write_xlsx(sig_pep_raw_mancalc_NaHSECC_WT_1,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.4-WT_vs_NaHSECC_volcano_data.xlsx")

#################################################################################
#################################################################################
## Calculate pvals from ttest between WT ECC and mdx ECC for common (256) significant peptides between WTECC:WT and mdx:WT screen
## using ox/total values normalized to mdx

# ## Calculate pvals
pvals_man_ratio_mdxECC_WTECC <-
  plyr::adply(
    WT_NaHS_comb_abundance_ratios,
    .margins = 1,
    .fun = t_test,
    grp1 = c(179:183), #mdxECC
    grp2 = c(164:168) #WTECC
  ) %>% as.tibble()
pvals_man_ratio_mdxECC_WTECC <-
  data.frame('mdx ECC vs. WT ECC pvals' = pvals_man_ratio_mdxECC_WTECC$p_val, check.names = FALSE)
pvals_man_ratio_mdxECC_WTECC$'mdx ECC vs. WT ECC pvals' <-
  -1 * log10(pvals_man_ratio_mdxECC_WTECC$`mdx ECC vs. WT ECC pvals`)
pvals_man_ratio_mdxECC_WTECC$'Adjusted pvalues' <-
  p.adjust(pvals_man_ratio_mdxECC_WTECC$`mdx ECC vs. WT ECC pvals`,
           method = "fdr",
           n = 758) #adjust for changing datasets
pvals_man_ratio_mdxECC_WTECC$'Log10 Adjusted pvalues' <-
  -1 * log10(pvals_man_ratio_mdxECC_WTECC$`Adjusted pvalues`)

## Create dataframe for volcano plot for mdx/WT data with raw pvalues using MANUALLY CALCULATED VALUES
volcano_data_mdxECC_WTECC_raw_pval <-
  data.frame(
    'log_fc mdxECC_WTECC' = WT_NaHS_comb_abundance_ratios$'mdx_ECC/WT_ECC norm (ox/total) log2',
    'log_pval mdxECC_WTECC' = pvals_man_ratio_mdxECC_WTECC$`mdx ECC vs. WT ECC pvals`,
    check.names = FALSE
  )

## Volcano plot for raw pvals
volcano_data_mdxECC_WTECC_raw_pval %>%
  # Add a threhold for significant observations
  mutate(threshold = if_else(`log_fc mdxECC_WTECC` >= 0 & `log_pval mdxECC_WTECC` >= 1.3 |
                               `log_fc mdxECC_WTECC` <= -0 & `log_pval mdxECC_WTECC` >= 1.3,"A", "B")) %>%
  # Plot with points coloured according to the threshold
  ggplot(aes(`log_fc mdxECC_WTECC`, `log_pval mdxECC_WTECC`, colour = threshold)) +
  geom_point(alpha = 0.5) + # Alpha sets the transparency of the points
  # Add dotted lines to indicate the threshold, semi-transparent
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -0, linetype = 2, alpha = 0.5) +
  # x axis limits
  xlim(-5, 5) +
  # y axis limits
  ylim(0, 7) +
  # Set the colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10(raw p-value)") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend

sig_pep_raw_mancalc_mdxECC_WTECC_1 <-
  volcano_data_mdxECC_WTECC_raw_pval %>%
  mutate(
    threshold_mdx_WT_ECC = if_else(
      `log_fc mdxECC_WTECC` >= 0 &
        `log_pval mdxECC_WTECC` >= 1.3 |
        `log_fc mdxECC_WTECC` <= -0 &
        `log_pval mdxECC_WTECC` >= 1.3,
      "A",
      "B"
    )
  )
peptides_with_raw_sig_pep_mancalc_mdxECC_WTECC <-
  cbind(WT_NaHS_comb_abundance_ratios,
        sig_pep_raw_mancalc_mdxECC_WTECC_1)
sig_pep_raw_mancalc_mdx_WT_ECC <-
  peptides_with_raw_sig_pep_mancalc_mdxECC_WTECC[peptides_with_raw_sig_pep_mancalc_mdxECC_WTECC$threshold_mdx_WT_ECC == 'A', ]

# library("writexl")
# write_xlsx(sig_pep_raw_mancalc_mdx_WT_ECC_256,"/Users/User/Desktop/WT_ECC_mdx_R_Files/sig_pep_raw_mancalc_mdx_WTECC.xlsx")
# write_xlsx(sig_pep_raw_mancalc_mdxECC_WTECC_256_1,"/Users/User/Desktop/WT_ECC_mdx_R_Files/WT_ECC_vs_mdx(wt)_volcano_data.xlsx")





#################################################################################
#################################################################################
#################################################################################
## Calculate peptide overlap between WT and mdx response to ECC

## calculate overlap across all combinations of 6plex 
sig_pep_overlap_sequence <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_ECC_sig_pep = sig_pep_raw_mancalc_WT_ECC$'Peptide Sequence',
      mdx_ECC_sig_pep = sig_pep_raw_mancalc_mdx_ECC$'Peptide Sequence'
    )
    ))

sig_pep_overlap_geneID <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_ECC_sig_pep = sig_pep_raw_mancalc_WT_ECC$'Gene Symbol',
      mdx_ECC_sig_pep = sig_pep_raw_mancalc_mdx_ECC$'Gene Symbol'
    )
  ))

library(VennDiagram) 
v1 <- venn.diagram(
  list(
    WT = sig_pep_raw_mancalc_WT_ECC$'Peptide Sequence',
    mdx = sig_pep_raw_mancalc_mdx_ECC$'Peptide Sequence'
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  col = "black",
  fill = c("dodgerblue",
           "seagreen3"
           ),
  alpha = 0.50,
  cat.pos = c(270,90),
  cat.dist = c(0.5,0.5),
  cex = c(
    1
  ),
  ##to eliminate one plex, comment out 16 cex parameters
  cat.col = c("dodgerblue",
              "seagreen3"
              ),
  cat.cex = 1,
  cat.fontface = "bold",
  margin = 0.1,
  # inverted = TRUE
)

grid.newpage()
grid.draw(v1)


sig_pep_overlap_WT_mdx <-
  WT_NaHS_comb_abundance_ratios[WT_NaHS_comb_abundance_ratios$`Peptide Sequence` %in% sig_pep_overlap_sequence$Overlap,]

library("writexl")
write_xlsx(sig_pep_overlap_WT_mdx,"Desktop/WT_NaHS_combo_analysis/sig_pep_overlap_WT_mdx.xlsx")

#################################################################################
#################################################################################
#################################################################################
## Calculate peptide overlap between WTECC:WT, mdxECC:WT, and NaHSECC:WT

# install.packages("viridis")  # Install

png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 1200)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library("viridis")           # Load
color_palette = viridis(6)
library(VennDiagram) 
v1 <- venn.diagram(
  list(
    'mdx:WT' = na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),
    'mdx+ECC:WT' = na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence'),
    'mdx+ECC+NaHS:WT' = na.omit(sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence')
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  col = "black",
  fill = color_palette[c(2,3,4)],
  # fill = c("dodgerblue",
  #          "seagreen3",
  #          "goldenrod1"
  # ),
  alpha = 0.50,
  cat.pos = c(-20, 0, 180),
  cat.dist = c(0.06, 0.05, 0.05),
  cex = 1.5,
  cat.cex = 1.5,
  cat.fontface = "bold",
  margin = 0.1,
  # inverted = TRUE
)
grid.newpage()
grid.draw(v1)
dev.off()



Venn_regions_overlap_1 <- calculate.overlap(list(
  'mdx:WT' = na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),
  'mdx+ECC:WT' = na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence'),
  'mdx+ECC+NaHS:WT' = na.omit(sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence')
))

WTECC_mdxECC_NaHSECC_sequence_overlap <-
  sig_pep_raw_mancalc_WT_ECC[sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence` %in% Venn_regions_overlap_1$a5,]
WTECC_NaHSECC_sequence_overlap <-
  sig_pep_raw_mancalc_WT_ECC[sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence` %in% Venn_regions_overlap_1$a4,]
mdx_ECC_NaHS_unique_sequence_overlap <-
  sig_pep_raw_mancalc_NaHSECC_WT[sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence' %in% Venn_regions_overlap_mdxgroups_vs_WT$a7,]


##Calculate the mean difference between ECC condition and baseline of significant peptides

## calculated delta for WTECC, mdxECC, NaHSECC overlap
delta_WTECC_mdxECC_NaHSECC <- data.frame(
  "Delta WT_WTECC" = WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): WT_ECC norm (ox/total) mean`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): WT norm (ox/total) mean`,
  "Delta mdx_mdxECC" = WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): mdx_ECC norm (ox/total) mean` / WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): mdx(combined) norm (ox/total) mean`,
  "Delta mdx_NaHSECC" = WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): mdx_NaHS norm (ox/total) mean` / WTECC_mdxECC_NaHSECC_sequence_overlap$`Abundances (Grouped): mdx(combined) norm (ox/total) mean`
)


# library("writexl")
# write_xlsx(WTECC_mdxECC_NaHSECC_sequence_overlap,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/WTECC_mdxECC_NaHSECC_sequence_overlap.xlsx")
# write_xlsx(delta_baseline_ECC,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/delta_baseline_ECC.xlsx")
# write_xlsx(delta_WTECC_NaHSECC_overlap,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/delta_WTECC_NaHSECC_overlap.xlsx")

#################################################################################
#################################################################################
#################################################################################
## Calculate peptide overlap between WTECC:WT, mdxECC:mdx, and NaHSECC:mdx



# install.packages("viridis")  # Install

png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 600)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library("viridis")           # Load
color_palette = viridis(6)
library(VennDiagram) 
v1 <- venn.diagram(
  list(
    "WTECC:WT" = sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence',
    "mdxECC:mdx" = sig_pep_raw_mancalc_mdx_ECC$'Peptide_Sequence',
    "NaHSECC:mdx" = sig_pep_raw_mancalc_NaHS_mdx$'Peptide_Sequence'
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  col = "black",
  fill = color_palette[c(2,3,4)],
  # fill = c("dodgerblue",
  #          "seagreen3",
  #          "goldenrod1"
  # ),
  alpha = 0.50,
  cat.pos = c(-20, 20, 180),
  cat.dist = c(0.08, 0.08, 0.08),
  cex = 1.5,
  cat.cex = 1.5,
  cat.fontface = "bold",
  margin = 0.1,
  # inverted = TRUE
)
grid.newpage()
grid.draw(v1)
dev.off()



Venn_regions_overlap_2 <- calculate.overlap(list(
  "WTECC:WT" = sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence',
  "mdxECC:mdx" = sig_pep_raw_mancalc_mdx_ECC$'Peptide_Sequence',
  "NaHSECC:mdx" = sig_pep_raw_mancalc_NaHS_mdx$'Peptide_Sequence'
))

WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap <-
  sig_pep_raw_mancalc_WT_ECC[sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence` %in% Venn_regions_overlap_2$a5,]
WTECC_NaHSECC_relcontrol_sequence_overlap <-
  sig_pep_raw_mancalc_WT_ECC[sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence` %in% Venn_regions_overlap_2$a4,]
mdx_only_relcontrol_sequence_overlap <-
  sig_pep_raw_mancalc_mdx_ECC[sig_pep_raw_mancalc_mdx_ECC$`Peptide_Sequence` %in% Venn_regions_overlap_2$a3,]


##Calculate the mean difference between ECC condition and baseline of significant peptides

## calculated delta for WTECC, mdxECC, NaHSECC overlap
delta_WTECC_mdxECC_NaHSECC_relcontrol <- data.frame(
  "Delta WT_WTECC" = WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): WT_ECC norm (ox/total) mean`/ WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): WT norm (ox/total) mean`,
  "Delta mdx_mdxECC" = WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx_ECC norm (ox/total) mean` / WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx(combined) norm (ox/total) mean`,
  "Delta mdx_NaHSECC" = WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx_NaHS norm (ox/total) mean` / WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx(combined) norm (ox/total) mean`
)



delta_WTECC_mdxECC_NaHSECC_allplex <- data.frame(
  "Delta WT_WTECC plex 1" = WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ECC norm ox/total Plex1`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ox/total norm Plex1`,
  "Delta WT_WTECC plex 2" = WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ECC norm ox/total Plex2`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ox/total norm Plex2`,
  "Delta WT_WTECC plex 3" = WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ECC norm ox/total Plex3`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ox/total norm Plex3`,
  "Delta WT_WTECC plex 4" = WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ECC norm ox/total Plex4`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ox/total norm Plex4`,
  "Delta WT_WTECC plex 5" = WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ECC norm ox/total Plex5`/ WTECC_mdxECC_NaHSECC_sequence_overlap$`WT ox/total norm Plex5`,
  
  "Delta mdx_mdxECC plex 1" = WTECC_mdxECC_NaHSECC_sequence_overlap$`ECC norm ox/total Plex1` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex1 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 2" = WTECC_mdxECC_NaHSECC_sequence_overlap$`ECC norm ox/total Plex2` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex2 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 3" = WTECC_mdxECC_NaHSECC_sequence_overlap$`ECC norm ox/total Plex3` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex3 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 4" = WTECC_mdxECC_NaHSECC_sequence_overlap$`ECC norm ox/total Plex4` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex4 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 5" = WTECC_mdxECC_NaHSECC_sequence_overlap$`ECC norm ox/total Plex5` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex5 norm (ox/total) mean`,
  
  "Delta mdx_NaHSECC plex 1" = WTECC_mdxECC_NaHSECC_sequence_overlap$`NaHS norm ox/total Plex1` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex1 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 2" = WTECC_mdxECC_NaHSECC_sequence_overlap$`NaHS norm ox/total Plex2` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex2 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 3" = WTECC_mdxECC_NaHSECC_sequence_overlap$`NaHS norm ox/total Plex3` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex3 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 4" = WTECC_mdxECC_NaHSECC_sequence_overlap$`NaHS norm ox/total Plex4` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex4 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 5" = WTECC_mdxECC_NaHSECC_sequence_overlap$`NaHS norm ox/total Plex5` / WTECC_mdxECC_NaHSECC_sequence_overlap$`mdx(combined)_plex5 norm (ox/total) mean`
  
)

delta_WTECC_mdxECC_NaHSECC_relcontrol_allplex <- data.frame(
  "Delta WT_WTECC plex 1" = WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ECC norm ox/total Plex1`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ox/total norm Plex1`,
  "Delta WT_WTECC plex 2" = WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ECC norm ox/total Plex2`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ox/total norm Plex2`,
  "Delta WT_WTECC plex 3" = WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ECC norm ox/total Plex3`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ox/total norm Plex3`,
  "Delta WT_WTECC plex 4" = WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ECC norm ox/total Plex4`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ox/total norm Plex4`,
  "Delta WT_WTECC plex 5" = WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ECC norm ox/total Plex5`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`WT ox/total norm Plex5`,
  
  "Delta mdx_mdxECC plex 1" = WTECC_NaHSECC_relcontrol_sequence_overlap$`ECC norm ox/total Plex1` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex1 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 2" = WTECC_NaHSECC_relcontrol_sequence_overlap$`ECC norm ox/total Plex2` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex2 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 3" = WTECC_NaHSECC_relcontrol_sequence_overlap$`ECC norm ox/total Plex3` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex3 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 4" = WTECC_NaHSECC_relcontrol_sequence_overlap$`ECC norm ox/total Plex4` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex4 norm (ox/total) mean`,
  "Delta mdx_mdxECC plex 5" = WTECC_NaHSECC_relcontrol_sequence_overlap$`ECC norm ox/total Plex5` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex5 norm (ox/total) mean`,
  
  "Delta mdx_NaHSECC plex 1" = WTECC_NaHSECC_relcontrol_sequence_overlap$`NaHS norm ox/total Plex1` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex1 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 2" = WTECC_NaHSECC_relcontrol_sequence_overlap$`NaHS norm ox/total Plex2` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex2 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 3" = WTECC_NaHSECC_relcontrol_sequence_overlap$`NaHS norm ox/total Plex3` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex3 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 4" = WTECC_NaHSECC_relcontrol_sequence_overlap$`NaHS norm ox/total Plex4` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex4 norm (ox/total) mean`,
  "Delta mdx_NaHSECC plex 5" = WTECC_NaHSECC_relcontrol_sequence_overlap$`NaHS norm ox/total Plex5` / WTECC_NaHSECC_relcontrol_sequence_overlap$`mdx(combined)_plex5 norm (ox/total) mean`
  
)

## calculated delta for WTECC, NaHSECC overlap
delta_WTECC_NaHSECC_relcontrol_overlap <- data.frame(
  "Delta WT_WTECC" = WTECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): WT_ECC norm (ox/total) mean`/ WTECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): WT norm (ox/total) mean`,
  "Delta mdx_NaHSECC" = WTECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx_NaHS norm (ox/total) mean` / WTECC_NaHSECC_relcontrol_sequence_overlap$`Abundances (Grouped): mdx(combined) norm (ox/total) mean`
)

# 
# library("writexl")
# write_xlsx(WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/WTECC_mdxECC_NaHSECC_relcontrol_sequence_overlap.xlsx")
# 
# write_xlsx(delta_WTECC_NaHSECC_relcontrol_overlap,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/delta_WTECC_NaHSECC_relcontrol.xlsx")




#################################################################################
#################################################################################
#################################################################################
## Calculate peptide overlap between significant peptides from mdx:wt, mdxECC:WT, mdxECCNaHS:WT, WTECC:WT


# install.packages("viridis")  # Install
library("viridis")           # Load
color_palette <- c(viridis(6))

png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 600)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library(VennDiagram) 
# dev.new(
# )
v1 <- venn.diagram(
  list(
    'WT+ECC:WT' = na.omit(sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence'),
    'mdx+ECC+NaHS:WT' = na.omit(sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence'),
    'mdx:WT' = na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),
    'mdx+ECC:WT' = na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence')
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  col = "black",
  fill = color_palette[c(2,3,4,5)],
  alpha = 0.50,
  cat.pos = c(240, 120, 330,30),
  cat.dist = c(0.3, 0.35, 0.15,0.15),
  cex = 1.2,
  ##to eliminate one plex, comment out 16 cex parameters
  cat.col = color_palette[c(2,3,4,5)],
  cat.cex = 1.2,
  cat.fontface = "bold",
  margin = 0.3,
  # inverted = TRUE
)

grid.newpage()
grid.draw(v1)
dev.off()


##use the following for 2+ group venn diagram 
Venn_regions_overlap_mdxgroups_WTECC_vs_WT <- calculate.overlap(list(
  'mdx:WT' = na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),
  'mdx+ECC:WT' = na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence'),
  'mdx+ECC+NaHS:WT' = na.omit(sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence'),
  'WT+ECC:WT' = na.omit(sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence')
))

ECCNaHS_vs_WTECC_unique_sequence_overlap <-
  sig_pep_raw_mancalc_NaHSECC_WT[sig_pep_raw_mancalc_NaHSECC_WT$'Peptide_Sequence' %in% Venn_regions_overlap_mdxgroups_WTECC_vs_WT$a2,]
mdxECC_only_unique_sequence_overlap <-
  sig_pep_raw_mancalc_mdxECC_WT[sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence' %in% Venn_regions_overlap_mdxgroups_WTECC_vs_WT$a14,]



############################

# install.packages('gplots')
library(gplots)
v_sublist <- 
  list(
    WT = sig_pep_raw_mancalc_WT_ECC$'Peptide Sequence',
    mdx = sig_pep_raw_mancalc_mdx_ECC$'Peptide Sequence'
  )

v1_sublist <- venn(v_sublist, show.plot=FALSE)
v1_sublist_lengths <-
  data.frame('Array Lengths' = lengths(attributes(v1_sublist)$intersections),
             check.names = FALSE) #leaves spaces in place between word in column names)
v1_sublist_max_length <- max(v1_sublist_lengths)
v1_sublist_1 <- c(attributes(v1_sublist)$intersections)

unique_pep_WT_ECC <-
  data.frame(WT_NaHS_comb_abundance_ratios %>% filter(WT_NaHS_comb_abundance_ratios$'Peptide Sequence' %in% v1_sublist_1$WT), check.names = FALSE)
unique_pep_mdx_ECC <-
  data.frame(WT_NaHS_comb_abundance_ratios %>% filter(WT_NaHS_comb_abundance_ratios$'Peptide Sequence' %in% v1_sublist_1$mdx), check.names = FALSE)

## Calculate peptide overlap between mdx+ECC and mdx+ECC+NaHS response to ECC

## calculate overlap across all combinations of 6plex 
sig_pep_NaHS_ECC_ox_total <- drop_na(sig_pep_NaHS_ECC_ox_total)

sig_pep_overlap_sequence_NaHS_ECC <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_ECC_sig_pep = sig_pep_NaHS_ECC_ox_total$'Peptide Sequence',
      mdx_ECC_sig_pep = sig_pep_raw_mancalc_mdx_ECC$'Peptide Sequence'
    )
  ))

sig_pep_overlap_geneID_NaHS_ECC <-
  data.frame(Overlap = Reduce(
    intersect,
    list(
      WT_ECC_sig_pep = sig_pep_NaHS_ECC_ox_total$'Gene Symbol',
      mdx_ECC_sig_pep = sig_pep_raw_mancalc_mdx_ECC$'Gene Symbol'
    )
  ))


png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 600)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library(VennDiagram)
color_palette <- c(viridis(6))
# dev.new()
v1 <- venn.diagram(
  list(
     'WT+ECC:WT' = na.omit(sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence'),
     'mdx:WT' = na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence')
   
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  scaled = FALSE,
  col = "black",
  fill = color_palette[c(2,3)],
  alpha = 0.50,
  cat.pos = c(30,330),
  cat.dist = c(0.07,0.07),
  cex = c(
    2
  ),
  ##to eliminate one plex, comment out 16 cex parameters
  cat.col = color_palette[c(2,3)],
  cat.cex = 2,
  cat.fontface = "bold",
  margin = 0.1,
  # inverted = TRUE
)

grid.newpage()
grid.draw(v1)
dev.off()

## use the following for only two group venn diagrams (3 part diagram)
##calculate center overlap region of venn diagram
mdx_WTECC_relWT_sig_pep_overlap <- data.frame('overlap mdx:WT and WTECC:WT'= intersect(na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),na.omit(sig_pep_raw_mancalc_WT_ECC$'Peptide_Sequence')))
##calculate which values occur in sig_pep_raw_mancalc_mdx_WT but not sig_pep_raw_mancalc_mdxECC_WT (values in A but not B)
##reduce dataframe of sig pep with all variables down to just the overlaping observations
mdx_WTECC_relWT_unique_pep_data <-
  sig_pep_raw_mancalc_mdx_WT[sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence' %in% mdx_WTECC_relWT_sig_pep_overlap$overlap.mdx.WT.and.WTECC.WT,]





# mdx_WT_only_sig_pep <- data.frame('unique mdx sig pep'= setdiff(na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence'),na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence')))
# ##calculate which values occur in sig_pep_raw_mancalc_mdxECC_WT but not sig_pep_raw_mancalc_mdx_WT (values in A but not B)
# mdxECC_WT_only_sig_pep <- data.frame('unique mdxECC sig pep'= setdiff(na.omit(sig_pep_raw_mancalc_mdxECC_WT$'Peptide_Sequence'),na.omit(sig_pep_raw_mancalc_mdx_WT$'Peptide_Sequence')))



##Calculate the mean difference between ECC condition and baseline of significant peptides

## calculated delta for WTECC, mdxECC, NaHSECC overlap
delta_WTECC_mdx_relWT <- data.frame(
  "Delta WT_WTECC" = mdx_WTECC_relWT_unique_pep_data$`Abundances (Grouped): WT_ECC norm (ox/total) mean`/ mdx_WTECC_relWT_unique_pep_data$`Abundances (Grouped): WT norm (ox/total) mean`,
  "Delta WT_mdx" = mdx_WTECC_relWT_unique_pep_data$`Abundances (Grouped): mdx(combined) norm (ox/total) mean` / mdx_WTECC_relWT_unique_pep_data$`Abundances (Grouped): WT norm (ox/total) mean`
)

# library("writexl")
# write_xlsx(delta_WTECC_mdx_relWT,"/Users/User/Desktop/WT_NaHS_combo_analysis/Sig_Pep_Delta_Analysis/delta_WTECC_mdx_relWT.xlsx")

###################



sig_pep_overlap_NaHS_mdx <-
  WT_NaHS_comb_abundance_ratios[WT_NaHS_comb_abundance_ratios$`Peptide Sequence` %in% sig_pep_overlap_sequence_NaHS_ECC$Overlap,]

# library("writexl")
# write_xlsx(sig_pep_overlap_WT_mdx,"Desktop/WT_NaHS_combo_analysis/sig_pep_overlap_WT_mdx.xlsx")


# install.packages('gplots')
library(gplots)
v_sublist <- 
  list(
    WT = sig_pep_raw_mancalc_WT_ECC$'Peptide Sequence',
    mdx = sig_pep_raw_mancalc_mdx_ECC$'Peptide Sequence'
  )

v1_sublist <- venn(v_sublist, show.plot=FALSE)
v1_sublist_lengths <-
  data.frame('Array Lengths' = lengths(attributes(v1_sublist)$intersections),
             check.names = FALSE) #leaves spaces in place between word in column names)
v1_sublist_max_length <- max(v1_sublist_lengths)
v1_sublist_1 <- c(attributes(v1_sublist)$intersections)

unique_pep_WT_ECC <-
  data.frame(WT_NaHS_comb_abundance_ratios %>% filter(WT_NaHS_comb_abundance_ratios$'Peptide Sequence' %in% v1_sublist_1$WT), check.names = FALSE)
unique_pep_mdx_ECC <-
  data.frame(WT_NaHS_comb_abundance_ratios %>% filter(WT_NaHS_comb_abundance_ratios$'Peptide Sequence' %in% v1_sublist_1$mdx), check.names = FALSE)



# column_names <- data.frame(names(WT_NaHS_comb_abundance_ratios))
# write_xlsx(column_names,"/Users/User/Desktop/column_names.xlsx")

library("writexl")
write_xlsx(unique_pep_mdx_ECC,"/Users/User/Desktop/WT_NaHS_combo_analysis/unique_pep_mdx_ECC.xlsx")

##################################################################################
##################################################################################
##################################################################################

## Screen 1 (WT screen) peptide overlap venn 


# install.packages("viridis")  # Install
png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 300)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library(VennDiagram)
library("viridis")  
color_palette <- c(viridis(6))
v1 <- venn.diagram(
  list(
    '6-Plex 1' = pep_overlap_WTsc_threshclean_plex1$sequence,
    '6-Plex 2' = pep_overlap_WTsc_threshclean_plex2$sequence,
    '6-Plex 3' = pep_overlap_WTsc_threshclean_plex3$sequence,
    '6-Plex 4' = pep_overlap_WTsc_threshclean_plex4$sequence,
    '6-Plex 5' = pep_overlap_WTsc_threshclean_plex5$sequence
  ),
  filename = NULL,
  disable.logging = TRUE,
  height = 8, 
  width = 8, 
  resolution = 300, 
  imagetype = "png", 
  units = "in", 
  compression = "lzw",
  col = "black",
  fill = color_palette[c(1,2,3,4,5)],
  alpha = 0.50,
  cex = c(
    1.5,
    1.5,
    1.5,
    1.5,
    1.5,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    2.2
  ),
  cat.col = color_palette[c(1,2,3,4,5)],
  cat.cex = 1.5,
  cat.dist = c(0.2, 0.3, 0.2, 0.25, 0.3),
  cat.fontface = "bold",
  margin = 0.15
)
grid.newpage()
grid.draw(v1)
dev.off()

##################################################################################
##################################################################################
##################################################################################

## Screen 2 (NaHS screen) peptide overlap venn 

# install.packages("viridis")  # Install
png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 300)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library(VennDiagram)
library("viridis")  
color_palette <- c(viridis(6))     
v1 <- venn.diagram(
  list(
    '6-Plex 1' = pep_overlap_NaHSsc_threshclean_plex1$sequence,
    '6-Plex 2' = pep_overlap_NaHSsc_threshclean_plex2$sequence,
    '6-Plex 3' = pep_overlap_NaHSsc_threshclean_plex3$sequence,
    '6-Plex 4' = pep_overlap_NaHSsc_threshclean_plex4$sequence,
    '6-Plex 5' = pep_overlap_NaHSsc_threshclean_plex5$sequence
  ),
  filename = NULL,
  disable.logging = TRUE,
  height = 8, 
  width = 8, 
  resolution = 300, 
  imagetype = "png", 
  units = "in", 
  compression = "lzw",
  col = "black",
  fill = color_palette[c(1,2,3,4,5)],
  alpha = 0.50,
  cex = c(
    1.5,
    1.5,
    1.5,
    1.5,
    1.5,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    1.2,
    2.2
  ),
  cat.col = color_palette[c(1,2,3,4,5)],
  cat.cex = 1.5,
  cat.dist = c(0.2, 0.3, 0.2, 0.25, 0.3),
  cat.fontface = "bold",
  margin = 0.15
)
grid.newpage()
grid.draw(v1)
dev.off()



## WT peptide overlap venn 
overlap_sequence_WT_all_plex_threshclean <-
  data.frame(Overlap_WT = Reduce(
    intersect,
    list(
      WT_Plex1 = pep_overlap_WTsc_threshclean_plex1,
      WT_Plex2 = pep_overlap_WTsc_threshclean_plex2,
      WT_Plex3 = pep_overlap_WTsc_threshclean_plex3,
      WT_Plex4 = pep_overlap_WTsc_threshclean_plex4,
      WT_Plex5 = pep_overlap_WTsc_threshclean_plex5
    )
  ))

## NaHS peptide overlap venn 
overlap_sequence_NaHS_all_plex_threshclean <-
  data.frame(Overlap_NaHS = Reduce(
    intersect,
    list(
      NaHS_Plex1 = pep_overlap_NaHSsc_threshclean_plex1,
      NaHS_Plex2 = pep_overlap_NaHSsc_threshclean_plex2,
      NaHS_Plex3 = pep_overlap_NaHSsc_threshclean_plex3,
      NaHS_Plex4 = pep_overlap_NaHSsc_threshclean_plex4,
      NaHS_Plex5 = pep_overlap_NaHSsc_threshclean_plex5
    )
  ))


png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 7, units = 'in', res = 300)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Venn_Diagram")
library(VennDiagram) 
library("viridis")  
color_palette <- c(viridis(6))
v1 <- venn.diagram(
  list(
    'Screen 1' = overlap_sequence_WT_all_plex_threshclean$sequence,
    'Screen 2' = overlap_sequence_NaHS_all_plex_threshclean$sequence
  ),
  ## for creating a viewer file:
  filename = NULL,
  euler.d = TRUE,
  scaled = FALSE,
  col = "black",
  fill = color_palette[c(2,3)],
  alpha = 0.50,
  cat.pos = c(330,30),
  cat.dist = c(0.07,0.07),
  cex = c(2.5,3.5,2.5),
  cat.col = color_palette[c(2,3)],
  cat.cex = 2,
  cat.fontface = "bold",
  margin = 0.1,
  rotation.degree = 180 ## flips datasets so screen 1 is on left side of diagram
  # inverted = TRUE,
  # reverse = TRUE
)

grid.newpage()
grid.draw(v1)
dev.off()








#################################################################################
#################################################################################
#################################################################################
## Create heatmap 
library(gplots)

dev.new()

WT_NaHS_comb_abundance_ratios["120",'Gene_Symbol'] <- 'Cacna1s'
WT_NaHS_comb_abundance_ratios["121",'Gene_Symbol'] <- 'Cacna1s'
WT_NaHS_comb_abundance_ratios["122",'Gene_Symbol'] <- 'Cacna1s'



heatmap_sig_contraction_genes <-
  subset(
    WT_NaHS_comb_abundance_ratios,
    Gene_Symbol %in% c(
      'Ryr1',
      'Mylk2',
      'Trim72',
      'Myom1',
      'Myom2',
      'Myom3',
      'Mybpc1',
      'Mybpc2',
      'Myh4',
      'Casq2',
      'Actn3',
      'Actn2',
      'Pgam2',
      'Klhl41',
      'Ckmt2',
      'Des',
      'MyoT',
      'Tnnc2',
      'Tnni2',
      'Cacna1s'
    )
  )

sig_contraction_pep_list <- data.frame(Sig_Pep_Seq = sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence`)

heatmap_sig_contraction_pep <- heatmap_sig_contraction_genes[heatmap_sig_contraction_genes$Peptide_Sequence %in% sig_contraction_pep_list$Sig_Pep_Seq,]





# heatmap_df <- data.frame(
#   # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
#   'mdx' = heatmap_sig_contract$`Abundances (Grouped): mdx mean`,
#   'ECC' = heatmap_sig_contract$`Abundances (Grouped): ECC mean`,
#   'NAC' = heatmap_sig_contract$`Abundances (Grouped): NAC mean`,
#   check.names = FALSE
# )


heatmap_df <- data.frame(
  # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
  'WT plex 1' = heatmap_sig_contraction_pep$`WT ox/total norm Plex1`,
  'WT plex 2' = heatmap_sig_contraction_pep$`WT ox/total norm Plex2`,
  'WT plex 3' = heatmap_sig_contraction_pep$`WT ox/total norm Plex3`,
  'WT plex 4' = heatmap_sig_contraction_pep$`WT ox/total norm Plex4`,
  'WT plex 5' = heatmap_sig_contraction_pep$`WT ox/total norm Plex5`,
  'WT ECC plex 1' = heatmap_sig_contraction_pep$`WT ECC norm ox/total Plex1`,
  'WT ECC plex 2' = heatmap_sig_contraction_pep$`WT ECC norm ox/total Plex2`,
  'WT ECC plex 3' = heatmap_sig_contraction_pep$`WT ECC norm ox/total Plex3`,
  'WT ECC plex 4' = heatmap_sig_contraction_pep$`WT ECC norm ox/total Plex4`,
  'WT ECC plex 5' = heatmap_sig_contraction_pep$`WT ECC norm ox/total Plex5`,
  'mdx plex 1' = heatmap_sig_contraction_pep$`mdx norm ox/total Plex1`,
  'mdx plex 2' = heatmap_sig_contraction_pep$`mdx norm ox/total Plex2`,
  'mdx plex 3' = heatmap_sig_contraction_pep$`mdx norm ox/total Plex3`,
  'mdx plex 4' = heatmap_sig_contraction_pep$`mdx norm ox/total Plex4`,
  'mdx plex 5' = heatmap_sig_contraction_pep$`mdx norm ox/total Plex5`,
  'mdx ECC plex 1' = heatmap_sig_contraction_pep$`ECC norm ox/total Plex1`,
  'mdx ECC plex 2' = heatmap_sig_contraction_pep$`ECC norm ox/total Plex2`,
  'mdx ECC plex 3' = heatmap_sig_contraction_pep$`ECC norm ox/total Plex3`,
  'mdx ECC plex 4' = heatmap_sig_contraction_pep$`ECC norm ox/total Plex4`,
  'mdx ECC plex 5' = heatmap_sig_contraction_pep$`ECC norm ox/total Plex5`,
  'mdx ECC NaHS plex 1' = heatmap_sig_contraction_pep$`NaHS norm ox/total Plex1`,
  'mdx ECC NaHS plex 2' = heatmap_sig_contraction_pep$`NaHS norm ox/total Plex2`,
  'mdx ECC NaHS plex 3' = heatmap_sig_contraction_pep$`NaHS norm ox/total Plex3`,
  'mdx ECC NaHS plex 4' = heatmap_sig_contraction_pep$`NaHS norm ox/total Plex4`,
  'mdx ECC NaHS plex 5' = heatmap_sig_contraction_pep$`NaHS norm ox/total Plex5`,
  check.names = FALSE
)

heatmap_df <- data.frame(
    # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
    'WT' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): WT norm (ox/total) mean',
    'WT ECC' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): WT_ECC norm (ox/total) mean',
    # 'mdx(wt)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(wt) norm (ox/total) mean',
    'mdx(combined)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',
    # 'mdx(NaHS)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean',
    'mdx ECC' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx_ECC norm (ox/total) mean',
    'mdx NaHS' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean',
    check.names = FALSE
    )

heatmap_matx <- as.matrix(heatmap_df)

color_palette = viridis(6)
# scaleRYG <- colorRampPalette(c("green","black","red"), 
#                              space = "rgb")(75) 

## for viridis color scheme
scaleRYG <- colorRampPalette(color_palette[c(1,3,4, 6)],space = "rgb")(75) 

# lmat is a matrix describing how the screen is to be broken up.
# By default, heatmap.2 divides the screen into a four element grid,
# so lmat is a 2x2 matrix. The number in each element of the matrix
# describes what order to plot the next four plots in. Heatmap.2 plots
# its elements in the following order:
# 
#   1. Heatmap,
#   2. Row dendrogram,
#   3. Column dendrogram,
#   4. Key
# so the default lmat is:
# 
#   > rbind(4:3,2:1)
# [,1] [,2]
# [1,]    4    3
# [2,]    2    1
dev.new()
HM_output <- heatmap.2(
  heatmap_matx,
  # Rowv = as.dendrogram(row.clus),
  Colv = F,
  dendrogram = "none",
  Rowv = FALSE, #toggle on/ off dendrogram
  scale = "row",
  col = scaleRYG,
  key = TRUE,
  keysize = 1.8,
  key.par = list(mar = c(2, 4, 4, 1)),
  #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
  density.info = "density",
  # key.xlab = 1,
  # key.ylab = 1,
  # key.xtickfun = 2,
  # key.ytickfun = 2,
  trace = "none",
  srtCol = -45,
  adjCol = c(.1, .5),
  #adjust col text placement
  # adjRow = c(0, 0), #adjust row text placement
  cexRow = 0.1,
  #adjust row text size
  cexCol = 2,
  #adjust col text size
  # labCol = 2,
  # labRow = gene_row_lab$`Row Labels`,
  # xlab = "Identifier",
  offsetRow = 0.0001,
  ylab = "Row #",
  # lmat = rbind(c(0,3), c(2,1), c(0,4)),
  lmat = rbind(c(4, 0), c(0, 3), c(2, 1)),
  lwid = c(2, 3),
  lhei = c(0.3, 0.005, 2),
  margins = c(4, 4)
)
dev.off()
## Run heatmap routine above to generate the scaled values for heatmap
## and then use those values in the routine below to calculate the gene 
## symbols associated with each heatmap row
row_num_correct_ori <- rev(c(HM_output$rowInd)) ##rev used to flip the values from top to bottom because the heatmap is generated from bottom up
gene_row_lab_correct_ori <- data.frame(
  "Row Number" = row_num_correct_ori,
  'Row Labels' = heatmap_sig_contraction_pep[row_num_correct_ori,]$`Gene_Symbol`,
  check.names = FALSE)

### extract raw z-score used to generate heatmap with associated row numbers and gene symbols
row_num_invert <- (c(HM_output$rowInd))
gene_row_lab_incorrect_ori <- data.frame(
  "Row Number" = row_num_invert,
  'Row Labels' = heatmap_sig_contraction_pep[row_num_invert ,]$`Gene_Symbol`,
  check.names = FALSE)
HM_output_values <- data.frame(
  'row_means' = HM_output$rowMeans,
  'row_indices' = HM_output$rowInd,
  'carpet' = t(HM_output$carpet),
  "Row Number" = row_num_invert,
  'Row Labels' = heatmap_sig_contraction_pep[row_num_invert,]$`Gene_Symbol`
)
HM_output_values <- HM_output_values[rev(rownames(HM_output_values)),]

## save the heatmap with the gene symbol row names as a png
png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 7, height = 12, units = 'in', res = 1200)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Heatmap")
# dev.new()
HM_output <- heatmap.2(
  heatmap_matx,
  # Rowv = as.dendrogram(row.clus),
  dendrogram = "none",
  Rowv = FALSE, #toggle on/ off dendrogram
  Colv = F,
  scale = "row",
  col = scaleRYG,
  key = TRUE,
  keysize = 1.8,
  key.par = list(mar = c(2, 4, 4, 1)),
  #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
  density.info = "density",
  # key.xlab = 1,
  # key.ylab = 1,
  # key.xtickfun = 2,
  # key.ytickfun = 2,
  trace = "none",
  srtCol = -45,
  adjCol = c(.1, .5), #adjust col text placement
  # adjRow = c(0, 0), #adjust row text placement
  cexRow = (1),
  #adjust row text size
  cexCol = 0.2,
  #adjust col text size
  # labCol = 2,
  # ylab = NULL, #turn off row label
  xlab = NULL, #turn off col label
  # labRow = FALSE, # Turn off row labels
  # labCol = FALSE, # Turn off col labels
  # xlab = "Identifier",
  offsetRow = 0.0001,
  ylab = "Gene Symbol",
  labRow = gene_row_lab_incorrect_ori$`Row Labels`, #use for adding gene id row labels
  lmat = rbind(c(4, 0), c(0, 3), c(2, 1)),
  lwid = c(3, 5),
  lhei = c(0.7, 0.001, 4),
  margins = c(7, 5)
)
dev.off()

## to select certain rows from main dataframe that are of interest based on heatmap
interest_HM_pep <- data.frame(
  WT_NaHS_comb_abundance_ratios[c(108,178,351,374,148,113,177,110,116,577,344),]
)

##write interest file to desktop
library("writexl")
write_xlsx(interest_HM_pep,"/Users/User/Desktop/WT_NaHS_combo_analysis/interest_HM_pep.xlsx")



#################################################################################
#################################################################################
#################################################################################
## Create heatmap for all unique genes from the 758 common peptides between both WT and NaHS screens
library(gplots)

# dev.new()

# WT_NaHS_comb_abundance_ratios["120",'Gene_Symbol'] <- 'Cacna1s'
# WT_NaHS_comb_abundance_ratios["121",'Gene_Symbol'] <- 'Cacna1s'
# WT_NaHS_comb_abundance_ratios["122",'Gene_Symbol'] <- 'Cacna1s'
# 
# 


heatmap_sig_contraction_genes <-
  subset(
    WT_NaHS_comb_abundance_ratios,
    Gene_Symbol %in% c(
      'Ryr1', 
      'Mylk2',
      'Trim72',
      'Myom1',
      'Myom2',
      'Myom3',
      'Mybpc1',
      'Mybpc2',
      'Myh4',
      'Casq2',
      'Actn3',
      'Actn2',
      'Pgam2',
      'Klhl41',
      'Ckmt2',
      'Des',
      'MyoT',
      'Tnnc2',
      'Tnni2',
      'Cacna1s'
    )
  )

##write interest file to desktop
library("writexl")
write_xlsx(heatmap_sig_contraction_genes,"/Users/User/Desktop/WT_NaHS_combo_analysis/New_Analysis_Results/2023.4.11_WT_NaHS_combo_abudance_ratios_subset_Contractile_Peptides2.xlsx")
# 



# sig_contraction_pep_list <- data.frame(Sig_Pep_Seq = sig_pep_raw_mancalc_WT_ECC$`Peptide_Sequence`)
# 
# heatmap_sig_contraction_pep <- heatmap_sig_contraction_genes[heatmap_sig_contraction_genes$Peptide_Sequence %in% sig_contraction_pep_list$Sig_Pep_Seq,]





# heatmap_df <- data.frame(
#   # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
#   'mdx' = heatmap_sig_contract$`Abundances (Grouped): mdx mean`,
#   'ECC' = heatmap_sig_contract$`Abundances (Grouped): ECC mean`,
#   'NAC' = heatmap_sig_contract$`Abundances (Grouped): NAC mean`,
#   check.names = FALSE
# )


heatmap_df <- data.frame(
  # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
  'WT plex 1' = heatmap_sig_contraction_genes$`WT ox/total norm Plex1`,
  'WT plex 2' = heatmap_sig_contraction_genes$`WT ox/total norm Plex2`,
  'WT plex 3' = heatmap_sig_contraction_genes$`WT ox/total norm Plex3`,
  'WT plex 4' = heatmap_sig_contraction_genes$`WT ox/total norm Plex4`,
  'WT plex 5' = heatmap_sig_contraction_genes$`WT ox/total norm Plex5`,
  'WT ECC plex 1' = heatmap_sig_contraction_genes$`WT ECC norm ox/total Plex1`,
  'WT ECC plex 2' = heatmap_sig_contraction_genes$`WT ECC norm ox/total Plex2`,
  'WT ECC plex 3' = heatmap_sig_contraction_genes$`WT ECC norm ox/total Plex3`,
  'WT ECC plex 4' = heatmap_sig_contraction_genes$`WT ECC norm ox/total Plex4`,
  'WT ECC plex 5' = heatmap_sig_contraction_genes$`WT ECC norm ox/total Plex5`,
  'mdx plex 1' = heatmap_sig_contraction_genes$`mdx norm ox/total Plex1`,
  'mdx plex 2' = heatmap_sig_contraction_genes$`mdx norm ox/total Plex2`,
  'mdx plex 3' = heatmap_sig_contraction_genes$`mdx norm ox/total Plex3`,
  'mdx plex 4' = heatmap_sig_contraction_genes$`mdx norm ox/total Plex4`,
  'mdx plex 5' = heatmap_sig_contraction_genes$`mdx norm ox/total Plex5`,
  'mdx ECC plex 1' = heatmap_sig_contraction_genes$`ECC norm ox/total Plex1`,
  'mdx ECC plex 2' = heatmap_sig_contraction_genes$`ECC norm ox/total Plex2`,
  'mdx ECC plex 3' = heatmap_sig_contraction_genes$`ECC norm ox/total Plex3`,
  'mdx ECC plex 4' = heatmap_sig_contraction_genes$`ECC norm ox/total Plex4`,
  'mdx ECC plex 5' = heatmap_sig_contraction_genes$`ECC norm ox/total Plex5`,
  'mdx ECC NaHS plex 1' = heatmap_sig_contraction_genes$`NaHS norm ox/total Plex1`,
  'mdx ECC NaHS plex 2' = heatmap_sig_contraction_genes$`NaHS norm ox/total Plex2`,
  'mdx ECC NaHS plex 3' = heatmap_sig_contraction_genes$`NaHS norm ox/total Plex3`,
  'mdx ECC NaHS plex 4' = heatmap_sig_contraction_genes$`NaHS norm ox/total Plex4`,
  'mdx ECC NaHS plex 5' = heatmap_sig_contraction_genes$`NaHS norm ox/total Plex5`,
  check.names = FALSE
)

heatmap_df <- data.frame(
    # # # 'Gene Symbol' = heatmap_sig_contraction_genes$'Gene Symbol',
    'WT' = heatmap_sig_contraction_genes$'Abundances (Grouped): WT norm (ox/total) mean',
    'WT ECC' = heatmap_sig_contraction_genes$'Abundances (Grouped): WT_ECC norm (ox/total) mean',
    # 'mdx(wt)' = heatmap_sig_contraction_genes$'Abundances (Grouped): mdx(wt) norm (ox/total) mean',
    'mdx(combined)' = heatmap_sig_contraction_genes$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',
    # 'mdx(NaHS)' = heatmap_sig_contraction_genes$'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean',
    'mdx ECC' = heatmap_sig_contraction_genes$'Abundances (Grouped): mdx_ECC norm (ox/total) mean',
    'mdx NaHS' = heatmap_sig_contraction_genes$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean',
    check.names = FALSE
    )
# heatmap_df <- data.frame(
#   # # # 'Gene Symbol' = WT_NaHS_comb_abundance_ratios$'Gene Symbol',
#   'WT' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): WT norm (ox/total) mean',
#   'WT ECC' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): WT_ECC norm (ox/total) mean',
#   # 'mdx(wt)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(wt) norm (ox/total) mean',
#   'mdx(combined)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(combined) norm (ox/total) mean',
#   # 'mdx(NaHS)' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx(NaHS) norm (ox/total) mean',
#   'mdx ECC' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx_ECC norm (ox/total) mean',
#   'mdx NaHS' = WT_NaHS_comb_abundance_ratios$'Abundances (Grouped): mdx_NaHS norm (ox/total) mean',
#   check.names = FALSE
# )

heatmap_df_inverted <- map_df(heatmap_df,rev)
heatmap_matx <- as.matrix(heatmap_df_inverted)
color_palette = viridis(7)
# scaleRYG <- colorRampPalette(c("green","black","red"), 
#                              space = "rgb")(75) 

## for viridis color scheme
scaleRYG <- colorRampPalette(color_palette[c(1,4, 7)],space = "rgb")(75) 

# lmat is a matrix describing how the screen is to be broken up.
# By default, heatmap.2 divides the screen into a four element grid,
# so lmat is a 2x2 matrix. The number in each element of the matrix
# describes what order to plot the next four plots in. Heatmap.2 plots
# its elements in the following order:
# 
#   1. Heatmap,
#   2. Row dendrogram,
#   3. Column dendrogram,
#   4. Key
# so the default lmat is:
# 
#   > rbind(4:3,2:1)
# [,1] [,2]
# [1,]    4    3
# [2,]    2    1
dev.new() # run this multiple times to get get a new plot space to show up outside of Rstudio
HM_output <- heatmap.2(
  heatmap_matx,
  # Rowv = as.dendrogram(row.clus),
  Colv = F,
  dendrogram = "none",
  Rowv = FALSE, #toggle on/ off dendrogram
  scale = "row",
  col = scaleRYG,
  key = TRUE,
  keysize = 1.8,
  key.par = list(mar = c(2, 4, 4, 1)),
  #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
  density.info = "density",
  # key.xlab = 1,
  # key.ylab = 1,
  # key.xtickfun = 2,
  # key.ytickfun = 2,
  trace = "none",
  srtCol = -45,
  adjCol = c(.1, .5),
  #adjust col text placement
  # adjRow = c(0, 0), #adjust row text placement
  cexRow = 0.1,
  #adjust row text size
  cexCol = 2,
  #adjust col text size
  # labCol = 2,
  # labRow = gene_row_lab$`Row Labels`,
  # xlab = "Identifier",
  offsetRow = 0.0001,
  ylab = "Row #",
  # lmat = rbind(c(0,3), c(2,1), c(0,4)),
  lmat = rbind(c(4, 0), c(0, 3), c(2, 1)),
  lwid = c(2, 3),
  lhei = c(0.6, 0.01, 2),
  margins = c(6, 6)
)
dev.off()
# ## Run heatmap routine above to generate the scaled values for heatmap
# ## and then use those values in the routine below to calculate the gene 
# ## symbols associated with each heatmap row
# row_num_correct_ori <- rev(c(HM_output$rowInd)) ##rev used to flip the values from top to bottom because the heatmap is generated from bottom up
# gene_row_lab_correct_ori <- data.frame(
#   "Row Number" = row_num_correct_ori,
#   'Row Labels' = heatmap_sig_contraction_genes[row_num_correct_ori,]$`Gene_Symbol`,
#   check.names = FALSE)
# 
# ### extract raw z-score used to generate heatmap with associated row numbers and gene symbols
# row_num_invert <- (c(HM_output$rowInd))
# gene_row_lab_incorrect_ori <- data.frame(
#   "Row Number" = row_num_invert,
#   'Row Labels' = heatmap_sig_contraction_genes[row_num_invert ,]$`Gene_Symbol`,
#   check.names = FALSE)
# HM_output_values <- data.frame(
#   'row_means' = HM_output$rowMeans,
#   'row_indices' = HM_output$rowInd,
#   'carpet' = t(HM_output$carpet),
#   "Row Number" = row_num_invert,
#   'Row Labels' = heatmap_sig_contraction_genes[row_num_invert,]$`Gene_Symbol`
# )
# HM_output_values <- HM_output_values[rev(rownames(HM_output_values)),]

#create dataframe for heatmap routine to reference for generation of row labels. Since the heatmap dataframe was reversed above, there is no need to reverse the row labels.
gene_row_lab_correct_ori <- data.frame(
    "Row Number" = row_num_correct_ori,
    'Row Labels' =heatmap_sig_contraction_genes[row_num_correct_ori,]$`Gene_Symbol`,
    check.names = FALSE)


## save the heatmap with the gene symbol row names as a png
png(paste(format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "png", sep = "."), width = 5, height = 12, units = 'in', res = 600)
setwd("/Users/User/Desktop/WT_NaHS_combo_analysis/Heatmap")
# dev.new()
HM_output <- heatmap.2(
  heatmap_matx,
  # Rowv = as.dendrogram(row.clus),
  # # dendrogram = "none",
  Rowv = FALSE, #toggle on/ off dendrogram
  Colv = FALSE,
  scale = "row",
  col = scaleRYG,
  key = TRUE,
  keysize = 1.8,
  key.par = list(mar = c(2, 4, 4, 1)),
  #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
  density.info = "density",
  # key.xlab = 1,
  # key.ylab = 1,
  # key.xtickfun = 2,
  # key.ytickfun = 2,
  trace = "none",
  srtCol = -45,
  adjCol = c(.1, .5), #adjust col text placement
  # adjRow = c(0, 0), #adjust row text placement
  cexRow = (0.5),
  #adjust row text size
  cexCol = 0.2,
  #adjust col text size
  # labCol = 2,
  # ylab = NULL, #turn off row label
  xlab = NULL, #turn off col label
  # labRow = FALSE, # Turn off row labels
  # labCol = FALSE, # Turn off col labels
  # xlab = "Identifier",
  offsetRow = 0.0001,
  ylab = "Gene Symbol",
  labRow = gene_row_lab_correct_ori$`Row Labels`, #use for adding gene id row labels
  lmat = rbind(c(4, 0), c(0, 3), c(2, 1)),
  lwid = c(2, 2),
  lhei = c(0.7, 0.001, 4),
  margins = c(7, 5)
)
dev.off()

## to select certain rows from main dataframe that are of interest based on heatmap
interest_HM_pep <- data.frame(
  WT_NaHS_comb_abundance_ratios[c(108,178,351,374,148,113,177,110,116,577,344),]
)

##write interest file to desktop
library("writexl")

