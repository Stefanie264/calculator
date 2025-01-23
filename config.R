# URLs -------------------

CGB_link="https://docs.google.com/spreadsheets/d/1e1Z5d_ubCZrykY1JZWOvTnWPA3Y_4avqoajerZL80s8/edit?pli=1#gid=0"
# CGB_link='https://docs.google.com/spreadsheets/d/1e1Z5d_ubCZrykY1JZWOvTnWPA3Y_4avqoajerZL80s8/edit#gid=0'
# this is the link to the out of use CGB_link='https://docs.google.com/spreadsheets/d/1IhBGQYExstG8HlBZg8y1M-02qmicb8W7VVdaWk6z3MY/edit#gid=0'

sarcoma_link="https://docs.google.com/spreadsheets/d/1AYbvXdlh-b7DNuKXRtrf_oLKNdTBRMN5m4prviCXmmE/edit#gid=1762189528"
#sarcoma_link='https://docs.google.com/spreadsheets/d/1wI4_mNYghaI4QeFjv8r-pP03FdXgxv2Q31smbbcJ-KQ/edit#gid=613115592'

# libraries --------------

pkgs = c('googlesheets4'
         ,'readr'
         ,'circlize'
         ,'tidyverse'
         ,'data.table')


lib = installed.packages()
installed=pkgs %in% rownames(lib)
not_installed = which(!installed)
if(length(not_installed)>0){
  for(i in not_installed) install.packages(pkgs[i], dependencies=T)
}

lib = installed.packages()
installed=pkgs %in% rownames(lib)
not_installed = which(!installed)
if(length(not_installed)>0){
  for(i in not_installed) BiocManager::install(pkgs[i])
}



for(i in pkgs) suppressPackageStartupMessages(library(i, character.only =T))

# Routines ---------------
message("[*] Loaging routines")

get_barcode = function(df){
  barcode = with(df,
                 sprintf("%s-%s-%02s-%s%s%s-%s%s%s", 
                         project_id, 
                         # -
                         patient_id, 
                         # -
                         
                         ifelse (TISSUES[tissue]!=60,
                                 TISSUES[tissue]
                                 ,paste0("6", toupper(letters[xenograft_generation+1] ) )),
                         # -
                         MOLECULES[molecule],
                         ANALYTES[analyte],
                         KIT2[kit], 
                         #-
                         ifelse((is.na(biopsy_index) | biopsy_index=='' ),0,biopsy_index),
                         ifelse((is.na(sample_index) | sample_index=='' ),0,sample_index),
                         ifelse((is.na(ngs_index) | ngs_index=='' ),0,ngs_index)
                 )
  )
  barcode
}

# Encodings --------------

MOLECULES = c( 'DNA' = 0, 'RNA' = 1, 'Protein' = 2 )

ANALYTES = c('WHOLE_EXOME'  = 0,
             'GENE_PANEL'   = 1,
             'FUSION_PANEL' = 2,
             'RNASEQ'       = 3, 
             'ONT'          = 4,
             'ONT_PANEL'    = 5,
             'SNP_ARRAY'    = 6,
             'MS'           = 7,
             'ATACSEQ'      = 8
             )

KIT = list(
  'WHOLE_EXOME'   = c('Agilent_SSV6_cosmic'                       = 0 ),
  'GENE_PANEL'    = c('400_custom'                                = 0 ),
  'FUSION_PANEL'  = c('Illumina_pancancer_fusion'                 = 0 ),
  'RNASEQ'        = c( 'Illumina_TruSeq_mRNA'                   = 0
                       , 'Illumina_TruSeq_totalRNA'               = 1
                       , 'Lexogen_CORALL_totalRNA'                  = 2
                       , 'Lexogen_QuantSeq3'                        = 3 
                       , 'Illumina_Stranded_Total_RNA_LigRibo-Zero' = 4 ),
  'ONT'           = c('directMRNA'   = 0
                      , 'directCDNA' = 1
                      , 'cDNA'       = 2
                      , 'ligation'   = 3 ),
  'SNP_ARRAY'     = c('Affimetrix_OncoScan_FFPE_SNP' = 0 ),
  'MS'            = c('blabla'),
  'ATACSEQ'       =c('NextSeq_1000_2000_Reagents' = 0 )

)

KIT2 = c(
'Agilent_SSV6_cosmic'                       = 0 ,
'400_custom'                                = 0 ,
'Illumina_pancancer_fusion'                 = 0 ,
'Illumina_TruSeq_mRNA'                   = 0,
'Illumina_TruSeq_totalRNA'               = 1,
'Lexogen_CORALL_totalRNA'                  = 2,
'Lexogen_QuantSeq3'                        = 3, 
'Illumina_Stranded_Total_RNA_LigRibo-Zero' = 4 ,
'directMRNA'   = 0,
'directCDNA' = 1,
'cDNA'       = 2,
'ligation'   = 3 ,
'Affimetrix_OncoScan_FFPE_SNP' = 0 
)


TISSUES =c (
  # 0.PRIMARY ********************************************
  'BIOPSY_PRIMARY_SOLID_TUMOR'                      = 0,
  'PRIMARY_SOLID_TUMOR'                             = 1,
  'RECURRENT_SOLID_TUMOR'                           = 2,
  'ADDITIONAL_NEW_PRIMARY'                          = 3,
  'PRIMARY_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD'   = 4,
  'PRIMARY_BLOOD_DERIVED_CANCER_BONE_MARROW'        = 5,
  'RECURRENT_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD' = 6,
  'RECURRENT_BLOOD_DERIVED_CANCER_BONE_MARROW'      = 7,
  'TUMOR_ORGANIC_CELLS'                             = 8,
  'BIOPSY_RECURRENT_TUMOR'                          = 9,
  
  # 1.NORMAL *********************************************
  'BLOOD_DERIVED_NORMAL'                            = 10,
  'SOLID_TISSUE_NORMAL'                             = 11,
  'BUCCAL_CELL_NORMAL'                              = 12,
  'EBV_IMMORTALIZED_NORMAL'                         = 13,
  'BONE_MARROW_NORMAL'                              = 14,
  'BLOOD_DERIVED_MOTHER'                            = 15,
  'BLOOD_DERIVED_FATHER'                            = 16,
  'IMMUNE_CELL_DERIVED_TUMOR'                       = 17,
  
  # 2.CONTROL Other **************************************
  'CONTROL_ANALYTE'                                 = 20,
  
  # 3.METASTASIS *****************************************
  'BIOPSY_METASTASIS'                               = 30,
  'METASTASIS'                                      = 31,
  'ADDITIONAL_METASTASIS'                           = 32,
  'METASTASIS_OF_RECURRENT_TUMOR'                   = 33,
  'BIOPSY_METASTASIS_OF_RECURRENT_TUMOR'            = 34,
  'SECOND_METASTASIS_OF_RECURRENT_TUMOR'            = 35,
  'BIOPSY_SECOND_METASTASIS_OF_RECURRENT_TUMOR'     = 36,

  # 5.CELL-LINES *****************************************
  'CELL_LINE_DERIVED_PRIMARY_TUMOR'                 = 50,
  'CELL_LINE_DERIVED_METASTASIS'                    = 51,
  'CELL_LINE_DERIVED_NORMAL_TISSUE'                 = 52,
  'CELL_LINE_DERIVED_MOUSE_NORMAL_TISSUE'           = 53,
  'CELL_LINE_DERIVED_MOUSE_PRIMARY_TUMOR'           = 54,
  'CELL_LINE_DERIVED_RECURRENT_TUMOR'               = 55,
  'CELL_LINE_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = 56,
  

  # 6.PDX ************************************************
  'PRIMARY_XENOGRAFT_TISSUE'                        = 60,
  'CELL_LINE_DERIVED_XENOGRAFT_TISSUE'              = 61,
  'CELL_LINE_XENOGRAFT_TISSUE'                      = 62,

  
  # 7.ORGANOIDS ******************************************
  "ORGANOIDS_DERIVED_PRIMARY_TUMOR"                 = 70,
  "ORGANOIDS_DERIVED_METASTASIS"                    = 71,
  'ORGANOIDS_DERIVED_NORMAL_TISSUE'                 = 72,
  'ORGANOIDS_DERIVED_CELL_LINE'                     = 73,
  'ORGANOIDS_DERIVED_RECURRENT_TUMOR'               = 74,
  'ORGANOIDS_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = 75,

  # 8.SPHEROIDS ******************************************
  
  "SPHEROIDS_DERIVED_PRIMARY_TUMOR"                 = 80,
  "SPHEROIDS_DERIVED_METASTASIS"                    = 81,
  'SPHEROIDS_DERIVED_NORMAL_TISSUE'                 = 82,
  'SPHEROIDS_DERIVED_CELL_LINE'                     = 83,
  'SPHEROIDS_DERIVED_RECURRENT_TUMOR'               = 84,
  'SPHEROIDS_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = 85

  
  
)

TISSUES_string =c (
  # 0.PRIMARY ********************************************
  'BIOPSY_PRIMARY_SOLID_TUMOR'                      = '00',
  'PRIMARY_SOLID_TUMOR'                             = '01',
  'RECURRENT_SOLID_TUMOR'                           = '02',
  'ADDITIONAL_NEW_PRIMARY'                          = '03',
  'PRIMARY_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD'   = '04',
  'PRIMARY_BLOOD_DERIVED_CANCER_BONE_MARROW'        = '05',
  'RECURRENT_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD' = '06',
  'RECURRENT_BLOOD_DERIVED_CANCER_BONE_MARROW'      = '07',
  'TUMOR_ORGANIC_CELLS'                             = '08',
  'BIOPSY_RECURRENT_TUMOR'                          = '09',
  
  # 1.NORMAL *********************************************
  'BLOOD_DERIVED_NORMAL'                            = '10',
  'SOLID_TISSUE_NORMAL'                             = '11',
  'BUCCAL_CELL_NORMAL'                              = '12',
  'EBV_IMMORTALIZED_NORMAL'                         = '13',
  'BONE_MARROW_NORMAL'                              = '14',
  'BLOOD_DERIVED_MOTHER'                            = '15',
  'BLOOD_DERIVED_FATHER'                            = '16',
  'IMMUNE_CELL_DERIVED_TUMOR'                       = '17',
  
  
  # 2.CONTROL Other **************************************
  'CONTROL_ANALYTE'                                 = '20',
  
  # 3.METASTASIS *****************************************
  'BIOPSY_METASTASIS'                               = '30',
  'METASTASIS'                                      = '31',
  'ADDITIONAL_METASTASIS'                           = '32',
  'METASTASIS_OF_RECURRENT_TUMOR'                   = '33',
  'BIOPSY_METASTASIS_OF_RECURRENT_TUMOR'            = '34',
  'SECOND_METASTASIS_OF_RECURRENT_TUMOR'            = '35',
  'BIOPSY_SECOND_METASTASIS_OF_RECURRENT_TUMOR'     = '36',
  
  # 5.CELL-LINES *****************************************
  'CELL_LINE_DERIVED_PRIMARY_TUMOR'                 = '50',
  'CELL_LINE_DERIVED_METASTASIS'                    = '51',
  'CELL_LINE_DERIVED_NORMAL_TISSUE'                 = '52',
  'CELL_LINE_DERIVED_MOUSE_NORMAL_TISSUE'           = '53',
  'CELL_LINE_DERIVED_MOUSE_PRIMARY_TUMOR'           = '54',
  'CELL_LINE_DERIVED_RECURRENT_TUMOR'               = '55',
  'CELL_LINE_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = '56',
  

  # 6.PDX ************************************************
  'PRIMARY_XENOGRAFT_TISSUE'                        = '60',
  'CELL_LINE_DERIVED_XENOGRAFT_TISSUE'              = '61',
  'CELL_LINE_XENOGRAFT_TISSUE'                      = '62',
  
  # 7.ORGANOIDS ******************************************
  "ORGANOIDS_DERIVED_PRIMARY_TUMOR"                 = '70',
  "ORGANOIDS_DERIVED_METASTASIS"                    = '71',
  'ORGANOIDS_DERIVED_NORMAL_TISSUE'                 = '72',
  'ORGANOIDS_DERIVED_CELL_LINE'                     = '73',
  'ORGANOIDS_DERIVED_RECURRENT_TUMOR'               = '74',
  'ORGANOIDS_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = '75',

  # 8.SPHEROIDS ******************************************
  "SPHEROIDS_DERIVED_PRIMARY_TUMOR"                 = '80',
  "SPHEROIDS_DERIVED_METASTASIS"                    = '81',
  'SPHEROIDS_DERIVED_NORMAL_TISSUE'                 = '82',
  'SPHEROIDS_DERIVED_CELL_LINE'                     = '83',
  'SPHEROIDS_DERIVED_RECURRENT_TUMOR'               = '84',
  'SPHEROIDS_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = '85'
)

CENTERS = c(
  "OIRM"          = 1,
  "TRIESTE"       = 2,
  "RIZZOLI"       = 3,
  "SANTORSOLA"    = 4,
  "PINI"          = 5,
  "PAVIA"         = 6,
  "PADOVA"        = 7,
  "IIGM"          = 8,
  "REGINAELENA"   = 9,
  "FPO"           = 10,
  "BOLOGNA"       = 11,
  "MBC"           = 12,
  "IRB"           = 13
)

DISEASES=c(
  'OS'     = "Osteosarcoma"
  ,'OB'     = "Osteoblastoma"                                  
  ,'EWS'    = "Ewing's Sarcoma"
  ,'RMS'    = "Rabdomyosarcoma"
  ,'LMS'    = "Leiomyosarcoma" 
  ,'LS'     = "Liposarcoma"
  ,'CS'     = "Condrosarcoma" 
  ,'EC'     = "Embrional Carcinoma"
  ,'MPNST'  = "Malignant Peripheral Nerve Sheath Tumor"
  ,'SS'     = "Sinovial Sarcoma" 
  ,'MT'     = "Mesenchymal Tumor"
  ,'AS'     = "Angiosarcoma"
  ,'HE'     = "Hemangioendothelioma"
  ,'ASPS'   = "Alveolar soft part Sarcoma"
  ,'BL'     = "Burkitt's Lymphoma"
  ,'MHF'    = "Malignant fibrous histiocytoma of bone"         
  ,'LI'     = "Li Fraumeni"
  ,'LCH'    = "Langherans cells hystiocitosi"            
  ,'GCTOB'  = "Giant Cell Tumor of the bone"                                   
  ,'Healty' = "Healthy tissue"
  ,'INF'    = "Inflammation"
  ,'EM'     = "Ectomesenchyme"
  ,'WT'     = "Wilms' tumor"
  ,'GN'     = "Ganglioneuroma" 
  ,'EPI'    = "Epithelioid sarcoma"
  ,'OGTC'   = 'Ovarian granulosa tumor cell'
  ,'GBM'    = 'Glioblastoma'
  ,'CRC'    = 'Colorectal Adenocarcinoma'
  ,'PC'     = 'Prostate Adenocarcinoma'
  ,'DBA'    = 'Diamond-Blackfan anemia'
  ,'BC'     = 'Breast Cancer'
  ,'MDB'    = "Medulloblastoma"
  ,'INLBE'  = 'Immortalized normal lung broncoalverolar epithelium'
  ,'MEF'    = 'Mouse Embrionic Fibroblast'
)

DISEASES_code = names(DISEASES)
names(DISEASES_code) = DISEASES
# Color Encoding ----------

color_analyte = c('WHOLE_EXOME'  = "#f6eff7",
                  'GENE_PANEL'   = "#d0d1e6",
                  'FUSION_PANEL' = "#a6bddb",
                  'RNASEQ'       = "#67a9cf", 
                  'ONT_RNA'      = "#1c9099",
                  'ONT_DNA'      = "#016c59",
                  'SNP_ARRAY'    = "black",
                  'ATACSEQ'      = "#0f07f5" )

color_kit = c(
  'Agilent_SSV6_cosmic'                      = "#ffffcc",
  '400_custom'                               = "#ffeda0",
  'Illumina_pancancer_fusion'                = "#fed976",
  'Illumina_TruSeq_mRNA'                   = "#e0fe4c",
  'Illumina_TruSeq_totalRNA'               = "#fd8d3c",
  'Lexogen_CORALL_totalRNA'                  = "#fc4e2a",
  'Lexogen_QuantSeq3'                        = "#e31a1c",
  "Illumina_Stranded_Total_RNA_LigRibo-Zero" = "#da15e8",
  'directMRNA'                               = "#bd0026",
  'ligation'                                 = "#800026",
  'Affimetrix_OncoScan_FFPE_SNP'             = "black",
  'NextSeq_1000_2000_Reagents'               = "#0b9499" 
)


color_tissue_general =c (
  "PRIMARY"="orangered1",
  "RECURRENT"="brown",
  "METASTASIS"="gold1",
  "CELL_LINE"="chartreuse"
)

color_tissue =c (
  # 0.PRIMARY ********************************************
  'BIOPSY_PRIMARY_SOLID_TUMOR'                      = "orangered1",
  'PRIMARY_SOLID_TUMOR'                             = "brown1",
  'RECURRENT_SOLID_TUMOR'                           = "orangered3",
  'ADDITIONAL_NEW_PRIMARY'                          = "white",
  'PRIMARY_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD'   = "white",
  'PRIMARY_BLOOD_DERIVED_CANCER_BONE_MARROW'        = "white",
  'RECURRENT_BLOOD_DERIVED_CANCER_PERIPHERAL_BLOOD' = "white",
  'RECURRENT_BLOOD_DERIVED_CANCER_BONE_MARROW'      = "white",
  'TUMOR_ORGANIC_CELLS'                             = "white",
  'BIOPSY_RECURRENT_TUMOR'                          = "red3",
  
  # 1.NORMAL *********************************************
  'BLOOD_DERIVED_NORMAL'                            = "midnightblue",
  'SOLID_TISSUE_NORMAL'                             = "#0095b6",
  'BUCCAL_CELL_NORMAL'                              = "white",
  'EBV_IMMORTALIZED_NORMAL'                         = "white",
  'BONE_MARROW_NORMAL'                              = "white",
  'BLOOD_DERIVED_MOTHER'                            = "white",
  'BLOOD_DERIVED_FATHER'                            = "white",
  "IMMUNE_CELL_DERIVED_TUMOR"                       = "cornflowerblue",
  
  
  # 2.CONTROL Other **************************************
  'CONTROL_ANALYTE'                                 = "white",
  
  # 3.METASTASIS *****************************************
  'BIOPSY_METASTASIS'                               = "goldenrod1",
  'METASTASIS'                                      = "gold1",
  'ADDITIONAL_METASTASIS'                           = "white",
  'METASTASIS_OF_RECURRENT_TUMOR'                   = "goldenrod3",
  'BIOPSY_METASTASIS_OF_RECURRENT_TUMOR'            = "goldenrod4",
  'SECOND_METASTASIS_OF_RECURRENT_TUMOR'            = "goldenrod2",
  'BIOPSY_SECOND_METASTASIS_OF_RECURRENT_TUMOR'     = "goldenrod",
  
  
  # 5.CELL-LINES *****************************************
  'CELL_LINE_DERIVED_PRIMARY_TUMOR'                 = "chartreuse4",
  'CELL_LINE_DERIVED_METASTASIS'                    = "forestgreen",
  'CELL_LINE_DERIVED_NORMAL_TISSUE'                 = "#0095b6",
  'CELL_LINE_DERIVED_MOUSE_NORMAL_TISSUE'           = "white",
  'CELL_LINE_DERIVED_MOUSE_PRIMARY_TUMOR'           = "white",
  'CELL_LINE_DERIVED_RECURRENT_TUMOR'               = "chartreuse",
  'CELL_LINE_DERIVED_METASTASIS_OF_RECURRENT_TUMOR' = "chartreuse2",
  
  
  # 6.PDX ************************************************
  'PRIMARY_XENOGRAFT_TISSUE'                        = "lightpink",
  'CELL_LINE_DERIVED_XENOGRAFT_TISSUE'              = "lightpink3",
  'CELL_LINE_XENOGRAFT_TISSUE'                      = "lightpink4",
  
  # 7.ORGANOIDS ******************************************
  "ORGANOIDS_DERIVED_PRIMARY_TUMOR"                 = "darkorchid1",
  "ORGANOIDS_DERIVED_METASTASIS"                    = "darkorchid2",
  "ORGANOIDS_DERIVED_NORMAL_TISSUE"                 = "white",
  'ORGANOIDS_DERIVED_CELL_LINE'                     = "darkorchid3",
  'ORGANOIDS_DERIVED_RECURRENT_TUMOR'               = "darkorchid4"            ,

  # SPHEROIDS ******************************************
  "SPHEROIDS_DERIVED_PRIMARY_TUMOR"                 = "firebrick1",
  "SPHEROIDS_DERIVED_METASTASIS"                    = "firebrick2",
  'SPHEROIDS_DERIVED_NORMAL_TISSUE'                 = "white",
  'SPHEROIDS_DERIVED_CELL_LINE'                     = "firebrick3",
  'SPHEROIDS_DERIVED_RECURRENT_TUMOR'               = "firebrick4",
  "SPHEROIDS_DERIVED_METASTASIS_OF_RECURRENT_TUMOR" = "firebrick4"

)

color_molecule=c("DNA"= "red",
                 "RNA"= "blue",
                 "Protein" = 'yellow')



color_biopsy=c(
  "0"="#bc80bd",
  "1"="#7c77a1",
  "2"="#fccde5",
  "3"="#b3de69",
  "4"="#fdb462",
  "5"="#80b1d3",
  "6"="#fb8072",
  "7"="#bebada",
  "8"="#ffffb3",
  "9"="#8dd3c7"
)

color_sample_index=c(
  "0"="#6a3d9a",
  "1"="#cab2d6",
  "2"="#b2df8a",
  "3"="#33a02c",
  "4"="#fb9a99",
  "5"="#e31a1c",
  "6"="#fdbf6f",
  "7"="#ff7f00",
  "8"="#1f78b4",
  "9"="#a6cee3"
)

color_ngs_index=c(
  "0"="cyan3",
  "1"="maroon",
  "2"="khaki",
  "3"="palegreen3",
  "4"="mediumpurple",
  "5"="orange3",
  "6"="tan1",
  "7"="seagreen1",
  "8"="honeydew",
  "9"="peachpuff"
)

color_center = c(
  "OIRM"       = "violetred2",
  "TRIESTE"    = "#1AFF1A",
  "RIZZOLI"    = "#4B0092",
  "SANTORSOLA" = "#E1BE6A",
  "PINI_MILANO"= "#40B0A6",
  "PAVIA"      = "#FEFE62",
  "IIGM"       = "#D35FB7",
  "PADOVA"     = "cyan1",
  "RIZ_BOLOGNA" = "burlywood4",
  "ORS_BOLOGNA" = "burlywood2",
  "FPO"        = "red4",
  "REGINAELENA"= "plum",
  "Unknown"    = "black",
  "INT_MILANO"  = "blue",
  "StJude"     = "tan",
  "PALERMO"    = "midnightblue",
  "MBC"        ="#ff0873",
  "CTO"        = "darkmagenta",
  "ROMA"      = "pink",
  "PISA"     ="magenta",
  "BARI"      = "green",
  "SAN_GIOVANNI_ROTONDO"= "forestgreen",
  "UDINE"=        "deepskyblue",
  "CAGLIARI"=      "darkolivegreen1",
  "GENOVA_GASLINI"=  "darkorange1",
  "FIRENZE_MEYER"= "firebrick1",
  "GEMELLI"="lavender"
  
)

color_tissue_type=c(
  "FRESH"="limegreen",
  "FFPE" ="lightsalmon",
  "FROZEN"="cornflowerblue",
  "NO"="black"
)


color_seq=c(
  "TRUE"="black",
  "FALSE"="white"
  
)



color_disease=c(
  "Osteosarcoma"="deepskyblue1",
  "Osteoblastoma" ="white",                                 
  "Ewing's Sarcoma"="tomato",
  "Rhabdomyosarcoma"="deeppink3",
  "Leiomyosarcoma" ="pink",
  "Liposarcoma"="midnightblue",
  "Chondrosarcoma"="gray78", 
  "Embrional Carcinoma"="white",
  "Malignant Peripheral Nerve Sheath Tumor"="white",
  "Synovial Sarcoma" ="yellow",
  "Mesenchymal Tumor"="palevioletred1",
  "Angiosarcoma"="gray31",
  "Hemangioendothelioma"="forestgreen",
  "Alveolar Soft Part Sarcoma"="mediumpurple",
  "Burkitt's Lymphoma"="white",
  "Malignant fibrous histiocytoma of bone"="white",      
  "Li Fraumeni"="white",
  "Langherans cells hystiocitosi" ="white",           
  "Giant Cell Tumor of the bone" ="white",                                  
  "Healthy tissue"="bisque",
  "Inflammation"="white",
  "Ectomesenchyme"="palegreen4",
  "Wilms' tumor"="cadetblue1",
  "Ganglioneuroma"="steelblue", 
  "Epithelioid Sarcoma"="hotpink4",
  'Ovarian granulosa tumor cell'="palegreen",
  "Fibrosarcoma"="saddlebrown",
  "Diffuse large B-cell lymphoma"="bisque3",
  'Glioblastoma'="orchid2",
  'Colorectal Adenocarcinoma'="skyblue1",
  'Prostate Adenocarcinoma' = "slateblue1",
  'Breast Cancer'="mistyrose",
  'Diamond-Blackfan anemia'= 'black', 
  "Medulloblastoma" = "#FC46AA",
  "Retinoblastoma"="green",
  "Unknown"                    = "grey",
  "Undifferentiated Sarcoma"="grey",
  "Lung Adenocarcinoma"="#2885bf",
  "Lung Squamous Cell Carcinoma"="#28bfa4",
  "Fibroblast"="#b88528",
  "Other"= "aquamarine1",
  "Other Soft Tissue sarcoma"="chartreuse",
  "Kidney Renal Clear Cell Carcinoma"="#5B342E",
  "Epithelial Ovarian Cancer"="magenta",
  "Immortalized normal lung broncoalverolar epithelium"="#B2182B",
  "Mouse Embrionic Fibroblast"="#F4A582"
)

color_RIN_categories=c(
  "[0,2]"="dodgerblue4",
  "(2,4]"="dodgerblue1",
  "(4,6]"="gold",
  "(6,8]"="darkorange",
  "(8,10]"="firebrick2"
)

color_DIV200_categories=c(
  "[0,0.2]"="#482677",
  "(0.2,0.4]"="#39558C",
  "(0.4,0.6]"="#238A8D",
  "(0.6,0.8]"="#3CBC75",
  "(0.8,1]"="#94D840"
)

color_fragment_categories=c(
  "<=250"="white",
  "(250,300]"="#FFA07A",
  "(300,350]"="#CD5C5C",
  "(350,400]"="#B22222",
  ">400"="#800000"
)

color_read_length=c(
  "51"="#CCE5FF",
  "71"="#66FFFF",
  "76"="#66B2FF",
  "101"="#0066CC", 
  "126"="#3333FF",
  "151"="#0000CC",
  "201"="#000066",
  "301"="#003366"
)


library(circlize)
# color_duplicates=colorRamp2(c(0,10,20,30,40,50,60,70,80,90,100), colorRampPalette(c("limegreen", "yellow", "orange", "red"))(11))
color_duplicates=colorRamp2(c(0,10,20,30,40,50,60,70,80,90,100), topo.colors(11))

# color_GC=colorRamp2(c(40,50,60), colorRampPalette( c("#5ab4ac","#f5f5f5","#d8b365"))(3))
color_GC=colorRamp2(c(40,50,60), viridis::viridis_pal(option = 'C')(3))

color_failed=colorRamp2(c(0,10,20,30,40,50,60,70,80,90,100), colorRampPalette(c("#66a61e","white","#e7298a"))(11))

color_warn=colorRamp2(c(0,10,20,30,40,50,60,70,80,90,100), colorRampPalette(c("#66a61e","white","#e7298a"))(11))

color_inner_distance=colorRamp2(c(-150, -125, -100, -50,-25,-5, 0,5,25, 50, 100, 125,150), colorRampPalette(c("hotpink","slategray1","mediumpurple4" ))(13))

color_dnase=c(  "TRUE" = "#11d99d",  "FALSE"= "#d60d3f")

color_uoc  =rev(c("cadetblue4","cadetblue","cadetblue3", "cadetblue2", "cadetblue1", "lightgray", "gray",'black'))
color_cov = c( 'MEAN_BAIT_COVERAGE'='black','MEAN_TARGET_COVERAGE'='grey','MEDIAN_TARGET_COVERAGE'='red')
color_usbl = c( 'PCT_USABLE_BASES_ON_TARGET'='black','PCT_USABLE_BASES_ON_BAIT'='grey')


col_comorbid=c(
"NO"     =  "white"   ,              
"OTHER"    =    "indianred1" ,            
"RETINOBLASTOMA"     =    "purple",   
"LI-FRAUMENI SYNDROME"   = "darksalmon" , 
"STILLING DUANE II SYNDROME"= "gold",
"BLACKFAN DIAMOND ANEMIA"  ="darkslategray1"  ,
"DOWN SYNDROME"      =    "darkseagreen1"  , 
"RB1 SYNDROME"      =    "red2"  ,  
"STROMME SYNDROME"    =  "lavender"  ,  
"ATAXIA TELANGIECTASIA"  = "pink"
)

col_organ_site=c("BONE"="black", "SOFT TISSUE + BONE"="purple", "SKIN"="pink", "SOFT TISSUE"="orchid", "LUNG"="cadetblue",  "BRAIN"="cyan", "PLEURA"="blue",
                 "LIVER"="brown", "UNKNOWN"="whitesmoke", "TESTICLE"="yellow")



col_subtype=c("Telangiectatic"="#E65100",
                 "Osteoblastic"="#004D40",
                 "Osteoblastic/Condroblastic"="#FDD835",
                 "Chondroblastic" ="#2196F3",
                 "Fibroblastic"="#EC407A",
                 "NAS"="#5D4037",
                 "Alveolar"="cyan",
                 "Myxoid"="pink",
                 "Pleomorphic"="orange",
                 "NS"="white",
                 "MPNST (tumore maligno guaina nervosa periferica)"="lightskyblue1",
                 "Primitive Mesenchimal Myxoid Tumor"="magenta",
                 "Embryonal"="mistyrose1",
                 "Mesenchymal Tumor"="mediumspringgreen",
                 "Malignant fibrous histiocytoma of bone"="moccasin",
                 "Infantile Fibrosarcoma"="midnightblue",
                 "Parosteal"="mediumturquoise",
                 "Osteo/Chondroblastic"="mediumvioletred",
                 "Biphasic"="lightyellow",
                 "Monophasic"="maroon1"

              )



col_sex=c("F"="deeppink", "M"="darkturquoise")


col_age=colorRamp2(c(0,2,4,6,8,10,12,14,16,18,20,25,30), colorRampPalette(c("#FFF59D", "#FDD835", "#F9A825", "#FF6F00", "#E65100", "#D84315"))(13))


col_necrosis=colorRamp2(c(0,10, 20, 30, 40, 50, 60, 70, 80, 90, 100), colorRampPalette(c("#4FC3F7", "#03A9F4", "#0288D1", "#01579B", "#0047A1", "#1A237E","#311B92", "#4A148C", "#880E4F", "#B71C1C"))(11))


col_surv=colorRamp2(c(0,6, 12, 24, 36, 200), colorRampPalette(c("#3E2723", "#79554B", "#BCAAA4", "#DCEDC8", "#88C34A"))(6))


col_fup=c("T"="chartreuse2" , "F"="black", "LOST TO FUP"="bisque2")

col_stadiation=c("METASTATIC"="orange", "LOCALIZED"="purple")

col_prev_therapy=c(
  "YES"="violetred3"
  
)


col_enrollment=c(
  
  "2015"="#080807",
  "2016"="#220ceb",
  "2017"="#0c70eb",
  "2018"="#22c9b3",
  "2019"="#30fc03",
  "2020"="#e8fc05",
  "2021"="#f5d442",
  "2022"="#f54254",
  "2023"="#f542cb"
)



  RCOLORS_no_grey = colors()
RCOLORS_no_grey = RCOLORS_no_grey[ !grepl("grey|gray",RCOLORS_no_grey )]

# !!! DEPRECATED !!!!
# dates=unique(db$sequencing_date)
# dates=dates[order(dates)]
# 
# color_date=c(
# 
#   "2017-11-30" ="#080807",
#   "2017-12-01"="#2b0563",
#   "2017-12-04" ="#5d0dd6",
#   "2018-01-30" ="#220ceb",
#   "2018-09-11"="#11509e",
#   "2019-02-01" ="#0c70eb",
#   "2019-03-08" ="#30a9d1",
#   "2019-03-21"="#1abad6",
#   "2019-11-11" ="#1ee7eb",
#   "2020-01-31"="#22c9b3",
#   "2020-02-21" ="#1ddeb7",
#   "2020-07-10" ="#12de7f",
#   "2020-07-30" ="#09e827",
#   "2020-09-11"="#07f527",
#   "2020-09-23" ="#30fc03",
#   "2020-10-27" ="#c4fc1c",
#   "2020-11-13" ="#e8fc05",
#   "2020-12-03"="#fce005"
# 
# )


