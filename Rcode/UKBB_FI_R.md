Recreating UKBB FI in R
================
Ruth CE Bowyer
2023-09-29

- [**0. About**](#0-about)
- [**1. Data**](#1-data)
- [**2. Function definition**](#2-function-definition)
- [**3. Clean datasets**](#3-clean-datasets)
  - [3.1 Glaucoma](#31-glaucoma)
  - [3.2 Cataracts](#32-cataracts)
  - [3.3 Hearing](#33-hearing)
  - [3.4 Infirmity](#34-infirmity)
  - [3.5 self-rated health](#35-self-rated-health)
  - [3.6 Falls](#36-falls)
  - [3.7 Breathing/wheeze](#37-breathingwheeze)
  - [3.8 Fatigue](#38-fatigue)
  - [3.9 Myocardial infarction (MI)](#39-myocardial-infarction-mi)
  - [3.10 Angina](#310-angina)
  - [3.11 Stroke](#311-stroke)
  - [3.12 High blood pressure (HBP)](#312-high-blood-pressure-hbp)
  - [3.13 Rheumatoid arthritis (RA)](#313-rheumatoid-arthritis-ra)
  - [3.14 Osteoarthritis](#314-osteoarthritis)
  - [3.15 Gout](#315-gout)
  - [3.16 Dental problems](#316-dental-problems)
  - [3.17 Chest pain](#317-chest-pain)
  - [3.18 Sciatica](#318-sciatica)
  - [3.19 Diabetes](#319-diabetes)
  - [3.20 Cancer](#320-cancer)
  - [3.21 Multiple cancers](#321-multiple-cancers)
  - [3.22 Fractures](#322-fractures)
  - [3.23 Deep vein thrombosis (DVT)](#323-deep-vein-thrombosis-dvt)
  - [3.24 Emphysema/chronic
    bronchitis](#324-emphysemachronic-bronchitis)
  - [3.25 Asthma](#325-asthma)
  - [3.26 Allergies](#326-allergies)
  - [3.27 Hypothyroidism](#327-hypothyroidism)
  - [3.28 Recent depressed mood
    (depression)](#328-recent-depressed-mood-depression)
  - [3.29 Anxiousness](#329-anxiousness)
  - [3.30 Severe anxiety](#330-severe-anxiety)
  - [3.31 Mood - misery](#331-mood---misery)
  - [3.32 Loneliness](#332-loneliness)
  - [3.33 Head and/or neck pain](#333-head-andor-neck-pain)
  - [3.34 to 3.39: Pain domains - (34) Back pain, (35) stomach
    pain, (36) Hip, (37) knee pain, (38) whole body, (39) facial
    pain](#334-to-339-pain-domains---34-back-pain-35-stomach-pain-36-hip-37-knee-pain-38-whole-body-39-facial-pain)
  - [3.40 Sleep](#340-sleep)
  - [3.41 High cholesterol](#341-high-cholesterol)
  - [3.42 - 3.49 Other conditions](#342---349-other-conditions)
- [**Generate FI**](#generate-fi)
  - [Combine domains](#combine-domains)
  - [Calculate score](#calculate-score)
  - [FI distribution](#fi-distribution)

## **0. About**

Converting the Stata script provided by Williams et al., to generate a
FI in UKBB

This script was run mostly via CREATE cluster

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

## **1. Data**

**Variables to select as per William’s et al**

Star indicates variable in an array:

- f.eid – participant identifier
- f.31.0\* – Sex
- f.34.\* – Year of birth
- f.52.\* – Month of birth
- f.21003.0\* – Age when attended assessment centre
- f.21000\* – Ethnicity
- f.1200.0\* – Sleeplessness / insomnia
- f.6148.0\* – Eye problems / disorders
- f.2227.0\* – Other eye problems
- f.2020.0\* – Loneliness / isolation
- f.2247.0\* – Hearing difficulties
- f.2188.0\* – Long-standing illness, disability or infirmity
- f.2178.0\* – Overall self-rated health
- f.2296.0\* – Falls in last year
- f.2316.0\* – Chest wheeze or whistling
- f.2080.0\* – Tiredness / Lethargy
- f.6150.0\* – Vascular problems
- f.20002.0\* – Non-cancer illness codes (all array variables are
  necessary to keep)
- f.6149.0\* – Mouth / dental problems
- f.2335.0\* – Chest pain
- f.2443.0\* – Diabetes
- f.2453.0\* – Cancer diagnosis
- f.134.0\* – Number of self-reported cancers
- f.2463.0\* – fractured/broken bones
- f.6152.0\* – Multiple medical conditions reported - e.g. DVT, asthma,
  eczema
- f.3786.0\* – Age of asthma diagnosis
- f.2473.0\* – Other serious medical conditions reported
- f.2050.0\* – Depressed moods
- f.1970.0\* – Nervous feelings
- f.1930.0\* – Miserableness
- f.6159.0\* – Pain in last month
- f.136.0\* – Number of operations
- f.6177.0\* – Medications used for cholesterol, blood pressure or
  diabetes (male)
- f.6153.0\* – Medications used for cholesterol, blood pressure or
  diabetes (female)
- f.137.0\* – Number of medications used
- f.40007.\* – Age at death for deceased participants

``` r
# This chunk ran for set up and then just loaded as below for subsequent iterations
#df.a derived as follows: 

load("~/max2ruth2/bd671644_recoded.Robj")

df.a <- bd %>% select("f.eid", "f.31.0.0", "f.34.0.0", "f.52.0.0",
                      "f.21003.0.0":"f.21003.3.0", "f.21000.0.0":"f.21000.3.0",
                      "f.1200.0.0":"f.1200.3.0", "f.6148.0.0":"f.6148.3.4", 
                      "f.2227.0.0":"f.2227.3.0", "f.2020.0.0":"f.2020.3.0",
                      "f.2247.0.0":"f.2247.3.0",
                      "f.2188.0.0":"f.2188.3.0", "f.2178.0.0":"f.2178.3.0",
                      "f.2296.0.0":"f.2296.3.0", "f.2316.0.0":"f.2316.3.0",
                      "f.2080.0.0":"f.2080.3.0", "f.6150.0.0":"f.6150.2.3",
                      "f.20002.0.0":"f.20002.3.33", "f.6149.0.0":"f.6149.3.5",
                      "f.2335.0.0":"f.2335.3.0", "f.2443.0.0":"f.2443.3.0",
                      "f.2453.0.0":"f.2453.3.0", "f.134.0.0":"f.134.3.0",
                      "f.2463.0.0":"f.2463.3.0", "f.6145.0.0":"f.6145.3.2",
                      "f.3786.0.0":"f.3786.3.0", "f.2473.0.0":"f.2473.3.0", "f.2050.0.0":"f.2050.3.0",
                      "f.1970.0.0":"f.1970.3.0", "f.1930.0.0":"f.1930.3.0", "f.6159.0.0":"f.6159.3.6",
                      "f.136.0.0":"f.136.3.0", "f.6177.0.0":"f.6177.3.2", "f.6153.0.0":"f.6153.3.3",
                      "f.137.0.0":"f.137.3.0", "f.40007.0.0":"f.40007.1.0",
                      "f.6152.0.0":"f.6152.3.4")

write.csv(df.a,"/users/k1511199/UKBB_multimorbidity/UKBB.df.a.csv", row.names=F)
```

``` r
df.a <- read.csv("/users/k1511199/UKBB_multimorbidity/UKBB.df.a.csv")

## NA those who withdrew 
x<-read.csv("~/max2ruth/w90865_2023-04-25.csv",h=F)

df.a <- df.a[-which(df.a$f.eid%in%x$V1),]

## set row names as study id for sense checking
row.names(df.a) <- paste0("id",df.a$f.eid)
```

## **2. Function definition**

Recoding NA to -99999 as, presumably due to package version install on
CREATE and dplyr, NA and ifelse behaviour is unusual

``` r
#Function to search within a range of cols for a text variable and recode as 1 - returns a vector of the var 

text_recode <- function(var1, #first var in range
                        var2 = NULL, #second var in range
                        text, #Text to search in array and recode to 1
                        NAvals, ...){ #Text to recode as NA (-99999) sep multiple with |
  var1 = var1
  var2 = var2
  text = text
  df.b <- df.a %>% select(var1:var2)
  
  df.b <- apply(df.b,2,as.character) #Can throw error in next step if all factors instead
  
  df.b[is.na(df.b)] <- -99999
  
  var <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text, b)), 1, 
           ifelse(all(grepl(NAvals, b)|grepl(-99999, b)), -99999, 0))})
  return(var)}

#Function as above but where the question only allows inference of not having the condition - so all no or NA answers are recorded as 0
text_recode0only <- function(var1, #first var in range
                        var2 = NULL, #second var in range
                        text, #Text to search in array and recode to 0
                        NAvals, ...){ #Text to recode as NA (-99999), sep multiple with |
  var1 = var1
  var2 = var2
  text = text
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
    var <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text, b)), 0, -99999)})
  return(var)}


#Function for numeric variables -- codes any mention of morb as 1, and if no 1s and all responses arent NA, returns 0

num_recode <- function(var1, #first var in range
                        var2 = NULL, #second var in range
                        code){ #Code for morb 
  var1 = var1
  var2 = var2
  code = code
  df.b <- df.a %>% select(var1:var2)
  

  df.b[is.na(df.b)] <- -99999
  var <- apply(as.data.frame(df.b),1,function(b){
    b <- as.numeric(b)
    ifelse(all(b==-99999), -99999, 
               ifelse(any(b==code),1,0))
  })
  
  return(var)}
```

## **3. Clean datasets**

### 3.1 Glaucoma

``` r
glaucoma_t <- text_recode(var1= "f.6148.0.0", #first var in range
                        var2 = "f.6148.3.4",#second var in range
                        text = "Glaucoma", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(glaucoma_t, useNA="ifany")
```

    ## glaucoma_t
    ## -99999      0      1 
    ## 281675 211833   8861

``` r
glaucoma_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1277) #Text to search in array and recode to NA)

table(glaucoma_n, useNA="ifany")
```

    ## glaucoma_n
    ## -99999      0      1 
    ## 111964 384034   6371

``` r
#This Q for no other eye problems
noeyeprobs <- text_recode0only(var1="f.2227.0.0",
                               var2="f.2227.3.0",
                               text="No")


glaucoma <- ifelse(glaucoma_n==1|glaucoma_t==1,1, #This condition first
                ifelse(glaucoma_n==0|glaucoma_t==0|noeyeprobs==0,0,NA))

table(glaucoma, useNA="ifany")
```

    ## glaucoma
    ##      0      1   <NA> 
    ## 492099   9151   1119

### 3.2 Cataracts

Deviation from DW method - for this var if answer is Do not know, coding
as 0 (as if they don’t know they probably don’t have cataracts)

``` r
cataracts_t <- text_recode(var1= "f.6148.0.0", #first var in range
                        var2 = "f.6148.3.4",#second var in range
                        text = "Cataract", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(cataracts_t, useNA="ifany")
```

    ## cataracts_t
    ## -99999      0      1 
    ## 281675 194952  25742

``` r
cataracts_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1278) #Text to search in array and recode to NA)

table(cataracts_n, useNA="ifany")
```

    ## cataracts_n
    ## -99999      0      1 
    ## 111964 378294  12111

``` r
#same no other eye probs logic

cataracts <- ifelse(cataracts_n==1|cataracts_t==1,1, #This condition first
                ifelse(cataracts_n==0|cataracts_t==0|noeyeprobs==0,0,NA))

table(cataracts, useNA="ifany")
```

    ## cataracts
    ##      0      1   <NA> 
    ## 474008  27242   1119

### 3.3 Hearing

Hearing: dichotomous on any/no hearing problems; deaf also coded to 1

Those answering ‘prefer not to answer’ are coded as missing

Deviating from DW if responded ‘Do not know’ coded as 0

``` r
hearing_t <-  text_recode(var1= "f.2247.0.0", #first var in range
                        var2 = "f.2247.3.0",#second var in range
                        text = "Yes", 
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(hearing_t, useNA="ifany")
```

    ## hearing_t
    ## -99999      0      1 
    ##  21726 346369 134274

``` r
hearing <- ifelse(hearing_t==-99999, NA, hearing_t)
table(hearing, useNA="ifany")
```

    ## hearing
    ##      0      1   <NA> 
    ## 346369 134274  21726

### 3.4 Infirmity

Coding as in 3c

``` r
infirmity_t <-  text_recode(var1= "f.2188.0.0", #first var in range
                        var2 = "f.2188.3.0",#second var in range
                        text = "Yes", 
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(infirmity_t, useNA="ifany")
```

    ## infirmity_t
    ## -99999      0      1 
    ##  12146 320483 169740

``` r
infirmity <- ifelse(infirmity_t==-99999, NA, infirmity_t)
table(infirmity, useNA="ifany")
```

    ## infirmity
    ##      0      1   <NA> 
    ## 320483 169740  12146

### 3.5 self-rated health

``` r
  var1 = "f.2178.0.0"
  var2 = "f.2178.3.0"
  text0 = "Excellent"
  text0.25 = "Good"
  text0.5 = "Fair"
  text1= "Poor"
  NAvals = "Do not know|Prefer not to answer"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
SRH_t <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text1, b)), 1,
                ifelse(any(grepl(text0.5, b)), 0.5,
                       ifelse(any(grepl(text0.25, b)), 0.25,
                              ifelse(any(grepl(text0, b)), 0, -99999))))})

table(SRH_t, useNA="ifany")
```

    ## SRH_t
    ## -99999      0   0.25    0.5      1 
    ##   3320  72691 290256 111962  24140

``` r
SRH <- ifelse(SRH_t==-99999, NA, SRH_t)
table(SRH, useNA="ifany")
```

    ## SRH
    ##      0   0.25    0.5      1   <NA> 
    ##  72691 290256 111962  24140   3320

### 3.6 Falls

(DW’s STATA code):

gen i6_falls = 0 if n_2296==1 replace i6_falls = 0.5 if n_2296==2
replace i6_falls = 1 if n_2296==3

``` r
  var1 = "f.2296.0.0"
  var2 = "f.2296.3.0"
  text0 = "No falls"
  text0.5 = "Only one fall"
  text1= "More than one fall"
  NAvals = "Prefer not to answer|Do not know"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
falls_t <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text1, b)), 1,
                ifelse(any(grepl(text0.5, b)), 0.5,
                              ifelse(any(grepl(text0, b)), 0, -99999)))})

table(falls_t, useNA="ifany")
```

    ## falls_t
    ## -99999      0    0.5      1 
    ##   2582 389548  73571  36668

``` r
falls <- ifelse(falls_t==-99999, NA, falls_t)
table(falls, useNA="ifany")
```

    ## falls
    ##      0    0.5      1   <NA> 
    ## 389548  73571  36668   2582

### 3.7 Breathing/wheeze

``` r
wheeze_t <- text_recode(var1= "f.2316.0.0", #first var in range
                        var2 = "f.2316.3.0",#second var in range
                        text = "Yes", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") 

table(wheeze_t, useNA="ifany")
```

    ## wheeze_t
    ## -99999      0      1 
    ##  10858 381548 109963

``` r
wheeze <- ifelse(wheeze_t==-99999, NA, wheeze_t)
table(wheeze, useNA="ifany")
```

    ## wheeze
    ##      0      1   <NA> 
    ## 381548 109963  10858

### 3.8 Fatigue

(DW stata code):

gen i8_fatigue = 0 if n_2080==1 replace i8 = 0.25 if n_2080==2 replace
i8 = 0.75 if n_2080==3 replace i8 = 1 if n_2080==4

``` r
  var1 = "f.2080.0.0"
  var2 = "f.2080.3.0"
  text0 = "Not at all"
  text0.25 = "Several days"
  text0.75 = "More than half the days"
  text1= "Nearly every day"
  NAvals = "Do not know|Prefer not to answer"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
fatigue_t <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text1, b)), 1,
                ifelse(any(grepl(text0.75, b)), 0.75,
                       ifelse(any(grepl(text0.25, b)), 0.25,
                              ifelse(any(grepl(text0, b)), 0, -99999))))})

table(fatigue_t, useNA="ifany")
```

    ## fatigue_t
    ## -99999      0   0.25   0.75      1 
    ##  15938 215958 203857  32810  33806

``` r
fatigue <- ifelse(fatigue_t==-99999, NA, fatigue_t)
table(fatigue, useNA="ifany")
```

    ## fatigue
    ##      0   0.25   0.75      1   <NA> 
    ## 215958 203857  32810  33806  15938

### 3.9 Myocardial infarction (MI)

``` r
MI_t <- text_recode(var1= "f.6150.0.0", #first var in range
                        var2 = "f.6150.2.3",#second var in range
                        text = "Heart attack", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(MI_t, useNA="ifany")
```

    ## MI_t
    ## -99999      0      1 
    ##   2184 488339  11846

``` r
MI_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1075) #Text to search in array and recode to NA)
table(MI_n, useNA="ifany")
```

    ## MI_n
    ## -99999      0      1 
    ## 111964 378163  12242

``` r
MI <- ifelse(MI_n==1|MI_t==1,1, #This condition first
                ifelse(MI_n==0|MI_t==0,0,NA))

table(MI, useNA="ifany")
```

    ## MI
    ##      0      1   <NA> 
    ## 489001  12509    859

### 3.10 Angina

``` r
angina_t <- text_recode(var1= "f.6150.0.0", #first var in range
                        var2 = "f.6150.2.3",#second var in range
                        text = "Angina", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(angina_t, useNA="ifany")
```

    ## angina_t
    ## -99999      0      1 
    ##   2184 483700  16485

``` r
angina_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1074) #Text to search in array and recode to NA)
table(angina_n, useNA="ifany")
```

    ## angina_n
    ## -99999      0      1 
    ## 111964 373515  16890

``` r
angina <- ifelse(angina_n==1|angina_t==1,1, #This condition first
                ifelse(angina_n==0|angina_t==0,0,NA))

table(angina, useNA="ifany")
```

    ## angina
    ##      0      1   <NA> 
    ## 484249  17261    859

### 3.11 Stroke

``` r
stroke_t <- text_recode(var1= "f.6150.0.0", var2 = "f.6150.2.3",
                        text = "Stroke", NAvals = "Prefer not to answer|Do not know") 

table(stroke_t, useNA="ifany")
```

    ## stroke_t
    ## -99999      0      1 
    ##   2184 492285   7900

``` r
#'1081' (ambiguous stroke)
stroke_n1 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1081)
table(stroke_n1, useNA="ifany")
```

    ## stroke_n1
    ## -99999      0      1 
    ## 111964 383185   7220

``` r
#'1583' (ischaemic)
stroke_n2 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1583) #Text to search in array and recode to NA)
table(stroke_n2, useNA="ifany")
```

    ## stroke_n2
    ## -99999      0      1 
    ## 111964 390337     68

``` r
stroke <- ifelse(stroke_n1==1|stroke_n2==1|stroke_t==1,1, #This condition first
                ifelse(stroke_n1==0|stroke_n2==0|stroke_t==0,0,NA))

table(stroke, useNA="ifany")
```

    ## stroke
    ##      0      1   <NA> 
    ## 493099   8411    859

### 3.12 High blood pressure (HBP)

``` r
HBP_t <- text_recode(var1= "f.6150.0.0", #first var in range
                        var2 = "f.6150.2.3",#second var in range
                        text = "High blood pressure", #Text to search in array and recode to 1
                        NAvals = "Prefer not to answer|Do not know") #Text to recode, sep multiple with 

table(HBP_t, useNA="ifany")
```

    ## HBP_t
    ## -99999      0      1 
    ##   2184 362115 138070

``` r
HBP_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1065) #Text to search in array and recode to NA)
table(HBP_n, useNA="ifany")
```

    ## HBP_n
    ## -99999      0      1 
    ## 111964 253063 137342

``` r
HBP <- ifelse(HBP_n==1|HBP_t==1,1, #This condition first
                ifelse(HBP_n==0|HBP_t==0,0,NA))

table(HBP, useNA="ifany")
```

    ## HBP
    ##      0      1   <NA> 
    ## 357931 143579    859

### 3.13 Rheumatoid arthritis (RA)

Coding ‘Do not know’ as to other serios medical conditions also as 0 -
difference from DW method

``` r
RA_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1464) #Text to search in array and recode to NA)

table(RA_n, useNA="ifany")
```

    ## RA_n
    ## -99999      0      1 
    ## 111964 384319   6086

``` r
#This Q for Other serious medical condition/disability diagnosed by doctor
noothercond <- text_recode0only(var1="f.2473.0.0",
                               var2="f.2473.3.0",
                               text="No|Do not know")

table(noothercond)
```

    ## noothercond
    ## -99999      0 
    ##  94465 407904

``` r
RA <- ifelse(RA_n==1, 1, #This condition first
                ifelse(RA_n==0|noothercond==0,0,NA))

table(RA, useNA="ifany")
```

    ## RA
    ##      0      1   <NA> 
    ## 491681   6086   4602

### 3.14 Osteoarthritis

``` r
osteoarthritis_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1465) #Text to search in array and recode to NA)
table(osteoarthritis_n, useNA="ifany")
```

    ## osteoarthritis_n
    ## -99999      0      1 
    ## 111964 345233  45172

``` r
osteoarthritis <- ifelse(osteoarthritis_n==1, 1, #This condition first
                ifelse(osteoarthritis_n==0|noothercond==0,0,NA))

table(osteoarthritis, useNA="ifany")
```

    ## osteoarthritis
    ##      0      1   <NA> 
    ## 452595  45172   4602

### 3.15 Gout

``` r
gout_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1466) #Text to search in array and recode to NA)
table(gout_n, useNA="ifany")
```

    ## gout_n
    ## -99999      0      1 
    ## 111964 382603   7802

``` r
gout <- ifelse(gout_n==1, 1, #This condition first
                ifelse(gout_n==0|noothercond==0,0,NA))

table(gout, useNA="ifany")
```

    ## gout
    ##      0      1   <NA> 
    ## 489965   7802   4602

### 3.16 Dental problems

``` r
dental.problems_t <- text_recode(var1= "f.6149.0.0", var2 = "f.6149.3.5",
                        text = "Bleeding gums|Dentures|Loose teeth|Mouth ulcers|Painful gums|Toothache", #Text to search in array and recode to 1
                        NAvals ="Prefer not to answer|Do not know") 

table(dental.problems_t, useNA="ifany")
```

    ## dental.problems_t
    ## -99999      0      1 
    ##   5318 289783 207268

``` r
dental.problems <- ifelse(dental.problems_t==-99999, NA, dental.problems_t)
table(dental.problems, useNA="ifany")
```

    ## dental.problems
    ##      0      1   <NA> 
    ## 289783 207268   5318

### 3.17 Chest pain

Deviating from DW to code ‘Do not know’ as 0

``` r
chest.pain_t <- text_recode(var1= "f.2335.0.0", var2 = "f.2335.3.0",
                        text = "Yes",
                        NAvals = "Prefer not to answer|Do not know") 

table(chest.pain_t, useNA="ifany")
```

    ## chest.pain_t
    ## -99999      0      1 
    ##   6247 409780  86342

``` r
chest.pain <- ifelse(chest.pain_t==-99999, NA, chest.pain_t)
table(chest.pain, useNA="ifany")
```

    ## chest.pain
    ##      0      1   <NA> 
    ## 409780  86342   6247

### 3.18 Sciatica

``` r
sciatica_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1476) #Text to search in array and recode to NA)
table(sciatica_n, useNA="ifany")
```

    ## sciatica_n
    ## -99999      0      1 
    ## 111964 384292   6113

``` r
sciatica <- ifelse(sciatica_n==1, 1, #This condition first
                ifelse(sciatica_n==0|noothercond==0,0,NA))

table(sciatica, useNA="ifany")
```

    ## sciatica
    ##      0      1   <NA> 
    ## 491654   6113   4602

### 3.19 Diabetes

``` r
diabetes_t <- text_recode(var1= "f.2443.0.0", var2 = "f.2443.3.0",
                        text = "Yes", NAvals = "Prefer not to answer|Do not know") 

table(diabetes_t, useNA="ifany")
```

    ## diabetes_t
    ## -99999      0      1 
    ##   2496 471078  28795

``` r
#'1220 diabetes
diabetes_n1 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1220)
table(diabetes_n1, useNA="ifany")
```

    ## diabetes_n1
    ## -99999      0      1 
    ## 111964 367279  23126

``` r
#'1223 T2 diabetes
diabetes_n2 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1223) #Text to search in array and recode to NA)
table(diabetes_n2, useNA="ifany")
```

    ## diabetes_n2
    ## -99999      0      1 
    ## 111964 385517   4888

``` r
diabetes <- ifelse(diabetes_n1==1|diabetes_n2==1|diabetes_t==1,1, #This condition first
                ifelse(diabetes_n1==0|diabetes_n2==0|diabetes_t==0,0,NA))

table(diabetes, useNA="ifany")
```

    ## diabetes
    ##      0      1   <NA> 
    ## 472391  29121    857

### 3.20 Cancer

``` r
cancer_t  <- text_recode(var1= "f.2453.0.0", var2 = "f.2453.3.0",
                        text = "Yes", NAvals = "Prefer not to answer|Do not know") 

table(cancer_t, useNA="ifany")
```

    ## cancer_t
    ## -99999      0      1 
    ##   2591 456058  43720

``` r
#134 - number self reported cancers 
var1 = "f.134.0.0"
  var2 = "f.134.3.0"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999

#Extracts the highest number across the array 
cancer_n_total <- apply(df.b,1, function(b){max(b)})

table(cancer_n_total, useNA="ifany")
```

    ## cancer_n_total
    ## -99999      0      1      2      3      4      5      6 
    ##    843 454301  43910   2998    271     36      8      2

``` r
cancer <- ifelse(cancer_n_total>=1|cancer_t==1,1, #This condition first
                ifelse(cancer_n_total==0|cancer_t==0,0,NA))

table(cancer, useNA="ifany")
```

    ## cancer
    ##      0      1   <NA> 
    ## 451805  50166    398

### 3.21 Multiple cancers

``` r
cancer.multi <- ifelse(cancer_n_total>1,1, #This condition first
                  ifelse(cancer_n_total<=1,0,NA))

table(cancer.multi, useNA="ifany")
```

    ## cancer.multi
    ##      0      1 
    ## 499054   3315

### 3.22 Fractures

Again ‘Do not know’ coded as zero

``` r
fractures_t <- text_recode(var1= "f.2463.0.0", var2 = "f.2463.3.0",
                        text = "Yes",
                        NAvals = "Prefer not to answer|Do not know") 

table(fractures_t, useNA="ifany")
```

    ## fractures_t
    ## -99999      0      1 
    ##   3430 445367  53572

``` r
fractures <- ifelse(fractures_t==-99999, NA, fractures_t)
table(fractures, useNA="ifany")
```

    ## fractures
    ##      0      1   <NA> 
    ## 445367  53572   3430

### 3.23 Deep vein thrombosis (DVT)

``` r
dvt_t <- text_recode(var1= "f.6152.0.0", var2 = "f.6152.3.4",
                        text = "DVT", NAvals = "Prefer not to answer|Do not know") 

table(dvt_t, useNA="ifany")
```

    ## dvt_t
    ## -99999      0      1 
    ##   2075 489365  10929

``` r
dvt_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1094)
table(dvt_n, useNA="ifany")
```

    ## dvt_n
    ## -99999      0      1 
    ## 111964 379982  10423

``` r
dvt <- ifelse(dvt_n==1|dvt_t==1,1, #This condition first
                ifelse(dvt_n==0|dvt_t==0,0,NA))

table(dvt, useNA="ifany")
```

    ## dvt
    ##      0      1   <NA> 
    ## 490492  11087    790

### 3.24 Emphysema/chronic bronchitis

``` r
#emphysema is the same variable as CD 
emphysema_t <- text_recode(var1= "f.6152.0.0", var2 = "f.6152.3.4",
                        text = "Emphysema", NAvals = "Prefer not to answer|Do not know") 

table(emphysema_t, useNA="ifany")
```

    ## emphysema_t
    ## -99999      0      1 
    ##   2075 491310   8984

``` r
emphysema_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1113)
table(emphysema_n, useNA="ifany")
```

    ## emphysema_n
    ## -99999      0      1 
    ## 111964 383239   7166

``` r
emphysema <- ifelse(emphysema_n==1|emphysema_t==1,1, #This condition first
                ifelse(emphysema_n==0|emphysema_t==0,0,NA))

table(emphysema, useNA="ifany")
```

    ## emphysema
    ##      0      1   <NA> 
    ## 492502   9077    790

### 3.25 Asthma

``` r
asthma_t <- text_recode(var1= "f.6152.0.0", var2 = "f.6152.3.4",
                        text = "Asthma", NAvals = "Prefer not to answer|Do not know") 

table(asthma_t, useNA="ifany")
```

    ## asthma_t
    ## -99999      0      1 
    ##   2075 440453  59841

``` r
asthma_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1111)
table(asthma_n, useNA="ifany")
```

    ## asthma_n
    ## -99999      0      1 
    ## 111964 330466  59939

``` r
asthma <- ifelse(asthma_n==1|asthma_t==1,1, #This condition first
                ifelse(asthma_n==0|asthma_t==0,0,NA))

table(asthma, useNA="ifany")
```

    ## asthma
    ##      0      1   <NA> 
    ## 440264  61315    790

### 3.26 Allergies

Coded to include eczema and hayfever/rhinitis

``` r
#grepl will pick up Hayfever, allergic rhinitis or eczema
allergies_t <- text_recode(var1= "f.6152.0.0", var2 = "f.6152.3.4",
                        text = "Hayfever", NAvals = "Prefer not to answer|Do not know") 

table(allergies_t, useNA="ifany")
```

    ## allergies_t
    ## -99999      0      1 
    ##   2075 380570 119724

``` r
#1387 hayfever/rhinitis
allergies_n1 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1387)
table(allergies_n1, useNA="ifany")
```

    ## allergies_n1
    ## -99999      0      1 
    ## 111964 357431  32974

``` r
#1452 // eczema code
allergies_n2 <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1452)
table(allergies_n2, useNA="ifany")
```

    ## allergies_n2
    ## -99999      0      1 
    ## 111964 374889  15516

``` r
allergies <- ifelse(allergies_n1==1|allergies_n2==1|allergies_t==1,1, #This condition first
                ifelse(allergies_n1==0|allergies_n2==0|allergies_t==0,0,NA))

table(allergies, useNA="ifany")
```

    ## allergies
    ##      0      1   <NA> 
    ## 373942 127637    790

### 3.27 Hypothyroidism

``` r
hypothyroidism_n <- num_recode(var1= "f.20002.0.0", #first var in range
                        var2 = "f.20002.3.33",#second var in range
                        code=1226) #Text to search in array and recode to NA)
table(hypothyroidism_n, useNA="ifany")
```

    ## hypothyroidism_n
    ## -99999      0      1 
    ## 111964 364728  25677

``` r
hypothyroidism <- ifelse(hypothyroidism_n==1, 1, #This condition first
                ifelse(hypothyroidism_n==0|noothercond==0,0,NA))


table(hypothyroidism, useNA="ifany")
```

    ## hypothyroidism
    ##      0      1   <NA> 
    ## 472090  25677   4602

### 3.28 Recent depressed mood (depression)

Same Q re timings as in 3.5? Also wonder if coding should be consistent
with that? (0.5 here rather than 0.2?)

``` r
 var1 = "f.2050.0.0"
  var2 = "f.2050.3.0"
  text0 = "Not at all"
  text0.5 = "Several days"
  text0.75 = "More than half the days"
  text1= "Nearly every day"
  NAvals = "Do not know|Prefer not to answer"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
depression_t <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text1, b)), 1,
                ifelse(any(grepl(text0.75, b)), 0.75,
                       ifelse(any(grepl(text0.5, b)), 0.5,
                              ifelse(any(grepl(text0, b)), 0, -99999))))})

table(depression_t, useNA="ifany")
```

    ## depression_t
    ## -99999      0    0.5   0.75      1 
    ##  22104 359450  94661  15947  10207

``` r
depression <- ifelse(depression_t==-99999, NA, depression_t)
table(depression, useNA="ifany")
```

    ## depression
    ##      0    0.5   0.75      1   <NA> 
    ## 359450  94661  15947  10207  22104

### 3.29 Anxiousness

Deviating from method to code ‘Do not know’ as 0

``` r
anxiousness_t <- text_recode(var1= "f.1970.0.0", var2 = "f.1970.3.0",
                        text = "Yes",
                        NAvals = "Prefer not to answer|Do not know") 

table(anxiousness_t, useNA="ifany")
```

    ## anxiousness_t
    ## -99999      0      1 
    ##  13563 368983 119823

``` r
anxiousness <- ifelse(anxiousness_t==-99999, NA, anxiousness_t)
table(anxiousness, useNA="ifany")
```

    ## anxiousness
    ##      0      1   <NA> 
    ## 368983 119823  13563

### 3.30 Severe anxiety

``` r
severe.anxiety_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=1111)
table(severe.anxiety_n, useNA="ifany")
```

    ## severe.anxiety_n
    ## -99999      0      1 
    ## 111964 330466  59939

``` r
severe.anxiety <- ifelse(severe.anxiety_n==1, 1, #This condition first
                ifelse(severe.anxiety_n==0|noothercond==0,0,NA))

table(severe.anxiety, useNA="ifany")
```

    ## severe.anxiety
    ##      0      1   <NA> 
    ## 437828  59939   4602

### 3.31 Mood - misery

``` r
misery_t <- text_recode(var1= "f.1930.0.0", var2 = "f.1930.3.0",
                        text = "Yes",
                        NAvals = "Prefer not to answer|Do not know") 

table(misery_t, useNA="ifany")
```

    ## misery_t
    ## -99999      0      1 
    ##   9507 275443 217419

``` r
misery <- ifelse(misery_t==-99999, NA, misery_t)
table(misery, useNA="ifany")
```

    ## misery
    ##      0      1   <NA> 
    ## 275443 217419   9507

### 3.32 Loneliness

``` r
loneliness_t <- text_recode(var1= "f.2020.0.0", var2 = "f.2020.3.0",
                        text = "Yes",
                        NAvals = "Prefer not to answer|Do not know") 

table(loneliness_t, useNA="ifany")
```

    ## loneliness_t
    ## -99999      0      1 
    ##   9009 396655  96705

``` r
loneliness <- ifelse(loneliness_t==-99999, NA, loneliness_t)
table(loneliness, useNA="ifany")
```

    ## loneliness
    ##      0      1   <NA> 
    ## 396655  96705   9009

### 3.33 Head and/or neck pain

Assume head pain = headache

``` r
head.neck.pain_t <- text_recode(var1= "f.6159.0.0", var2 = "f.6159.3.6",
                        text = "Neck or shoulder pain|Headache",
                        NAvals = "Prefer not to answer|Do not know" ) 

table(head.neck.pain_t, useNA="ifany")
```

    ## head.neck.pain_t
    ## -99999      0      1 
    ##   2115 313291 186963

``` r
head.neck.pain <- ifelse(head.neck.pain_t==-99999, NA, head.neck.pain_t)
table(head.neck.pain, useNA="ifany")
```

    ## head.neck.pain
    ##      0      1   <NA> 
    ## 313291 186963   2115

### 3.34 to 3.39: Pain domains - (34) Back pain, (35) stomach pain, (36) Hip, (37) knee pain, (38) whole body, (39) facial pain

Same code so running in loop

``` r
t <- unique(df.a$f.6159.0.0)
t <- t[c(1,4,6:9)] #Pull out vars to be coded

pain.domainsL <- lapply(t, function(t){
  var_t <- text_recode(var1= "f.6159.0.0", var2 = "f.6159.3.6",
                        text = t,
                        NAvals = "Prefer not to answer|Do not know" ) 
  
  var <- ifelse(var_t==-99999, NA, var_t)})

names(pain.domainsL) <- gsub(" ", ".", t)

lapply(pain.domainsL, function(x){table(x, useNA="ifany")})
```

    ## $Pain.all.over.the.body
    ## x
    ##      0      1   <NA> 
    ## 490740   9514   2115 
    ## 
    ## $Hip.pain
    ## x
    ##      0      1   <NA> 
    ## 437054  63200   2115 
    ## 
    ## $Knee.pain
    ## x
    ##      0      1   <NA> 
    ## 381777 118477   2115 
    ## 
    ## $Stomach.or.abdominal.pain
    ## x
    ##      0      1   <NA> 
    ## 453099  47155   2115 
    ## 
    ## $Back.pain
    ## x
    ##      0      1   <NA> 
    ## 360762 139492   2115 
    ## 
    ## $Facial.pain
    ## x
    ##      0      1   <NA> 
    ## 490169  10085   2115

``` r
list2env(pain.domainsL, .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

### 3.40 Sleep

ACE touchscreen question “Do you have trouble falling asleep at night or
do you wake up in the middle of the night?”

``` r
var1 = "f.1200.0.0"
  var2 = "f.1200.3.0"
  text0 = "Never/rarely"
  text0.5 = "Sometimes"
  text1= "Usually"
  NAvals = "Prefer not to answer|Do not know"
  
  df.b <- df.a %>% select(var1:var2)
  
  df.b[is.na(df.b)] <- -99999
  
sleep_t <- apply(df.b,1, function(b){
    v <- ifelse(any(grepl(text1, b)), 1,
                ifelse(any(grepl(text0.5, b)), 0.5,
                              ifelse(any(grepl(text0, b)), 0, -99999)))})

table(sleep_t, useNA="ifany")
```

    ## sleep_t
    ## -99999      0    0.5      1 
    ##   1447 109840 236745 154337

``` r
sleep <- ifelse(sleep_t==-99999, NA, sleep_t)
table(sleep, useNA="ifany")
```

    ## sleep
    ##      0    0.5      1   <NA> 
    ## 109840 236745 154337   1447

### 3.41 High cholesterol

*note* - need to combine separate vars for male and female
participants - based on medication use

``` r
#female var
high.cholesterol_t1 <- text_recode(var1= "f.6153.0.0", var2 = "f.6153.3.3",
                        text = "Cholesterol",
                        NAvals = "Prefer not to answer|Do not know") 

table(high.cholesterol_t1, useNA="ifany")
```

    ## high.cholesterol_t1
    ## -99999      0      1 
    ## 232386 230998  38985

``` r
#male var
high.cholesterol_t2 <- text_recode(var1= "f.6177.0.0", var2 = "f.6177.3.2",
                        text = "Cholesterol",
                        NAvals = "Prefer not to answer|Do not know") 

table(high.cholesterol_t2, useNA="ifany")
```

    ## high.cholesterol_t2
    ## -99999      0      1 
    ## 277211 165838  59320

``` r
high.cholesterol <- ifelse(high.cholesterol_t1==1|high.cholesterol_t2==1, 1,
                           ifelse(high.cholesterol_t1==0|high.cholesterol_t2==0,0,NA))
                     
table(high.cholesterol, useNA="ifany")
```

    ## high.cholesterol
    ##      0      1   <NA> 
    ## 396836  98305   7228

### 3.42 - 3.49 Other conditions

All same coding (and as same as other above but easier to keep track
this way):

- 42 Pneumonia - code 1398 in 20002
- 43 Gastric reflux - code 1138 in 20002
- 44 Hiatus hernia - code 1474 in 20002
- 45 diverticulitis - code 1458 in 20002
- 46 Gall stones - code 1162 in 20002
- 47 Psoriasis - code 1453 in 20002
- 48 Osteoporosis - code 1309 in 20002
- 49 Migraine - code 1265 in 20002

``` r
codes <- c(1398, 1138, 1474, 1458, 1162, 1453, 1309, 1265)

other.conds.L <- lapply(codes, function(i){

      var_n <- num_recode(var1= "f.20002.0.0", var2 = "f.20002.3.33",
                        code=i)
  
      var <- ifelse(var_n==1, 1, #This condition first
                ifelse(var_n==0|noothercond==0,0,NA))})

names(other.conds.L) <- c("pneumonia", "gastric.reflux", "hiatus.hernia",
                          "diverticulitis", "gall.stones", "psoriasis",
                          "osteoporosis", "migrane")

lapply(other.conds.L, function(x){table(x, useNA="ifany")})
```

    ## $pneumonia
    ## x
    ##      0      1   <NA> 
    ## 489300   8467   4602 
    ## 
    ## $gastric.reflux
    ## x
    ##      0      1   <NA> 
    ## 472167  25600   4602 
    ## 
    ## $hiatus.hernia
    ## x
    ##      0      1   <NA> 
    ## 485145  12622   4602 
    ## 
    ## $diverticulitis
    ## x
    ##      0      1   <NA> 
    ## 491221   6546   4602 
    ## 
    ## $gall.stones
    ## x
    ##      0      1   <NA> 
    ## 488561   9206   4602 
    ## 
    ## $psoriasis
    ## x
    ##      0      1   <NA> 
    ## 491380   6387   4602 
    ## 
    ## $osteoporosis
    ## x
    ##      0      1   <NA> 
    ## 488600   9167   4602 
    ## 
    ## $migrane
    ## x
    ##      0      1   <NA> 
    ## 481204  16563   4602

``` r
list2env(other.conds.L, .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

## **Generate FI**

### Combine domains

``` r
FI.domains <- data.frame(id=df.a$f.eid, D1.glaucoma=glaucoma,
  D2.cataracts=cataracts, D3.hearing=hearing,
  D4.infirmity=infirmity, D5.self.rated.health=SRH,
  D6.falls=falls, D7.wheeze=wheeze,
  D8.fatigue=fatigue, D9.MI=MI, D10.angina=angina,
  D11.stroke=stroke, D12.HBP=HBP, D13.RA=RA,
  D14.osteoarthritis=osteoarthritis, D15.gout=gout,
  D16.dental.probelms=dental.problems,
  D17.chest.pain=chest.pain, D18.sciatica=sciatica,
  D19.diabetes=diabetes, D20.cancer=cancer,
  D21.cancer.multi=cancer.multi,
  D22.fractures=fractures, D23.DVT=dvt,
  D24.emphysema=emphysema, D25.asthma=asthma,
  D28.allergies=allergies,
  D27.hypothyroidism=hypothyroidism,
  D28.depression=depression, D29.anxiousness=anxiousness,
  D30.severe.anxiety=severe.anxiety, D31.misery=misery,
  D32.loneliness=loneliness,
  D33.head.neck.pain=head.neck.pain, 
  D34.back.pain=Back.pain, D35.stomach.pain=Stomach.or.abdominal.pain,
  D36.hip.pain=Hip.pain, D37.knee.pain=Knee.pain, 
  D38.whole.body.pain=Pain.all.over.the.body, 
  D39.facial.pain=Facial.pain, D40.sleep=sleep,
  D41.high.cholesterol=high.cholesterol, 
  D42.pneumonia=pneumonia, D43.gastric.reflux=gastric.reflux,
  D44.hiatus.hernia=hiatus.hernia, D45.diverticulitis=diverticulitis,
  D46.gall.stones=gall.stones, D47.psoriasis=psoriasis, 
  D48.osteoporosis=osteoporosis, D49.migrane=migrane)
```

### Calculate score

One method of FI is to divide the sum of all score by the number of
variables available for that individual (ie the number of non NA
domains)

``` r
rs <- rowSums(FI.domains[c(2:49)],na.rm=T)
FI.domains$nNas <- apply(FI.domains[c(2:49)],1,function(x){sum(is.na(x))})

FI.domains$fi <- rs/(49-FI.domains$nNas) #49 is max number of domains - calculating here the number of domains to divide the total score by
```

### FI distribution

``` r
library(ggplot2)
ggplot(FI.domains, aes(fi)) + geom_histogram(fill="cornflowerblue") + theme_minimal() + xlab("FI") + ylab("n")
```

![](UKBB_FI_R_files/figure-gfm/fi%20histogram-1.png)<!-- -->

``` r
write.csv(FI.domains, "FI.domains.DNKasNA.csv", row.names=F)
```
