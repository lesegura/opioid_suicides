library(tidyverse)

### I am going to pull the data from dropbox. First, save the dropbox links to an object
dataUrl <- c("https://www.dropbox.com/s/rhki8s9so275ucv/NSDUH_2015.RData?dl=0", "https://www.dropbox.com/s/d24bhm356bbnd1h/NSDUH_2016.RData?dl=0", 
             "https://www.dropbox.com/s/sragzrksgvj1jk3/NSDUH_2017.RData?dl=0", "https://www.dropbox.com/s/3cqon8ki69p1cgd/NSDUH_2018.RData?dl=0")

### change the dl-0 at the end for dl=1
for(i in dataUrl){
  str_sub(i, -1, -1) <- "1"
  load(url(i))
}

ls(pattern = "PUF") ### look for the dataset names, they start with PUF

dataList <- list(NSDUH15 = PUF2015_021518, 
                 NSDUH16 = PUF2016_022818, 
                 NSDUH17 = PUF2017_100918, 
                 NSDUH18 = PUF2018_100819) ### save datasets into a list


### Check the codebooks for the variable names that you need, and save them to an object
myvars <- c("questid2", "vestr", "verep", "analwt_c", "opinmyr", "opinmmon", "mrjylu",
            "fentpdapyu", "fentpdpymu", "pnrndaypm", "heraglst", "heryrtot",
            "herylu", "hermlu", "irpnrnminit", "irpnrnmage", "irpnrnmyfu", "mrjyr", "mrjmon", "heryr", 
            "herflag", "hermon", "pnranyflag", "pnranyyr", "pnrnmflag", "pnrnmyr", "pnrnmmon", "mrjyrbfr",
            "mjonlyflag", "mjonlyyr", "mjonlymon", "opinmyr", "opinmmon", "health2",
            "herpnryr", "illflag", "illyr", "illmon", "illemflag", "illemyr", "illemmon", 
            "bngdrkmon", "hvydrkmon", "alcmon", "illalcmon", "abodher", "udpypnr",
            "suicthnk", "suicplan", "mhsuithk", "adwrsthk", "adwrspln", "adwrsatp", "ad_mdea9", 
            "yowrdlot", "yowrdbtr", "yowrsthk", "yowrspln", "yowrsatp", "yo_mdea9", "pnrrsotrs2", 
            "smiyr_u", "amiyr_u", "smmiyr_u", "mmiyr_u", "lmiyr_u", "lmmiyru", "mi_cat_u", "smisudpy", 
            "amisudpy", "lmmisudpy", "snrlgsvc", "snrlgimp", "snrldcsn", "amdelt", "amdeyr", "ymdelt", 
            "ymdeyr", "ytxmdeyr", "yrxmdeyr", "ymdetxrx", "yhltmde", "yaltmde", "ymdehprx", "ymdehpo", "ymderxo2", 
            "ymdeharx", "ydocmde", "yomdmde", "ypsy1mde", "ypsy2mde", "ysocmde", "ycounmde", "yomhmde", "ynursmde", 
            "yrelmde", "yhbchmde", "yothmde", "anymhin2", "anymhout", "anysmh2", "anynsmh", "anymhed2", "anysedmf", 
            "yped", "yjail", "spinvst2", "spoutvst", "smhvst2", "smhdpr2", "smhfmly2", "smhsui2", "smhbrk2", "smhfea2",
            "smhschl2", "smhangr2", "smhfrnd2", "smhotpp2", "smheat2", "smhfite2", "smhmend2", "ymhosptx", "ymhnsptx", 
            "ysptxnmh", "ymhasptx", "newrace2", "irsex", "catage", "catag2", "catag3", "catag6", "catag7", "coutyp4", 
            "pden10", "cocyr", "crkyr", "lsdyr", "pcpyr", "hallucyr", "ecstmoyr", "damtfxyr", "oxycnanyyr",
            "ketminyr", "salviayr", "inhalyr", "methamyr", "trqanyyr", "stmanyyr", "sedanyyr", "methamyr",
            "sedanyyr", "trqnmyr", "dsthop12", "psyanyyr", "oxycnnmyr", "trqnmyr", "stmnmyr", "sednmyr", "psychyr", "talkprob")


### look to do some data wrangling. 
for(i in 1:4){
  names(dataList[[i]]) <- tolower(names(dataList[[i]])) ### turning variable names to low caps
  dataList[[i]] <- dataList[[i]][ , myvars] ### subsetting each dataset with only the variables in myvars
  dataList[[i]][["year"]] <- i + 2014 ### creating a year variable (starting year - 1)
  dataList[[i]][["newid"]] <- seq_len(nrow(dataList[[i]])) ### creating a numeric variable 1 to the length of the dataset.
  dataList[[i]][["realid"]] <- (dataList[[i]][["year"]] * 1e5) + as.numeric(dataList[[i]][["newid"]]) ### creating a unique id for each observation
}

nsduh <- do.call(rbind, dataList) ### unlist the datasets and append them together

rm("PUF2015_021518", "PUF2016_022818", "PUF2017_100918", "PUF2018_100819", "dataList") ### remove extra stuff from the environment

adolescents <- nsduh[nsduh$catag6 == 1, ] ### subsetting dataset for only adolescents.

setwd("./data") ### set working directory

save(adolescents, file = "adolescents.RData") ### save data as an Rdataset
