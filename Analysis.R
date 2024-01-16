setwd("/Users/shreya/Documents/Holmusk Interview/SDS RWE Data Challenge Dataset/")
clinical_data <- read.csv("clinical_data.csv")
demographics <- read.csv("demographics.csv")
bill_id <- read.csv("bill_id.csv")
bill_amount <- read.csv("bill_amount.csv")

#----------Merging tables--------------------------
colnames(clinical_data) <- c("patient_id", colnames(clinical_data)[2:26])

clinical_data$date_of_admission <- as.Date(clinical_data$date_of_admission, format = "%d/%m/%y")
clinical_data$date_of_discharge <- as.Date(clinical_data$date_of_discharge, format = "%d/%m/%y")

clinical_data_with_demo <- merge(x = clinical_data,
                                 y = demographics,
                                 by = "patient_id")

bill_id$date_of_admission <- as.Date(bill_id$date_of_admission, format = "%Y-%m-%d")

clinical_data_v3 <- merge(x = clinical_data_with_demo,
                          y = bill_id,
                          by = c("patient_id", "date_of_admission"))

table(clinical_data_v3$patient_id[1:100])
length(unique(clinical_data_v3$patient_id))

clinical_data_with_bill <- merge(x = clinical_data_v3,
                                 y = bill_amount,
                                 by = "bill_id")

total_bill <- aggregate(amount ~ patient_id, clinical_data_with_bill, FUN = sum)

#----------Cleaning Clinical Data with demographics--------------------------

summary(clinical_data_with_demo)
clinical_data_with_demo$medical_history_hbp[which(clinical_data_with_demo$medical_history_hbp == "No")] = 0
clinical_data_with_demo$medical_history_hbp[which(clinical_data_with_demo$medical_history_hbp == "Yes")] = 1
unique(clinical_data_with_demo$medical_history_hbp)
clinical_data_with_demo[is.na(clinical_data_with_demo)] <- "NA"

temp <- c("medical_history_dia", "medical_history_sud", "medical_history_hbp", "medical_history_ren", "medical_history_tum", "medical_history_anx", "medical_history_mood")
clinical_data_with_demo[temp] <- lapply(clinical_data_with_demo[temp], factor)
clinical_data_with_demo[11:21] <- lapply(clinical_data_with_demo[11:21], factor)
sapply(clinical_data_with_demo, class)
unique(clinical_data_with_demo$gender)
clinical_data_with_demo$gender[which(clinical_data_with_demo$gender == "Female")] = "f"
clinical_data_with_demo$gender[which(clinical_data_with_demo$gender == "Male")] = "m"
unique(clinical_data_with_demo$race)
clinical_data_with_demo$race[which(clinical_data_with_demo$race == "chinese")] = "Chinese"
clinical_data_with_demo$race[which(clinical_data_with_demo$race == "India")] = "Indian"

unique(clinical_data_with_demo$resident_status)
clinical_data_with_demo$resident_status[which(clinical_data_with_demo$resident_status == "Singapore citizen")] = "Singaporean"

#----------Calculating additional indicators--------------------------

clinical_data_with_demo$date_of_birth <- as.Date(clinical_data_with_demo$date_of_birth, format = "%Y-%m-%d")
library(eeptools)
clinical_data_with_demo$age <- floor(age_calc(clinical_data_with_demo$date_of_birth, units = "years"))
clinical_data_with_demo$los <- as.numeric(clinical_data_with_demo$date_of_discharge - clinical_data_with_demo$date_of_admission)
clinical_data_with_demo$cgis_diff <- clinical_data_with_demo$cgis_adm - clinical_data_with_demo$cgis_dis #calculating diff in CGIS after discharge and at the time of adm
summary(clinical_data_with_demo)

#----------------Data Visualisation----------------------------

library(ggplot2)
library(patchwork)
par(mfrow = c(1,2))
p1 <- ggplot(clinical_data_with_demo, aes(x = age, color = gender, fill = gender)) + geom_histogram(binwidth = 10) +
  labs(x = "\nAge (in years)", y = "\nCount") + theme_classic() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  scale_fill_manual(values = c("pink", "light blue"))

p1

p2 <- ggplot(clinical_data_with_demo, aes(x = race, color = gender, fill = gender))  + geom_bar() + theme_classic() + 
  labs(x = "\nRace", y = "") +
  theme(axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) + 
  scale_fill_manual(values = c("pink", "light blue"))

p1 + p2 + plot_layout(ncol = 2)

hist(clinical_data_with_demo$los, xlab = "Length of Stay (LOS)", main = "")
hist(clinical_data_with_demo$cgis_diff, xlab = "Diff in CGIS at discharge and adm", main = "")

fun <- function(x){
  count <- length(which(clinical_data_with_demo[x] == "1"))
  return(count)
}

df <- data.frame(var = c("Diabetes", "Substance Use Disorder", "High Blood Pressure", "Renal Failure", "Solid Tumor", "Anxiety Disorder", "Mood Disorders" ),
                 count = c(fun("medical_history_dia"), fun("medical_history_sud"), fun("medical_history_hbp"), fun("medical_history_ren"), fun("medical_history_tum"),
                           fun("medical_history_anx"), fun("medical_history_mood")))

ggplot(data = df, aes(x = var, y = count, fill = var)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "", y = "Count") +
  scale_fill_manual(values = rep("#4675f6",7)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15), legend.position = "none", plot.title = element_text(hjust = 0.5)) 

df2 <- data.frame(var = c("Abnormal Sleep", "Anhedonia", "Poor Appetite", "Feeling Depressed/Hopeless", "Suicidal Thoughts"),
                  Count = c(fun("symptom_1"), fun("symptom_2"), fun("symptom_3"), fun("symptom_4"), fun("symptom_5")))

ggplot(data = df2, aes(x = var, y = Count, fill = var)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "", y = "Count", title = "Current Symptoms of Patients") +
  scale_fill_manual(values = rep("#4675f6",7)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15), legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) 

df3 <- data.frame(var = c("Anxiolytics", "Anticonvulsants", "Antidepressents", "SSRI", "Psychotherapy", "Other Psych. Meds"),
                  Count = c(fun("trt_anx"), fun("trt_con"), fun("trt_adt"), fun("trt_ssr"), fun("trt_the"), fun("trt_oth")))

ggplot(data = df3, aes(x = var, y = Count, fill = var)) + geom_bar(stat = "identity") + theme_classic() + labs(x = "", y = "Count", title = "Current Treatments of Patients") +
  scale_fill_manual(values = rep("#4675f6",7)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15), plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15), legend.position = "none") 

table(clinical_data_with_demo$resident_status)
colnames(clinical_data_with_demo)

par(mfrow = c(1,1))
hist(total_bill$amount, xlab = "Total Hospital Bill (in $)", main = "")
#------------------Correlation---------------------------------------------------

library(ltm)
biserial.cor(y = clinical_data_with_demo$trt_oth, x = clinical_data_with_demo$cgis_diff) #use bi-serial correlation because trt is not numerical 
biserial.cor(y = clinical_data_with_demo$trt_the, x = clinical_data_with_demo$cgis_diff)
biserial.cor(y = clinical_data_with_demo$trt_adt, x = clinical_data_with_demo$cgis_diff)
biserial.cor(y = clinical_data_with_demo$trt_con, x = clinical_data_with_demo$cgis_diff)
biserial.cor(y = clinical_data_with_demo$trt_anx, x = clinical_data_with_demo$cgis_diff)
biserial.cor(y = clinical_data_with_demo$trt_ssr, x = clinical_data_with_demo$cgis_diff)


#----------- Matching and Regression--------------------------
options(scipen = 999) #removing scientific notation
y = clinical_data_with_demo[,11:16] #all interventions
x = clinical_data_with_demo[,c(4:10,17:23,26:29,31)] #all matching/log reg indicators

table(clinical_data_with_demo$trt_anx)
table(clinical_data_with_demo$trt_adt)
table(clinical_data_with_demo$trt_con)
table(clinical_data_with_demo$trt_oth)
table(clinical_data_with_demo$trt_ssr)
table(clinical_data_with_demo$trt_the)

library(MatchIt)
library(MASS)

#creating a function to generate the matching equation, and generate the outputs, plot the propensity scores and extract the matched data. 

matching <- function(trt_var, matching_vars, replace = F){
  match_formula <- formula(paste(trt_var, "~", paste(matching_vars, collapse = "+")))
  out.match <- matchit(match_formula, 
                       data = clinical_data_with_demo,
                       distance = "glm",
                       method = "nearest",
                       replace = replace)
  print(summary(out.match))
  plot(out.match, type = "hist", interactive = F)
  if(replace == F){
    return(match.data(out.match))
  }
  else{
    return(get_matches(out.match))
  }
}

ssr.data <- matching("trt_ssr", colnames(x)) 
model.ssr <- glm(cgis_diff~ trt_ssr, data = ssr.data)
summary(model.ssr)

anx.data <- matching("trt_anx", colnames(x))
model.anx <- glm(cgis_diff ~ trt_anx, data = anx.data)
summary(model.anx)

con.data <- matching("trt_con", colnames(x))
model.con <- glm(cgis_diff ~ trt_con, data = con.data)
summary(model.con)

adt.data <- matching("trt_adt", colnames(x), replace = T)
model.adt <- glm(cgis_diff~trt_adt, data = adt.data)
summary(model.adt)

the.data <- matching("trt_the", colnames(x), replace = T )
model.the <- glm(cgis_diff~trt_the, data = the.data)
summary(model.the)

oth.data2 <- matching("trt_oth", colnames(x), replace = T)
model.oth.2 <- glm(cgis_diff~trt_oth, data = oth.data2)
summary(model.oth.2)


#------------------Fitting Log Regression------------------------

fitting_log_res <- function(y_var_name, x_var_names){
  match_formula <- formula(paste(y_var_name, "~", paste(x_var_names, collapse = "+")))
  fit <- glm(match_formula, family = binomial(), data = clinical_data_with_demo)
  return(fit)
}
anx.lr <- fitting_log_res("trt_anx", colnames(x))
summary(anx.lr)

con.lr <- fitting_log_res("trt_con", colnames(x))
summary(con.lr)

ssr.lr <- fitting_log_res("trt_ssr", colnames(x))
summary(ssr.lr)

the.lr <- fitting_log_res("trt_the", colnames(x))
summary(the.lr)

adt.lr <- fitting_log_res("trt_adt", colnames(x))
summary(adt.lr)

oth.lr <- fitting_log_res("trt_oth", colnames(x))
summary(oth.lr)

