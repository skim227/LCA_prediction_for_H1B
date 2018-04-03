# Project Proposal for 15.062 Data Mining
# Approval/Rejection predictions of labor condition application (LCA) for H-1B visa petitions
# Written by Sohae Kim

setwd("/Users/s_kim/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/15.062/FinalProject") 
set.seed(1)

#install.packages("readxl")
#install.packages("RColorBrewer")
#install.packages("wesanderson")
#library(RColorBrewer)
#library(wesanderson)
library(readxl)
library(ggplot2)
library(stringr)

df <- read_excel("H-1B_FY2018.xlsx")
#df <- read_excel("H-1B_Disclosure_Data_FY17.xlsx") # Consider uncomment only when your computer has enough free ram ~3G.
df.backup <- df
df <- df.backup
colnames(df)[which(names(df) == "H-1B_DEPENDENT")] <- "H1B_DEPENDENT"
colSums(is.na(df)*1)
df[,c("EMPLOYER_BUSINESS_DBA","EMPLOYER_ADDRESS","EMPLOYER_POSTAL_CODE","EMPLOYER_COUNTRY","EMPLOYER_PROVINCE","EMPLOYER_PHONE","EMPLOYER_PHONE_EXT", #"AGENT_ATTORNEY_NAME",
      "AGENT_ATTORNEY_CITY","AGENT_ATTORNEY_STATE","JOB_TITLE","SOC_NAME","WAGE_RATE_OF_PAY_TO","LABOR_CON_AGREE","PUBLIC_DISCLOSURE_LOCATION","WORKSITE_COUNTY",
      "WORKSITE_POSTAL_CODE","ORIGINAL_CERT_DATE")] <- NULL
summary(df)

factor.list <- c("CASE_STATUS","VISA_CLASS","EMPLOYER_NAME","EMPLOYER_CITY","EMPLOYER_STATE","AGENT_REPRESENTING_EMPLOYER","SOC_CODE","NAICS_CODE","FULL_TIME_POSITION",
                 "PW_UNIT_OF_PAY","PW_WAGE_LEVEL","PW_SOURCE","PW_SOURCE_YEAR","PW_SOURCE_OTHER","WAGE_UNIT_OF_PAY","H1B_DEPENDENT","WILLFUL_VIOLATOR","SUPPORT_H1B",
                 "WORKSITE_CITY","WORKSITE_STATE")
for (i in 1:length(factor.list)) {
  df[which(df[,factor.list[i]] == "N/A" | df[,factor.list[i]] == "NA", arr.ind = TRUE),factor.list[i]] <- NA
}
df[,factor.list] <- lapply(df[,factor.list], factor)
colSums(is.na(df)*1)
levels(df$PW_WAGE_LEVEL) <- c(levels(df$PW_WAGE_LEVEL), "N/A")
#levels(df$H1B_DEPENDENT) <- c(levels(df$H1B_DEPENDENT), "N/A")
#levels(df$SUPPORT_H1B) <- c(levels(df$SUPPORT_H1B), "N/A")
df[,c("WILLFUL_VIOLATOR","PW_SOURCE_OTHER")] <- NULL
df[is.na(df$PW_WAGE_LEVEL),]$PW_WAGE_LEVEL<- "N/A"
df[is.na(df$AGENT_REPRESENTING_EMPLOYER),]$AGENT_REPRESENTING_EMPLOYER<- "N" #"N/A"
df[is.na(df$H1B_DEPENDENT),]$H1B_DEPENDENT<- "N" #"N/A"
df[is.na(df$SUPPORT_H1B),]$SUPPORT_H1B<- "N" #"N/A"
colSums(is.na(df)*1)

df$H1B_DEPENDENT <- 1*(df$H1B_DEPENDENT == "Y")
df$SUPPORT_H1B <- 1*(df$SUPPORT_H1B == "Y")
df$AGENT_REPRESENTING_EMPLOYER <- 1*(df$AGENT_REPRESENTING_EMPLOYER == "Y")
df$FULL_TIME_POSITION <- 1*(df$FULL_TIME_POSITION=="Y")
#df$WILLFUL_VIOLATOR <- 1*(df$WILLFUL_VIOLATOR == "Y")
df$SOC_CODE <- str_extract(df$SOC_CODE, "\\d{2}")
df$NAICS_CODE <- str_extract(df$NAICS_CODE, "\\d{2}")
df[,c("SOC_CODE","NAICS_CODE")] <- lapply(df[,c("SOC_CODE","NAICS_CODE")], factor)

summary(df)

tmp <- which(df$WAGE_RATE_OF_PAY_FROM > 9.99E+8, arr.ind=TRUE) #For 2018 data, this data point seems like a fake application with FROM YOUR SECRET SANTA on the WORKSITE_CITY: I-200-17347-732455	DENIED	12/13/17	12/15/17	H-1B	12/25/17	12/25/20	DELOITTE CONSULTING LLP		1700 MARKET STREET	PHILADELPHIA	PA	19103	UNITED STATES OF AMERICA		2152462300		Y	MICHAELS, REBECCA	TORONTO		U.S. LEGAL ASSISTANT	23-1011	LAWYERS	54161	1	1	0	0	0	0	0	Y	1,000,000,000.00	Year	N/A	OES	2017	OFLC ONLINE DATA CENTER	1,000,000,000.00	0.00	Year	N	N	NA			FROM YOUR	SECRET SANTA			
if(length(tmp)!=0) df <- df[-tmp,]

df_cert_den <- df[which(df$CASE_STATUS == "DENIED" | df$CASE_STATUS == "CERTIFIED"),]
factor.list <- c("CASE_STATUS","VISA_CLASS","EMPLOYER_NAME","EMPLOYER_CITY","EMPLOYER_STATE","SOC_CODE","NAICS_CODE","PW_UNIT_OF_PAY", #"H1B_DEPENDENT","SUPPORT_H1B",
                 "PW_WAGE_LEVEL","PW_SOURCE","PW_SOURCE_YEAR","WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")
df_cert_den[factor.list] <- lapply(df_cert_den[factor.list], factor)

######################################
# Exploratory visualizations with df #
######################################
boxplot(df$WAGE_RATE_OF_PAY_FROM)
boxplot(df$WAGE_RATE_OF_PAY_FROM ~ df$CASE_STATUS)
ggplot(data = df, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 1.0, show.legend = TRUE) + 
  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_log10(name="Prevailing wage", #limits=c(1e4, 1e6), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme( axis.text.x = element_text(face="bold", size=14), 
         axis.text.y = element_text(face="bold", size=14),
         axis.title.x  = element_text(size=14),
         axis.title.y  = element_text(size=14),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1))
ggplot(data = df, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 1.0, show.legend = TRUE)
ggplot(data = df[-c(exc,which(df$CASE_STATUS=="WITHDRAWN",arr.ind=TRUE)),], aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + 
  geom_point(size = 1.0, show.legend = TRUE)+ scale_y_log10() + scale_x_log10()
ggplot(data = df[c(which(df$CASE_STATUS=="DENIED",arr.ind=TRUE)),], aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + 
  geom_point(size = 1.0, show.legend = TRUE)+ scale_y_log10() + scale_x_log10()
plot(df$CASE_SUBMITTED, df$DECISION_DATE, type="p")
hist(df$DECISION_DATE,"days")
hist(df$CASE_SUBMITTED,"months")
boxplot(df$EMPLOYMENT_START_DATE ~ df$CASE_STATUS)
#Withdrawn tends to be old datas
ggplot(df, aes(x = CASE_SUBMITTED)) + geom_histogram() + scale_y_log10()


########################################################################
# Exploratory visualizations with df_cert_den, only CERTIFIED & DENIED #
########################################################################
boxplot(df_cert_den$WAGE_RATE_OF_PAY_FROM)
boxplot(df_cert_den$WAGE_RATE_OF_PAY_FROM ~ df_cert_den$CASE_STATUS)

#Need to convert the wages in the same unit
#Wages are grouped in four: (1) Prevailing wage & Wage rate of pay from < ~10K; (2) Prevailing wage < ~10K & Prevailing wage & Wage rate of pay from > ~10K; 
#(3) Prevailing wage > ~10K & Wage rate of pay from < ~10K; (4) Prevailing wage & Wage rate of pay from > ~10K;
ggplot(data = df_cert_den, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 1.0, show.legend = TRUE) + 
  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_log10(name="Prevailing wage", #limits=c(1e4, 1e6), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme( axis.text.x = element_text(face="bold", size=14), 
         axis.text.y = element_text(face="bold", size=14),
         axis.title.x  = element_text(size=14),
         axis.title.y  = element_text(size=14),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1)) #+ theme_linedraw()
ggplot(data = df_cert_den[c(which(df_cert_den$CASE_STATUS=="DENIED",arr.ind=TRUE)),], aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 1.0, show.legend = TRUE) + scale_y_log10() + scale_x_log10()

############################################
# Conversion of wages in a same unit, Year #
############################################
#There are 147 records that have mismatch between the units of prevailing wage & wage rate of pay from 2018 Q1 data
#There are 844 records that have mismatch between the units of prevailing wage & wage rate of pay from 2017 data
cbind(df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$PW_UNIT_OF_PAY, 
      df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$WAGE_UNIT_OF_PAY, 
      df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$PREVAILING_WAGE, 
      df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_FROM, 
      #df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_TO, 
      df_cert_den[which(df_cert_den$PW_UNIT_OF_PAY!=df_cert_den$WAGE_UNIT_OF_PAY,arr.ind=TRUE),]$CASE_STATUS)

# Conversion for prevailing wage
df_cert_den$PREVAILING_WAGE <- df_cert_den$PREVAILING_WAGE*26*(df_cert_den$PW_UNIT_OF_PAY == "Bi-Weekly") + df_cert_den$PREVAILING_WAGE*52*40*(df_cert_den$PW_UNIT_OF_PAY == "Hour") + 
  df_cert_den$PREVAILING_WAGE*12*(df_cert_den$PW_UNIT_OF_PAY == "Month") + df_cert_den$PREVAILING_WAGE*52*(df_cert_den$PW_UNIT_OF_PAY == "Week") + df_cert_den$PREVAILING_WAGE*1*(df_cert_den$PW_UNIT_OF_PAY == "Year")
# Conversion for wage rate of pay from & to
df_cert_den$WAGE_RATE_OF_PAY_FROM <- df_cert_den$WAGE_RATE_OF_PAY_FROM*26*(df_cert_den$WAGE_UNIT_OF_PAY == "Bi-Weekly") + 
  df_cert_den$WAGE_RATE_OF_PAY_FROM*52*40*(df_cert_den$WAGE_UNIT_OF_PAY == "Hour") + 
  df_cert_den$WAGE_RATE_OF_PAY_FROM*12*(df_cert_den$WAGE_UNIT_OF_PAY == "Month") + 
  df_cert_den$WAGE_RATE_OF_PAY_FROM*52*(df_cert_den$WAGE_UNIT_OF_PAY == "Week") +
  df_cert_den$WAGE_RATE_OF_PAY_FROM*1*(df_cert_den$WAGE_UNIT_OF_PAY == "Year")
#df_cert_den$WAGE_RATE_OF_PAY_TO <- df_cert_den$WAGE_RATE_OF_PAY_TO*26*(df_cert_den$WAGE_UNIT_OF_PAY == "Bi-Weekly") + 
#  df_cert_den$WAGE_RATE_OF_PAY_TO*52*40*(df_cert_den$WAGE_UNIT_OF_PAY == "Hour") + 
#  df_cert_den$WAGE_RATE_OF_PAY_TO*12*(df_cert_den$WAGE_UNIT_OF_PAY == "Month") + 
#  df_cert_den$WAGE_RATE_OF_PAY_TO*52*(df_cert_den$WAGE_UNIT_OF_PAY == "Week") +
#  df_cert_den$WAGE_RATE_OF_PAY_TO*1*(df_cert_den$WAGE_UNIT_OF_PAY == "Year")

#ggplot(data = df_cert_den, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 0.4, show.legend = TRUE) + 
#  theme( axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14)) +
#  scale_x_continuous(name="Prevailing wage") + #, limits=c(0, 3e5)) + #, breaks=seq(10,1e8,8)) 
#  scale_y_continuous(name="Wage rate of pay from") #, limits=c(0, 1e6))
ggplot(data = df_cert_den, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 1.0, show.legend = TRUE) + #scale_color_manual(values = wes_palette(n=2, name="GrandBudapest")) + #scale_color_brewer(palette="Dark2") + 
  scale_x_log10(name="Prevailing wage", #limits=c(1e4, 1e6), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme( axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14),
         axis.title.x  = element_text(size=14), axis.title.y  = element_text(size=14),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1))
summary(df_cert_den)

###########################################
# Removing outliers that are 100% DENIED. #
###########################################
#Outliers in two groups: (1) Prevailing wage and/or is very low, less than 10K; (2) Prevailing wage is very high, larger than ~1M.
#Outlier group (1)
cbind(df_cert_den[which(df_cert_den$PREVAILING_WAGE < 10000.0,arr.ind=TRUE),]$CASE_STATUS,
      df_cert_den[which(df_cert_den$PREVAILING_WAGE < 10000.0,arr.ind=TRUE),]$PREVAILING_WAGE,
      df_cert_den[which(df_cert_den$PREVAILING_WAGE < 10000.0,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_FROM #, df_cert_den[which(df_cert_den$PREVAILING_WAGE < 10000.0,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_TO
      )
cbind(df_cert_den[which(df_cert_den$WAGE_RATE_OF_PAY_FROM < 10000.0,arr.ind=TRUE),]$CASE_STATUS,
      df_cert_den[which(df_cert_den$WAGE_RATE_OF_PAY_FROM < 10000.0,arr.ind=TRUE),]$PREVAILING_WAGE,
      df_cert_den[which(df_cert_den$WAGE_RATE_OF_PAY_FROM < 10000.0,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_FROM #, df_cert_den[which(df_cert_den$WAGE_RATE_OF_PAY_FROM < 10000.0,arr.ind=TRUE),]$WAGE_RATE_OF_PAY_TO
      )
# All DENIED

#Outlier group (2)
head(cbind(df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$CASE_STATUS,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$EMPLOYER_NAME,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$VISA_CLASS,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$WAGE_RATE_OF_PAY_FROM,
           #df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$WAGE_RATE_OF_PAY_TO,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$WAGE_UNIT_OF_PAY,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$PREVAILING_WAGE,
           df_cert_den[order(df_cert_den$WAGE_RATE_OF_PAY_FROM,decreasing = TRUE),]$PW_UNIT_OF_PAY), 50)
#Not always DENIED
head(cbind(df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"CASE_STATUS"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"EMPLOYER_NAME"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"VISA_CLASS"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"WAGE_RATE_OF_PAY_FROM"],
           #df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"WAGE_RATE_OF_PAY_TO"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"WAGE_UNIT_OF_PAY"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"PREVAILING_WAGE"],
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"PW_UNIT_OF_PAY"], 
           df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE),"AGENT_ATTORNEY_NAME"]), 
    #125) # 2017 data
    30) # 2018 Q1 data
#View(df_cert_den[order(df_cert_den$PREVAILING_WAGE,decreasing = TRUE)[1:150],c("CASE_STATUS","JOB_TITLE","VISA_CLASS","WAGE_RATE_OF_PAY_FROM","WAGE_RATE_OF_PAY_TO","WAGE_UNIT_OF_PAY","PREVAILING_WAGE","PW_UNIT_OF_PAY")])
#Top 22 cases have denoted that the unit of prevailing wages are hourly-based, and the converted yearly-based wage is outrageously high. The wage rage of pay from is around 50K to 200K in yearly-based. 
#A couple of cases report ~77K and ~133K hourly based, and another couple report ~60K and ~68K hourly based. 
#Overall, the prevailing wage in yearly-based does not make common sense and mismatch a lot to the wage rate pay from as well. This would be due to the misinterpretation of the prevailing wage & unit.
#Always DENIED
which(df_cert_den$CASE_STATUS=="DENIED" & df_cert_den$AGENT_ATTORNEY_NAME == "BRADSHAW, MELANIE",arr.ind=TRUE) # For 2018 data
#df_cert_den[which(df_cert_den$CASE_STATUS=="DENIED", arr.ind=TRUE),]$AGENT_ATTORNEY_NAME[order(freq, decreasing = T),]

#df_cert_den$PW_UNIT_OF_PAY <- "Year"
#df_cert_den$WAGE_UNIT_OF_PAY <- "Year"

summary(df_cert_den)

# delete outlier, since they do not need a model to predict the decision. In the outlier range, all records are DENIED.
df_cdwo_outlier <- df_cert_den[-which(df_cert_den$PREVAILING_WAGE < 1e4 | df_cert_den$WAGE_RATE_OF_PAY_FROM < 1e4 | df_cert_den$PREVAILING_WAGE > 1e6,arr.ind=TRUE),]

############################################################################
# Visualization of the data without outler, which needs a prediction model #
############################################################################
#ggplot(data = df_cdwo_outlier, aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + geom_point(size = 0.6, show.legend = TRUE) + 
#  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
#                breaks = scales::trans_breaks("log10", function(x) 10^x),
#                labels = scales::trans_format("log10", scales::math_format(10^.x))) + #theme_linedraw() + 
#  scale_x_log10(name="Prevailing wage", limits=c(1e4, 1e6), 
#                breaks = scales::trans_breaks("log10", function(x) 10^x),
#                labels = scales::trans_format("log10", scales::math_format(10^.x))) + #theme_linedraw() + 
#  theme( axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14),
#         axis.title.x  = element_text(size=14), axis.title.y  = element_text(size=14),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_rect(colour = "black", size=1)) # may want to run twice when an error occurs. Warning msg with removed missing values are fine.
#library(ggpubr)
library(cowplot)
theme_set(theme_grey())
dn.plot <- ggplot(data = df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="DENIED",arr.ind=TRUE)),], aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + 
  geom_point(size = 1.0, show.legend = TRUE) + scale_color_manual(values="#00BFC4") +
  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + #theme_linedraw() + 
  scale_x_log10(name="Prevailing wage", limits=c(1e4, 1e6), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + border(color = "black", size = 1.0, linetype = 1) + #theme_linedraw() + 
  theme( axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14),
         axis.title.x  = element_text(size=14), axis.title.y  = element_text(size=14),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1), legend.text=element_text(size=14)) 
ct.plot <- ggplot(data = df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="CERTIFIED",arr.ind=TRUE)),], aes(x = PREVAILING_WAGE, y = WAGE_RATE_OF_PAY_FROM, color = CASE_STATUS)) + 
  geom_point(size = 1.0, show.legend = TRUE) +
  scale_y_log10(name="Wage rate of pay from", #limits=c(1e4, 1e9), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + #theme_linedraw() + 
  scale_x_log10(name="Prevailing wage", limits=c(1e4, 1e6), 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + border(color = "black", size = 1.0, linetype = 1) +#theme_linedraw() + 
  theme( axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14),
         axis.title.x  = element_blank(), axis.title.y  = element_text(size=14),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1), legend.text=element_text(size=14)) 
plot_grid(ct.plot, dn.plot, align = "v", nrow = 2) #+ panel_border(colour = "black", size = 1, linetype = 1,  remove = FALSE)  # May need to run several times due to Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : polygon edge not found
# We can see a hard rule on the decision: If the wage rate to pay from is lower than the prevailing wage, they are 100% DENIED. Otherwise, they can be either CERTIFIED or DENIED.
# Is there any difference between the CERTIFIED and the DENIED but with the paid wage rate higher than the prevailing wage? I cannot see that much difference.
# Therefore, here is where we need a good prediction model.
summary(df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="DENIED",arr.ind=TRUE)),])
summary(df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="DENIED"&df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM > df_cdwo_outlier$PREVAILING_WAGE,arr.ind=TRUE)),])
summary(df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="CERTIFIED",arr.ind=TRUE)),])

#########################
# Cleaning data further #
#########################
# Data still has some NAs, but I will just delete the rows with NAs since it is not that many.
colSums(is.na(df_cdwo_outlier)*1)
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
denied.ind <- which(cmp.df_cdwo_outlier$CASE_STATUS=="DENIED",arr.ind=TRUE)
denied_though.ind <- which(cmp.df_cdwo_outlier$CASE_STATUS=="DENIED"&cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM > cmp.df_cdwo_outlier$PREVAILING_WAGE,arr.ind=TRUE)
certified.ind <- which(cmp.df_cdwo_outlier$CASE_STATUS=="CERTIFIED",arr.ind=TRUE)
summary(cmp.df_cdwo_outlier[denied.ind,])
summary(cmp.df_cdwo_outlier[c(which(cmp.df_cdwo_outlier$CASE_STATUS=="DENIED"&cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM > cmp.df_cdwo_outlier$PREVAILING_WAGE,arr.ind=TRUE)),])
summary(cmp.df_cdwo_outlier[certified.ind,])


###################################
# Functions for all methods I use #
###################################
library(rpart)
library(caret)
library(rpart.plot)
library(neuralnet)
library(forecast)
library(adabag)
library(mlr) #For createDummyFeatures
library(plyr)

#Tree
tree <- function(trdf,vadf,tedf){
  tr <- rpart(CASE_STATUS ~ ., data = trdf)
  validation.prediction <- predict(tr, vadf)
  accr <- c(0,0.5)
  for (cr in c(seq(0.5,0.7, by=0.05),seq(0.5,0.3, by=-0.05))) {
    if(confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]] > accr[1]) {
      accr[1] <- confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]]
      accr[2] <- cr
    } 
  }
  print(accr)
  #summary(tr) 
  #rpart.plot(tr,cex=0.9)
  rpart.plot(tr, type=4, digits=-2, fallen.leaves=FALSE,cex=0.9)
  confusion.val <- confusionMatrix(ifelse(validation.prediction>accr[2], 1, 0), vadf$CASE_STATUS, positive = "1")
  confusion.test <- confusionMatrix(ifelse(predict(tr, tedf)>accr[2], 1, 0), tedf$CASE_STATUS, positive = "1")
  print(cbind(c(confusion.val$overall[1], confusion.val$byClass[1:4]), c(confusion.test$overall[1], confusion.test$byClass[1:4])))
  return(cbind(c(confusion.val$overall[1], confusion.val$byClass[1:4]), c(confusion.test$overall[1], confusion.test$byClass[1:4]))) 
}
#Logistic regression
logreg <- function(trdf,vadf,tedf){
  lr <- glm(CASE_STATUS ~ ., data = trdf, family = "binomial") 
  validation.prediction <- predict(lr, vadf, type = "response")
  accr <- c(0,0.5)
  for (cr in c(seq(0.5,0.7, by=0.05),seq(0.5,0.3, by=-0.05))) {
    if(confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]] > accr[1]) {
      accr[1] <- confusionMatrix(ifelse(predict(lr, vadf, type = "response")>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]]
      accr[2] <- cr
    } 
  }
  print(accr)
  #summary(lr)
  confusion.val <- confusionMatrix(ifelse(validation.prediction>accr[2], 1, 0), vadf$CASE_STATUS, positive = "1")
  confusion.test <- confusionMatrix(ifelse(predict(lr, tedf, type = "response")>accr[2], 1, 0), tedf$CASE_STATUS, positive = "1")
  return(cbind(c(confusion.val$overall[1], confusion.val$byClass[1:4]), c(confusion.test$overall[1], confusion.test$byClass[1:4])))
}
#knn
knearn <- function(trdf,vadf,tedf){
  accr <- c(0,1)
  for (cr in 1:15) {
    kn <- class::knn(train = trdf[, -1], test = vadf[,-1], cl = trdf[, 1], k = cr, prob=TRUE)
    if(confusionMatrix(kn, vadf[,1], positive = "1")$overall[[1]] > accr[1]) {
      accr[1] <- confusionMatrix(kn, vadf[,1], positive = "1")$overall[[1]]
      accr[2] <- cr
    }
    #print(c(cr,accr,confusionMatrix(class::knn(train = trdf[, -1], test = tedf[,-1], cl = trdf[, 1], k = cr, prob=TRUE), tedf[,1], positive = "1")$byClass[[11]]))
  }
  confusion.val <- confusionMatrix(class::knn(train = trdf[, -1], test = vadf[,-1], cl = trdf[, 1], k = accr[2], prob=TRUE), vadf[,1], positive = "1")
  confusion.test <- confusionMatrix(class::knn(train = trdf[, -1], test = tedf[,-1], cl = trdf[, 1], k = accr[2], prob=TRUE), tedf[,1], positive = "1")
  return(cbind(c(confusion.val$overall[1], confusion.val$byClass[1:4]), c(confusion.test$overall[1], confusion.test$byClass[1:4])))
}
#Neural net
nnet_opt <- function(i,trdf,vadf,tedf){
  form_nn <- as.formula(paste("CASE_STATUS ~ ",paste(names(trdf[,-1]), collapse=" + "),sep = ""))
  nn <- neuralnet(formula = form_nn , data = trdf, linear.output = T, hidden = i)
  validation.prediction <- as.vector(neuralnet::compute(nn, vadf[, -1])$net.result)
  #print(c(max(ifelse(validation.prediction>0.5, 1, 0)),ifelse(validation.prediction>0.5, 1, 0)))
  #print(vadf$CASE_STATUS)
  accr <- c(0,0.5)
  for (cr in c(seq(0.5,0.65, by=0.05),seq(0.5,0.3, by=-0.05))){
    #print(table(ifelse(validation.prediction>cr, 1, 0)))
    #print(cr)
    #print(table(vadf$CASE_STATUS))
    #print(confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$byClass[[11]])
    if(confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]] > accr[1]) {
      accr[1] <- confusionMatrix(ifelse(validation.prediction>cr, 1, 0), vadf$CASE_STATUS, positive = "1")$overall[[1]]
      accr[2] <- cr
    }
  }
  print(accr)
  confusion.val <- confusionMatrix(ifelse(validation.prediction>accr[2], 1, 0), vadf$CASE_STATUS, positive = "1")
  confusion.test <- confusionMatrix(ifelse(as.vector(neuralnet::compute(nn, tedf[, -1])$net.result)>accr[2], 1, 0), tedf$CASE_STATUS, positive = "1")
  
  return(cbind(c(confusion.val$overall[1], confusion.val$byClass[1:4]), c(confusion.test$overall[1], confusion.test$byClass[1:4])))
  #plot(nn)
}


#################################
# (1) Category into dummy variables #
#################################
#####2018
train.ind <- c(sample(denied.ind, length(denied.ind)*0.5), sample(certified.ind, length(denied.ind)*0.5))
valid.ind <- c(sample(setdiff(denied.ind, train.ind),length(denied.ind)*0.25), sample(setdiff(certified.ind, train.ind),length(denied.ind)*0.25))  
test.ind <- c(sample(setdiff(denied.ind, c(train.ind,valid.ind)),length(denied.ind)*0.25), sample(setdiff(certified.ind, c(train.ind,valid.ind)),length(denied.ind)*0.25))  

#train.ind <- c(sample(denied_though.ind, length(denied_though.ind)*0.5), sample(certified.ind, length(denied_though.ind)*0.5))
#valid.ind <- c(sample(setdiff(denied_though.ind, train.ind),length(denied_though.ind)*0.25), sample(setdiff(certified.ind, train.ind),length(denied_though.ind)*0.25))  
#test.ind <- c(sample(setdiff(denied_though.ind, c(train.ind,valid.ind)),length(denied_though.ind)*0.25), sample(setdiff(certified.ind, c(train.ind,valid.ind)),length(denied_though.ind)*0.25))  
#####2017
#train.ind <- c(sample(denied.ind, length(denied.ind)*0.12), sample(certified.ind, length(denied.ind)*0.12))
#valid.ind <- c(sample(setdiff(denied.ind, train.ind),length(denied.ind)*0.06), sample(setdiff(certified.ind, train.ind),length(denied.ind)*0.06))  
#test.ind <- c(sample(setdiff(denied.ind, c(train.ind,valid.ind)),length(denied.ind)*0.06), sample(setdiff(certified.ind, c(train.ind,valid.ind)),length(denied.ind)*0.06))  

# Reset data with the complete cases without outliers & initialize
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
cmp.df_cdwo_outlier$CASE_STATUS <- 1*(cmp.df_cdwo_outlier$CASE_STATUS == "CERTIFIED")
cmp.df_cdwo_outlier[,c("CASE_NUMBER","CASE_SUBMITTED","DECISION_DATE","EMPLOYMENT_START_DATE","EMPLOYMENT_END_DATE","EMPLOYER_NAME","EMPLOYER_CITY","EMPLOYER_STATE","AGENT_ATTORNEY_NAME",
                       "PW_UNIT_OF_PAY","PW_SOURCE_OTHER","WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")] <- NULL
cmp.df_cdwo_outlier$WAGE_DIFF <- cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM - cmp.df_cdwo_outlier$PREVAILING_WAGE
cmp.df_cdwo_outlier <- createDummyFeatures(cmp.df_cdwo_outlier, cols = c("VISA_CLASS","SOC_CODE","NAICS_CODE","PW_WAGE_LEVEL","PW_SOURCE","PW_SOURCE_YEAR"), method = "reference")
# Assgin to train, validation and test data sets
train.df <- cmp.df_cdwo_outlier[train.ind,]
valid.df <- cmp.df_cdwo_outlier[valid.ind,]
test.df <- cmp.df_cdwo_outlier[test.ind,]
# Normalize continuous numerical variables of the train, validation and test data sets
conum.ind <- which(names(train.df) %in% c("TOTAL_WORKERS","NEW_EMPLOYMENT","CONTINUED_EMPLOYMENT","CHANGE_PREVIOUS_EMPLOYMENT","NEW_CURRENT_EMPLOYMENT","CHANGE_EMPLOYER",
                                          "AMENDED_PETITION","PREVAILING_WAGE","WAGE_RATE_OF_PAY_FROM","WAGE_DIFF"))
normalization.model <- preProcess(train.df[,conum.ind], method = c("center","scale"))
train.df.norm <- train.df
valid.df.norm <- valid.df
test.df.norm <- test.df
train.df.norm[,conum.ind] <- predict(normalization.model, train.df[,conum.ind])
valid.df.norm[,conum.ind] <- predict(normalization.model, valid.df[,conum.ind])
test.df.norm[,conum.ind] <- predict(normalization.model, test.df[,conum.ind])
summary(train.df.norm)

cat.val.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
cat.tst.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
#Base prediction
confusion.val <- confusionMatrix((valid.df$WAGE_RATE_OF_PAY_FROM >= valid.df$PREVAILING_WAGE)*1, valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix((test.df$WAGE_RATE_OF_PAY_FROM >= test.df$PREVAILING_WAGE)*1, test.df$CASE_STATUS, positive = "1")
cat.val.Accuracy$Model <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
cat.tst.Accuracy$Model <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

confusion.val <- confusionMatrix(rep(1,length(valid.ind)), valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix(rep(1,length(test.ind)), test.df$CASE_STATUS, positive = "1")
cat.val.Accuracy$Rand <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
cat.tst.Accuracy$Rand <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

#Tree
tmp <- tree(train.df.norm, valid.df.norm, test.df.norm)
cat.val.Accuracy$TR <- tmp[,1]
cat.tst.Accuracy$TR <- tmp[,2]

#Logistic regression
tmp <- logreg(train.df.norm, valid.df.norm, test.df.norm)
#tmp <- logreg(train.df.norm[,-c(72,83,88)], valid.df.norm[,-c(72,83,88)], test.df.norm[,-c(72,83,88)])
#tmp <- logreg(train.df.norm[,-43], valid.df.norm[,-43], test.df.norm[,-43])
#tmp <- logreg(train.df.norm[,c(1:73,75:82,84:87,89:92)], valid.df.norm[,c(1:73,79:82,84:87,89:92)], test.df.norm[,c(1:73,79:82,84:87,89:92)]) #Warnings ignored
#tmp <- logreg(train.df.norm[,c(1:42,44:77,79:82,84:87,89:107)], valid.df.norm[,c(1:42,44:77,79:82,84:87,89:107)], test.df.norm[,c(1:42,44:77,79:82,84:87,89:107)]) #Warnings ignored
#tmp <- logreg(train.df.norm[,c(1:49,52:75,77:85,87:96)], valid.df.norm[,c(1:49,52:75,77:85,87:96)], test.df.norm[,c(1:49,52:75,77:85,87:96)]) #Warnings ignored
cat.val.Accuracy$LR <- tmp[,1]
cat.tst.Accuracy$LR <- tmp[,2]

#knn
tmp <- knearn(train.df.norm, valid.df.norm, test.df.norm)
cat.val.Accuracy$KNN <- tmp[,1]
cat.tst.Accuracy$KNN <- tmp[,2]

#Neural net
tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm) #Due to a warning or error, may need to run one or a couple more.
#tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm)
cat.val.Accuracy$NN1 <- tmp[,1]
cat.tst.Accuracy$NN1 <- tmp[,2]

tmp <- nnet_opt(2,train.df.norm, valid.df.norm, test.df.norm)
cat.val.Accuracy$NN2 <- tmp[,1]
cat.tst.Accuracy$NN2 <- tmp[,2]

tmp <- nnet_opt(3,train.df.norm, valid.df.norm, test.df.norm)
cat.val.Accuracy$NN3 <- tmp[,1]
cat.tst.Accuracy$NN3 <- tmp[,2]


###############################################
# (2) Category into numerical frequency variables #
###############################################
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
cmp.df_cdwo_outlier$CASE_STATUS <- 1*(cmp.df_cdwo_outlier$CASE_STATUS == "CERTIFIED")
cmp.df_cdwo_outlier[,c("CASE_NUMBER","CASE_SUBMITTED","DECISION_DATE","EMPLOYMENT_START_DATE","EMPLOYMENT_END_DATE","EMPLOYER_CITY","EMPLOYER_STATE", #"AGENT_ATTORNEY_NAME","EMPLOYER_NAME",
                       "PW_UNIT_OF_PAY","PW_SOURCE_OTHER","WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")] <- NULL
cmp.df_cdwo_outlier$WAGE_DIFF <- cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM - cmp.df_cdwo_outlier$PREVAILING_WAGE
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, VISA_CLASS = ave(seq(nrow(cmp.df_cdwo_outlier)), VISA_CLASS, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, SOC_CODE = ave(seq(nrow(cmp.df_cdwo_outlier)), SOC_CODE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, NAICS_CODE = ave(seq(nrow(cmp.df_cdwo_outlier)), NAICS_CODE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_WAGE_LEVEL = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_WAGE_LEVEL, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_SOURCE = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_SOURCE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_SOURCE_YEAR = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_SOURCE_YEAR, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, H1B_DEPENDENT = ave(seq(nrow(cmp.df_cdwo_outlier)), H1B_DEPENDENT, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, SUPPORT_H1B = ave(seq(nrow(cmp.df_cdwo_outlier)), SUPPORT_H1B, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, EMPLOYER_NAME = ave(seq(nrow(cmp.df_cdwo_outlier)), EMPLOYER_NAME, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, AGENT_ATTORNEY_NAME = ave(seq(nrow(cmp.df_cdwo_outlier)), AGENT_ATTORNEY_NAME, FUN=length))
train.df <- cmp.df_cdwo_outlier[train.ind,]
valid.df <- cmp.df_cdwo_outlier[valid.ind,]
test.df <- cmp.df_cdwo_outlier[test.ind,]

normalization.model <- preProcess(train.df[,-1], method = c("scale","center"))
train.df.norm <- train.df
valid.df.norm <- valid.df
test.df.norm <- test.df
train.df.norm[,-1] <- predict(normalization.model, train.df[,-1])
valid.df.norm[,-1] <- predict(normalization.model, valid.df[,-1])
test.df.norm[,-1] <- predict(normalization.model, test.df[,-1])
summary(train.df.norm)

num.val.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
num.tst.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
#Base prediction
confusion.val <- confusionMatrix((valid.df$WAGE_RATE_OF_PAY_FROM >= valid.df$PREVAILING_WAGE)*1, valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix((test.df$WAGE_RATE_OF_PAY_FROM >= test.df$PREVAILING_WAGE)*1, test.df$CASE_STATUS, positive = "1")
num.val.Accuracy$Model <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
num.tst.Accuracy$Model <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

confusion.val <- confusionMatrix(rep(1,length(valid.ind)), valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix(rep(1,length(test.ind)), test.df$CASE_STATUS, positive = "1")
num.val.Accuracy$Rand <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
num.tst.Accuracy$Rand <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

#Tree
tmp <- tree(train.df.norm, valid.df.norm, test.df.norm)
num.val.Accuracy$TR <- tmp[,1]
num.tst.Accuracy$TR <- tmp[,2]

#Logistic regression
tmp <- logreg(train.df.norm, valid.df.norm, test.df.norm) #need to check 1:17 and 513:516
num.val.Accuracy$LR <- tmp[,1]
num.tst.Accuracy$LR <- tmp[,2]

#knn
tmp <- knearn(train.df.norm, valid.df.norm, test.df.norm)
num.val.Accuracy$KNN <- tmp[,1]
num.tst.Accuracy$KNN <- tmp[,2]

#Neural net
tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm)
num.val.Accuracy$NN1 <- tmp[,1]
num.tst.Accuracy$NN1 <- tmp[,2]

tmp <- nnet_opt(2,train.df.norm, valid.df.norm, test.df.norm)
num.val.Accuracy$NN2 <- tmp[,1]
num.tst.Accuracy$NN2 <- tmp[,2]

tmp <- nnet_opt(3,train.df.norm, valid.df.norm, test.df.norm)
num.val.Accuracy$NN3 <- tmp[,1]
num.tst.Accuracy$NN3 <- tmp[,2]








##################################################
#         (3) Unbalanced raw sampling            #
# Treating category variables by dummy variables #
##################################################
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
length.cmp <- dim(cmp.df_cdwo_outlier)[1]
######### With 2018 Q1 data
train.ind <- sample(1:length.cmp, length(denied.ind)*0.5)
valid.ind <- sample(setdiff(1:length.cmp, train.ind),length(denied.ind)*0.25)  
test.ind <- sample(setdiff(1:length.cmp, c(train.ind,valid.ind)),length(denied.ind)*0.25)  

#train.ind <- sample(1:length.cmp, length(denied_though.ind))
#valid.ind <- sample(setdiff(1:length.cmp, train.ind),length(denied_though.ind)*0.5)  
#test.ind <- sample(setdiff(1:length.cmp, c(train.ind,valid.ind)),length(denied_though.ind)*0.5)  
######## With 2017 data
#train.ind <- sample(1:length.cmp, length(denied.ind)*0.24)
#valid.ind <- sample(setdiff(1:length.cmp, train.ind),length(denied.ind)*0.12)  
#test.ind <- sample(setdiff(1:length.cmp, c(train.ind,valid.ind)),length(denied.ind)*0.12)  

# Reset data with the complete cases without outliers & initialize
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
cmp.df_cdwo_outlier$CASE_STATUS <- 1*(cmp.df_cdwo_outlier$CASE_STATUS == "CERTIFIED")
cmp.df_cdwo_outlier[,c("CASE_NUMBER","CASE_SUBMITTED","DECISION_DATE","EMPLOYMENT_START_DATE","EMPLOYMENT_END_DATE","EMPLOYER_NAME","EMPLOYER_CITY","EMPLOYER_STATE","AGENT_ATTORNEY_NAME",
                       "PW_UNIT_OF_PAY","PW_SOURCE_OTHER","WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")] <- NULL
cmp.df_cdwo_outlier$WAGE_DIFF <- cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM - cmp.df_cdwo_outlier$PREVAILING_WAGE
cmp.df_cdwo_outlier <- createDummyFeatures(cmp.df_cdwo_outlier, cols = c("VISA_CLASS","SOC_CODE","NAICS_CODE","PW_WAGE_LEVEL","PW_SOURCE","PW_SOURCE_YEAR"), method = "reference")
# Assgin to train, validation and test data sets
train.df <- cmp.df_cdwo_outlier[train.ind,]
valid.df <- cmp.df_cdwo_outlier[valid.ind,]
test.df <- cmp.df_cdwo_outlier[test.ind,]
# Normalize continuous numerical variables of the train, validation and test data sets
conum.ind <- which(names(train.df) %in% c("TOTAL_WORKERS","NEW_EMPLOYMENT","CONTINUED_EMPLOYMENT","CHANGE_PREVIOUS_EMPLOYMENT","NEW_CURRENT_EMPLOYMENT","CHANGE_EMPLOYER",
                                          "AMENDED_PETITION","PREVAILING_WAGE","WAGE_RATE_OF_PAY_FROM","WAGE_DIFF"))
normalization.model <- preProcess(train.df[,conum.ind], method = c("center","scale"))
train.df.norm <- train.df
valid.df.norm <- valid.df
test.df.norm <- test.df
train.df.norm[,conum.ind] <- predict(normalization.model, train.df[,conum.ind])
valid.df.norm[,conum.ind] <- predict(normalization.model, valid.df[,conum.ind])
test.df.norm[,conum.ind] <- predict(normalization.model, test.df[,conum.ind])
summary(train.df.norm)

###################
# Prediction calculation #
###################
ub.cat.val.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
ub.cat.tst.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
#Base prediction
confusion.val <- confusionMatrix((valid.df$WAGE_RATE_OF_PAY_FROM >= valid.df$PREVAILING_WAGE)*1, valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix((test.df$WAGE_RATE_OF_PAY_FROM >= test.df$PREVAILING_WAGE)*1, test.df$CASE_STATUS, positive = "1")
ub.cat.val.Accuracy$Model <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
ub.cat.tst.Accuracy$Model <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

confusion.val <- confusionMatrix(rep(1,length(valid.ind)), valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix(rep(1,length(test.ind)), test.df$CASE_STATUS, positive = "1")
ub.cat.val.Accuracy$Rand <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
ub.cat.tst.Accuracy$Rand <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

#Tree
tmp <- tree(train.df.norm, valid.df.norm, test.df.norm)
ub.cat.val.Accuracy$TR <- tmp[,1]
ub.cat.tst.Accuracy$TR <- tmp[,2]

#Logistic regression
tmp <- logreg(train.df.norm, valid.df.norm, test.df.norm) #Warnings ignored
#tmp <- logreg(train.df.norm[,-c(50:51,72,83,88)], valid.df.norm[,-c(50:51,72,83,88)], test.df.norm[,-c(50:51,72,83,88)])
#tmp <- logreg(train.df.norm[,c(1:75,78,81:88,90:92)], valid.df.norm[,c(1:75,78,81:88,90:92)], test.df.norm[,c(1:75,78,81:88,90:92)]) #Warnings ignored
#tmp <- logreg(train.df.norm[,c(1:49,52:75,77:85,87:92)], valid.df.norm[,c(1:49,52:75,77:85,87:92)], test.df.norm[,c(1:49,52:75,77:85,87:92)]) #Warnings ignored
#tmp <- logreg(train.df.norm[,c(1:49,52:75,77:85,87:96)], valid.df.norm[,c(1:49,52:75,77:85,87:96)], test.df.norm[,c(1:49,52:75,77:85,87:96)]) #Warnings ignored
ub.cat.val.Accuracy$LR <- tmp[,1]
ub.cat.tst.Accuracy$LR <- tmp[,2]

#knn
tmp <- knearn(train.df.norm, valid.df.norm, test.df.norm)
ub.cat.val.Accuracy$KNN <- tmp[,1]
ub.cat.tst.Accuracy$KNN <- tmp[,2]

#Neural net
tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm) #Due to a warning or error, may need to run one or a couple more.
#tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm)
ub.cat.val.Accuracy$NN1 <- tmp[,1]
ub.cat.tst.Accuracy$NN1 <- tmp[,2]

tmp <- nnet_opt(2,train.df.norm, valid.df.norm, test.df.norm)
ub.cat.val.Accuracy$NN2 <- tmp[,1]
ub.cat.tst.Accuracy$NN2 <- tmp[,2]

tmp <- nnet_opt(3,train.df.norm, valid.df.norm, test.df.norm)
ub.cat.val.Accuracy$NN3 <- tmp[,1]
ub.cat.tst.Accuracy$NN3 <- tmp[,2]



##################################################
#         (4) Unbalanced raw sampling            #
# Treating category variables by using frequency #
##################################################
cmp.df_cdwo_outlier <- df_cdwo_outlier[complete.cases(df_cdwo_outlier),]
cmp.df_cdwo_outlier$CASE_STATUS <- 1*(cmp.df_cdwo_outlier$CASE_STATUS == "CERTIFIED")
cmp.df_cdwo_outlier[,c("CASE_NUMBER","CASE_SUBMITTED","DECISION_DATE","EMPLOYMENT_START_DATE","EMPLOYMENT_END_DATE","EMPLOYER_CITY","EMPLOYER_STATE", #"AGENT_ATTORNEY_NAME","EMPLOYER_NAME",
                       "PW_UNIT_OF_PAY","PW_SOURCE_OTHER","WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")] <- NULL
cmp.df_cdwo_outlier$WAGE_DIFF <- cmp.df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM - cmp.df_cdwo_outlier$PREVAILING_WAGE
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, VISA_CLASS = ave(seq(nrow(cmp.df_cdwo_outlier)), VISA_CLASS, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, SOC_CODE = ave(seq(nrow(cmp.df_cdwo_outlier)), SOC_CODE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, NAICS_CODE = ave(seq(nrow(cmp.df_cdwo_outlier)), NAICS_CODE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_WAGE_LEVEL = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_WAGE_LEVEL, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_SOURCE = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_SOURCE, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, PW_SOURCE_YEAR = ave(seq(nrow(cmp.df_cdwo_outlier)), PW_SOURCE_YEAR, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, H1B_DEPENDENT = ave(seq(nrow(cmp.df_cdwo_outlier)), H1B_DEPENDENT, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, SUPPORT_H1B = ave(seq(nrow(cmp.df_cdwo_outlier)), SUPPORT_H1B, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, EMPLOYER_NAME = ave(seq(nrow(cmp.df_cdwo_outlier)), EMPLOYER_NAME, FUN=length))
cmp.df_cdwo_outlier <- transform(cmp.df_cdwo_outlier, AGENT_ATTORNEY_NAME = ave(seq(nrow(cmp.df_cdwo_outlier)), AGENT_ATTORNEY_NAME, FUN=length))
train.df <- cmp.df_cdwo_outlier[train.ind,]
valid.df <- cmp.df_cdwo_outlier[valid.ind,]
test.df <- cmp.df_cdwo_outlier[test.ind,]

normalization.model <- preProcess(train.df[,-1], method = c("scale","center"))
train.df.norm <- train.df
valid.df.norm <- valid.df
test.df.norm <- test.df
train.df.norm[,-1] <- predict(normalization.model, train.df[,-1])
valid.df.norm[,-1] <- predict(normalization.model, valid.df[,-1])
test.df.norm[,-1] <- predict(normalization.model, test.df[,-1])
summary(train.df.norm)

ub.num.val.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
ub.num.tst.Accuracy <- data.frame(Model = rep(NA,5), Rand = rep(NA,5), TR = rep(NA,5), LR = rep(NA,5), KNN = rep(NA,5), NN1 = rep(NA,5), NN2 = rep(NA,5), NN3 = rep(NA,5))
#Base prediction
confusion.val <- confusionMatrix((valid.df$WAGE_RATE_OF_PAY_FROM >= valid.df$PREVAILING_WAGE)*1, valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix((test.df$WAGE_RATE_OF_PAY_FROM >= test.df$PREVAILING_WAGE)*1, test.df$CASE_STATUS, positive = "1")
ub.num.val.Accuracy$Model <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
ub.num.tst.Accuracy$Model <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

confusion.val <- confusionMatrix(rep(1,length(valid.ind)), valid.df$CASE_STATUS, positive = "1")
confusion.test <- confusionMatrix(rep(1,length(test.ind)), test.df$CASE_STATUS, positive = "1")
ub.num.val.Accuracy$Rand <- c(confusion.val$overall[1], confusion.val$byClass[1:4])
ub.num.tst.Accuracy$Rand <- c(confusion.test$overall[1], confusion.test$byClass[1:4])

#Tree
tmp <- tree(train.df.norm, valid.df.norm, test.df.norm)
ub.num.val.Accuracy$TR <- tmp[,1]
ub.num.tst.Accuracy$TR <- tmp[,2]

#Logistic regression
tmp <- logreg(train.df.norm, valid.df.norm, test.df.norm) #need to check 1:17 and 513:516
ub.num.val.Accuracy$LR <- tmp[,1]
ub.num.tst.Accuracy$LR <- tmp[,2]

#knn
tmp <- knearn(train.df.norm, valid.df.norm, test.df.norm)
ub.num.val.Accuracy$KNN <- tmp[,1]
ub.num.tst.Accuracy$KNN <- tmp[,2]

#Neural net
tmp <- nnet_opt(1,train.df.norm, valid.df.norm, test.df.norm)
ub.num.val.Accuracy$NN1 <- tmp[,1]
ub.num.tst.Accuracy$NN1 <- tmp[,2]

tmp <- nnet_opt(2,train.df.norm, valid.df.norm, test.df.norm)
ub.num.val.Accuracy$NN2 <- tmp[,1]
ub.num.tst.Accuracy$NN2 <- tmp[,2]

tmp <- nnet_opt(3,train.df.norm, valid.df.norm, test.df.norm)
ub.num.val.Accuracy$NN3 <- tmp[,1]
ub.num.tst.Accuracy$NN3 <- tmp[,2]






ens.cat.tst.Accuracy <- rbind(ens.cat.tst.Accuracy,cat.tst.Accuracy[1,])
ens.cat.val.Accuracy <- rbind(ens.cat.val.Accuracy,cat.val.Accuracy[1,])
ens.ub.cat.tst.Accuracy <- rbind(ens.ub.cat.tst.Accuracy,ub.cat.tst.Accuracy[1,])
ens.ub.cat.val.Accuracy <- rbind(ens.ub.cat.val.Accuracy,ub.cat.val.Accuracy[1,])
ens.num.tst.Accuracy <- rbind(ens.num.tst.Accuracy,num.tst.Accuracy[1,])
ens.num.val.Accuracy <- rbind(ens.num.val.Accuracy,num.val.Accuracy[1,])
ens.ub.num.tst.Accuracy <- rbind(ens.ub.num.tst.Accuracy, ub.num.tst.Accuracy[1,])
ens.ub.num.val.Accuracy <- rbind(ens.ub.num.val.Accuracy, ub.num.val.Accuracy[1,])

ens.cat.tst.Accuracy
ens.cat.val.Accuracy
ens.num.tst.Accuracy
ens.num.val.Accuracy
ens.ub.cat.tst.Accuracy
ens.ub.cat.val.Accuracy
ens.ub.num.tst.Accuracy
ens.ub.num.val.Accuracy


  






  
names <- c("Model", "Rand", "TR", "LR", "KNN", "NN1", "NN2", "NN3")
ens.cat.val.Accuracy <- data.frame()
ens.cat.tst.Accuracy <- data.frame()
ens.ub.cat.val.Accuracy <- data.frame()
ens.ub.cat.tst.Accuracy <- data.frame()
ens.num.val.Accuracy <- data.frame()
ens.num.tst.Accuracy <- data.frame()
ens.ub.num.val.Accuracy <- data.frame()
ens.ub.num.tst.Accuracy <- data.frame()
for (k in names) {
  ens.cat.val.Accuracy[[k]]<-as.numeric()
  ens.cat.tst.Accuracy[[k]]<-as.numeric()
  ens.ub.cat.val.Accuracy[[k]]<-as.numeric()
  ens.ub.cat.tst.Accuracy[[k]]<-as.numeric()
  ens.num.val.Accuracy[[k]]<-as.numeric()
  ens.num.tst.Accuracy[[k]]<-as.numeric()
  ens.ub.num.val.Accuracy[[k]]<-as.numeric()
  ens.ub.num.tst.Accuracy[[k]]<-as.numeric()
}




ggplot(train.df,aes(x=NAICS_CODE,group=CASE_STATUS,fill=CASE_STATUS)) + theme_bw() + geom_bar() #+ geom_histogram(position="dodge",binwidth=0.25)
which(train.df$CASE_STATUS=="Denied" & )

ggplot(train.df, aes(x=CASE_STATUS, y=WAGE_RATE_OF_PAY_FROM)) + geom_boxplot() + scale_y_log10()

#for (i in 1:length(train.ind)-1) {
#  for(j in i+1:length(train.ind)) {
#    if(as.character(df_cdwo_outlier[i,"EMPLOYER_NAME"]) == as.character(df_cdwo_outlier[j,"EMPLOYER_NAME"])) { c(i,j) }
#  }
#  #which(as.character(df_cdwo_outlier[i,"EMPLOYER_NAME"]) == df_cdwo_outlier[i+1:nrow(df_cdwo_outlier),"EMPLOYER_NAME"] & as.character(df_cdwo_outlier[i,"EMPLOYER_CITY"]) == df_cdwo_outlier[i+1:nrow(df_cdwo_outlier),"EMPLOYER_CITY"], arr.ind=TRUE )
#  #which(as.character(df_cdwo_outlier[i,"EMPLOYER_NAME"]) == df_cdwo_outlier[i+1:nrow(df_cdwo_outlier),"EMPLOYER_NAME"] & as.character(df_cdwo_outlier[i,"NAICS_CODE"]) == df_cdwo_outlier[i+1:nrow(df_cdwo_outlier),"NAICS_CODE"], arr.ind=TRUE )
#}

cbind(summary(df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="DENIED"&df_cdwo_outlier$WAGE_RATE_OF_PAY_FROM > df_cdwo_outlier$PREVAILING_WAGE,arr.ind=TRUE)),]$EMPLOYER_NAME)) 
cbind(summary(df_cdwo_outlier[c(which(df_cdwo_outlier$CASE_STATUS=="CERTIFIED",arr.ind=TRUE)),]$EMPLOYER_NAME))

df_cdwo_outlier$PW_WAGE_LEVEL


boxplot(df_cert_den$WAGE_RATE_OF_PAY_FROM ~ df_cert_den$CASE_STATUS,log = "y")

ggplot(df_cert_den,aes(x=WAGE_RATE_OF_PAY_FROM,group=CASE_STATUS,fill=CASE_STATUS)) + geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

ggplot(df_cert_den, aes(x=CASE_STATUS, y=WAGE_RATE_OF_PAY_FROM)) + geom_boxplot() + scale_y_log10() #+ geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))

