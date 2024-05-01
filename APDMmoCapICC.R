df <- read.csv("data_avg.csv") 
df_MoCap <- df[which(df$System == "Motion Capture"),] 
df_MoCap_D1 <- df_MoCap[which(df_MoCap$Day == 1),] 
df_MoCap_D2 <- df_MoCap[which(df_MoCap$Day == 2),] 

## Stance_P 
r_Stance_P <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Paretic"), "Stance"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Paretic"), "Stance"]) 
icc(r_Stance_P, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95) 

## Stance_NP 
r_Stance_NP <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Non_Paretic"), "Stance"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Non_Paretic"), "Stance"]) 
icc(r_Stance_NP, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95) 

## Double_Support_P 
r_Double_Support_P <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Paretic"), "Double_Support"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Paretic"), "Double_Support"]) 
icc(r_Double_Support_P, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95) 

## Double_Support_NP 
r_Double_Support_NP <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Non_Paretic"), "Double_Support"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Non_Paretic"), "Double_Support"]) 
icc(r_Double_Support_NP, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95) 

## Gait_Speed_P 
r_Gait_Speed_P <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Paretic"), "Gait_Speed"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Paretic"), "Gait_Speed"]) 
icc(r_Gait_Speed_P, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95)     

## Gait_Speed_NP 
r_Gait_Speed_NP <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Non_Paretic"), "Gait_Speed"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Non_Paretic"), "Gait_Speed"]) 
icc(r_Gait_Speed_NP, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95)

## Stride_Length_P 
r_Stride_Length_P <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Paretic"), "Stride_Length"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Paretic"), "Stride_Length"]) 
icc(r_Stride_Length_P, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95)

## Stride_Length_NP 
r_Stride_Length_NP <- cbind(df_MoCap_D1[which(df_MoCap_D1$P_or_NP == "Non_Paretic"), "Stride_Length"], df_MoCap_D2[which(df_MoCap_D2$P_or_NP == "Non_Paretic"), "Stride_Length"]) 
icc(r_Stride_Length_NP, model = "twoway", type = "agreement", unit = "single", r0 = 0, conf.level = 0.95)
    