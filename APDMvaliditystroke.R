df <- read.csv("data_avg.csv")
df_D1 <-df[which(df$Day == 1),]
df_APDM <-df_D1[which(df_D1$System == "IMU"),]
df_Vicon <-df_D1[which(df_D1$System == "Motion Capture"),]

## Stance_P 
X_Stance_P <- df_Vicon[which(df_Vicon$P_or_NP == "Paretic"), "Stance"] 
Y_Stance_P <- df_APDM[which(df_APDM$P_or_NP == "Paretic"), "Stance"] 


tmp.ccc <-CCC(X_Stance_P, Y_Stance_P, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))


plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Stance Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")
   
abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")


lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")

z <- lm(Y_Stance_P ~ X_Stance_P)
par(pty = "s")
plot(X_Stance_P, Y_Stance_P, main="Stance Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

#################
## Stance Non Paretic
X_Stance_NP <- df_Vicon[which(df_Vicon$P_or_NP == "Non_Paretic"), "Stance"]
Y_Stance_NP <- df_APDM[which(df_APDM$P_or_NP == "Non_Paretic"), "Stance"]
                       
tmp.ccc <-CCC(X_Stance_NP, Y_Stance_NP, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Stance Non-Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")

lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")

z <- lm(Y_Stance_NP ~ X_Stance_NP)

par(pty = "s")
plot(X_Stance_NP, Y_Stance_NP, main="Stance Non-Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

########################################
#Double Support paretic 
X_Double_Support_P <- df_Vicon[which(df_Vicon$P_or_NP == "Paretic"), "Double_Support"]
Y_Double_Support_P <- df_APDM[which(df_APDM$P_or_NP == "Paretic"), "Double_Support"]


tmp.ccc <-CCC(X_Double_Support_P, Y_Double_Support_P, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Double Support Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")

lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Double_Support_P ~ X_Double_Support_P)

par(pty = "s")
plot(X_Double_Support_P, Y_Double_Support_P, main="Double Support Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

####################
#Double Support Non paretic
X_Double_Support_NP <- df_Vicon[which(df_Vicon$P_or_NP == "Non_Paretic"), "Double_Support"]
Y_Double_Support_NP <- df_APDM[which(df_APDM$P_or_NP == "Non_Paretic"), "Double_Support"] 

tmp.ccc <-CCC(X_Double_Support_NP, Y_Double_Support_NP, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Double Support Non-Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")


lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Double_Support_NP ~ X_Double_Support_NP)
par(pty = "s")
plot(X_Double_Support_NP, Y_Double_Support_NP, main="Double Support Non-Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

##################
#Swing Paretic 
X_Swing_P <- df_Vicon[which(df_Vicon$P_or_NP == "Paretic"), "Swing"]
Y_Swing_P <- df_APDM[which(df_APDM$P_or_NP == "Paretic"), "Swing"] 

tmp.ccc <-CCC(X_Swing_P, Y_Swing_P, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Swing Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")


lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Swing_P ~ X_Swing_P)
par(pty = "s")
plot(X_Swing_P, Y_Swing_P, main="Swing Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

#########################
#Swing Non Paretic 
X_Swing_NP <- df_Vicon[which(df_Vicon$P_or_NP == "Non_Paretic"), "Swing"]
Y_Swing_NP <- df_APDM[which(df_APDM$P_or_NP == "Non_Paretic"), "Swing"] 

tmp.ccc <-CCC(X_Swing_NP, Y_Swing_NP, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Swing Non-Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")

lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Swing_NP ~ X_Swing_NP)
par(pty = "s")
plot(X_Swing_NP, Y_Swing_NP, main="Swing Non-Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

############################################
#Gait Spped Paretic

X_Gait_Speed_P <- df_Vicon[which(df_Vicon$P_or_NP == "Paretic"), "Gait_Speed"]
Y_Gait_Speed_P <- df_APDM[which(df_APDM$P_or_NP == "Paretic"), "Gait_Speed"]

tmp.ccc <-CCC(X_Gait_Speed_P, Y_Gait_Speed_P, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Gait Speed Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")



lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Gait_Speed_P ~ X_Gait_Speed_P)
par(pty = "s")
plot(X_Gait_Speed_P, Y_Gait_Speed_P, main="Gait Speed Paretic Reference", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)
########################
#Gait Speed Non Paretic

X_Gait_Speed_NP <- df_Vicon[which(df_Vicon$P_or_NP == "Non_Paretic"), "Gait_Speed"]
Y_Gait_Speed_NP <- df_APDM[which(df_APDM$P_or_NP == "Non_Paretic"), "Gait_Speed"]

tmp.ccc <-CCC(X_Gait_Speed_NP, Y_Gait_Speed_NP, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Gait Speed Non-Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")




lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Gait_Speed_NP ~ X_Gait_Speed_NP)
par(pty = "s")
plot(X_Gait_Speed_NP, Y_Gait_Speed_NP, main="Gait Speed Non-Paretic Reference", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)
#################################
#Stride Length Paretic

X_Stride_Length_P <- df_Vicon[which(df_Vicon$P_or_NP == "Paretic"), "Stride_Length"]
Y_Stride_Length_P <- df_APDM[which(df_APDM$P_or_NP == "Paretic"), "Stride_Length"] 

tmp.ccc <-CCC(X_Stride_Length_P, Y_Stride_Length_P, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Stride Length Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")

lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Stride_Length_P ~ X_Stride_Length_P)
par(pty = "s")
plot(X_Stride_Length_P,Y_Stride_Length_P, main="Step Length Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

CCC(X_Stride_Length_P, Y_Stride_Length_P, ci = "z-transform", conf.level = 0.95, na.rm = FALSE) 
##################################
#Stride Length Non Paretic 
X_Stride_Length_NP <- df_Vicon[which(df_Vicon$P_or_NP == "Non_Paretic"), "Stride_Length"]
Y_Stride_Length_NP <- df_APDM[which(df_APDM$P_or_NP == "Non_Paretic"), "Stride_Length"] 

tmp.ccc <-CCC(X_Stride_Length_NP, Y_Stride_Length_NP, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta,main="Stride Length Non-Paretic", pch = 16, 
   xlab = "Average of IMU & Motion Capture", 
   ylab = "Difference between IMU & Motion Capture")

abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")


lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(Y_Stride_Length_NP ~ X_Stride_Length_NP)
par(pty = "s")
plot(X_Stride_Length_NP,Y_Stride_Length_NP, main="Step Length Non Paretic", xlab = "Motion Capture System", 
   ylab = "IMU", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)

CCC(X_Stride_Length_NP, Y_Stride_Length_NP, ci = "z-transform", conf.level = 0.95, na.rm = FALSE) 
