rm(list=ls())
if("openxlsx" %in% rownames(installed.packages()) == FALSE) { install.packages("openxlsx") } ; library(openxlsx)
if("lme4" %in% rownames(installed.packages()) == FALSE) { install.packages("lme4") } ; library(lme4)
if("lmerTest" %in% rownames(installed.packages()) == FALSE) { install.packages("lmerTest") } ; library(lmerTest)
if("car" %in% rownames(installed.packages()) == FALSE) { install.packages("car") } ; library(car)
r <- 1


# ---------------------------------------------------------------------------------------------------------------------------------
# (1) Define path, read data and define species to analyze ------------------------------------------------------------------------
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"
test.data <- read.table(paste0(path,"orangutanR_infants.csv"), header=TRUE, sep=",")
infant.age.dat <- read.xlsx(xlsxFile=paste0(path,"Orang_infant_age.xlsx"), sheet=1, colNames=FALSE) ; colnames(infant.age.dat) <- c("animal_id","ageYears")

# Set Species to "B"=Bornean, "S"=Sumatran or "BS"=Bornean+Sumatran
Species <- "BS"
# Set Group to "IF"=Infants --> Mothers or "IO"=Infants --> Others
Group <- "IF"
# Set comparisons
Comp <- "WB" # "WB"=within-between, "WC"=wild-captive
# Specify whether you want to perform matrix permutations
MatrixPerm <- TRUE

# ---------------------------------------------------------------------------------------------------------------------------------
# (2) Subset data frame to the specified group ------------------------------------------------------------------------------------
if(Group=="IF") { dat <- subset(test.data, kinship=='motoff' & agediff=='older') }
if(Group=="IO") { dat <- subset(test.data, kinship!='motoff' & age!='Ad') }
if(Group=="IP") { dat <- subset(test.data, kinship!='motoff' & age!='Ad' & agediff!='older')}
if(Group=="IA") { dat <- subset(test.data, kinship!='motoff' & age!='Ad' & agediff=='older')}

colnames(dat)[which(colnames(dat)=="ID_sign")] <- "animal_id"
colnames(dat)[which(colnames(dat)=="Signal")] <- "Behavior2"
dat <- as.data.frame(na.omit(dat[ ,c("Context","Behavior2","animal_id","species","group","setting","ID_obs")]))
table(dat$animal_id)
dat$species_setting <- paste(dat$species, dat$setting, sep="_")
table(dat$Behavior2, dat$species_setting)
table(dat$species_setting)

# ---------------------------------------------------------------------------------------------------------------------------------
# (3) Omit all levels of animal_id that contributed fewer than 30 cases -----------------------------------------------------------
NID <- data.frame(table(dat$animal_id)) ; colnames(NID) <- c("animal_id","NID")
NID$animal_id <- as.character(NID$animal_id)
NID30 <- subset(NID, NID>=30)
dat <- subset(dat, animal_id %in% NID30$animal_id)

# ---------------------------------------------------------------------------------------------------------------------------------
# (4) Subset data frame to the specified species ----------------------------------------------------------------------------------
if(Species=="B") { dat <- subset(dat, species=="Bor") }
if(Species=="S") { dat <- subset(dat, species=="Sum") }
if(Species=="BS") { dat <- dat }

# ---------------------------------------------------------------------------------------------------------------------------------
# (5) Create a table for the set of behaviors (Behavior2) used by every individual (animal_ID) AT LEAST TWICE ---------------------
## create data frame in which every combination of individual and behaviour is counted
dat.ind.bhv <- data.frame(table(dat$animal_id,dat$Behavior2)) ; colnames(dat.ind.bhv) <- c("animal_id","Behavior2","N")
dat.ind.con <- data.frame(table(dat$animal_id,dat$Context)) ; colnames(dat.ind.con) <- c("animal_id","Context","N")
## subset dat.ind.bhv to include only rows where N>=2
dat.ind.bhv2 <- subset(dat.ind.bhv, N>=2)
dat.ind.con2 <- subset(dat.ind.con, N>=1)
## create data frame with the number of behaviours for each individual
dat.ind.bhv2.m <- data.frame(table(dat.ind.bhv2$animal_id)) ; colnames(dat.ind.bhv2.m) <- c("animal_id","NRep")
dat.ind.con2.m <- data.frame(table(dat.ind.con2$animal_id)) ; colnames(dat.ind.con2.m) <- c("animal_id","NCon")
## add predictor variables to dat.ind.bhv2.m
test.data.dup <- test.data[!duplicated(test.data$animal_id),c("animal_id","setting","species","sex","age", "group")]
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, test.data.dup, by.x="animal_id", by.y="animal_id")
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, NID, by.x="animal_id", by.y="animal_id")
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, dat.ind.con2.m, by.x="animal_id", by.y="animal_id")
if(Group=="IF") { dat.ind.bhv2.m$partner <- "Mother" }
if(Group=="IO") { dat.ind.bhv2.m$partner <- "Others" }
if(Group=="IP") { dat.ind.bhv2.m$partner <- "Peers" }
if(Group=="IA") { dat.ind.bhv2.m$partner <- "Older" }


## merge infant age to table
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, infant.age.dat, by.x="animal_id", by.y="animal_id", sort=FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------
# (6) Calculate Dice coefficient based on formula: dc = (2 x number of behaviours two inds have in common)/(R of ind1 + R ind2) ---
## create data frame with animal_id as rows and columns
dat.ind.ind <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.ind.ind) <- dat.ind.bhv2.m$animal_id
rownames(dat.ind.ind) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
	for(j in 1:(nrow(dat.ind.bhv2.m))) {
		Ind1 <- subset(dat.ind.bhv2, animal_id==dat.ind.bhv2.m$animal_id[i])
		Ind2 <- subset(dat.ind.bhv2, animal_id==dat.ind.bhv2.m$animal_id[j])
		ovrlp <- length(which(Ind1$Behavior2 %in% Ind2$Behavior2))
		RInd1 <- subset(dat.ind.bhv2.m, animal_id==dat.ind.bhv2.m$animal_id[i])$NRep	# which is the same as nrow(Ind1)
		RInd2 <- subset(dat.ind.bhv2.m, animal_id==dat.ind.bhv2.m$animal_id[j])$NRep	# which is the same as nrow(Ind2)
		dat.ind.ind[which(rownames(dat.ind.ind)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.ind.ind)==dat.ind.bhv2.m$animal_id[j])] <- (2 * ovrlp) / (RInd1 + RInd2)
	}
}
## set diagonal to NA
Dice <- as.matrix(dat.ind.ind)
diag(Dice) <- NA
#Dice <- as.data.frame(Dice)

# ---------------------------------------------------------------------------------------------------------------------------------
# (7-A) Define within and between setting dyads -------------------------------------------------------------------------------------
if(Comp=="WB") {
## create data frame with animal_id as rows and columns
dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
	for(j in 1:(nrow(dat.ind.bhv2.m))) {
		Ind1 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[i])
		Ind2 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[j])
		if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
		if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
		if(Ind1$setting[1]==Ind2$setting[1]) { wth.btw <- "W" } else { wth.btw <- "B" }
		dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
	}
}

## set diagonal to NA
wth.btw <- as.matrix(dat.wth.btw)
diag(wth.btw) <- NA
#wth.btw <- as.data.frame(wth.btw)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (7-B) Define within and between setting dyads ---
if(Comp=="WC") {
## create data frame with animal_id as rows and columns
dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
  for(j in 1:(nrow(dat.ind.bhv2.m))) {
    wth.btw <- NA
    Ind1 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[i])
    Ind2 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[j])
    if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
    if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
    if(Ind1$setting[1]=="wild" & Ind2$setting[1]=="wild") { wth.btw <- "W" }
    if(Ind1$setting[1]=="captive" & Ind2$setting[1]=="captive") { wth.btw <- "C" }
    dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
  }
}
## set diagonal to NA
wth.btw <- as.matrix(dat.wth.btw)
diag(wth.btw) <- NA
#wth.btw <- as.data.frame(wth.btw)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (8-A) Select within and between group dyads from the Dice data frame --------------------------------------------------------------
if(Comp=="WB") {
Dice.Within <- ifelse(wth.btw=="W",Dice,NA)
Dice.Between <- ifelse(wth.btw=="B",Dice,NA)

Emp.MDiceW <- mean(Dice.Within[lower.tri(Dice.Within, diag = FALSE)],na.rm=TRUE)
Emp.MDiceB <- mean(Dice.Between[lower.tri(Dice.Between, diag = FALSE)],na.rm=TRUE)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (8-B) Select within and between group dyads from the Dice data frame --------------------------------------------------------------
if(Comp=="WC") {
Dice.Wild <- ifelse(wth.btw=="W",Dice,NA)
Dice.Captive <- ifelse(wth.btw=="C",Dice,NA)

(Emp.MDiceWild <- mean(Dice.Wild[lower.tri(Dice.Wild, diag = FALSE)],na.rm=TRUE))
(Emp.MDiceCaptive <- mean(Dice.Captive[lower.tri(Dice.Captive, diag = FALSE)],na.rm=TRUE))
}

# Bornean N = 4 (C), N = 8 (W)
# Bornean IF within-captive: 0.69
# Bornean IF within-wild: 0.68
#

# Sumatran N = 9
# Sumatran moms within-captive:  0.54
# Sumatran moms within-wild: 0.74


# ---------------------------------------------------------------------------------------------------------------------------------
# (9-A) Create data frame dd with columns Ind1, Ind2, Dice, Within/Between --------------------------------------------------------
if(Comp=="WB") {
yW <- expand.grid(rownames(Dice.Within), colnames(Dice.Within))
labs <- yW[as.vector(upper.tri(Dice.Within, diag = FALSE)), ]
yW <- cbind(labs, Dice.Within[upper.tri(Dice.Within,diag=FALSE)])
colnames(yW) <- c("Ind1","Ind2","Dice")
yW <- yW[!is.na(yW$Dice), ]
yW$WB <- rep("Within",nrow(yW))

yB <- expand.grid(rownames(Dice.Between), colnames(Dice.Between))
labs <- yB[as.vector(upper.tri(Dice.Between, diag = FALSE)), ]
yB <- cbind(labs, Dice.Between[upper.tri(Dice.Between,diag=FALSE)])
colnames(yB) <- c("Ind1","Ind2","Dice")
yB <- yB[!is.na(yB$Dice), ]
yB$WB <- rep("Between",nrow(yB))
dd <- rbind(yW,yB)
dat.sub <- dat[,c("animal_id","species","group","setting")]
dat.sub <- dat.sub[!duplicated(dat.sub), ]
dd <- merge(dd, dat.sub, by.x="Ind1", by.y="animal_id", sort=FALSE, all.y=FALSE)
dd <- merge(dd, dat.sub, by.x="Ind2", by.y="animal_id", sort=FALSE)
colnames(dd)[which(colnames(dd)=="species.x")] <- "species.ind1"
colnames(dd)[which(colnames(dd)=="group.x")] <- "group.ind1"
colnames(dd)[which(colnames(dd)=="setting.x")] <- "setting.ind1"
colnames(dd)[which(colnames(dd)=="species.y")] <- "species.ind2"
colnames(dd)[which(colnames(dd)=="group.y")] <- "group.ind2"
colnames(dd)[which(colnames(dd)=="setting.y")] <- "setting.ind2"
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (9-B) Create data frame dd with columns Ind1, Ind2, Dice, Wild/Captive ----------------------------------------------------------
if(Comp=="WC") {
yW <- expand.grid(rownames(Dice.Wild), colnames(Dice.Wild))
labs <- yW[as.vector(upper.tri(Dice.Wild, diag = FALSE)), ]
yW <- cbind(labs, Dice.Wild[upper.tri(Dice.Wild,diag=FALSE)])
colnames(yW) <- c("Ind1","Ind2","Dice")
yW <- yW[!is.na(yW$Dice), ]
yW$WB <- rep("Wild",nrow(yW))

yC <- expand.grid(rownames(Dice.Captive), colnames(Dice.Captive))
labs <- yC[as.vector(upper.tri(Dice.Captive, diag = FALSE)), ]
yC <- cbind(labs, Dice.Captive[upper.tri(Dice.Captive,diag=FALSE)])
colnames(yC) <- c("Ind1","Ind2","Dice")
yC <- yC[!is.na(yC$Dice), ]
yC$WB <- rep("Captive",nrow(yC))
dd <- rbind(yW,yC)
dat.sub <- dat[,c("animal_id","species","group","setting")]
dat.sub <- dat.sub[!duplicated(dat.sub), ]
dd <- merge(dd, dat.sub, by.x="Ind1", by.y="animal_id", sort=FALSE, all.y=FALSE)
dd <- merge(dd, dat.sub, by.x="Ind2", by.y="animal_id", sort=FALSE)
colnames(dd)[which(colnames(dd)=="species.x")] <- "species.ind1"
colnames(dd)[which(colnames(dd)=="group.x")] <- "group.ind1"
colnames(dd)[which(colnames(dd)=="setting.x")] <- "setting.ind1"
colnames(dd)[which(colnames(dd)=="species.y")] <- "species.ind2"
colnames(dd)[which(colnames(dd)=="group.y")] <- "group.ind2"
colnames(dd)[which(colnames(dd)=="setting.y")] <- "setting.ind2"
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (10-A) Perform matrix permutation test --------------------------------------------------------------------------------------------
if(Comp=="WB" & MatrixPerm == TRUE) {
	dat.inc <- subset(dat, animal_id %in% dat.ind.bhv2.m$animal_id)
	ind.setting <- unique(dat.inc[,c("animal_id","group","species","setting")])
	ind.setting$species_setting <- paste(ind.setting$species, ind.setting$setting, sep="_")
	
	nPerm <- 1000
	dat.Perm <- c()
	for(k in 1:nPerm) {
		## permute setting in dat
		ind.setting.perm <- ind.setting
		ind.setting.perm$setting <- sample(ind.setting.perm$setting, nrow(ind.setting.perm), replace=FALSE)
		dat.inc.perm <- dat.inc[,-which(colnames(dat.inc)=="setting")]
		dat.inc.perm <- merge(dat.inc.perm, ind.setting.perm, by.x="animal_id", by.y="animal_id", sort=FALSE)
		## repeat step 5 & 6
		## create data frame with animal_id as rows and columns
		dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
		colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		## loop over all pairs of individuals
		for(i in 1:(nrow(dat.ind.bhv2.m))) {
			for(j in 1:(nrow(dat.ind.bhv2.m))) {
				Ind1 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[i])
				Ind2 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[j])
				if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
				if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
				if(Ind1$setting[1]==Ind2$setting[1]) { wth.btw <- "W" } else { wth.btw <- "B" }
				dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
			}
		}
		## set diagonal to NA
		wth.btw <- as.matrix(dat.wth.btw)
		diag(wth.btw) <- NA
		Dice.Within <- ifelse(wth.btw=="W",Dice,NA)
		Dice.Between <- ifelse(wth.btw=="B",Dice,NA)
		MDiceW <- mean(Dice.Within[lower.tri(Dice.Within, diag = FALSE)],na.rm=TRUE)
		MDiceB <- mean(Dice.Between[lower.tri(Dice.Between, diag = FALSE)],na.rm=TRUE)
		dat.Perm[k] <- MDiceW - MDiceB
		flush.console()
		if(k %% 10 == 0) { print(paste0("Finished ", k, " out of ", nPerm, " simulations")) }
	}
	
	hist(dat.Perm)
	abline(v=Emp.MDiceW - Emp.MDiceB, col="red")
	Pval.WB <- 1 - sum(dat.Perm<=(Emp.MDiceW - Emp.MDiceB)) / length(dat.Perm)	# This is the P-value
}
# Significance thresholds are P>=0.975 and P<=0.025. 
# This is because we are looking at the deviation from 0, which can either be negative or positive.
# Because the distribution of differences is not necessarily symmetric around zero, we cannot calculate P-values using absolute values.

# ---------------------------------------------------------------------------------------------------------------------------------
# (10-B) Perform matrix permutation test --------------------------------------------------------------------------------------------
if(Comp=="WC" & MatrixPerm == TRUE) {
	dat.inc <- subset(dat, animal_id %in% dat.ind.bhv2.m$animal_id)
	ind.setting <- unique(dat.inc[,c("animal_id","group","species","setting")])
	ind.setting$species_setting <- paste(ind.setting$species, ind.setting$setting, sep="_")
	
	nPerm <- 1000
	dat.Perm <- c()
	for(k in 1:nPerm) {
		## permute setting in dat
		ind.setting.perm <- ind.setting
		ind.setting.perm$setting <- sample(ind.setting.perm$setting, nrow(ind.setting.perm), replace=FALSE)
		dat.inc.perm <- dat.inc[,-which(colnames(dat.inc)=="setting")]
		dat.inc.perm <- merge(dat.inc.perm, ind.setting.perm, by.x="animal_id", by.y="animal_id", sort=FALSE)
		## repeat step 5 & 6
		## create data frame with animal_id as rows and columns
		dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
		colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		## loop over all pairs of individuals
		for(i in 1:(nrow(dat.ind.bhv2.m))) {
			for(j in 1:(nrow(dat.ind.bhv2.m))) {
				wth.btw <- NA
				Ind1 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[i])
				Ind2 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[j])
				if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
				if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
				if(Ind1$setting[1]=="wild" & Ind2$setting[1]=="wild") { wth.btw <- "W" }
				if(Ind1$setting[1]=="captive" & Ind2$setting[1]=="captive") { wth.btw <- "C" }
				dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
			}
		}
		## set diagonal to NA
		wth.btw <- as.matrix(dat.wth.btw)
		diag(wth.btw) <- NA
		Dice.Wild <- ifelse(wth.btw=="W",Dice,NA)
		Dice.Captive <- ifelse(wth.btw=="C",Dice,NA)
		MDiceW <- mean(Dice.Wild[lower.tri(Dice.Wild, diag = FALSE)],na.rm=TRUE)
		MDiceC <- mean(Dice.Captive[lower.tri(Dice.Captive, diag = FALSE)],na.rm=TRUE)
		dat.Perm[k] <- MDiceW - MDiceC
		flush.console()
		if(k %% 10 == 0) { print(paste0("Finished ", k, " out of ", nPerm, " simulations")) }
	}

	hist(dat.Perm)
	abline(v=Emp.MDiceWild - Emp.MDiceCaptive, col="red")
	Pval.WC <- 1 - sum(dat.Perm<=(Emp.MDiceWild - Emp.MDiceCaptive)) / length(dat.Perm)	# This is the P-value
}
# Significance thresholds are P>=0.975 and P<=0.025. 
# This is because we are looking at the deviation from 0, which can either be negative or positive.
# Because the distribution of differences is not necessarily symmetric around zero, we cannot calculate P-values using absolute values.

# ---------------------------------------------------------------------------------------------------------------------------------
# (11-A) Print out empirical Dice coefficients and P-values of the matrix permutation tests -----------------------------------------
if(Comp=="WB") {
## sample sizes
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }

## empirical Dice within
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }

## empirical Dice between
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }

## P-values of the matrix permutation tests
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }

if(r==1) { outResults <- data.frame(matrix(rep(NA,6), ncol=6)) ; colnames(outResults) <- c("Species","Group","N","Emp.MDiceW","Emp.MDiceB","Pval.WB") }
outResults[r,"Species"] <- Species
outResults[r,"Group"] <- Group
outResults[r,"N"] <- nrow(dat.ind.bhv2.m)
outResults[r,"Emp.MDiceW"] <- Emp.MDiceW
outResults[r,"Emp.MDiceB"] <- Emp.MDiceB
outResults[r,"Pval.WB"] <- Pval.WB
r <- r + 1

#   Species Group  N Emp.MDiceW Emp.MDiceB Pval.WB
# 1       B    IF 12  0.6858453  0.5750977    0
# 2       S    IF 13  0.5565279  0.4036022    0
# 3       B    IO  9  0.8267473  0.3609093    0
# 4       S    IO 12  0.7190610  0.5772387    0
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (11-B) Print out empirical Dice coefficients and P-values of the matrix permutation tests -----------------------------------------
if(Comp=="WC") {
## sample sizes
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }

## empirical Dice captive
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }

## empirical Dice wild
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }

## P-values of the matrix permutation tests
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }

if(r==1) { outResults <- data.frame(matrix(rep(NA,6), ncol=6)) ; colnames(outResults) <- c("Species","Group","N","Emp.MDiceCaptive","Emp.MDiceWild","Pval.WC") }
outResults[r,"Species"] <- Species
outResults[r,"Group"] <- Group
outResults[r,"N"] <- nrow(dat.ind.bhv2.m)
outResults[r,"Emp.MDiceCaptive"] <- Emp.MDiceCaptive
outResults[r,"Emp.MDiceWild"] <- Emp.MDiceWild
outResults[r,"Pval.WC"] <- Pval.WC
r <- r + 1

#   Species Group  N Emp.MDiceCaptive Emp.MDiceWild Pval.WC
# 1       B    IF 12        0.6817849     0.6867154    0.46
# 2       B    IO  9        0.8289495     0.8230769   0.473
# 3       S    IF 13        0.3749052     0.6862583   0.006
# 4       S    IO 12        0.7138462     0.7300121   0.420
}





# ---------------------------------------------------------------------------------------------------------------------------------
# (12) Store and merge relevant data frames ---------------------------------------------------------------------------------------
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IF"
if(Species == "BS" & Group == "IF") { dat.ind.mot <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IO"
if(Species == "BS" & Group == "IO") { dat.ind.oth <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IP"
if(Species == "BS" & Group == "IP") { dat.ind.pee <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IA"
if(Species == "BS" & Group == "IA") { dat.ind.old <- dat.ind.bhv2.m }


## Merge dat.ind.mot and dat.ind.oth
dat.ind <- rbind(dat.ind.mot,dat.ind.oth)


# Model 1 Repertoire size
setwd("C:/Users/Marlen Fröhlich/Documents/R/roger/")
source("diagnostic_fcns.r"); contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

dat.ind.mot$z.age=as.vector(scale(dat.ind.mot$ageYears))	
dat.ind.mot$z.nid=as.vector(scale(dat.ind.mot$NID))
dat.ind.mot$z.log.nid=as.vector(scale(log(dat.ind.mot$NID)))
dat.ind.mot$obs.level.re=as.factor(1:nrow(dat.ind.mot))

dat.ind.oth$z.age=as.vector(scale(dat.ind.oth$ageYears))	
dat.ind.oth$z.nid=as.vector(scale(dat.ind.oth$NID))
dat.ind.oth$z.log.nid=as.vector(scale(log(dat.ind.oth$NID)))
dat.ind.oth$obs.level.re=as.factor(1:nrow(dat.ind.oth))

dat.ind.pee$z.age=as.vector(scale(dat.ind.pee$ageYears))	
dat.ind.pee$z.nid=as.vector(scale(dat.ind.pee$NID))
dat.ind.pee$z.log.nid=as.vector(scale(log(dat.ind.pee$NID)))
dat.ind.pee$obs.level.re=as.factor(1:nrow(dat.ind.pee))

dat.ind.old$z.age=as.vector(scale(dat.ind.old$ageYears))	
dat.ind.old$z.nid=as.vector(scale(dat.ind.old$NID))
dat.ind.old$z.log.nid=as.vector(scale(log(dat.ind.old$NID)))
dat.ind.old$obs.level.re=as.factor(1:nrow(dat.ind.old))


#### Model 1a: Repertoire size w mothers
#Check collinarity: max VIF = 2.1
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.log.nid, data=dat.ind.mot)
library(car)
vif(xres)

mod.NRep.m <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex + setting + species + z.log.nid + 
              (1|group), family = poisson, data = dat.ind.mot, control = contr)
null.NRep.m <- glmer(formula = NRep ~  sex + z.log.nid + 
                       (1|group), family = poisson, data = dat.ind.mot, control = contr)

summary(mod.NRep.m)
overdisp.test(mod.NRep.m) # dispersion parameter 0.48 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.m)) #25
length(residuals(mod.NRep.m)) #25

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.m, mod.NRep.m, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NRep.m    4 129.5997 134.4752 -60.79986 121.5997       NA NA         NA
#mod.NRep.m     8 132.8600 142.6110 -58.42999 116.8600 4.739752  4  0.3150575


### Model 1b: Repertoire size w other conspecifics

#Check collinarity: max VIF = 3.8
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.log.nid, data=dat.ind.oth)
library(car)
vif(xres)

mod.NRep.o <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex + setting + species + z.log.nid +
                    (1|group), family = poisson, data = dat.ind.oth, control = contr)
null.NRep.o <- glmer(formula = NRep ~  sex + z.log.nid +
                    (1|group), family = poisson, data = dat.ind.oth, control = contr)

#Check for overdispersion:
overdisp.test(mod.NRep.o) # dispersion parameter 0.62 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.o), coefficients(mod.NRep.o)+
        t(apply(X=dfbeta(mod.NRep.o), MARGIN=2, FUN=range)))

summary(mod.NRep.o)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.61310    0.16247  16.084  < 2e-16 ***
#z.age        0.02061    0.08284   0.249  0.80348    
#I(z.age^2)  -0.08449    0.09779  -0.864  0.38759    
#sexM         0.13424    0.12816   1.047  0.29488    
#settingwild -0.42027    0.19139  -2.196  0.02810 *  
#speciesSum   0.51802    0.17100   3.029  0.00245 ** 
#z.log.nid    0.34880    0.10839   3.218  0.00129 **


# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.o)) #21
length(residuals(mod.NRep.o)) #21

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.o, mod.NRep.o, test="Chisq"))
npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
null.NRep.o    4 125.2473 129.4254 -58.62366 117.2473       NA NA         NA
mod.NRep.o     8 120.1950 128.5512 -52.09752 104.1950 13.05228  4 0.01102316


#get chi square and p-values (all)
drop1(mod.NRep.o,  test ="Chisq")
#z.age         1 118.26 0.0620 0.803297   
#I(z.age^2)    1 118.94 0.7458 0.387804   
#sex           1 119.29 1.0941 0.295574   
#setting       1 123.03 4.8371 0.027854 * 
#species       1 126.32 8.1238 0.004369 **
#z.log.nid     1 125.61 7.4110 0.006483 **


### Model 1c: Repertoire size w peers

#Check collinarity: max VIF = 4.04
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.log.nid, data=dat.ind.pee)
library(car)
vif(xres)

mod.NRep.p <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex +setting + species + z.log.nid +
                      (1|group), family = poisson, data = dat.ind.pee, control = contr)
null.NRep.p <- glmer(formula = NRep ~  sex + z.log.nid +
                       (1|group), family = poisson, data = dat.ind.pee, control = contr)
					   
#Check for overdispersion:
overdisp.test(mod.NRep.p) # dispersion parameter 0.70 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.p), coefficients(mod.NRep.p)+
        t(apply(X=dfbeta(mod.NRep.p), MARGIN=2, FUN=range)))


summary(mod.NRep.p)
#(Intercept)  2.54599    0.25003  10.183   <2e-16 ***
#z.age        0.05148    0.09325   0.552   0.5809    
#I(z.age^2)  -0.12235    0.11594  -1.055   0.2913    
#sexM         0.04583    0.17183   0.267   0.7897    
#settingwild -0.42078    0.21266  -1.979   0.0479 *  
# speciesSum   0.43966    0.28815   1.526   0.1271    
#z.log.nid    0.33078    0.16181   2.044   0.0409 *  


# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.p)) #15
length(residuals(mod.NRep.p)) #15

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.p, mod.NRep.p, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NRep.p    4 85.59207 88.42427 -38.79604 77.59207       NA NA         NA
#mod.NRep.p     8 85.91450 91.57890 -34.95725 69.91450 7.677568  4  0.1041294

#get chi square and p-values (all)
drop1(mod.NRep.p,  test ="Chisq")
#z.age         1 84.221 0.3062 0.58001  
#I(z.age^2)    1 85.049 1.1347 0.28677  
#sex           1 83.985 0.0708 0.79023  
#setting       1 87.917 4.0028 0.04543 *
#species       1 86.215 2.3000 0.12937  
#z.log.nid     1 87.646 3.7316 0.05339 .

### Model 1d: Repertoire size w older partners

#Check collinarity: max VIF = 3.5
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid, data=dat.ind.old)
library(car)
vif(xres)

mod.NRep.a <- glmer(formula = NRep ~  z.age + I(z.age^2) + sex + setting + species + z.log.nid +
                      (1|group), family = poisson, data = dat.ind.old, control = contr)
null.NRep.a <- glmer(formula = NRep ~  sex + z.log.nid +
                       (1|group), family = poisson, data = dat.ind.old, control = contr)
					   
#Check for overdispersion:
overdisp.test(mod.NRep.a) # dispersion parameter 0.29 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.p), coefficients(mod.NRep.p)+
        t(apply(X=dfbeta(mod.NRep.p), MARGIN=2, FUN=range)))

summary(mod.NRep.a)
#(Intercept)  2.56591    0.14352  17.878  < 2e-16 ***
# z.age       -0.01847    0.08946  -0.206  0.83645    
#I(z.age^2)  -0.11701    0.14429  -0.811  0.41740    
#sexM         0.07560    0.19991   0.378  0.70532    
#settingwild -0.13196    0.25762  -0.512  0.60848    
#speciesSum   0.27653    0.22912   1.207  0.22747    
#z.log.nid    0.42377    0.13295   3.187  0.00144 ** 

# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.a)) #16
length(residuals(mod.NRep.a)) #16

#Likelihood ratio test
as.data.frame(anova(null.NRep.a, mod.NRep.a, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NRep.a    4 84.54129 87.63164 -38.27064 76.54129       NA NA         NA
#mod.NRep.a     8 90.60364 96.78435 -37.30182 74.60364 1.937654  4   0.747225

#get chi square and p-values (all)
drop1(mod.NRep.a,  test ="Chisq")
#<none>          90.604                   
#z.age         1 88.646  0.0426 0.83657   
#I(z.age^2)    1 89.260  0.6567 0.41772   
#sex           1 88.747  0.1431 0.70519   
#setting       1 88.866  0.2623 0.60857   
#species       1 90.082  1.4785 0.22401   
#z.log.nid     1 98.960 10.3561 0.00129 **


#### Model 2a: Context variety w mothers
mod.NCon.m <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex + setting + species + z.log.nid + 
                      (1|group), family = poisson, data = dat.ind.mot, control = contr)
null.NCon.m <- glmer(formula = NCon ~  sex + z.log.nid + 
                      (1|group), family = poisson, data = dat.ind.mot, control = contr)
					  
summary(mod.NCon.m)
overdisp.test(mod.NCon.m) # dispersion parameter 0.14 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.m)) #25
length(residuals(mod.NCon.m)) #25

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.m, mod.NCon.m, test="Chisq"))
#            npar       AIC       BIC    logLik deviance     Chisq Df Pr(>Chisq)
#null.NCon.m    4  93.73413  98.60963 -42.86706 85.73413        NA NA         NA
#mod.NCon.m     8 101.53244 111.28345 -42.76622 85.53244 0.2016854  4  0.9952446


#### Model 2b: Context variety w others

mod.NCon.o <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex +setting + species + z.log.nid + 
                  (1|group), family = poisson, data = dat.ind.oth, control = contr)
null.NCon.o <- glmer(formula = NCon ~ sex + z.log.nid + 
                      (1|group), family = poisson, data = dat.ind.oth, control = contr)
					  
summary(mod.NCon.o)
# Over-dispersion test
overdisp.test(mod.NCon.o) # dispersion parameter 0.43 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.o)) #21
length(residuals(mod.NCon.o)) #21

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.o, mod.NCon.o, test="Chisq"))
#            npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.o    4 84.66576 88.84385 -38.33288 76.66576       NA NA         NA
#mod.NCon.o     8 85.93734 94.29352 -34.96867 69.93734 6.728423  4   0.150955


#### Model 2c: Context variety w peers

mod.NCon.p <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex + setting + species + z.log.nid + 
                      (1|group), family = poisson, data = dat.ind.pee, control = contr)
null.NCon.p <- glmer(formula = NCon ~ sex + z.log.nid + 
                       (1|group), family = poisson, data = dat.ind.pee, control = contr)
					   
summary(mod.NCon.p)
# Over-dispersion test
overdisp.test(mod.NCon.p) # dispersion parameter 0.79 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.p)) #15
length(residuals(mod.NCon.p)) #15

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.p, mod.NCon.p, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.p    4 62.10936 64.94156 -27.05468 54.10936       NA NA         NA
#mod.NCon.p     8 65.25796 70.92236 -24.62898 49.25796 4.851392  4  0.3028882

#### Model 2d: Context variety w olders
mod.NCon.a <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex + setting + species + z.log.nid + 
                      (1|group), family = poisson, data = dat.ind.old, control = contr)
null.NCon.a <- glmer(formula = NCon ~ sex + z.log.nid + 
                       (1|group), family = poisson, data = dat.ind.old, control = contr)
					   
summary(mod.NCon.a)
# Over-dispersion test
overdisp.test(mod.NCon.a) # dispersion parameter 0.24 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.a)) #16
length(residuals(mod.NCon.a)) #16

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.a, mod.NCon.a, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.a    4 60.82104 63.91140 -26.41052 52.82104       NA NA         NA
#mod.NCon.a     8 67.04114 73.22185 -25.52057 51.04114 1.779901  4  0.7761574

source("D:/R/lm_course/glmm_stability.r")
mod.NRep.m.stab=glmm.model.stab(model.res=mod.NRep.m, contr=contr, ind.cases=F, para=T)
mod.NRep.o.stab=glmm.model.stab(model.res=mod.NRep.o, contr=contr, ind.cases=F, para=T)
mod.NCon.m.stab=glmm.model.stab(model.res=mod.NCon.m, contr=contr, ind.cases=F, para=T)
mod.NCon.o.stab=glmm.model.stab(model.res=mod.NCon.o, contr=contr, ind.cases=F, para=T)



#Plots
dat.ind.oth$sex =as.factor(dat.ind.oth$sex)
dat.ind.oth$sex.code=as.numeric(dat.ind.oth$sex==levels(dat.ind.oth$sex)[2])
sex.code.c=dat.ind.oth$sex.code-mean(dat.ind.oth$sex.code)

dat.ind.mot$sex =as.factor(dat.ind.mot$sex)
dat.ind.mot$sex.code=as.numeric(dat.ind.mot$sex==levels(dat.ind.mot$sex)[2])
sex.code.c=dat.ind.mot$sex.code-mean(dat.ind.mot$sex.code)

plot.mod.NRep.o <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.nid + (1|group),
                      data = dat.ind.oth)

plot.mod.NRep.m <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.nid  + (1|group), data = dat.ind.mot)

plot.mod.NRep.o <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.log.nid + (1|group),
                      data = dat.ind.oth)

plot.mod.NRep.m <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.log.nid  + (1|group), data = dat.ind.mot)

setwd("C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/")
source("local_jitter_AlexHausmann.R")
require(effects)



#Plot RS directed at others
dat.ind.oth$XPos <- ifelse(dat.ind.oth$species=="Bor",1,2)
EF <- Effect(c("species","setting"),plot.mod.NRep.o,se=TRUE)
dat.EF <- as.data.frame(EF)

# Add colour column
dat.ind.oth$colourBG <- ifelse(dat.ind.oth$setting=="wild",rgb(255, 210, 128, maxColorValue=255),rgb(128, 128, 255, maxColorValue=255))
dat.ind.oth$colourL <- ifelse(dat.ind.oth$setting=="wild",rgb(255, 192, 77, maxColorValue=255),rgb(77, 77, 255, maxColorValue=255))

# Open empty plot (IMPORTANT: THE PLOT HAS TO BE OPEN BEFORE RUNNING THE FUNCTION)
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"


svg(filename=paste0(path,"IndRepOth_v5.svg",sep=""), height=90/25.4, width=90/25.4, family="Arial", pointsize=9)
OF <- 0.1

par(mar=c(2.7, 3.2, 0.2, 0.2), mgp=c(1.3, 0.2, 0), tcl=-0.25, cex=1)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="Orang-utan species", ylab="", cex=1.5) ; par(new=TRUE)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="", ylab="Signal repertoire directed at others", cex=1.5, mgp=c(2.2, 0.2, 0))

X0 <-local_jitter(fact_coord = dat.ind.oth$XPos, gradual_coord = dat.ind.oth$NRep, categories = as.character(dat.ind.oth$setting), factorial_axis = 1, buffer = 0.45, sizes = sqrt(dat.ind.oth$NID)/4, verbose=F, iterations=1000)
points(X0,dat.ind.oth$NRep,cex=sqrt(dat.ind.oth$NID)/4, pch=21, bg=dat.ind.oth$colourBG, col=dat.ind.oth$colourL)

arrows(1-OF,dat.EF$lower[1],1-OF,dat.EF$upper[1],code=3,length=0.1,angle=90)
points(x=1-OF,y=dat.EF$fit[1], pch=23, col="black", bg="blue", cex=3)

arrows(1+OF,dat.EF$lower[3],1+OF,dat.EF$upper[3],code=3,length=0.1,angle=90)
points(x=1+OF,y=dat.EF$fit[3], pch=23, col="black", bg="orange", cex=3)

arrows(2-OF,dat.EF$lower[2],2-OF,dat.EF$upper[2],code=3,length=0.1,angle=90)
points(x=2-OF,y=dat.EF$fit[2], pch=23, col="black", bg="blue", cex=3)

arrows(2+OF,dat.EF$lower[4],2+OF,dat.EF$upper[4],code=3,length=0.1,angle=90)
points(x=2+OF,y=dat.EF$fit[4], pch=23, col="black", bg="orange", cex=3)

axis(1,at=c(1,2), label=c("Bornean","Sumatran"), tcl=-0.25, cex=1.5)
axis(2,at=seq(0,40,by=5), label=c("0","5","10","15","20","25", "30", "35", "40"), tcl=-0.25, las=2, mgp=c(1.2, 0.4, 0))
legend("topright", pt.bg=c("blue","orange"), pch=23, legend=c("captive","wild"), bty="n", pt.cex=2)
text(x=0.5,y=40, "B", cex=1.5)

box()
dev.off()
#border=c(rgb(77, 77, 255, maxColorValue=255),rgb(255, 192, 77, maxColorValue=255)), fill=c(rgb(128, 128, 255, maxColorValue=255), rgb(255, 210, 128, maxColorValue=255))


#Plot RS directed at mothers
dat.ind.mot$XPos <- ifelse(dat.ind.mot$species=="Bor",1,2)
EF <- Effect(c("species","setting"),plot.mod.NRep.m,se=TRUE)
dat.EF <- as.data.frame(EF)

# Add colour column
dat.ind.mot$colourBG <- ifelse(dat.ind.mot$setting=="wild",rgb(255, 210, 128, maxColorValue=255),rgb(128, 128, 255, maxColorValue=255))
dat.ind.mot$colourL <- ifelse(dat.ind.mot$setting=="wild",rgb(255, 192, 77, maxColorValue=255),rgb(77, 77, 255, maxColorValue=255))

# Open empty plot (IMPORTANT: THE PLOT HAS TO BE OPEN BEFORE RUNNING THE FUNCTION)
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"

svg(filename=paste0(path,"IndRepMot_v5.svg",sep=""), height=90/25.4, width=90/25.4, family="Arial", pointsize=9)
OF <- 0.1

par(mar=c(2.7, 3.2, 0.2, 0.2), mgp=c(1.3, 0.2, 0), tcl=-0.25, cex=1)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="Orang-utan species", ylab="", cex=1.5) ; par(new=TRUE)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="", ylab="Signal repertoire directed at mothers", mgp=c(2.2, 0.2, 0), cex=1.5)

X0 <-local_jitter(fact_coord = dat.ind.mot$XPos, gradual_coord = dat.ind.mot$NRep, categories = as.character(dat.ind.mot$setting), factorial_axis = 1, buffer = 0.45, sizes = sqrt(dat.ind.mot$NID)/4, verbose=F, iterations=1000)
points(X0,dat.ind.mot$NRep,cex=sqrt(dat.ind.mot$NID)/4, pch=21, bg=dat.ind.mot$colourBG, col=dat.ind.mot$colourL)

arrows(1-OF,dat.EF$lower[1],1-OF,dat.EF$upper[1],code=3,length=0.1,angle=90)
points(x=1-OF,y=dat.EF$fit[1], pch=23, col="black", bg="blue", cex=3)

arrows(1+OF,dat.EF$lower[3],1+OF,dat.EF$upper[3],code=3,length=0.1,angle=90)
points(x=1+OF,y=dat.EF$fit[3], pch=23, col="black", bg="orange", cex=3)

arrows(2-OF,dat.EF$lower[2],2-OF,dat.EF$upper[2],code=3,length=0.1,angle=90)
points(x=2-OF,y=dat.EF$fit[2], pch=23, col="black", bg="blue", cex=3)

arrows(2+OF,dat.EF$lower[4],2+OF,dat.EF$upper[4],code=3,length=0.1,angle=90)
points(x=2+OF,y=dat.EF$fit[4], pch=23, col="black", bg="orange", cex=3)

axis(1,at=c(1,2), label=c("Bornean","Sumatran"), tcl=-0.25)
axis(2,at=seq(0,40,by=5), label=c("0","5","10","15","20","25", "30", "35", "40"), tcl=-0.25, las=2, mgp=c(1.2, 0.4, 0))
legend("topright", pt.bg=c("blue","orange"), pch=23, legend=c("captive","wild"), bty="n", pt.cex=2)
text(x=0.5,y=40, "A", cex=1.5)

box()
dev.off()
#border=c(rgb(77, 77, 255, maxColorValue=255),rgb(255, 192, 77, maxColorValue=255)), fill=c(rgb(128, 128, 255, maxColorValue=255), rgb(255, 210, 128, maxColorValue=255))


#Plots Dice Within-Between

library(ggplot2)
theme_marlen_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 12,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 12,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour=NA),
                         axis.title.x = element_text(size = 13, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 13, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 11, family="sans", margin = margin(t = 10)),
                         legend.key = element_blank(),
                         legend.position = "right",
                         legend.spacing.x = unit(0.2, 'cm'),
                         strip.text = element_text(size = 11))

levels(dd$species.ind1) <- c("Bornean", "Sumatran")


#Single Plot (IF and IO separately)
dodge.posn <- position_dodge(.9)
#svg("/Users/mfroehlich/R/self.svg", width=90/25.4, height=90/25.4, pointsize=9,family="arial")
mod.site <- ggplot(dd, aes(x = species.ind1, y = Dice))
mod.site + geom_boxplot(aes(fill = WB), width = 0.9) +
  geom_point(aes(fill = WB),position= dodge.posn, shape = 1, colour = "black", alpha = 0.5) +
  theme_marlen_ss +
  scale_y_continuous("Similarity of mother-directed repertoires", limits = c(0, 1)) +
  scale_x_discrete("Orang-utan species",
                   limits = c("Bor", "Sum"),
                   labels = c("Bornean", "Sumatran"))+
  scale_fill_manual(values=c("gray", "white"),name="Setting",
                    breaks=c("Between","Within"),
                    labels=c("Between","Within"))+
  # facet_wrap(~species.ind1)+
  stat_summary(fun=mean, geom="point",shape =23, fill ="black",aes(group=WB), position=position_dodge(.9), 
               color="black", size=3)


#Combined Plot?
dodge.posn <- position_dodge(.9)
#svg("/Users/mfroehlich/R/self.svg", width=90/25.4, height=90/25.4, pointsize=9,family="arial")
mod.site <- ggplot(dice, aes(x = Partner, y = Dice))
mod.site + geom_boxplot(aes(fill = WB), width = 0.9) +
  geom_point(aes(fill = WB),position= dodge.posn, shape = 1, colour = "black", alpha = 0.5) +
  theme_marlen_ss +
  scale_y_continuous("Repertoire similarity (Dice coefficients)") +
  scale_x_discrete("Interaction Partner",
                   limits = c("Mothers", "Others"),
                   labels = c("Mother-directed", "Other-directed"))+
  scale_fill_manual(values=c("blue", "orange"),name="Setting",
                    breaks=c("Between","Within"),
                    labels=c("Between","Within"))+
  facet_wrap(~species.ind1)+
  stat_summary(fun=mean, geom="point",shape =23, fill ="black",aes(group=WB), position=position_dodge(.9), 
               color="black", size=3)



write.table(dat,file ="D:/R/MS Orang Infants/dat_IO.csv", sep=",", col.names=TRUE, row.names=FALSE)
