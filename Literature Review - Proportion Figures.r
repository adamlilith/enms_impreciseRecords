# install.packages('gsheet')
library(gsheet)

data<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1Gy5SBPsJFsxIiXeHD--a6Z_30HoXJbpVUJTHLL4j6Nc/edit?ts=5e1a393a#gid=993073646')
data<-data[-1,]
data$`Used museum or herbarium data`<-as.numeric(data$`Used museum or herbarium data`)
data$`Removed Imprecise Records`<-as.numeric(data$`Removed Imprecise Records`)
data$`Needs cleaned?`<-as.numeric(data$`Needs cleaned?`)
data$`Cleaning Methods Described`<-as.numeric(data$`Cleaning Methods Described`)

# Relevant Articles -------------------------------------------------------

data.relevant<-data[which(data$`Used museum or herbarium data`==1),]
needs.cleaned<-data[which(data$`Needs cleaned?`==1),]

# proportion change figure -------------------------------------------------------------------

locality.uncertainty<-needs.cleaned$`Removed Imprecise Records`
cleaning.described<-needs.cleaned$`Cleaning Methods Described`

variable<-locality.uncertainty

vals<-sample(variable)

plot(cumsum(vals)/1:length(vals), type="l", ylim=c(0,1), xlab="Number of Articles", ylab="Proportion")

nsim<-99

prop.vals<-vector(length=nsim, mode="list")

for(i in 1:nsim){
  vals<-sample(variable)
  prop.vals[[i]]<-cumsum(vals)/1:length(vals)
}

lapply(prop.vals, lines)

lines(Reduce('+', prop.vals)/nsim, col="red", lwd=3)

### plot SD of proportion of articles with cleaning methods described
data$`Cleaning Methods Described` <- as.numeric(data$`Cleaning Methods Described`)
data$`Cleaning Methods Described` <- as.numeric(data$`Removed Imprecise Records`)
relevant <- data[!is.na(data$`Cleaning Methods Described`), ]

se <- rep(NA, nrow(relevant) - 1)
for (n in 1:(nrow(relevant) - 1)) se[n] <- sd(relevant$`Cleaning Methods Described`[1:(n + 1)]) / sqrt(n + 1)

plot(2:nrow(relevant), se, type='l', xlab='Cumulative articles with cleaning methods described', ylab='SE of proportion of articles with cleaning methods described', ylim=c(0, max(se)))
abline(h=0.05, lty='dotted')
abline(h=0.01, lty='dotted')

### proportion of articles describing methods that remove records based on uncertainty

print(paste('Number of articles reporting cleaning methods:', sum(relevant$`Cleaning Methods Described` == 1)))
print(paste('Number of articles discarding uncertain records:', sum(relevant$`Removed Imprecise Records` == 1, na.rm=TRUE)))

se <- rep(NA, nrow(relevant) - 1)
for (n in 1:(nrow(relevant) - 1)) se[n] <- sd(relevant$`Removed Imprecise Records`[1:(n + 1)]) / sqrt(n + 1)

plot(2:nrow(relevant), se, type='l', xlab='Cumulative articles with cleaning methods described', ylab='SE of proportion of articles discarding uncertain records', ylim=c(0, max(se)))
abline(h=0.05, lty='dotted')
abline(h=0.01, lty='dotted')


### plot proportion of articles in each "cleaning" class
data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Gy5SBPsJFsxIiXeHD--a6Z_30HoXJbpVUJTHLL4j6Nc/edit?ts=5e1a393a#gid=993073646')

data <- as.data.frame(data)
data <- data[-1, ]

data$needsCleaned <- as.integer(data$needsCleaned)
data$cleaningDescribed <- as.integer(data$cleaningDescribed)
data$removedErroneousRecords <- as.integer(data$removedErroneousRecords)
data$removedUnnaturalRecords <- as.integer(data$removedUnnaturalRecords)
data$removedImpreciseRecords <- as.integer(data$removedImpreciseRecords)
data$removedGeogOutliers <- as.integer(data$removedGeogOutliers)
data$removedEnvOutliers <- as.integer(data$removedEnvOutliers)

needsCleaned <- data[!is.na(data$needsCleaned), ]
needsCleaned <- needsCleaned[needsCleaned$needsCleaned == 1, ]

x <- c(
	cleaningDescribed = sum(needsCleaned$cleaningDescribed, na.rm=TRUE),
	removedErroneousRecords = sum(needsCleaned$removedErroneousRecords, na.rm=TRUE),
	removedUnnaturalRecords = sum(needsCleaned$removedUnnaturalRecords, na.rm=TRUE),
	removedImpreciseRecords = sum(needsCleaned$removedImpreciseRecords, na.rm=TRUE),
	removedGeogOutliers = sum(needsCleaned$removedGeogOutliers, na.rm=TRUE),
	removedEnvOutliers = sum(needsCleaned$removedEnvOutliers, na.rm=TRUE)
)

x <- 100 * x / nrow(needsCleaned)

barplot(x)


	
	
	

