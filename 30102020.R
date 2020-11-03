#MB12 lecture of 30.10.2020
#Energy production across Europe per Power Unit

df <- read.csv("day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")
df

unique(df)
#get all the variables in this column
unique(df$V12)
#how often does each variable come up?
table(df$V12)

df
table(df$V14,df$V12)

#group the data on the basis of one value
#aggregate(var1,by=list(category),FUN=sum)
output <- aggregate(df$ActualGenerationOutput, by=list(df$PowerSystemResourceName), FUN=mean)
plot(output$Group.1, output$x)

capacity <- aggregate(df$InstalledGenCapacity, by=list(df$Day), FUN=mean)
capacity
barplot(capacity[,2],1)
barplot(capacity$x, capacity$Group.1)



#jacob live coding

summary(df)
dim(df)
colnames(df)

#categorical variables

unique(df$MapCode)#first exception: germany 4 times, netherlands with 3 letters
unique(df$GenerationUnitEIC)
length(unique(df$GenerationUnitEIC))

#actual vs. installed capacity
plot(x=df$InstalledGenCapacity, y=df$ActualGenerationOutput) #not nice plot as we have outliers
max(df$ActualGenerationOutput, na.rm=T)
df[which.max(df$ActualGenerationOutput),] #where in my dataframe is lying the outlier? its a water reservoir in Romania -maybe faulty?
df <- df[-which.max(df$ActualGenerationOutput),] #removing this value
plot(x=df$InstalledGenCapacity, y=df$ActualGenerationOutput) #again plotting, still one outlier
df <- df[-which.max(df$ActualGenerationOutput),]#again removing the max value

plot(x=df$InstalledGenCapacity, y=df$ActualGenerationOutput) #now it should be a nice plot

#create counts of Production Typ

counts <- table(df$ProductionTypeName)

#aggregate Generation by day
prod_by_day <- aggregate(df$ActualGenerationOutput,
                         by=list(Day=df$Day),
                         FUN=sum,
                         na.rm=T)
prod_by_day$x <- prod_by_day$x *0.001 #gigawatt
plot(x=prod_by_day$Day, y=prod_by_day$x)

