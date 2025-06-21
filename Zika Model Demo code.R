#Zika Prediction project Demo:
  
 #Building the ModelDataset:

Data1=read.csv("Zika Cases.csv",skip=0, header=TRUE, stringsAsFactors=FALSE)
Data1$IncidenceRate=(Data1$Cases/Data1$Population)*100000
Data1=mutate(Data1,IncidenceRate = round(Data1$IncidenceRate, digits=2))
Data1a=aggregate(Data1$IncidenceRate, by=list(FDF=Data1$FDF,Country=Data1$CountryandTerritory),FUN=max)
Data1b=reshape(Data1a[,c("FDF","Country", "x")], timevar="FDF", idvar="Country", times="x", direction="wide")
ZikaIncidence=plyr::rename(Data1b,c("x.Deaths"="Deaths","x.Suspected"="Suspected","x.Confirmed"="ZikaIncidenceRate"))
Data3=read.csv("Dengue 2014.csv",skip=0, header=TRUE, stringsAsFactors=FALSE)
Data3a=plyr::rename(Data3,c("IncidenceRateper100000" = "DengueIncidenceRate"))
DengueIncidence=mutate(Data3a,GeographicalLocation = ifelse(Geo.Loc!="NLC", ifelse(Geo.Loc!="LCR","notcaribbean","caribbean"),"caribbean"))
Data2=read.csv("Health Expenditure by GDP.csv",skip = 4,header = TRUE,stringsAsFactors=FALSE )
Data2a=Data2[,c("Country","X2014")]
Data2b=plyr::rename(Data2a,c("X2014" = "Expenditure2014"))
Data2c=Data2b[complete.cases(Data2b),]
HealthExpenditure=Data2c[complete.cases(Data2c),]
ZikaandHealthExpenditure=inner_join(ZikaIncidence,HealthExpenditure)
ModelDataset=inner_join(ZikaIncidence,DengueIncidence, by="Country")

#Plotting Variables and calculationg correlations:

ggplot(ZikaIncidence,aes(Country,ZikaIncidenceRate))+geom_col(fill="blue")+ggtitle("Zika Incidence by Country")+theme_bw()+theme(axis.text.x=element_text(angle = -90, hjust = 0))

ggplot(ModelDataset,aes(DengueIncidenceRate,ZikaIncidenceRate))+geom_point(size=5)+ggtitle("Zika Incidence vs Dengue Incidence")+theme(axis.text.x=element_text(angle = -90, hjust = 0))+
theme(plot.title = element_text(size = 22, face = "bold"))+scale_x_log10() + scale_y_log10()+geom_smooth(method = "lm", se = FALSE)
cor(ModelDataset$DengueIncidenceRate,ModelDataset$ZikaIncidenceRate)

ggplot(ModelDataset,aes(GeographicalLocation,ZikaIncidenceRate, fill=GeographicalLocation))+geom_boxplot()+ggtitle("Zika Incidence by Geo Location")+theme(axis.text.x=element_text(angle = -90, hjust = 0))+ 
theme(plot.title = element_text(size = 22, face = "bold"))+scale_y_log10()

ggplot(ModelDataset,aes(DengueIncidenceRate,ZikaIncidenceRate, color=GeographicalLocation))+geom_point(size=5)+ggtitle("Zika Incidence vs Dengue Incidence and Geo Location")+theme(axis.text.x=element_text(angle = -90, hjust = 0))+
theme(plot.title = element_text(size = 22, face = "bold"))+scale_x_log10() + scale_y_log10()+geom_smooth(method = "lm", se = FALSE)

ggplot(ZikaandHealthExpenditure,aes(Expenditure2014,ZikaIncidenceRate,color=Country))+geom_point()+ theme(plot.title = element_text(size = 22, face = "bold"))+scale_x_log10() + scale_y_log10()+ggtitle("Zika Incidence vs Health Expenditure")
cor(ZikaandHealthExpenditure$ZikaIncidenceRate,ZikaandHealthExpenditure$Expenditure2014)

#Building the Zika Prediction Model
set.seed(39)
train_control <- trainControl(method="repeatedcv", number=2, repeats=10)
m1 <- train(ZikaIncidenceRate~DengueIncidenceRate:GeographicalLocation, data=ModelDataset, trControl=train_control, method="rf")
m2 <- train(ZikaIncidenceRate~DengueIncidenceRate:GeographicalLocation, data=ModelDataset, trControl=train_control, method="brnn")
m3 <- train(ZikaIncidenceRate~DengueIncidenceRate:GeographicalLocation, data=ModelDataset, trControl=train_control, method="lm")


allModels=resamples(list(RandomForest=m1,NeuralNet=m2, LinearModel=m3))

bwplot(allModels,scales=list(relation="free"), group=allModels,par.settings = list( box.umbrella=list(col= c("red", "green", "blue")), 
box.dot=list(col= c("red", "green", "blue")),box.rectangle = list(col= c("red", "green", "blue"))))



#-------------------------------------------------------------------------------------------------------
#Add median values to box plot
LogRateData=mutate(ModelDataset,ZikaIncidenceRate = log10(ModelDataset$ZikaIncidenceRate))
LogRateData=mutate(LogRateData,ZikaIncidenceRate = round(LogRateData$ZikaIncidenceRate, digits=2))
median <- aggregate(ZikaIncidenceRate~GeographicalLocation, LogRateData, median)

ggplot(ModelDataset,aes(GeographicalLocation,ZikaIncidenceRate, fill=GeographicalLocation))+geom_boxplot()+ggtitle("Zika Incidence by Geographical Location")+theme(axis.text.x=element_text(angle = -90, hjust = 0))+ 
theme(plot.title = element_text(size = 22, face = "bold"))+scale_y_log10()+ geom_text(data = median, aes(label = ZikaIncidenceRate, y = ZikaIncidenceRate+5))
