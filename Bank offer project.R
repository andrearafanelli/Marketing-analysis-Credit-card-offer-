library(plyr)
data <- read.delim('Creditcard Marketing.txt',sep=',')
data<-data[!duplicated(data$Customer.Number),] #no duplicati ID
summary(is.na(data)) #24 clienti senza average balance in quarter
data<-data[-1] #ID 
data$Average.Balance<-as.numeric(data$Average.Balance)
#REORDER FACTORs

data$Credit.Rating<-factor(data$Credit.Rating,ordered = TRUE,levels=c('Low','Medium','High'))
data$Income.Level<-factor(data$Income.Level,ordered = TRUE,levels=c('Low','Medium','High'))

##########
###############################
#EDA
library(ggplot2)
data2 <- subset(data, Offer.Accepted == 1) #solo clienti che hanno accettato 

#Y:
ggplot(data, aes(Offer.Accepted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='paleturquoise1')+
  labs(y='Frequency',x='Acceptance of offer')+
  theme_minimal()+
  ggtitle('Frequency of offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#INCOME LEVEL:
#tutto il dataset
ggplot(data,aes(x=Income.Level))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='sienna2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Income Level')+
  ggtitle('Distribution of income \n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Income.Level))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='sienna2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Income Level')+
  ggtitle('Distribution of inco
          me')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#PROPERTY OF HOUSE 
ggplot(data,aes(x=Own.Your.Home))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='green2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Own home')+
  ggtitle('Distribution of having own home\n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Own.Your.Home))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='green2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Own home')+
  ggtitle('Distribution of having own home')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#MAILER TYPE
ggplot(data,aes(x=Mailer.Type))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='firebrick2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Mailer type')+
  ggtitle('Distribution of mailer type\n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Mailer.Type))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='firebrick2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Mailer type')+
  ggtitle('Distribution of mailer type')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#OVERDRAFT PROTECTION
ggplot(data,aes(x=Overdraft.Protection))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='gold2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Ovedraft Protection')+
  ggtitle('Distribution of having overdraft protection\n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Overdraft.Protection))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='gold2')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Ovedraft Protection')+
  ggtitle('Distribution of having overdraft protection')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#REWARD OFFERED
ggplot(data,aes(x=Reward))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='turquoise3')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Reward offered')+
  ggtitle('Distribution of offered reward \n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Reward))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='turquoise3')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Reward offered')+
  ggtitle('Distribution of credit card rewards')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#CREDIT RATING
ggplot(data,aes(x=Credit.Rating))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='red3')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Credit Rating')+
  ggtitle('Distribution of credit rating \n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2,aes(x=Credit.Rating))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position = position_dodge(preserve = "single"),colour='black',fill='red3')+
  facet_wrap(~Offer.Accepted)+
  theme_minimal()+
  labs(fill='Offered accepted',y='Frequency',x='Credit Rating')+
  ggtitle('Distribution of credit rating')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#NUMBER OF INDIVIDUALS:
ggplot(data,aes(x=Offer.Accepted,fill=Offer.Accepted,y=Household.Size))+
  geom_boxplot()+
  theme_minimal()+
  labs(fill='Offer acceptance',y='Number of individuals in family',x='Offer acceptance')+
  ggtitle('Distribution of offer acceptance\n by household size')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2, aes(x = Household.Size))+
  geom_histogram(bins = 10, color = "black", fill = "steelblue2")+
  labs(y='Count',x='Household size')+
  ggtitle('Number of individuals in family')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

#NUMBER OF CREDIT CARDS
ggplot(data,aes(x=Offer.Accepted,fill=Offer.Accepted,y=X..Credit.Cards.Held))+
  geom_boxplot(fill='coral1')+
  theme_minimal()+
  labs(fill='Offer acceptance',y='Number of credit cards',x='Offer acceptance')+
  ggtitle('Distribution of offer acceptance\n by number of credit cards owned')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2, aes(x = X..Credit.Cards.Held))+
  geom_histogram(bins = 5, color = "black", fill = "coral1")+
  theme_minimal()+
  labs(y='Count',x='Number of credit cards')+
  ggtitle('Number of credit cards')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

#NUMBER BANK ACCOUNTS -> non serve a niente 
ggplot(data,aes(x=Offer.Accepted,fill=Offer.Accepted,y=X..Homes.Owned ))+
  geom_boxplot(fill="palegreen1")+
  theme_minimal()+
  labs(y='Count',x='Number of non-credit bank accounts')+
  ggtitle('Number of non-credit bank accounts \n by offer acceptance')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))
#
ggplot(data2, aes(x = X..Homes.Owned))+
  geom_histogram(bins = 5, color = "black", fill = "palegreen1")+
  labs(y='Count',x='Number of non-credit bank accounts')+
  ggtitle('Number of non-credit bank accounts ')+
  theme(plot.title = element_text(hjust = 0.5,face='bold.italic'))

#AVG BALANCE
hist(data2$Average.Balance,main="Average balance",
     col='yellow')

############ CHECK CORRELATIONS
corr_data<-na.omit(data)
corr<-cor(corr_data[,c(5,8,9,10,12,13,14,15,16)])

library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
#SI NOTA UNA CORRELAZIONE TRA Q3-Q4,Q3-Q2,Q2-Q1 IN PIU' HANNO MISSING VALUES 
#AVG BALANCE E' LA MEDIA DI QUESTI -> TOLGO QUESTI
data<-data[-c(13:16)] #AVG BALANCE

############# CLUSTERING (CI METTE ORE A CALCOLARE LA MATRICE DELLE DISTANZE)
library(StatMatch)

distanza=gower.dist(data,KR.corr=T) #USO GOWER PERCHE' VARIABILI MISTE 

# forzo l'oggetto a essere una distanza: 
distanza=as.dist(distanza)

hierarchical=hclust(distanza,method="ward.D2")#WARD
 
plot(hierarchical,hang=-1)
rect.hclust(hierarchical, 4)

cluster_hie=cutree(hierarchical,k=4)
table(cluster_hie)


data$cluster_hie=cluster_hie
cluster1=data[which(data$cluster_hie==1),]
cluster2=data[which(data$cluster_hie==2),]
cluster3=data[which(data$cluster_hie==3),]
cluster4=data[which(data$cluster_hie==4),]

table(cluster1$Offer.Accepted)
table(cluster2$Offer.Accepted)
table(cluster3$Offer.Accepted)
table(cluster4$Offer.Accepted)
data<-data[,-13]
#######CLUSTER INFO
plot(cluster1$Income.Level,main="Income Level",
     col='yellow')
plot(cluster2$Income.Level,main="Income Level",
     col='yellow')
plot(cluster3$Income.Level,main="Income Level",
     col='yellow')
plot(cluster4$Income.Level,main="Income Level",
     col='yellow')

plot(cluster1$Overdraft.Protection,main="Overdraft protection",
     col='pink')
plot(cluster2$Overdraft.Protection,main="Overdraft protection",
     col='pink')
plot(cluster3$Overdraft.Protection,main="Overdraft protection",
     col='pink')
plot(cluster4$Overdraft.Protection,main="Overdraft protection",
     col='pink')

hist(cluster1$Household.Size,main="Household size",
     col='blue')
hist(cluster2$Household.Size,main="Household size",
     col='blue')
hist(cluster3$Household.Size,main="Household size",
     col='blue')
hist(cluster4$Household.Size,main="Household size",
     col='blue')

plot(cluster1$Own.Your.Home,main="Own home",
     col='yellow')
plot(cluster2$Own.Your.Home,main="Own home",
     col='yellow')
plot(cluster3$Own.Your.Home,main="Own home",
     col='yellow')
plot(cluster4$Own.Your.Home,main="Own home",
     col='yellow')

plot(cluster1$Reward,main="Reward offered",
     col='red')
plot(cluster2$Reward,main="Reward offered",
     col='red')
plot(cluster3$Reward,main="Reward offered",
     col='red')
plot(cluster4$Reward,main="Reward offered",
     col='red')

plot(cluster1$Credit.Rating,main="Credit rating",
     col='orange')
plot(cluster2$Credit.Rating,main="Credit rating",
     col='orange')
plot(cluster3$Credit.Rating,main="Credit rating",
     col='orange')
plot(cluster4$Credit.Rating,main="Credit rating",
     col='orange')

plot(cluster1$Mailer.Type,main="Mailer type",
     col='lightblue')
plot(cluster2$Mailer.Type,main="Mailer type",
     col='lightblue')
plot(cluster3$Mailer.Type,main="Mailer type",
     col='lightblue')
plot(cluster4$Mailer.Type,main="Mailer type",
     col='lightblue')



hist(cluster1$X..Credit.Cards.Held,main="Average balance",
     col='yellow')
hist(cluster2$X..Credit.Cards.Held,main="Average balance",
     col='yellow')
hist(cluster3$X..Credit.Cards.Held,main="Average balance",
     col='yellow')
hist(cluster4$X..Credit.Cards.Held,main="Average balance",
     col='yellow')

#####################
################### TRAIN TEST SPLIT
library(caTools)
set.seed(123)
split = sample.split(data, SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
prop.table(table(train$Offer.Accepted)) #Mantenuta stessa proporzione
prop.table(table(train$Offer.Accepted))

############# RESAMPLING THE TRAIN SET-> IMBALANCED DATA 
library(ROSE)

train_balanced <- ovun.sample(Offer.Accepted~ ., train, method = "both", p=0.5, seed = 1)$data

###########PREDICTIVE MODELS:

#TREE
library(tree)

model_tree<-tree(Offer.Accepted~.,train_balanced,method='Gini')
set.seed(123)
cv_tree<-cv.tree(model_tree,FUN=prune.misclass)
plot(cv_tree$dev~cv_tree$size,type="b",
                                  xlab = 'Size',ylab = 'Deviance')
best <- cv_tree$size[which(cv_tree$dev==min(cv_tree$dev))]
best
prune<-prune.misclass(model_tree,best=4)#NOT TO COMPLEX
summary(prune)
plot(prune)
text(prune,pretty=0)
#Prediction
pred_tree<-predict(prune,test,type="class")

table(pred_tree,test$Offer.Accepted)


## ------------------------------------------------------------------------


#STEPWISE

glm.null=glm(Offer.Accepted~1,data=train_balanced,family='binomial')
glm.full=glm(Offer.Accepted~.,data=train_balanced,family ='binomial')
summary(glm.null)
summary(glm.full)

glm_for=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
             direction='forward',trace=2)

glm_back=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
              direction='backward',
              trace=2)
glm_both=step(glm.null,scope=list(lower=formula(glm.null),upper=formula(glm.full)),
              direction='both',
              trace=0)
#### BEST SUBSET 
library(bestglm)
train_balanced$y=train_balanced$Offer.Accepted #legge la y se è l'ultima colonna
train_balanced<-train_balanced[,-1] #tolgo offer accepted che adesso è come ultima colonna
glm_best=bestglm(train_balanced,
                 IC="AIC", 
                 nvmax = 11,
                 family = binomial)
summary(glm_best$BestModel)
# AIC SIMILE TRA FORWARD, BOTHWISE, BEST: USO LA BEST CHE MI GARANTISCE LA SCELTA DEL MIGLIOR MODELLO 

##########GLM
glm1<-glm(y ~ Credit.Rating + Mailer.Type + Reward + Income.Level + 
            Household.Size + X..Homes.Owned + Overdraft.Protection + 
            Average.Balance,data=train_balanced,family=binomial)
colnames(test)[1]='y'
glm1_pred<-predict(glm1,test,type=c('response'))


####ROC

library(pROC)

g <- roc( test$y~ glm1_pred,algorithm=1)
plot(g) 
auc(g)#75%
#THRESHOLD
matplot(data.frame(g$sensitivities, g$specificities), x = g$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

#CONFUSION MATRIX AND INDECES
library(InformationValue)
optCutOff= 0.5 #default threshold
confusionMatrix(test$y, glm1_pred, threshold = optCutOff)

optCutOff= 0.4 #try 0.4
confusionMatrix(test$y, glm1_pred, threshold = optCutOff)