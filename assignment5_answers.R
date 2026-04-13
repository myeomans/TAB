################################################
#
#        Text Analysis for Business
#
#           Assignment 5 Answers
#
#
################################################



# Run these every time
library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(politeness)
library(sentimentr)
library(gemini.R)

source("vectorFunctions.R")
vecSmall<-readRDS("data/vecSmall.RDS")
source("TAB_dfm.R")
source("kendall_acc.R")

ecMain<-readRDS("data/earningsDat_3Y.RDS") 

ecQA<-readRDS("data/earningsQandA_3Y.RDS")%>%
  mutate(wordcount=str_count(text,"[[:alpha:]]+")) %>%
  filter(callID%in%ecMain$callID)


################################################
# Question 1
################################################

ecQA %>%
  filter(asker==1) %>%
  group_by(callID,askerID) %>%
  summarize(qcount=n()) %>%
  group_by(askerID) %>%
  summarize(m=mean(qcount),
            se=sd(qcount)/sqrt(n())) %>%
  mutate(askerID=as.numeric(askerID)) %>%
  filter(askerID<20) %>%
  ggplot(aes(x=askerID,y=m,
             ymin=m-se,ymax=m+se)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(x="Asker Order",y="Number of Questions Asked")


################################################
# Question 2
################################################

# Join turn-level data (from the first ten questions) to the call-level data
ecMain_merged <- ecMain %>%
  # combine answer text as a single document and merge
  left_join(ecQA %>%
              filter(asker==0 & question <=10) %>%
              group_by(callID) %>%
              summarize(answertext=paste(text,collapse=" "),
                        answer_wdct=str_count(answertext,"[[:alpha:]]+"))) %>%
  # combine question text as a single document and merge
  left_join(ecQA %>%
              filter(asker==1 & question <=10) %>%
              group_by(callID) %>%
              summarize(questiontext=paste(text,collapse=" "),
                        question_wdct=str_count(questiontext,"[[:alpha:]]+")))

ecMain_train<-ecMain_merged %>%
  filter(FY==2011)

ecMain_test<-ecMain_merged %>%
  filter(FY==2012)

# Questions First

ecMain_train_dfm_q<-TAB_dfm(ecMain_train$questiontext)

EPSmodel_q<-cv.glmnet(x=as.matrix(ecMain_train_dfm_q),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_q)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_q %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_q),
                       freq=colMeans(ecMain_train_dfm_q)))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 20,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Question")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))

########## Answers next

ecMain_train_dfm_a<-TAB_dfm(ecMain_train$answertext)

EPSmodel_a<-cv.glmnet(x=as.matrix(ecMain_train_dfm_a),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_a)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_a %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_a),
                       freq=colMeans(ecMain_train_dfm_a)))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 20,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Answer")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))



########## Combined

ecMain_train_dfm_qx<-ecMain_train_dfm_q
colnames(ecMain_train_dfm_qx)<-paste0(colnames(ecMain_train_dfm_qx),"XXX")

ecMain_train_dfm_c<-cbind(ecMain_train_dfm_a,
                          ecMain_train_dfm_qx)

EPSmodel_c<-cv.glmnet(x=as.matrix(ecMain_train_dfm_c),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_c)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_c %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_c),
                       freq=colMeans(ecMain_train_dfm_c))) %>%
  mutate(q_feature=1*grepl("XXX",ngram),
         fname=gsub("XXX","",ngram,fixed=T))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=fname,color=q_feature)) +
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 50,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Q-A Pair")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))

################################################
# Question 3
################################################

# Questions first

ecMain_test_dfm_q<-TAB_dfm(ecMain_test$questiontext,
                              min.prop=0) %>%
  dfm_match(colnames(ecMain_train_dfm_q)) %>%
  as.matrix()

test_EPS_predict_q<-predict(EPSmodel_q,
                            newx=ecMain_test_dfm_q,
                            s="lambda.min")

kendall_acc(test_EPS_predict_q,
            ecMain_test$EPS_actual)


# Answers next

ecMain_test_dfm_a<-TAB_dfm(ecMain_test$answertext,
                              min.prop=0) %>%
  dfm_match(colnames(ecMain_train_dfm_a))

test_EPS_predict_a<-predict(EPSmodel_a,
                            newx=ecMain_test_dfm_a,
                            s="lambda.min")

kendall_acc(test_EPS_predict_a,
            ecMain_test$EPS_actual)

# Combined features

ecMain_test_dfm_qx<-ecMain_test_dfm_q
colnames(ecMain_test_dfm_qx)<-paste0(colnames(ecMain_test_dfm_qx),"XXX")

ecMain_test_dfm_c<-cbind(ecMain_test_dfm_a,
                         ecMain_test_dfm_qx)

test_EPS_predict_c<-predict(EPSmodel_c,
                            newx=ecMain_test_dfm_c,
                            s="lambda.min")

kendall_acc(test_EPS_predict_c,
            ecMain_test$EPS_actual)


################################################
# Question 4
################################################


QApairs=ecQA %>%
  left_join(ecMain %>%
              select(callID,FY,FQ,EPS_actual,EPS_consens)) %>%
  filter(followup==0 & FY==2010 & FQ==1) %>%
  mutate(qnum=question,
         asker=ifelse(asker==1,"question","answer")) %>%
  group_by(callID,qnum,asker) %>%
  summarize(text=paste(text,collapse=" "),
            EPS_consens=first(EPS_consens),
            EPS_actual=first(EPS_actual)) %>%
  pivot_wider(names_from="asker",values_from="text")%>%
  ungroup() %>%
  slice(1:2000)

##### 4a - gemini labels

gemini.R::setAPI("AIzaSyA1kcHjek4VXVDJQy_SSE2NZQkG-6rahOc")

# something with gemini
task<-"I am going to give you a question from a quarterly earnings call. 
  I want you to tell me if the question is easy or hard to answer.
  Use a numeric 1 to 9 scale, where 1 indicates very easy, and 9 indicates very hard. 
  Higher numbers indicate higher difficulty of the question.
  Only respond with a single-digit number from 1 to 9.
  
  Here is the question:   
  "
QApairs$hard_guess<-NA
for(q in 1:nrow(QApairs)){
  
  response<-gemini_chat(paste(task,QApairs[q,]$question),
                        model = "2.0-flash-lite")
  QApairs[q,]$hard_guess<-response$outputs
  print(q)
}

QApairs$hard_guess <- as.numeric(gsub("\n","",QApairs$hard_guess,fixed=T))

hist(QApairs$hard_guess)

saveRDS(QApairs,file="QApairs.RDS")
##### 4b - question features

QApairs$sim=NaN
tpb=txtProgressBar(0,nrow(QApairs))
for(x in 1:nrow(QApairs)){
  QApairs[x,]$sim=vecSimCalc(x=QApairs[x,]$question,
                             y=QApairs[x,]$answer,
                             vecfile=vecSmall)
  setTxtProgressBar(tpb,x)
}

QApairs$Qsent=sentiment_by(QApairs$question) %>%
  pull(ave_sentiment)
QApairs$Asent=sentiment_by(QApairs$answer) %>%
  pull(ave_sentiment)


Qngram<-TAB_dfm(QApairs$question)
Angram<-TAB_dfm(QApairs$answer)


saveRDS(QApairs,file="QApairs.RDS")

Qmodel<-cv.glmnet(Qngram,
                  QApairs$hard_guess)

Amodel<-cv.glmnet(Angram,
                  QApairs$hard_guess)

modelPlot(Qmodel,Qngram) +
  ylab("Uses per Question")

modelPlot(Amodel,Angram) +
  ylab("Uses per Answer")




# Run these every time
library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(politeness)
library(sentimentr)
library(gemini.R)

source("vectorFunctions.R")
vecSmall<-readRDS("data/vecSmall.RDS")
source("TAB_dfm.R")
source("kendall_acc.R")

ecMain<-readRDS("data/earningsDat_3Y.RDS") 

ecQA<-readRDS("data/earningsQandA_3Y.RDS")%>%
  mutate(wordcount=str_count(text,"[[:alpha:]]+")) %>%
  filter(callID%in%ecMain$callID)


################################################
# Question 1
################################################

ecQA %>%
  filter(asker==1) %>%
  group_by(callID,askerID) %>%
  summarize(qcount=n()) %>%
  group_by(askerID) %>%
  summarize(m=mean(qcount),
            se=sd(qcount)/sqrt(n())) %>%
  mutate(askerID=as.numeric(askerID)) %>%
  filter(askerID<20) %>%
  ggplot(aes(x=askerID,y=m,
             ymin=m-se,ymax=m+se)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(x="Asker Order",y="Number of Questions Asked")


################################################
# Question 2
################################################

# Join turn-level data (from the first ten questions) to the call-level data
ecMain_merged <- ecMain %>%
  # combine answer text as a single document and merge
  left_join(ecQA %>%
              filter(asker==0 & question <=10) %>%
              group_by(callID) %>%
              summarize(answertext=paste(text,collapse=" "),
                        answer_wdct=str_count(answertext,"[[:alpha:]]+"))) %>%
  # combine question text as a single document and merge
  left_join(ecQA %>%
              filter(asker==1 & question <=10) %>%
              group_by(callID) %>%
              summarize(questiontext=paste(text,collapse=" "),
                        question_wdct=str_count(questiontext,"[[:alpha:]]+")))

ecMain_train<-ecMain_merged %>%
  filter(FY==2011)

ecMain_test<-ecMain_merged %>%
  filter(FY==2012)

# Questions First

ecMain_train_dfm_q<-TAB_dfm(ecMain_train$questiontext)

EPSmodel_q<-cv.glmnet(x=as.matrix(ecMain_train_dfm_q),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_q)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_q %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_q),
                       freq=colMeans(ecMain_train_dfm_q)))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 20,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Question")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))

########## Answers next

ecMain_train_dfm_a<-TAB_dfm(ecMain_train$answertext)

EPSmodel_a<-cv.glmnet(x=as.matrix(ecMain_train_dfm_a),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_a)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_a %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_a),
                       freq=colMeans(ecMain_train_dfm_a)))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 20,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Answer")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))



########## Combined

ecMain_train_dfm_qx<-ecMain_train_dfm_q
colnames(ecMain_train_dfm_qx)<-paste0(colnames(ecMain_train_dfm_qx),"XXX")

ecMain_train_dfm_c<-cbind(ecMain_train_dfm_a,
                          ecMain_train_dfm_qx)

EPSmodel_c<-cv.glmnet(x=as.matrix(ecMain_train_dfm_c),
                      y=(ecMain_train$EPS_actual))

plot(EPSmodel_c)

#### Interpret with a coefficient plot
plotDat<-EPSmodel_c %>%
  coef(s="lambda.min") %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(ecMain_train_dfm_c),
                       freq=colMeans(ecMain_train_dfm_c))) %>%
  mutate(q_feature=1*grepl("XXX",ngram),
         fname=gsub("XXX","",ngram,fixed=T))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=fname,color=q_feature)) +
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 50,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Q-A Pair")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))

################################################
# Question 3
################################################

# Questions first

ecMain_test_dfm_q<-TAB_dfm(ecMain_test$questiontext,
                              min.prop=0) %>%
  dfm_match(colnames(ecMain_train_dfm_q)) %>%
  as.matrix()

test_EPS_predict_q<-predict(EPSmodel_q,
                            newx=ecMain_test_dfm_q,
                            s="lambda.min")

kendall_acc(test_EPS_predict_q,
            ecMain_test$EPS_actual)


# Answers next

ecMain_test_dfm_a<-TAB_dfm(ecMain_test$answertext,
                              min.prop=0) %>%
  dfm_match(colnames(ecMain_train_dfm_a))

test_EPS_predict_a<-predict(EPSmodel_a,
                            newx=ecMain_test_dfm_a,
                            s="lambda.min")

kendall_acc(test_EPS_predict_a,
            ecMain_test$EPS_actual)

# Combined features

ecMain_test_dfm_qx<-ecMain_test_dfm_q
colnames(ecMain_test_dfm_qx)<-paste0(colnames(ecMain_test_dfm_qx),"XXX")

ecMain_test_dfm_c<-cbind(ecMain_test_dfm_a,
                         ecMain_test_dfm_qx)

test_EPS_predict_c<-predict(EPSmodel_c,
                            newx=ecMain_test_dfm_c,
                            s="lambda.min")

kendall_acc(test_EPS_predict_c,
            ecMain_test$EPS_actual)


################################################
# Question 4
################################################


QApairs=ecQA %>%
  left_join(ecMain %>%
              select(callID,FY,FQ,EPS_actual,EPS_consens)) %>%
  filter(followup==0 & FY==2010 & FQ==1) %>%
  mutate(qnum=question,
         asker=ifelse(asker==1,"question","answer")) %>%
  group_by(callID,qnum,asker) %>%
  summarize(text=paste(text,collapse=" "),
            EPS_consens=first(EPS_consens),
            EPS_actual=first(EPS_actual)) %>%
  pivot_wider(names_from="asker",values_from="text")%>%
  ungroup() %>%
  slice(1:2000)

##### 4a - gemini labels

gemini.R::setAPI("AIzaSyA1kcHjek4VXVDJQy_SSE2NZQkG-6rahOc")

# something with gemini
task<-"I am going to give you a question from a quarterly earnings call. 
  I want you to tell me if the question is easy or hard to answer.
  Use a numeric 1 to 9 scale, where 1 indicates very easy, and 9 indicates very hard. 
  Higher numbers indicate higher difficulty of the question.
  Only respond with a single-digit number from 1 to 9.
  
  Here is the question:   
  "
QApairs$hard_guess<-NA
for(q in 1:nrow(QApairs)){
  
  response<-gemini_chat(paste(task,QApairs[q,]$question),
                        model = "2.0-flash-lite")
  QApairs[q,]$hard_guess<-response$outputs
  print(q)
}

QApairs$hard_guess <- as.numeric(gsub("\n","",QApairs$hard_guess,fixed=T))

hist(QApairs$hard_guess)

saveRDS(QApairs,file="QApairs.RDS")
##### 4b - question features

QApairs$sim=NaN
tpb=txtProgressBar(0,nrow(QApairs))
for(x in 1:nrow(QApairs)){
  QApairs[x,]$sim=vecSimCalc(x=QApairs[x,]$question,
                             y=QApairs[x,]$answer,
                             vecfile=vecSmall)
  setTxtProgressBar(tpb,x)
}

QApairs$Qsent=sentiment_by(QApairs$question) %>%
  pull(ave_sentiment)
QApairs$Asent=sentiment_by(QApairs$answer) %>%
  pull(ave_sentiment)


Qngram<-TAB_dfm(QApairs$question)
Angram<-TAB_dfm(QApairs$answer)


saveRDS(QApairs,file="QApairs.RDS")

Qmodel<-cv.glmnet(Qngram,
                  QApairs$hard_guess)

Amodel<-cv.glmnet(Angram,
                  QApairs$hard_guess)

modelPlot(Qmodel,Qngram) +
  ylab("Uses per Question")

modelPlot(Amodel,Angram) +
  ylab("Uses per Answer")

