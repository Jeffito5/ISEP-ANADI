#### FUNTIONS ####

# Normalizacao dos dados
minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Desnormalizar os dados para avaliar o modelo
minmaxdesnorm <- function (x,goal.attrib){
  return (x*(max(goal.attrib)-min(goal.attrib))+min(goal.attrib))
}

# Erro medio absoluto
mae <- function(test, predicted) {
  mean(abs(test - predicted))
}

# Root Mean Square Error
rmse <- function(test, predicted){
  sqrt(mean((test-predicted)^2))
}

#### LIBRARIES ####
#instalar as livrarias necess?rias
if(!require('corrplot')){
  install.packages('corrplot')
  library(corrplot)}
if(!require('fastDummies')){
  install.packages('fastDummies')
  library(fastDummies)}
if(!require('plyr')){
  install.packages('plyr')
  library(plyr)}
if(!require('rpart')){
  install.packages('rpart')
  library(rpart)}
if(!require('rpart.plot')){
  install.packages('rpart.plot')
  library(rpart.plot)}
if(!require('neuralnet')){
  install.packages('neuralnet')
  library(neuralnet)}
if(!require('NeuralNetTools')){
  install.packages('NeuralNetTools')
  library(NeuralNetTools)}
if(!require('FNN')){
  install.packages('FNN')
  library(FNN)}
if(!require('class')){
  install.packages('class')
  library(class)}


################################################################################
# 8- Estude a capacidade preditiva relativamente ao atributo Ativo usando os seguintes métodos: ####

# 8a) árvore de decisão: ####

set.seed(123)

# Obter o index do set de testes e de treino para ser possivel validar o modelo
# train/test partitions- HOLDOUT
index <- sample(1:nrow(dataCliWithAll), as.integer(0.7 * nrow(dataCliWithAll)))
obj.train <- dataCliWithAll[index, ]
obj.tst <- dataCliWithAll[-index, ]

arvore <- rpart(Ativo ~. , data = obj.train, method="class")
#plot tree
par(xpd = TRUE)
plot(arvore, compress = TRUE);
text(arvore, use.n = TRUE)

prevs.modelo <- predict(arvore, obj.tst, type='class')

head(prevs.modelo)

# Matriz de confusao
cfmatrix <- table(obj.tst$Ativo, prevs.modelo)
cfmatrix

# Accuracy
#80.66351%
accuracy8a <- sum(diag(cfmatrix))/sum(cfmatrix)

# 8b) rede neuronal: ####

data.norm <- as.data.frame(apply(dataCliWithAll, 2, minmaxnorm))

# Holdout
index <- sample(1:nrow(data.norm), 0.7*nrow(data.norm))
data.train <- data.norm[index, ]
data.tst <- data.norm[-index, ]

set.seed(739) 

numnodes2<-c(12, 6, 3)

nn.model3 <- neuralnet(Ativo ~ ., data = data.train, hidden = numnodes2)

names(nn.model3)

# Sumário dos resultados
nn.model3$net.result
temp_test <- data.frame(data.tst)
temp_test$Ativo <- NULL
nn.predAtivo <- compute(nn.model3, temp_test)

nn.pred.ativo <- minmaxdesnorm(nn.predAtivo$net.result, dataCliWithAll$Ativo)
test.ativo <- minmaxdesnorm(data.tst$Ativo, dataCliWithAll$Ativo)

rmse(nn.pred.ativo, test.ativo) 

nn.pred.ativo <- sapply(nn.pred.ativo,round,digits=0)

nn.pred.ativo <- as.integer(nn.pred.ativo)
nn.pred.ativo <- nn.pred.ativo +1 

# Matriz de confusao
cfmatrix <- table(data.tst$Ativo, nn.pred.ativo)
# Accuracy
# 64.07%
accuracy8b <- sum(diag(cfmatrix))/sum(cfmatrix)

#________________________________________________________________________________________
# Alernativa do calculo da nn

# minmaxnorm <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# data.norm <- as.data.frame(lapply(dataCliWithAll, minmaxnorm))
# set.seed(123)
# index <- sample(1:nrow(data.norm), 0.7*nrow(data.norm))
# data.train <- data.norm[index, ]
# data.tst <- data.norm[-index, ]
# library(neuralnet)
# nn <- neuralnet(Ativo ~ Género + Maior65 + Colaborador + Dependentes + Fidelização + TipoServiço + FaturaEletronica + TarifaMensal + TotalTarifas + LinhasMultNão + LinhasMultSemServTelef + LinhasMultSim + ServNetDSL + ServNetFibra + ServNetNão + SegurOnlineNão + SegurOnlineSemServNet + SegurOnlineSim + CopiaDeSegurOnlineNão + CopiaDeSegurOnlineSemServNet + CopiaDeSegurOnlineSim + ProteçãoTMNão + ProteçãoTMSemServNet + ProteçãoTMSim + SupTécnicoNão + SupTécnicoSemServNet + SupTécnicoSim + ServStreamingTVNão + ServStreamingTVSemServNet + ServStreamingTVSim + ServStreamingFilmeNão + ServStreamingFilmeSemServNet + ServStreamingFilmeSim + TipoDeContratoAnual + TipoDeContratoBiAnual + TipoDeContratoMensal + PagamentoCartãoDeCrédito + PagamentoChequeElet + PagamentoChequePorEmail + PagamentoTransBancária , data=data.train, hidden=1, stepmax=1e7)
# # nn$result.matrix
# # plot(nn)
# temp_test <- data.frame(data.tst)
# temp_test$Ativo <- NULL
# # head(temp_test)
# nn.results <- compute(nn, temp_test)
# results <- data.frame(actual = data.tst$Ativo, prediction = nn.results$net.result)
# roundedresults<-sapply(results,round,digits=0)
# roundedresultsdf=data.frame(roundedresults)
# table(roundedresultsdf$actual,roundedresultsdf$prediction)

#________________________________________________________________________________________
# 8c) K-vizinhos-mais-próximos: ####

# Holdout
data.norm <- as.data.frame(lapply(dataCliWithAll, minmaxnorm))
set.seed(123)
index2 <- sample(1:nrow(data.norm), 0.7*nrow(data.norm))
ncols <- dim(data.norm)[2]
data.trainKNN <- data.norm[index2, -10] #Matriz (ou data frame) de exemplos de treino
data.tstKNN <- data.norm[-index2, -10] #Matriz (ou data frame) de exemplos de teste
train.ativoKNN <- data.norm[index2, 10] #Vetor com as respostas de cada observação do conjunto de treino
tst.ativoKNN <- data.norm[-index2, 10]

k <- c()
rmse9 <- c()

# mínimo

for (i in seq(1, 50, 2)){
  #K-NN para Regressão
  knnreg.pred <- knn.reg(data.trainKNN, data.tstKNN, train.ativoKNN, k=i)
  knnreg.pred$pred <- minmaxdesnorm(knnreg.pred$pred, dataCliWithAll$Ativo)
  mmd <- minmaxdesnorm(tst.ativoKNN, dataCliWithAll$Ativo)
  rmse9 <- c(rmse9, rmse(knnreg.pred$pred, mmd))
  k <- c(k, i)
}

cat("k min: ", k[which.min(rmse9)])
plot(k, rmse9)
## k min: 45

set.seed(123)
k <- c()
accuracy <- c()

# máximo

for (i in seq(1, 50, 2)){ 
  knn.pred <- knn(train=data.trainKNN, test=data.tstKNN, cl= train.ativoKNN, k=i) 
  m.conf <- table(tst.ativoKNN,knn.pred)
  accuracy <- c(accuracy, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i) 
}
cat("k max: ", k[which.max(accuracy)])
plot(k, accuracy)
## k max: 47

resNeigh<-data.frame(k,accuracy)
#procurar o K com maior taxa de acerto
#Max Accuracy- 80.09479%
resNeigh[resNeigh$accuracy==max(resNeigh$accuracy),]

################################################################################

# 9- Usando o método k-fold cross validation obtenha a média e o desvio padrão da taxa  ####
# de acerto da previsão do atributo Ativo com os dois melhores modelos obtidos na alínea anterior

# k-fold cross validation ####
# Nomalizamos os dados globais
data.9.norm <- as.data.frame(apply(dataCliWithAll,2,minmaxnorm))

kf.9 <- 10
folds.9 <- sample(1:kf.9, nrow(data.9.norm), replace=TRUE)

# Criamos uma matriz kf linhas e 2 colunas (2 modelos)
cv.error.9 <- matrix(nrow = kf.9, ncol=2)

# Obtemos o K max do exercicio anterior
k.9<-k[which.max(accuracy)]

for(j in 1:kf.9)
{
  train.9.cv <- data.9.norm[folds.9 != j, ]
  test.9.cv <- data.9.norm[folds.9 == j,]
  
  train.9.ativo <- dataCliWithAll[folds.9 != j, 10]
  tst.9.ativo <- dataCliWithAll[folds.9 == j, 10]
  
  
  knn.9.pred <- knn(train=train.9.cv[,-10], test=test.9.cv[,-10], cl=train.9.ativo, k = k.9)
  m.9.conf <- table(tst.9.ativo, knn.9.pred)
  
  rpart.9.model <- rpart(Ativo ~., method="class", data=train.9.cv)
  rpart.9.pred <- predict(rpart.9.model, test.9.cv, type="class")
  m.9.conf2 <- table(tst.9.ativo, rpart.9.pred)
  
  cv.error.9[j, ] <- c(sum(diag(m.9.conf))/sum(m.9.conf), sum(diag(m.9.conf2))/sum(m.9.conf2))
}

# Obtemos a nossa matriz ####
cv.error.9

# Obtemos a média ####
apply(cv.error.9, 2, mean)
# Obtemos o desvio padrão ####
apply(cv.error.9, 2, sd)

################################################################################

# 10- Verifique se existe diferença significativa no desempenho dos dois melhores  ####
# modelos obtidos anteriormente (use um nível de significância de 5%). Identifique o
# modelo que apresenta o melhor desempenho

# Observamos pelo exercício anterior são a árvore de regressão e os K-vizinhos-mais-próximos. 
# Contudo, iremos comprovar estatisticamente através dos testes: t.test e wilcox

# Criamos um dataframe para identificarmos as colunas
data.10 <- data.frame(cv.error.9)
colnames(data.10)[1] <- "KNN"
colnames(data.10)[2] <- "RegTree"

# H0: modelos são significativamente iguais VS H1: modelos são significativamente diferentes
# alfa = 5%
# p-value = 0.5255
# Como o p-value é superior a alfa não rejeitamos a hipótese nula. Ou seja, os modelos são significativamente iguais 
t.test(data.10$KNN, data.10$RegTree)

# H0: modelos são significativamente iguais VS H1: modelos são significativamente diferentes
# alfa = 5%
# p-value = 0.5966
# Como o p-value é superior a alfa não rejeitamos a hipótese nula. Ou seja, os modelos são significativamente iguais 
wilcox.test(data.10$KNN, data.10$RegTree)

# Boxplot com os dados ####
boxplot(data.10$KNN, data.10$RegTree, names = c("KNN", "RegTree"))

# Concluímos que a árvore de regressão e os K-vizinhos-mais-próximos são significativamente
# iguais. Para isso, utlizamos um boxplot e verificamos que a técnica da árvore de decisão
# tem, ligeiramente, um melhor desempenho com uma média de accuracy um pouco superior

################################################################################

# 11- Compare os resultados dos modelos. Discuta em detalhe qual o modelo que apresentou  ####
# melhor e pior desempenho de acordo com os critérios: Accuracy; Sensitivity; Specificity e F1

# Medidas de desempenho
# • Accuracy: taxa de exemplos acertados (do total de exemplos)
# • Taxa de erro: taxa de exemplos errados (do total de exemplos)
# • Precision: taxa de exemplos positivos classificados corretamente, entre todos os preditos como positivos
# • Recall (sensibilidade): taxa de acerto na classe positiva (do total de positivos, quantos foram detetados);
# • F1: média harmónica da precision e do recall com o objetivo de dar uma medida única que valorize de igual modo os erros cometidos em qualquer dos sentidos (FP ou FN).

# https://towardsdatascience.com/accuracy-recall-precision-f-score-specificity-which-to-optimize-on-867d3f11124

# True positive (TP)
# True negative (TN)
# False positive (FP)
# False negative (FN)

# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Precision = TP/(TP+FP)
# Recall = TP/(TP+FN)
# Specificity = TN/(TN+FP)
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)

# Calculo da Accuracy, Recall, Specificity e F1 para a Arvore de decisão 8a)

# Avaliação do resultado médio de 10 experiências
accuracy = numeric()
recall <- numeric()
precision <- numeric()
specificity <- numeric()

accuracy_rede = numeric()
recall_rede <- numeric()
precision_rede <- numeric()
specificity_rede <- numeric()

accuracy_k = numeric()
recall_k <- numeric()
precision_k <- numeric()
specificity_k <- numeric()

for (i in 1:10) {
  amostra<-sample(1:nrow(dataCliWithAll), 0.7*nrow(dataCliWithAll))
  dados.treino <- dataCliWithAll[amostra, ]
  dados.teste <- dataCliWithAll[-amostra, ]
  #________________________________________________________________________________________
  # Arvore de decisão
  arvore <- rpart(Ativo ~ ., data = dados.treino, method="class")
  prevs.modelo <- predict(arvore, dados.teste, type='class')
  m.conf<-table(dados.teste$Ativo,prevs.modelo)
  TP = m.conf[1,1]
  TN = m.conf[2,2]
  FP = m.conf[2,1]
  FN = m.conf[1,2]
  accuracy[i] = (TP+TN)/sum(TP, TN, FP, FN)
  recall[i] = TP/(TP+FN)
  precision[i] = TP/(TP+FP)
  specificity[i] = TN/(TN+FP)
  #________________________________________________________________________________________
  # Rede neuronal
  nn <- neuralnet(Ativo ~ ., data=dados.treino, hidden=1, stepmax=1e7)
  temp_test <- data.frame(dados.teste)
  temp_test$Ativo <- NULL
  nn.results <- compute(nn, temp_test)
  results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred.ativo)
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdf=data.frame(roundedresults)
  m.conf_rede <- table(roundedresultsdf$actual,roundedresultsdf$prediction)
  TP = m.conf_rede[1,1]
  TN = m.conf_rede[2,2]
  FP = m.conf_rede[2,1]
  FN = m.conf_rede[1,2]
  accuracy_rede[i] = (TP+TN)/sum(TP, TN, FP, FN)
  recall_rede[i] = TP/(TP+FN)
  precision_rede[i] = TP/(TP+FP)
  specificity_rede[i] = TN/(TN+FP)
  #________________________________________________________________________________________
  # K-vizinhos-mais-próximos
  knn.pred <- knn(train=data.trainKNN, test=data.tstKNN, cl= train.ativoKNN, k=i) 
  m.conf_k <- table(tst.ativoKNN,knn.pred)
  TP = m.conf_k[1,1]
  TN = m.conf_k[2,2]
  FP = m.conf_k[2,1]
  FN = m.conf_k[1,2]
  accuracy_k[i] = (TP+TN)/sum(TP, TN, FP, FN)
  recall_k[i] = TP/(TP+FN)
  precision_k[i] = TP/(TP+FP)
  specificity_k[i] = TN/(TN+FP)
  #________________________________________________________________________________________
  
}
f1=(2*round(mean(precision),3)*round(sd(recall),3))/(round(mean(precision),3)+round(sd(recall),3))
f1_rede=(2*round(mean(precision_rede),3)*round(sd(recall_rede),3))/(round(mean(precision_rede),3)+round(sd(recall_rede),3))
f1_k=(2*round(mean(precision_k),3)*round(sd(recall_k),3))/(round(mean(precision_k),3)+round(sd(recall_k),3))

# Calculo da Accuracy, Recall, Specificity e F1 para a arvore de decisão 8a)
cat(paste("taxa de acerto media:", 100*round(mean(accuracy),4), "%, desvio:", round(sd(accuracy),4)))
cat(paste("recall:", round(mean(recall),3), "%, desvio:", round(sd(recall),3)))
cat(paste("specificity:", round(mean(specificity),3), "%, desvio:", round(sd(specificity),3)))
cat(paste("F1: ",f1)) 
# Calculo da Accuracy, Recall, Specificity e F1 para a rede neuronal 8b)
cat(paste("taxa de acerto media rede:", 100*round(mean(accuracy_rede),4), "%, desvio:", round(sd(accuracy_rede),4)))
cat(paste("recall rede:", round(mean(recall_rede),3), "%, desvio:", round(sd(recall_rede),3)))
cat(paste("specificity rede:", round(mean(specificity_rede),3), "%, desvio:", round(sd(specificity_rede),3)))
cat(paste("F1 rede: ",f1_rede)) 
# Calculo da Accuracy, Recall, Specificity e F1 para a K-vizinhos-mais-próximos 8c)
cat(paste("taxa de acerto media k:", 100*round(mean(accuracy_k),4), "%, desvio:", round(sd(accuracy_k),4)))
cat(paste("recall k:", round(mean(recall_k),3), "%, desvio:", round(sd(recall_k),3)))
cat(paste("specificity k:", round(mean(specificity_k),3), "%, desvio:", round(sd(specificity_k),3)))
cat(paste("F1 k: ",f1_k)) 

