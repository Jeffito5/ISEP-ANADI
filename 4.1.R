#### FUNTIONS

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

### LIBRARIES
# instalar as livrarias necess?rias
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

################################################################################

# 1- Leitura, tamanho e sumario do ficheiro

Clientes_DataSet <- read.csv("~/R/Trabalho2/Clientes_DataSet.csv", header=TRUE)

file.info("~/R/Trabalho2/Clientes_DataSet.csv")$size
dim(Clientes_DataSet)

summary(Clientes_DataSet)

################################################################################

# 2- Pre-processamento dos dados

# 2a) Faca a identificao de NA e limpe o dataSet 
Clientes_DataSet <- na.omit(Clientes_DataSet)

# 2b) Identifique dados inconsistentes e outliers 
Clientes_DataSet <- Clientes_DataSet[(Clientes_DataSet$Fidelização>=0&Clientes_DataSet$TarifaMensal>=0&Clientes_DataSet$TotalTarifas>=0), ]
outliersTotalTarifas = boxplot(as.numeric(Clientes_DataSet$TotalTarifas))$out
outliersTarifaMensal = boxplot(as.numeric(Clientes_DataSet$TarifaMensal))$out
outliersFidelização = boxplot(as.numeric(Clientes_DataSet$Fidelização))$out

# 2c) Implemente a selecao de atributos, se aplicavel 
Clientes_DataSet <- subset(Clientes_DataSet, select = -c(ClienteID))

################################################################################

# 3- Crie um diagrama de correlacao entre todos os atributos e comente o que se observa

# colocar os dados num novo data.frame
dataCli <- Clientes_DataSet

# alterar os dados que apenas tem apenas 2 opcoes de resposta para 0 e 1
dataCli$Genero <- revalue(dataCli$Genero, c("Feminino" = 1))
dataCli$Genero <- revalue(dataCli$Genero, c("Masculino" = 0))

dataCli$Colaborador <- revalue(dataCli$Colaborador, c("Sim" = 1))
dataCli$Colaborador <- revalue(dataCli$Colaborador, c("Não" = 0))

dataCli$Dependentes <- revalue(dataCli$Dependentes, c("Sim" = 1))
dataCli$Dependentes <- revalue(dataCli$Dependentes, c("Não" = 0))

dataCli$TipoServiço <- revalue(dataCli$TipoServiço, c("Sim" = 1))
dataCli$TipoServiço <- revalue(dataCli$TipoServiço, c("Não" = 0))

dataCli$FaturaEletronica <- revalue(dataCli$FaturaEletronica, c("Sim" = 1))
dataCli$FaturaEletronica <- revalue(dataCli$FaturaEletronica, c("Não" = 0))

dataCli$Ativo <- revalue(dataCli$Ativo, c("Sim" = 1))
dataCli$Ativo <- revalue(dataCli$Ativo, c("Não" = 0))

# colocar os dados com mais de 2 opcoes num data.frame auxiliar para se utilizar a 
# livraria "fastDummies" com a funcao "dummy_cols"
dataCliAux <- data.frame(dataCli$LinhasMultiplas, dataCli$ServiçoInternet, dataCli$SegurançaOnline
                         , dataCli$CópiadeSegurançaOnline, dataCli$ProteçãoTM, dataCli$SuporteTécnico
                         , dataCli$ServiçoStreamingTV, dataCli$ServiçoStreamingFilmes
                         , dataCli$TipodeContrato, dataCli$MétododePagamento)

# colocar todos os dados com mais de 2 opcoes de resposta numa variavel auxiliar 
# para no fim se juntar aos dados iniciais so com 2 tipos de resposta
dataCliDummy<-dummy_cols(dataCliAux)

# data.frame com os dados com 2 opcoes e os dados dummy
dataCliWithAll <- data.frame(dataCli$Genero, dataCli$Maior65, dataCli$Colaborador, dataCli$Dependentes
                             , dataCli$Fidelização, dataCli$TipoServiço, dataCli$FaturaEletronica, 
                             dataCli$TarifaMensal, dataCli$TotalTarifas, dataCli$Ativo,
                             dataCliDummy)

# eliminacao das colunas utilizadas para obter os dados dummy (colunas com coisas escritas)
dataCliWithAll <- subset(dataCliWithAll, select = -c(dataCli.LinhasMultiplas, 
                                                     dataCli.ServiçoInternet, dataCli.SegurançaOnline
                                                     , dataCli.CópiadeSegurançaOnline
                                                     , dataCli.ProteçãoTM, dataCli.SuporteTécnico
                                                     , dataCli.ServiçoStreamingTV, dataCli.ServiçoStreamingFilmes
                                                     , dataCli.TipodeContrato, dataCli.MétododePagamento))
 
# colocacao das variaveis no tipo inteiro 
dataCliWithAll$dataCli.Genero = as.integer(dataCliWithAll$dataCli.Genero)
dataCliWithAll$dataCli.Colaborador = as.integer(dataCliWithAll$dataCli.Colaborador)
dataCliWithAll$dataCli.Dependentes = as.integer(dataCliWithAll$dataCli.Dependentes)
dataCliWithAll$dataCli.TipoServiço = as.integer(dataCliWithAll$dataCli.TipoServiço)
dataCliWithAll$dataCli.FaturaEletronica = as.integer(dataCliWithAll$dataCli.FaturaEletronica)
dataCliWithAll$dataCli.Ativo = as.integer(dataCliWithAll$dataCli.Ativo)

# alteracao do nome das colunas
colnames(dataCliWithAll)[1] <- "Género"
colnames(dataCliWithAll)[2] <- "Maior65"
colnames(dataCliWithAll)[3] <- "Colaborador"
colnames(dataCliWithAll)[4] <- "Dependentes"
colnames(dataCliWithAll)[5] <- "Fidelização"
colnames(dataCliWithAll)[6] <- "TipoServiço"
colnames(dataCliWithAll)[7] <- "FaturaEletronica"
colnames(dataCliWithAll)[8] <- "TarifaMensal"
colnames(dataCliWithAll)[9] <- "TotalTarifas"
colnames(dataCliWithAll)[10] <- "Ativo"
colnames(dataCliWithAll)[11] <- "LinhasMultNão"
colnames(dataCliWithAll)[12] <- "LinhasMultSemServTelef"
colnames(dataCliWithAll)[13] <- "LinhasMultSim"
colnames(dataCliWithAll)[14] <- "ServNetDSL"
colnames(dataCliWithAll)[15] <- "ServNetFibra"
colnames(dataCliWithAll)[16] <- "ServNetNão"
colnames(dataCliWithAll)[17] <- "SegurOnlineNão"
colnames(dataCliWithAll)[18] <- "SegurOnlineSemServNet"
colnames(dataCliWithAll)[19] <- "SegurOnlineSim"
colnames(dataCliWithAll)[20] <- "CopiaDeSegurOnlineNão"
colnames(dataCliWithAll)[21] <- "CopiaDeSegurOnlineSemServNet"
colnames(dataCliWithAll)[22] <- "CopiaDeSegurOnlineSim"
colnames(dataCliWithAll)[23] <- "ProteçãoTMNão"
colnames(dataCliWithAll)[24] <- "ProteçãoTMSemServNet"
colnames(dataCliWithAll)[25] <- "ProteçãoTMSim"
colnames(dataCliWithAll)[26] <- "SupTécnicoNão"
colnames(dataCliWithAll)[27] <- "SupTécnicoSemServNet"
colnames(dataCliWithAll)[28] <- "SupTécnicoSim"
colnames(dataCliWithAll)[29] <- "ServStreamingTVNão"
colnames(dataCliWithAll)[30] <- "ServStreamingTVSemServNet"
colnames(dataCliWithAll)[31] <- "ServStreamingTVSim"
colnames(dataCliWithAll)[32] <- "ServStreamingFilmeNão"
colnames(dataCliWithAll)[33] <- "ServStreamingFilmeSemServNet"
colnames(dataCliWithAll)[34] <- "ServStreamingFilmeSim"
colnames(dataCliWithAll)[35] <- "TipoDeContratoAnual"
colnames(dataCliWithAll)[36] <- "TipoDeContratoBiAnual"
colnames(dataCliWithAll)[37] <- "TipoDeContratoMensal"
colnames(dataCliWithAll)[38] <- "PagamentoCartãoDeCrédito"
colnames(dataCliWithAll)[39] <- "PagamentoChequeElet"
colnames(dataCliWithAll)[40] <- "PagamentoChequePorEmail"
colnames(dataCliWithAll)[41] <- "PagamentoTransBancária"

# matriz com a correlacao entre cada atributo
auxClienteCorr <- cor(dataCliWithAll)

# diagrama de correlacao
corrplot::corrplot(auxClienteCorr, method = "number", tl.cex = 0.4, number.cex = 0.44, col = "black")

################################################################################

# 4- Obtenha um modelo de regressao linear simples para a variavel objetivo para determinar 
# o periodo de Fidelizacao usando a tarifa mensal (TarifaMensal) 

# 4a) Apresente a funcaoo linear resultante 
set.seed(123)

# Obter o index do set de testes e de treino para ser possivel validar o modelo
# train/test partitions- HOLDOUT
index <- sample(1:nrow(dataCliWithAll), as.integer(0.7 * nrow(dataCliWithAll)))
obj.train <- dataCliWithAll[index, ]
obj.tst <- dataCliWithAll[-index, ]

obj.lm=lm(Fidelização~TarifaMensal, data=obj.train)

obj.lm

# 4b) Visualize a reta correspondente ao modelo de regressao linear simples e o respetivo diagrama de dispersao. 
plot(obj.train$Fidelização,obj.train$TarifaMensal, pch=20)
abline(obj.lm$coefficients[1],obj.lm$coefficients[2], col='red')

# 4c) Calcule o erro medio absoluto (MAE) e raiz quadrada do erro medio (RMSE) do modelo 
# sobre os 30% casos de teste. 
obj.pred <- predict(obj.lm,obj.tst)
d <- obj.tst$Fidelização - obj.pred #diferenca entre os valores aproximados e reais

# medida de avaliacao - MAE (Mean absolute error) 
mae1 <- mean(abs(d))
# medida de avaliacao - RMSE (Root Mean Squared Error) 
rmse1 <- sqrt(mean(d^2))

################################################################################

# 5- Tendo em conta o conjunto de dados apresentado, pretende-se prever o TotalTarifas, aplicando: 

# Matriz de resultados para mae e rmse de cada ponto do ex5 ####
tabelaResultado.5 <- matrix(nrow=3, ncol=2)

# 5a) Regressao linear multipla

set.seed(123)

# Obter o index do set de testes e de treino para ser possivel validar o modelo
# train/test partitions- HOLDOUT
index.5a <- sample(1:nrow(dataCliWithAll), as.integer(0.7 * nrow(dataCliWithAll)))
obj.train.5a <- dataCliWithAll[index.5a, ]
obj.tst.5a <- dataCliWithAll[-index.5a, ]

# regressao linar multipla
lmTotalTarifas.5a <- lm(TotalTarifas ~ ., data = obj.train.5a)

# sumario dos dados
summary(lmTotalTarifas.5a)

# sumario dos coeficientes
summary(lmTotalTarifas.5a)$coefficients

lmTotalTarifas.5a

# previsão do modelo
previsoes.modelo.reg.multipla.5a <- predict(lmTotalTarifas.5a, obj.tst.5a)

previsoes.modelo.reg.multipla.5a

# medida de avaliacao - MAE (Mean absolute error) 
#d2 <- obj.tst$TotalTarifas-previsoes.modelo.reg.multipla
#mae2 <- mean(abs(d2))
mae.5a <- mae(obj.tst.5a$TotalTarifas, previsoes.modelo.reg.multipla.5a)

#medida de avaliacao - RMSE (Root Mean Squared Error) 
#rmse2 <- sqrt(mean(d2^2))
rmse.5a <- rmse(obj.tst.5a$TotalTarifas, previsoes.modelo.reg.multipla.5a)

tabelaResultado.5[1,] <- c(mae.5a, rmse.5a)

# 5b) Arvore de regressao, usando a funcao rpart. Apresente a arvore de regressao obtida

set.seed(123)

# Obter o index do set de testes e de treino para ser possivel validar o modelo
# train/test partitions- HOLDOUT
index.5b <- sample(1:nrow(dataCliWithAll), as.integer(0.7 * nrow(dataCliWithAll)))
obj.train.5b <- dataCliWithAll[index.5b, ]
obj.tst.5b <- dataCliWithAll[-index.5b, ]

rpart.model.5b <-rpart(TotalTarifas ~. , method="anova", data = obj.train.5b)
rpart.model.5b
rpart.plot(rpart.model.5b, digits = 3)
rpart.pred.5b <- predict(rpart.model.5b, obj.tst.5b)

# medida de avaliacao - MAE (Mean absolute error) 
#d3 <- rpart.pred - obj.tst$TotalTarifas
#mae3 <- mean(abs(d3))
mae.5b <- mae(obj.tst.5b$TotalTarifas, rpart.pred.5b)

# medida de avaliacao - RMSE (Root Mean Squared Error) 
#rmse3 <- sqrt(mean(d3^2))
rmse.5b <- rmse(obj.tst.5b$TotalTarifas, rpart.pred.5b)

tabelaResultado.5[2,] <- c(mae.5b, rmse.5b)

# 5c) Rede neuronal usando a funcao neuralnet, fazendo variar os parametros. 
# Apresente a rede obtida 

index.5c <- sample(1:nrow(dataCliWithAll), as.integer(0.7 * nrow(dataCliWithAll)))
data.norm.5c <- as.data.frame(apply(dataCliWithAll,2,minmaxnorm))
data.train.norm.5c <- data.norm.5c[index.5c,]
data.test.norm.5c <- data.norm.5c[-index.5c,]

set.seed(739)

numnodes.5c <- 1
# Foi experimentado utilizar outro valor para a variável numnodes.5c. Verificou-se que,
# apesar do valor do mae nao ter diferido muito para apenas 1 no, o valor do rmse 
# baixou significativamente. Contudo, o grupo optou por utilizar 1 no
# numnodes.5c <- c(12,6,3)

#experimentar por exemplo com 2 nós, 2 níveis internos, etc
nn.model.5c <- neuralnet(TotalTarifas ~ ., data = data.train.norm.5c, hidden = numnodes.5c)

names(nn.model.5c) 

# Sumario dos resultados
nn.model.5c$result.matrix

plotnet(nn.model.5c, alpha.val = 0.6, circle_cex = 0.8, cex_val = 0.55, rel_rsc = 0.3, circle_col = list('purple', 'white', 'white'), bord_col = 'black', pos_col = "red")

temp_test.5c <- data.test.norm.5c
temp_test.5c$TotalTarifas <- NULL
nn.pred1.5c <- compute(nn.model.5c, temp_test.5c)

nn.pred.tt.5c <- minmaxdesnorm(nn.pred1.5c$net.result, dataCliWithAll$TotalTarifas)
test.tt.5c <- minmaxdesnorm(data.test.norm.5c$TotalTarifas, dataCliWithAll$TotalTarifas)

#d.5c <- nn.pred.tt.5c - data.test.norm.5c$TotalTarifas

# medida de avaliacao - MAE (Mean absolute error) 
#mae.5c <- mean(abs(d.5c))
mae.5c <- mae(data.test.norm.5c$TotalTarifas, nn.pred.tt.5c)

# medida de avaliacao - RMSE (Root Mean Squared Error)
rmse.5c <- rmse(nn.pred.tt.5c, test.tt.5c) 

tabelaResultado.5[3,] <- c(mae.5c, rmse.5c)

# TABELA DE RESULTADOS ex5 ####
apply(tabelaResultado.5,2,mean)
apply(tabelaResultado.5,2,sd)
resultados.5 <- data.frame(tabelaResultado.5)
colnames(resultados.5)[1] <- "MAE"
colnames(resultados.5)[2] <- "RMSE"
rownames(resultados.5)[1] <- "Regressão múltipla"
rownames(resultados.5)[2] <- "Árvore de regressão"
rownames(resultados.5)[3] <- "Rede Neuronal" 

resultados.5
################################################################################

# 6- Compare os resultados obtidos pelos modelos referidos na questao 5, usando o erro
# medio absoluto (MAE) e raiz quadrada do erro medio (RMSE)

set.seed(450)

#numnodes<-c(12,6,3)
numnodes <- 1

data <- as.data.frame(apply(dataCliWithAll,2,minmaxnorm))

k <- 10 #Testar e verificar se 
folds <- sample(1:k, nrow(data), replace=TRUE)
table(folds)

cv.error <- matrix(nrow=k, ncol=3)

for(i in 1:k){
  set.seed(k)
  
  train.cv <- data[folds != i, ]
  test.cv <- data[folds == i, ]

  # 5C nn.model
  nn.model2 <- neuralnet(TotalTarifas ~., data=train.cv, hidden = numnodes)
  temp_test2 <- data.frame(test.cv)
  temp_test2$TotalTarifas <- NULL
  nnet.pred2 <- compute(nn.model2, temp_test2)
  nnet.pred2 <- minmaxdesnorm(nnet.pred2$net.result, dataCliWithAll$TotalTarifas)
  
  # 5A lm
  mlr.model <- lm(TotalTarifas ~ ., data=train.cv)
  mlr.pred <- predict(mlr.model, test.cv)
  mlr.pred <- minmaxdesnorm(mlr.pred, dataCliWithAll$TotalTarifas)
  
  #5B rpart
  rpart.model <- rpart(TotalTarifas ~ ., data=train.cv)
  rpart.pred <- predict(rpart.model, newdata=test.cv)
  rpart.pred <- minmaxdesnorm(rpart.pred, dataCliWithAll$TotalTarifas)
  
  test.cv$TotalTarifas <- minmaxdesnorm(test.cv$TotalTarifas, dataCliWithAll$TotalTarifas)
  cv.error[i,] <- c(rmse(nnet.pred2, test.cv$TotalTarifas),
                    rmse(mlr.pred, test.cv$TotalTarifas), 
                    rmse(rpart.pred, test.cv$TotalTarifas))
}
cv.error
apply(cv.error,2,mean)
apply(cv.error,2,sd)

################################################################################

# 7- Justifique se os resultados obtidos para os dois melhores modelos sao
# estatisticamente significativos (para um nivel de significancia de 5%). Identifique o
# modelo que apresenta o melhor desempenho

# Observamos pelo exercício anterior através da média que os melhores modelos são a
# árvore de regressão (3) e a rede neuronal (1). Contudo, iremos comprovar estatisticamente
# através dos testes: t.test e wilcox

# H0: modelos são significativamente iguais VS H1: modelos são significativamente diferentes
# alfa = 5%
# p-value = 1.613e-12
# Como o p-value é menor que alfa rejeitamos a hipótese nula. Ou seja, os modelos são significativamente diferentes 
t.test(cv.error[,1],cv.error[,3])

# H0: modelos são significativamente iguais VS H1: modelos são significativamente diferentes
# alfa = 5%
# p-value = 1.083e-05
# Como o p-value é menor que alfa rejeitamos a hipótese nula. Ou seja, os modelos são significativamente diferentes 
wilcox.test(cv.error[,1], cv.error[,3])

# Concluímos, pelas médias obtidas no exercício 6, que a rede neuronal apresenta o melhor desempenho
# e, através deste exercício, verificamos que a rede neuronal é significativamente diferente
# da árvore de regressao.
