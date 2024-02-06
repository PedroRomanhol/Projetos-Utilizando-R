#### PACOTES ####

set.seed(1234)

#INSTALAR
install.packages("mirt")
install.packages('mirtCAT')
install.packages("psych")
install.packages('ggcorrplot')
install.packages("matrixStats")
install.packages("readxl")
install.packages('dplyr')
install.packages('data.table')
install.packages("reshape2")
install.packages("psych")install.packages("kableExtra")
install.packages("webshot")

#LER
library(mirt)
library(mirtCAT)
library(psych)
library(ggcorrplot)
library(matrixStats)
library(readxl)
library(dplyr)
library(data.table)
library(reshape2)
library(tidyr)
library(psych)
library(kableExtra)
library(webshot)

#### FUNÇÕES ####

# SEPARANDO LETRAS
separar_letras <- function(seq) {
  letras <- unlist(strsplit(seq, ""))
  return(letras)
}

criar_base_letras <- function(data, coluna_respostas, coluna_gabarito) {
  # Extrair as letras de cada resposta e gabarito
  letras_respostas <- strsplit(as.character(data[[coluna_respostas]]), "")
  letras_gabarito <- strsplit(as.character(data[[coluna_gabarito]]), "")
  
  # Criar um data frame com cada letra separada
  base_letras <- data.frame(
    Questao = rep(1:sapply(letras_respostas, length), times = nrow(data)),
    Resposta = unlist(letras_respostas),
    Gabarito = unlist(letras_gabarito)
  )
  
  return(base_letras)
}

# VERIFICANDO RESPOSTAS
verificar_resposta <- function(resposta_aluno, gabarito) {
  return(resposta_aluno == gabarito)
}

tri.3par <- function(theta, ai, bi, ci) {
  p.num = exp(1.702*ai*(theta - bi))
  p.deno = 1 + exp(1.702*ai*(theta -bi))
  p.acerto = ci + (1-ci)*p.num / p.deno 
  return(p.acerto) }

calcula.b <- function(c.modelo1){
  matrix.b = c()
  n = length(c.modelo1)-1
  for( i in 1:n){
    linha.b = (-1 * (c.modelo1[[i]][2]/(c.modelo1[[i]][1])))
    linha.b = c(c.modelo1[[i]][1]/1.702, linha.b, c.modelo1[[i]][3])
    matrix.b = rbind(matrix.b, linha.b)}
  row.names(matrix.b)=names(c.modelo1)[1:n]
  row.names(matrix.b) = gsub("Questao_", "Questão ", row.names(matrix.b))
  colnames(matrix.b) = c('Discriminação', "Dificuldade", "Acerto ao acaso")
  matrix.b = round(matrix.b, 4)
  frame.b = data.frame(row.names(matrix.b), matrix.b, row.names = NULL)
  names(frame.b)[1] = "Componentes"
  names(frame.b) = gsub("Acerto.ao.acaso", "Acerto ao acaso", names(frame.b))
  return(frame.b)}
#### LEITURA DE DADOS ####

DADOS_CH <- read.csv("C:/Users/PEDRO/Downloads/DADOS_CH.csv", sep=";")
dt = data.table(DADOS_CH)

# Separando colunas
TX_RESPOSTAS_CH <- separar_letras(dt$TX_RESPOSTAS_CH)
TX_GABARITO_CH <- separar_letras(dt$TX_GABARITO_CH)

separado <- criar_base_letras(dt, "TX_RESPOSTAS_CH", "TX_GABARITO_CH")

#Corrigindo as questões
resposta_aluno <- separado$Resposta # Respostas do aluno
gabarito <- separado$Gabarito # Gabarito correspondente ao tipo de prova
Correcao <- as.numeric(verificar_resposta(resposta_aluno, gabarito))

Corrigido = cbind(separado, Correcao)

#Montando tabela TRI
if (length(Correcao) != 45*5000) {
  stop("O comprimento do vetor não é compatível com o número de linhas e colunas especificados.")
}

TRI <- matrix(Correcao, nrow = 5000, ncol = 45, byrow = TRUE)
colnames(TRI) <- paste0("Questao_", 1:45)



# Questão 1) Ajustar um modelo de TRI de três parâmetros

#Estimador de Máxima Verossimilhança
modelo_EM = mirt(TRI, 1, itemtype = "3PL", method = "EM")
#Estimador de Bayes
modelo_BY = mirt(TRI, 1, itemtype = "3PL", method = "MHRM")


# Questão 2) Fornecer as estimativas dos parâmetros os itens na escala estudada no curso.

coef.itens = coef(modelo_EM)
coef.itens

estimadores.itens = calcula.b(coef.itens)
estimadores.itens

par_cor <- kable(estimadores.itens, "html") %>%
  kable_styling(full_width = FALSE)

# Questão 3) Identificar as seguintes questões: a mais fácil, a mais difícil, a com maior 
# discriminação e a com maior chance de acerto ao acaso da prova. Mostrar essas questões durante 
#a apresentação. Isso pode ser feito utilizando o PDF da prova fornecido nesta atividade.

# MAIS DIFICIL = maior bi (d) -> 9
# MAIS FÁCIL = menor bi (d) -> 19
# MAIOR DISCRIMINAÇÃO (a1) -> 33
# MAIOR CHANCE DE ACERTO AO ACASO -> 29

# Questão 4) Fornecer a estimativa da habilidade dos estudantes utilizando tanto o método de 
#Máxima Verossimilhança quanto a Estatística Bayesiana.

theta.EM = fscores(modelo_EM, method='ML', response.pattern = TRI)
theta.EM
theta.bayes = fscores(modelo_BY, method='EAP', response.pattern = TRI)
theta.frame = data.frame(theta.EM, theta.bayes)
theta.frame

# Questão 5) Avaliar o ajuste do modelo, utilizando:

# 5.1) Correlograma

# Correlação tetracórica
cor.bd = tetrachoric(TRI)$rho

cor.bd.graf = round(cor.bd, 1)
ggcorrplot(cor.bd.graf, hc.order = FALSE, tl.cex = 10,
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="",
           legend.title = "Correlação",
           ggtheme=theme_bw, 
           show.diag = TRUE,
           #p.mat = p.mat,
           pch.col = "blue")+
  scale_size(1)+
  theme(legend.key.size = unit(0.5, 'cm'), 
        legend.title = element_text(size=10), 
        legend.text = element_text(size=10))

# 5.2) Autovalores

autovalores = eigen(cor.bd)$values
var.autovalores <- autovalores/length(autovalores)


var.autovalores[var.autovalores <0]=0
var.autovalores = round((var.autovalores*100),0)
var.autovalores<- paste(var.autovalores, "%", sep = "")

nomes.eig = paste("Fator", c(1:length(autovalores)), sep = " ")
y.t = c(1:length(autovalores))


plot(y.t, autovalores, type = 'o', ylab = 'Autovalor', col = "blue",  xaxt = "n", xlab = "", pch = 20, ylim = c(0.9*min(autovalores), 1.1* max(autovalores)), cex.lab = 1, cex.axis = 1)
axis(1, y.t, nomes.eig , las = 2, cex.axis = 1)
abline(h=1, col = 'gray80', lty = 2)
text(y.t+0.15 , autovalores+0.15,  var.autovalores, cex = 1)

# 5.3) Análise Fatorial

n.fa = sum(autovalores >1)
solution = fa(cor.bd, nfactors = n.fa)
tab = cbind(solution$loadings[,1:n.fa])
tab = round(tab,2)
tab.aux = data.frame(row.names(tab), tab, row.names = NULL)
colnames(tab.aux) = c("Componentes", paste("Fator", c(1:n.fa), sep = " "))
tab.aux

max.linha = rowMaxs(as.matrix(tab.aux[,-1]))
tab.aux2 = tab.aux
tab.aux2[,-1][tab.aux[,-1] !=  max.linha ] = "-" 
tab.aux2

# 5.4) Indicador de Unidimensionalidade do pacote "psych"

resultado_unidim = unidim(TRI, cor = "tet")
resultado_unidim

# 5.5) Teste Qui-quadrado de Orlando e Thissen (2000)

#Teste Qui-quadrado  de Orlando e Thissen(2000)
itemfit(modelo_EM) # Default do comando itemfit