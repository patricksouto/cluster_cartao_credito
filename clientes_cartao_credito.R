### PERGUNTA PROBLEMA ###
# Como poderiamos segmentar meus clientes para direcionar propagandas de produtos (principalmente cartão de crédito) e serviços?

### BAIXANDO E ATIVANDO PACOTES ###
library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)

### TRATANDO E VISUALIZANDO A BASE ###

# Importando a base de dados
clientes <- read.csv("clientes_cartao_credito2.csv", sep = ";")
### APRENDIZADO ###: Para windows não se usa a "\" para importar dados. Usa-se "/"
### APRENDIZADO ###: Arquivos csv tem separadores, sendo vírgula ou ponto e vírgula

# Visualizando os dados
View(clientes)
head(clientes)
str(clientes)

# Excluindo a coluna SI_No
clientes$Sl_No <- NULL

# Mudando o nome das colunas
nomeColunas <- c("Limite_cartoes", "Qtnd_cartoes_cliente", "visitas_banco", "visitas_bankline", "total_ligacoes")
names(clientes) <- nomeColunas

# Tornando os IDs números das linhas
rownames(clientes) <- clientes [,1]
clientes <- clientes[,-1]

# Padronizando as variáveis
clientes_padronizado <- scale(clientes)
View(clientes_padronizado)

### TESTANDO O MÉTODO HIERÁRQUICO ###

# Calculando e visualizando a matriz de distâncias euclidianas
distancia <- dist(clientes_padronizado, method = "euclidean")
distancia

# Calculando o cluster pelo método do vizinho mais próximo
cluster.hierarquico <- hclust(distancia, method = "average")

# Criando o dendograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)
### Com muitos dados é difícil visualizar quantos grupos criar no dendograma

# Verificando, através do método Elbow, a quantidade ideal de grupos
fviz_nbclust(clientes_padronizado, FUN = hcut, method = "wss")
# Através do Elbow perce-be que 3 ou 4 grupos é a quantidade ideal. 

# criando 4 grupos
grupo_4 <- cutree(cluster.hierarquico, k = 4)
table(grupo_4)

# Transformando em data frame a saída do cluster
df_clientes_grupo <- data.frame(grupo_4)

# Juntando com a base original
base_final <- cbind(clientes, df_clientes_grupo)

# Por fim, realiza-se uma análise descritiva dos grupos
# Médias das variáveis por grupo
mediagrupo <- base_final %>%
  group_by(grupo_4) %>%
  summarise(n = n(),
            Limite_cartoes = mean(Limite_cartoes),
            Qtnd_cartoes_cliente = mean(Qtnd_cartoes_cliente),
            visitas_banco = mean(visitas_banco),
            visitas_bankline = mean(visitas_bankline),
            total_ligacoes = mean(total_ligacoes) )
mediagrupo

### CONCLUSÃO ###
# Apesar do método hierárquico não ser o mais adequado pela quantidade de dados, ele nos trouxe um resultado com 4 grupos.
# Através da análise descritiva dos grupos, percebe-se:
# GRUPO 1: 1 indivíduo outlier com alta soma de limite e com apenas 2 cartões. Não costuma visitar o banco fisicamente e nem no bankline. Poderia estar no grupo 4.
# GRUPO 2: 226 indivíduos com o limite mais baixo da amostra, poucos cartões (média de 2.43), poucas visitas fisicas e algumas visitas no bankline, mas com alto número de ligações ao banco.
# GRUPO 3: 379 indivíduos com o limite médio, porém com qntd alta de cartões. Faz mais visitas físicas no banco do que acessa o bankline e tem baixa quantidade de ligações.
# GRUPO 4: 49 indivíduos com limite muito alto, porém com grande quantidade de cartões. Acessam muito o bankline e bem pouco as agências físicas. Baixa taxa de ligações ao banco.
# A partir disso é possível entregar a área de negócio alguns insights:
# MARKETING
# Para estratégia de marketing, pode-se oferecer aos grupos 1 e 4 produtos mais caros, ao grupo 2 produtos mais baratos
# e ao grupo 3 produtos de médio valor.
# INVESTIMENTOS
# Para além dessa análise é possível direcionar mais opções de investimentos aos Grupos 1 e 4, pois aparentemente
# possuem alta renda. Como complemento, o grupo 4 mostra que gosta de possuir um grande número de cartões, e pode ser
# um público que vai aderir no futuro a produtos novos desse tipo. 
# OPERAÇÃO
# Também é possível enviar comunicações para o grupo 2 para tentar evitar a alta taxa de ligações ao banco, diminuindo
# os esforços operacionais. Ainda falando de operação é possivel tentar direcionar o grupo 3 para o acesso ao bankline,
# já que estão visitando muito a agência física. Por outro lado se a estratégia for ter clientes na agência física
# para vender produtos, é interessante tentar direcionar o grupo 2, que tem grande quantidade de pessoas, para as agências.

# Como lida-se com uma quantidade considerável de observações, será utilizado o método não hierárquico e a metodologia k-means

### TESTANDO O MÉTODO NÃO HIERÁRQUICO ###
# É válido ressaltar que como alguns passos já foram realizados, como por exemplo a importação, visualização e padronização da base de dados, iremos direto ao método.

# Criando cluster de 2 grupos com k-means (rodando o modelo)
clientes_k2 <- kmeans(clientes_padronizado, centers = 2)
# Visualizando os clusters
fviz_cluster(clientes_k2, data = clientes_padronizado, main = "Cluster k2")
# Percebe-se que 2 grupos não descrevem de maneira ótima a base, já que o grupo dois está muito espaçado. Pelo método Elbow para quantidade de grupos, foi possível verificar
# que a quantidade ideal de grupos são 3 ou 4. Portanto, assim como na análise do método hierárquico, faremos a divisão dos grupos em 4.

# Criando cluster de 4 grupos com k-means (rodando o modelo)
clientes_k4 <- kmeans(clientes_padronizado, centers = 4)
# Visualizando os clusters
fviz_cluster(clientes_k4 ,data = clientes_padronizado, main = "Cluster k4")
# Nota-se que, para este método, 3 grupos seria ideal.

# Criando cluster de 3 grupos com k-means (rodando o modelo)
clientes_k3 <- kmeans(clientes_padronizado, centers = 3)
# Visualizando os clusters
fviz_cluster(clientes_k3, data = clientes_padronizado, main = "Cluster k3")

# Criando gráficos para compará-los
G2 <- fviz_cluster(clientes_k2, geom = "point", data = clientes_padronizado) + ggtitle ("k = 2")
G3 <- fviz_cluster(clientes_k3, geom = "point", data = clientes_padronizado) + ggtitle ("K = 3")
G4 <- fviz_cluster(clientes_k4, geom = "point", data = clientes_padronizado) +ggtitle ("k = 4")

# Comparando os três gráficos na mesma tela
grid.arrange(G2, G3, G4, nrow = 2)
# Nota-se que de fato a melhor opção são 3 grupos

# Juntando os dados
clientes2 <- read.csv("clientes_cartao_credito2.csv", sep = ";")
clientes2clusters <- data.frame(clientes_k3$cluster)

# Agrupando clusters e base inicial
clientesfinal <- cbind(clientes2, clientes2clusters)

# Por fim, realiza-se uma análise descritiva dos grupos
# Médias das variáveis por grupo
head(clientesfinal)
mediagrupo_k <- clientesfinal %>%
  group_by(clientes_k3.cluster) %>%
  summarise(n = n(),
            Avg_Credit_Limit = mean(Avg_Credit_Limit),
            Total_Credit_Cards = mean(Total_Credit_Cards),
            Total_visits_bank = mean(Total_visits_bank),
            Total_visits_online = mean(Total_visits_online),
            Total_calls_made = mean(Total_calls_made) )
mediagrupo_k

### CONCLUSÃO ###
# Através da análise gráfica para o método k-means, percebe-se que a melhor opção é ter 3 grupos
# Através da análise descritiva básica, percebe-se que:
# GRUPO 1 = Possui 49 indivíduos com alto limite e grande quantidade de cartões em relação a amostra. Além disso, eles acessam os serviços via bankline e não vão muito às agências. Por fim, usam o canal telefônico poucas vezes.
# GRUPO 2 = Possui 224 indivíduos com baixo limite e pouca quantidade de cartões em relação a amostra. Também, acessam esporadicamente o bankline e pouquissimas vezes as agências físicas. No entano, usam bastante o canal telefônico.
# GRUPO 3 = Possui 382 indivíduos que tem médio limite e grande quantidiade de cartões em relação a amostra. Vão 3 vezes mais fisicamente ao banco se comparado ao canal bankline. usam pouco o canal telefônico para acessar o banco.
# A partir disso é possível entregar a área de negócio alguns insights:

## MARKETING E COMUNICAÇÃO ##
# PRODUTO CARTÃO DE CRÉDITO - Pode ser interessante direcionar propagandas sobre novos cartões para os grupos 1 e 3 que possuem muitos cartões e tendem a adquirir mais.
# PRODUTOS E INVESTIMENTOS - Pode ser interessante direcionar propagandas sobre produtos bancários e de parceiros (não bancários) de alto custo para o grupo 1 que demonstra possuir alta renda.
#                          - Pode ser avaliado o direcionamento de propagandas sobre produtos bancários sem taxas e de baixo e médio custo para os grupos 2 e 3 que possuem renda significativamente inferir, se comparado ao grupo 1 da amostra.

## OPERAÇÃO ##
# CANAL TELEFÔNICO - Pode fazer sentido enviar/reforçar procedimentos de autoatendimento para dúvidas e problemas para o grupo 2, que utiliza de demasiadamente o canal telefônico. Isso diminuiria os chamados e, consequentemente, o esforço operacional.
# PONTOS FÍSICOS (Objetivo de diminuir o fluxo) - O grupo 3 mostra um número alto de idas ao ponto físico se comparado a amostra. Pode ser avaliado o envio de instruções de como realizar os procedimentos via bankline. É importante avaliar quais procedimentos esse grupo realiza nos pontos físicos.
#                (Objetivo de aumentar o fluxo) - Os grupos 1 e 2 comparecem pouco aos pontos físicos. Portanto, pode ser incentivado o comparecimento físico desses clientes com promoções ou similar, caso o objetivo seja oferecer produtos fisicamente, por exemplo.
