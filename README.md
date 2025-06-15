# 🚴 Projeto de Análise de Procura de Bicicletas em Seul

Um projeto abrangente de ciência de dados que analisa os padrões de procura de bicicletas partilhadas em Seul e fornece informações preditivas através de um dashboard interativo.

## 📋 Visão Geral do Projeto

Este projeto implementa um pipeline completo de ciência de dados para analisar e prever a procura de bicicletas partilhadas, utilizando dados do sistema de bicicletas de Seul. O projeto combina web scraping, tratamento de dados (data wrangling), análise SQL, análise exploratória de dados, modelação de machine learning e visualização interativa.

### 🎯 Funcionalidades Principais

- **Recolha de Dados**: Web scraping e integração de API para dados de bicicletas partilhadas e meteorologia.
- **Processamento de Dados**: Limpeza avançada, transformação e engenharia de características (feature engineering).
- **Análise SQL**: Consultas de base de dados abrangentes para extração de insights.
- **Machine Learning**: Múltiplos modelos de regressão com comparação de desempenho.
- **Dashboard Interativo**: Previsões em tempo real com prognósticos para cidades globais.
- **Visualização**: Análise exploratória de dados rica com ggplot2.

## 🏗️ Estrutura do Projeto

```
ProjetoSAD3/
├── 📁 data/
│   ├── raw/                   # Dados brutos recolhidos (raw)
│   └── clean/                 # Conjuntos de dados processados e limpos
├── 📁 models/                 # Modelos de ML treinados e resultados
├── 📁 results/                # Resultados da análise SQL
├── 📁 visualizations/         # Gráficos e visualizações da EDA
├── 📄 01_data_collection.R     # Fase 1: Web scraping e recolha de dados
├── 📄 02_data_wrangling.R      # Fase 2: Limpeza e pré-processamento de dados
├── 📄 03_sql_analysis.R        # Fase 3: Consultas SQL e análise
├── 📄 04_eda_visualization.R   # Fase 4: Análise exploratória de dados
├── 📄 05_regression_modeling.R # Fase 5: Modelos de machine learning
├── 📄 06_shiny_dashboard.R     # Fase 6: Dashboard interativo
├── 📄 run_pipeline.R           # 🚀 Executor principal do pipeline
├── 📄 launch_dashboard.R       # 🌐 Lançador do Dashboard
└── 📄 README.md               # Este ficheiro
```

## 🚀 Como Começar

### Pré-requisitos

Certifique-se de que tem o R instalado (versão 4.0 ou superior), juntamente com os seguintes pacotes:

```r
# Pacotes principais
install.packages(c(
  "dplyr", "readr", "ggplot2", "lubridate", "stringr", "tidyr",
  "DBI", "RSQLite", "tidymodels", "glmnet", "randomForest",
  "shiny", "shinydashboard", "DT", "plotly", "leaflet",
  "rvest", "httr", "jsonlite"
))
```

### 🔄 Execução Completa do Pipeline

Para executar o projeto completo do início ao fim:

```r
# Executar o pipeline completo
source("run_pipeline.R")
```

Isto executará todas as fases sequencialmente:
1.  **Recolha de Dados** - Extrai dados dos sistemas de partilha de bicicletas e de meteorologia.
2.  **Tratamento de Dados** - Limpa e processa todos os conjuntos de dados.
3.  **Análise SQL** - Realiza consultas e análises à base de dados.
4.  **Visualização EDA** - Cria gráficos de análise exploratória de dados.
5.  **Modelação de Regressão** - Treina e avalia modelos de ML.
6.  **Lançamento do Dashboard** - Inicia o dashboard interativo Shiny.

### 🌐 Apenas o Dashboard

Se já executou o pipeline e pretende apenas iniciar o dashboard:

```r
# Lançar apenas o dashboard (requer que o pipeline tenha sido executado primeiro)
source("launch_dashboard.R")
```

O dashboard irá automaticamente:
- ✅ Verificar os pacotes necessários e instalá-los se estiverem em falta
- ✅ Verificar a existência de ficheiros essenciais
- ✅ Carregar o modelo de ML treinado
- ✅ Abrir no seu navegador web predefinido em `http://127.0.0.1:3838`

## 📊 Funcionalidades do Dashboard

### 🗺️ Mapa Preditivo
- Mapa interativo do mundo que mostra as previsões de procura de bicicletas
- Previsão a 5 dias para as principais cidades
- Previsões em tempo real baseadas na meteorologia
- Marcadores de cidades clicáveis com informação detalhada

### 📈 EDA de Seul
- Análise de séries temporais dos padrões de utilização de bicicletas
- Análise de distribuição e resumos estatísticos
- Análise sazonal com métricas chave

### 🌦️ Impacto Meteorológico
- Correlações entre a temperatura e a utilização de bicicletas
- Análise de padrões meteorológicos sazonais
- Filtragem interativa por estação e temperatura

### ⏰ Padrões Temporais
- Padrões de utilização horária por estação
- Identificação de horas de ponta
- Análise de padrões de deslocações pendulares

### 🤖 Desempenho do Modelo
- Comparação e classificação de modelos
- Métricas de desempenho (R², RMSE)
- Insights sobre a importância das características (feature importance)

## 🔬 Destaques da Análise

### Fontes de Dados
- **Dados de Partilha de Bicicletas de Seul**: Padrões históricos de aluguer com condições meteorológicas
- **Previsão Meteorológica Global**: Dados meteorológicos em tempo real para as principais cidades
- **Base de Dados de Cidades Mundiais**: Informação geográfica e demográfica

### Modelos de Machine Learning
- **Regressão Linear**: Modelo de base com variáveis meteorológicas
- **Regressão Múltipla**: Modelo melhorado com variáveis categóricas
- **Random Forest**: Modelo com melhor desempenho (88% de precisão)

### Principais Insights
- 🌡️ A **Temperatura** é o preditor mais forte da procura de bicicletas
- ⏰ **Horas de Ponta**: 8h e 18h (padrões de deslocações pendulares)
- 🗓️ **Variação Sazonal**: O verão regista uma utilização 4x superior à do inverno
- 🌦️ **Impacto Meteorológico**: Condições extremas reduzem significativamente a procura

## 📈 Desempenho do Modelo

| Modelo                | Score R² | RMSE | Características Principais                 |
|-----------------------|----------|------|--------------------------------------------|
| Random Forest         | 0.880    | 220  | Padrões não-lineares, interações de features |
| Regressão Múltipla    | 0.745    | 340  | Variáveis categóricas, interpretável       |
| Regressão Linear      | 0.610    | 420  | Apenas variáveis meteorológicas            |

## 🛠️ Implementação Técnica

### Pipeline de Processamento de Dados
- **Processamento de Strings**: Operações com Regex para limpeza de dados
- **Engenharia de Características**: Variáveis dummy, normalização, padronização
- **Tratamento de Valores Em Falta**: Imputação pela mediana e valores por defeito lógicos

### Análise SQL
- **Consultas Complexas**: Subconsultas, junções (joins), agregações
- **Análise de Séries Temporais**: Padrões e tendências sazonais
- **Cálculos Estatísticos**: Desvios padrão, percentis

### Machine Learning
- **Framework tidymodels**: Ecossistema moderno de ML em R
- **Validação Cruzada**: Avaliação robusta de modelos
- **Otimização de Hiperparâmetros**: Desempenho otimizado do modelo

## 🚨 Resolução de Problemas

### Problemas Comuns

**Erro "Faltam ficheiros essenciais":**
```r
# Certifique-se de que executa primeiro o pipeline completo
source("run_pipeline.R")
```

**Problemas na instalação de pacotes:**
```r
# Instale manualmente os pacotes em falta
install.packages("nome_do_pacote", dependencies = TRUE)
```

**O dashboard não carrega:**
- Verifique se a porta 3838 está disponível
- Tente executar a partir do RStudio em vez da linha de comandos
- Verifique se todos os ficheiros de dados existem nos diretórios corretos

## 📁 Descrição dos Ficheiros

| Ficheiro                    | Objetivo                       | Saída (Output)         |
|-----------------------------|--------------------------------|------------------------|
| `01_data_collection.R`      | Web scraping e chamadas de API | Ficheiros de dados brutos |
| `02_data_wrangling.R`       | Limpeza e pré-processamento    | Conjuntos de dados limpos |
| `03_sql_analysis.R`         | Consultas e análise de BD      | Resultados da análise  |
| `04_eda_visualization.R`    | Análise exploratória de dados  | Gráficos de visualização |
| `05_regression_modeling.R`  | Treino de modelos de ML        | Modelos treinados      |
| `06_shiny_dashboard.R`      | Dashboard interativo           | Aplicação web          |

## 🎯 Exemplos de Utilização

### Início Rápido
```r
# Análise completa num único comando
source("run_pipeline.R")
```

### Apenas o Dashboard
```r 
# Após executar o pipeline
source("launch_dashboard.R")
```

### Fases Individuais
```r
# Executar fases específicas
source("01_data_collection.R")
source("02_data_wrangling.R")
# ... etc
```

## 📜 Licença

Este projeto foi criado para fins educacionais e de investigação. 

## 🤝 Contribuições

Este projeto foi desenvolvido como parte de uma análise de ciência de dados. Sinta-se à vontade para fazer um fork e adaptá-lo às suas próprias necessidades de análise de partilha de bicicletas. 

Realizado por:

Ivanilson Braga – 30010789
Zakhar Khomyakivskyy – 30011355 
Ektiandro Elizabeth – 30011479
