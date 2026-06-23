# Padrões temporais e perfil sociodemográfico de dengue, chikungunya e Zika em Campos dos Goytacazes (2020–2025)

> **Painel interativo de inteligência epidemiológica para vigilância, ensino e pesquisa aplicada às arboviroses urbanas.**

**App publicado:** <https://arbovirosescamposiff.shinyapps.io/arboviroses/>

---

## Contexto científico

As arboviroses transmitidas pelo *Aedes aegypti* — dengue, chikungunya e Zika — representam um dos maiores desafios de saúde pública do Brasil contemporâneo. Em municípios de médio porte como **Campos dos Goytacazes (RJ)**, a coexistência sazonal desses três agravos impõe pressão contínua sobre a vigilância epidemiológica, a atenção primária e a capacidade de resposta do sistema local de saúde. Apesar da relevância, são escassos os painéis públicos que integrem os três agravos com granularidade municipal, documentação metodológica rastreável e indicadores de qualidade da informação.

Este projeto realiza uma **análise descritiva sistemática** dos registros de dengue, chikungunya e Zika notificados ao SINAN entre 2020 e 2025 para residentes e atendidos em Campos dos Goytacazes, com os seguintes propósitos:

- **Caracterizar a magnitude e a sazonalidade** de cada agravo ao longo dos seis anos, identificando anos epidêmicos, períodos de silêncio e diferenças de perfil entre as arboviroses.
- **Investigar o perfil sociodemográfico** dos casos — sexo, faixa etária, raça/cor, escolaridade e situação gestacional — para levantar hipóteses sobre exposição, vulnerabilidade e acesso ao diagnóstico.
- **Avaliar a qualidade do preenchimento** das fichas de notificação, quantificando campos ignorados, brancos ou ausentes por variável e por ano, como indicador da confiabilidade das inferências.
- **Mapear a distribuição espacial** dos registros de dengue por bairro, articulando as planilhas da vigilância municipal com a malha censitária do IBGE, e expor eventuais lacunas de correspondência territorial.
- **Fornecer dados organizados, figuras e tabelas exportáveis** para apoiar relatórios técnicos, apresentações acadêmicas, salas de situação e publicações científicas.
- **Problematizar os limites da notificação** como sinônimo de incidência real, discutindo o impacto do sub-registro, do atraso de encerramento e da incompletude diferencial entre variáveis e grupos populacionais.

O trabalho é desenvolvido no âmbito do **Instituto Federal Fluminense (IFF) — Campus Campos Guarus**, vinculado ao curso de Bacharelado em Enfermagem e financiado pelo **CNPq** como projeto de iniciação científica, sob orientação da Profa. Dra. Karla Rangel Ribeiro. A proposta articula **vigilância em saúde, ciência de dados, formação em enfermagem e comunicação científica aplicada ao território**.

---

## Perguntas que o painel ajuda a responder

| Eixo | Pergunta norteadora |
|------|---------------------|
| Temporal | Em quais anos houve picos de notificação? A sazonalidade difere entre dengue, chikungunya e Zika? |
| Etário | Quais faixas etárias concentram mais registros? Há diferença no perfil infantil, adulto e idoso? |
| Sexo | O predomínio feminino se mantém nas três doenças? O que pode explicar esse padrão? |
| Gestação | Qual a proporção de registros em gestantes? A Zika apresenta perfil gestacional distinto? |
| Raça/cor e escolaridade | Como se distribuem os casos segundo raça/cor e escolaridade? Os campos ignorados permitem inferência? |
| Qualidade da notificação | Quais variáveis apresentam maior incompletude? A qualidade melhorou ou piorou ao longo dos anos? |
| Espacial (dengue) | Quais bairros acumulam mais registros? Há bairros sem correspondência na malha cartográfica? |
| Comparação entre agravos | Chikungunya, dengue e Zika apresentam perfis sociodemográficos semelhantes ou distintos no mesmo município? |

---

## Principais achados (2020–2025)

- **Dengue** é o agravo predominante em volume absoluto, com explosão em **2024** (>19.700 casos confirmados), configurando o maior surto do período. A série histórica revela anos de baixa circulação (2020–2022) seguidos de escalada acentuada.
- **Chikungunya** exibe dois picos relevantes — 2020 e 2024 — com predomínio consistente do sexo feminino e concentração em adultos de 40–59 anos. A letalidade por chikungunya, embora baixa, está presente e requer atenção.
- **Zika** apresenta os menores volumes absolutos e maior sensibilidade ao sub-registro, especialmente após o fim da emergência de saúde pública internacional (2016–2017). A vigilância em gestantes segue como eixo crítico de interpretação.
- A **qualidade da notificação** é heterogênea: sexo e idade têm boa completude (>99%), enquanto **escolaridade e raça/cor** chegam a ultrapassar 60% de ignorados/brancos em alguns anos, comprometendo análises de desigualdade.
- O **mapa de dengue por bairro** evidencia concentração espacial em bairros centrais e populosos, mas a correspondência entre nomes de bairro da planilha e da malha do IBGE revela **divergências de grafia e localidades sem polígono** que exigem ajuste manual contínuo.
- A **comparação direta entre os três agravos** na mesma plataforma — com mesmo recorte temporal, territorial e metodológico — é uma contribuição distintiva deste painel frente a outros dashboards públicos.

---

## Metodologia

### Fonte dos dados

| Componente | Fonte | Detalhe |
|------------|-------|---------|
| Dengue e Zika (registros individuais) | SINAN-DENGUE e SINAN-ZIKA via pacote [`microdatasus`](https://github.com/rfsaldanha/microdatasus) | Download parametrizável; cache local em `data/app_cache/` |
| Chikungunya (agregado) | SINAN/SVS — tabela agregada do projeto | Ingestão via `SINAN-CHIKUNGUNYA` no pipeline |
| Dengue por bairro | Planilhas locais `DENGUEYYYY.xlsx` fornecidas pela vigilância municipal | `dados_sinan_campos/` |
| Malha territorial | `geobr` / IBGE — bairros de 2010 | Cache em `neighborhoods_2010_simplified.gpkg` |
| População residente | IBGE/SIDRA — tabela 6579, variável 9324 | Cache com fallback embutido |

### Critérios de inclusão e definição de caso

- **Município:** Campos dos Goytacazes (código IBGE 3301009).
- **Período:** 2020 a 2025 (ano epidemiológico).
- **Confirmação:** classificação final compatível com dengue, febre hemorrágica da dengue, chikungunya ou Zika conforme padronização do SINAN; excluídos registros descartados e inconclusivos (exceto quando analisados separadamente como indicador de qualidade).
- **Incidência:** casos confirmados / população estimada × 100.000 habitantes.

### Processamento

O pipeline é integralmente reprodutível e modular:

```
01_ingestao.R   → 02_limpeza.R  → 03_agregacao.R
04_mapas.R      → 05_validacao.R → 06_export_app.R
```

- A ingestão suporta modo **com e sem download** das APIs externas, controlado pela variável de ambiente `ARBOVIROSES_DOWNLOAD`.
- O app Shiny **não baixa dados no startup**: lê exclusivamente caches `.rds` e `.gpkg` pré-processados.
- A validação gera automaticamente relatório de auditoria (`data/audit/relatorio_auditoria.html`) com totais, completude e bairros não pareados.

### Visualização e comunicação

- **Gráficos interativos** (Plotly) com *hover* detalhado, *download* em alta resolução (PNG 600 DPI, PDF, SVG, TIFF) e legendas metodológicas.
- **Mapa de dengue** (Leaflet) com camada de bairros, escala de cores, *popup* com nome e contagem, e controle de camadas.
- **Painel de qualidade** com indicadores de completude por variável e alertas automáticos para campos com >30% de ignorados/brancos.
- **Comparador entre agravos** na página inicial com seletor de variável (ano, sexo, faixa etária, raça/cor, gestação).
- **Filtro global de período** na barra lateral que sincroniza todas as abas.

---

## Estrutura do repositório

```
├── app.R                     # Entrada modular do Shiny
├── app_arboviroses.R         # App monolítico legado (referência funcional)
├── R/
│   ├── app_legacy_original.R # Cópia fiel do original pré-modularização
│   ├── dados.R               # Leitura de caches, fallback e estruturas de dados
│   ├── graficos.R            # Funções de visualização (gráficos, qualidade, downloads)
│   ├── mapas.R               # Malha de bairros, correspondência espacial, Leaflet
│   ├── packages.R            # Verificação e carregamento de dependências
│   ├── server.R              # Lógica reativa do Shiny
│   ├── ui.R                  # Interface do usuário (UI completa)
│   └── utils.R               # Auditoria, validação, logs e utilidades
├── scripts/
│   ├── 01_ingestao.R         # Download/leitura de dados brutos (dengue, zika, chikungunya)
│   ├── 02_limpeza.R          # Padronização de variáveis e datas
│   ├── 03_agregacao.R        # Agregação por ano, sexo, idade, raça/cor, gestação etc.
│   ├── 04_mapas.R            # Preparação de bases espaciais
│   ├── 05_validacao.R        # Auditoria de totais, completude, bairros não mapeados
│   ├── 06_export_app.R       # Exportação dos .rds finais para data/app_cache/
│   ├── update_data.R         # Orquestrador do pipeline completo
│   ├── deploy.R              # Publicação no shinyapps.io com proteção contra conta antiga
│   └── fix_encoding_text.R   # Correção de encoding em textos do projeto
├── data/
│   ├── app_cache/            # Caches processados usados pelo app (versionados)
│   ├── audit/                # Relatórios automáticos de auditoria (versionados)
│   ├── raw/                  # Dados brutos baixados (não versionados)
│   ├── interim/              # Dados limpos intermediários (não versionados)
│   └── processed/            # Agregados processados (não versionados)
├── tests/testthat/           # Testes automatizados (11 testes, 0 falhas)
├── www/style.css             # Estilos incrementais (design visual principal preservado)
├── .github/workflows/        # CI/CD (GitHub Actions)
├── renv.lock                 # Ambiente R reprodutível
└── README.md
```

---

## Como restaurar o ambiente e rodar

```r
# 1. Restaurar pacotes R exatos do projeto
renv::restore()

# 2. Rodar o pipeline (sem download externo — usa planilhas e caches locais)
source("scripts/update_data.R")

# 3. Iniciar o app
shiny::runApp()
```

**Com download das APIs (microdatasus):**

```r
Sys.setenv(ARBOVIROSES_DOWNLOAD = "true")
source("scripts/update_data.R")
```

---

## Auditoria e metadados

O pipeline gera automaticamente:

| Arquivo | Conteúdo |
|---------|----------|
| `data/app_cache/metadata.json` | Data de atualização, fontes, anos, versão do pipeline, observações metodológicas |
| `data/audit/totais_por_ano_agravo.csv` | Casos confirmados, óbitos e ignorados por ano e agravo |
| `data/audit/completude_variaveis.csv` | Percentual de campos ignorados/brancos por variável, agravo e ano |
| `data/audit/bairros_nao_mapeados.csv` | Bairros da planilha sem correspondência na malha geobr/IBGE |
| `data/audit/relatorio_auditoria.html` | Relatório HTML consolidado com alertas metodológicos |

---

## Limitações

- **Notificação ≠ incidência real.** Subnotificação, atraso de encerramento, barreiras de acesso ao sistema de saúde e mudanças de definição de caso afetam a contagem. Os números representam *registros notificados*, não o total de casos ocorridos.
- **Incompletude diferencial.** Campos como escolaridade e raça/cor têm proporções elevadas de ignorados/brancos — as distribuições observadas nesses recortes devem ser interpretadas com cautela e sempre acompanhadas do indicador de completude.
- **Incidência aproximada.** O denominador populacional provém do IBGE/SIDRA (tabela 6579) e pode não estar disponível para todos os anos; quando ausente, replica-se o último valor conhecido, o que introduz imprecisão na taxa.
- **Malha territorial desatualizada.** A malha de bairros do IBGE data de 2010. Mudanças na delimitação ou nomenclatura de bairros desde então podem gerar desencontros com os registros das planilhas de notificação.
- **Chikungunya em tabela agregada.** Diferentemente de dengue e Zika — que dispõem de registros individuais baixados — os dados de chikungunya utilizam tabela agregada, limitando a granularidade temporal e geográfica.
- **Associações descritivas, não causais.** Todos os gráficos e tabelas são descritivos. Padrões observados não estabelecem nexo causal e devem ser ponto de partida para investigações complementares.

---

## Perspectivas

- Integração de **modelos de aprendizado de máquina interpretável** para identificar combinações de fatores associadas a maior risco de notificação e óbito.
- Ampliação da cobertura temporal para **séries históricas longas** (2010+), permitindo análise de tendências de longo prazo.
- Inclusão de **dados climáticos e entomológicos** (índice de infestação, precipitação, temperatura) para correlação ecológica.
- Expansão da análise espacial para **chikungunya e Zika**, condicionada à disponibilidade de registros por bairro.
- **Validação externa** dos achados com a vigilância municipal e com outros sistemas de informação (SIM, SIH).
- Migração para **modelo de nowcasting** para antecipação de cenários de curto prazo.

---

## Equipe

| Nome | Papel |
|------|-------|
| **Ryan de Paulo Santos** | Bacharelando em Enfermagem (IFF Guarus) — desenvolvimento, organização dos dados e análise |
| **Brenda Velasco Moreira** | Bacharelanda em Enfermagem (IFF Guarus) — leitura epidemiológica e organização científica |
| **Mirella Guimarães Lourenço de Souza** | Bacharelanda em Enfermagem (IFF Guarus) — interpretação dos indicadores |
| **Marcelly Rangel de Mello** | Bacharelanda em Enfermagem (IFF Guarus) — análise, revisão e comunicação dos resultados |
| **Profa. Dra. Karla Rangel Ribeiro** | Docente e orientadora (IFF Guarus) — vigilância em saúde, pesquisa aplicada e análise de dados |

**Contato:** [ryan.paulo@gsuite.iff.edu.br](mailto:ryan.paulo@gsuite.iff.edu.br)

---

## Licença

A definir antes de publicação ampla do repositório.
