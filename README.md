# Padroes temporais e perfil sociodemografico de dengue, chikungunya e Zika em Campos dos Goytacazes (2020:2025)

> **Painel interativo de inteligencia epidemiologica para vigilancia, ensino e pesquisa aplicada as arboviroses urbanas.**

**App publicado:** [arbovirosescamposiff.shinyapps.io/arboviroses](https://arbovirosescamposiff.shinyapps.io/arboviroses/)

**Repositorio:** [github.com/santosry/arboviroses-campos](https://github.com/santosry/arboviroses-campos)

**Licenca:** MIT

---

## Contexto cientifico

As arboviroses transmitidas pelo *Aedes aegypti* (dengue, chikungunya e Zika) representam um dos maiores desafios de saude publica do Brasil contemporaneo. Em municipios de medio porte como **Campos dos Goytacazes (RJ)**, a coexistencia sazonal desses tres agravos impoe pressao continua sobre a vigilancia epidemiologica, a atencao primaria e a capacidade de resposta do sistema local de saude. Apesar da relevancia, sao escassos os paineis publicos que integrem os tres agravos com granularidade municipal, documentacao metodologica rastreavel e indicadores de qualidade da informacao.

Este projeto realiza uma **analise descritiva sistematica** dos registros de dengue, chikungunya e Zika notificados ao SINAN entre 2020 e 2025 para residentes e atendidos em Campos dos Goytacazes, com os seguintes propositos:

*   **Caracterizar a magnitude e a sazonalidade** de cada agravo ao longo dos seis anos, identificando anos epidemicos, periodos de silencio e diferencas de perfil entre as arboviroses.
*   **Investigar o perfil sociodemografico** dos casos (sexo, faixa etaria, raca/cor, escolaridade e situacao gestacional) para levantar hipoteses sobre exposicao, vulnerabilidade e acesso ao diagnostico.
*   **Avaliar a qualidade do preenchimento** das fichas de notificacao, quantificando campos ignorados, brancos ou ausentes por variavel e por ano, como indicador da confiabilidade das inferencias.
*   **Mapear a distribuicao espacial** dos registros de dengue por bairro, articulando as planilhas da vigilancia municipal com a malha censitaria do IBGE, e expor eventuais lacunas de correspondencia territorial.
*   **Fornecer dados organizados, figuras e tabelas exportaveis** para apoiar relatorios tecnicos, apresentacoes academicas, salas de situacao e publicacoes cientificas.
*   **Problematizar os limites da notificacao** como sinonimo de incidencia real, discutindo o impacto do sub-registro, do atraso de encerramento e da incompletude diferencial entre variaveis e grupos populacionais.

O trabalho e desenvolvido no ambito do **Instituto Federal Fluminense (IFF), Campus Campos Guarus**, vinculado ao curso de Bacharelado em Enfermagem e financiado pelo **CNPq** como projeto de iniciacao cientifica, sob orientacao da Profa. Dra. Karla Rangel Ribeiro. A proposta articula **vigilancia em saude, ciencia de dados, formacao em enfermagem e comunicacao cientifica aplicada ao territorio**.

---

## Perguntas que o painel ajuda a responder

| Eixo | Pergunta norteadora |
|:---|---|
| Temporal | Em quais anos houve picos de notificacao? A sazonalidade difere entre dengue, chikungunya e Zika? |
| Etario | Quais faixas etarias concentram mais registros? Ha diferenca no perfil infantil, adulto e idoso? |
| Sexo | O predominio feminino se mantem nas tres doencas? O que pode explicar esse padrao? |
| Gestacao | Qual a proporcao de registros em gestantes? A Zika apresenta perfil gestacional distinto? |
| Raca/cor e escolaridade | Como se distribuem os casos segundo raca/cor e escolaridade? Os campos ignorados permitem inferencia? |
| Qualidade da notificacao | Quais variaveis apresentam maior incompletude? A qualidade melhorou ou piorou ao longo dos anos? |
| Espacial (dengue) | Quais bairros acumulam mais registros? Ha bairros sem correspondencia na malha cartografica? |
| Comparacao entre agravos | Chikungunya, dengue e Zika apresentam perfis sociodemograficos semelhantes ou distintos no mesmo municipio? |

---

## Principais achados (2020:2025)

*   **Dengue** e o agravo predominante em volume absoluto, com explosao em **2024** (>19.700 casos confirmados), configurando o maior surto do periodo. A serie historica revela anos de baixa circulacao (2020:2022) seguidos de escalada acentuada.
*   **Chikungunya** exibe dois picos relevantes (2020 e 2024), com predominio consistente do sexo feminino e concentracao em adultos de 40 a 59 anos. A letalidade por chikungunya, embora baixa, esta presente e requer atencao.
*   **Zika** apresenta os menores volumes absolutos e maior sensibilidade ao sub-registro, especialmente apos o fim da emergencia de saude publica internacional (2016:2017). A vigilancia em gestantes segue como eixo critico de interpretacao.
*   A **qualidade da notificacao** e heterogenea: sexo e idade tem boa completude (>99%), enquanto **escolaridade e raca/cor** chegam a ultrapassar 60% de ignorados/brancos em alguns anos, comprometendo analises de desigualdade.
*   O **mapa de dengue por bairro** evidencia concentracao espacial em bairros centrais e populosos, mas a correspondencia entre nomes de bairro da planilha e da malha do IBGE revela **divergencias de grafia e localidades sem poligono** que exigem ajuste manual continuo.
*   A **comparacao direta entre os tres agravos** na mesma plataforma (com mesmo recorte temporal, territorial e metodologico) e uma contribuicao distintiva deste painel frente a outros dashboards publicos.

---

## Metodologia

### Fonte dos dados

| Componente | Fonte | Detalhe |
|:---|---|:---|
| Dengue e Zika (registros individuais) | SINAN-DENGUE e SINAN-ZIKA via pacote [`microdatasus`](https://github.com/rfsaldanha/microdatasus) | Download parametrizavel; cache local em `data/app_cache/` |
| Chikungunya (registros individuais) | SINAN-CHIKUNGUNYA via pacote [`microdatasus`](https://github.com/rfsaldanha/microdatasus) | Download parametrizavel; cache local em `data/app_cache/`; fallback estatico se cache indisponivel |
| Dengue por bairro | Planilhas locais `DENGUEYYYY.xlsx` fornecidas pela **Subsecretaria de Vigilancia Epidemiologica de Campos dos Goytacazes** | `dados_sinan_campos/` (nao versionado) |
| Malha territorial | `geobr` / IBGE: bairros de 2010 | Cache em `neighborhoods_2010_simplified.gpkg` |
| Populacao residente | IBGE/SIDRA: tabela 6579, variavel 9324 | Cache com fallback embutido |

### Criterios de inclusao e definicao de caso

*   **Municipio:** Campos dos Goytacazes (codigo IBGE 3301009).
*   **Periodo:** 2020 a 2025 (ano epidemiologico).
*   **Confirmacao:** Classificacao final compativel com dengue, febre hemorragica da dengue, chikungunya ou Zika conforme padronizacao do SINAN; excluidos registros descartados e inconclusivos (exceto quando analisados separadamente como indicador de qualidade).
*   **Incidencia:** Casos confirmados / populacao estimada x 100.000 habitantes.

### Processamento

O pipeline e integralmente reprodutivel e modular:

```
01_ingestao.R -> 02_limpeza.R -> 03_agregacao.R
04_mapas.R    -> 05_validacao.R -> 06_export_app.R
```

*   A ingestao suporta modo **com e sem download** das APIs externas, controlado pela variavel de ambiente `ARBOVIROSES_DOWNLOAD`.
*   O app Shiny **nao baixa dados no startup**: le exclusivamente caches `.rds` e `.gpkg` pre-processados.
*   A validacao gera automaticamente relatorio de auditoria (`data/audit/relatorio_auditoria.html`) com totais, completude e bairros nao pareados.

### Visualizacao e comunicacao

*   **Graficos interativos** (Plotly) com *hover* detalhado, *download* em alta resolucao (PNG 600 DPI, PDF, SVG, TIFF) e legendas metodologicas.
*   **Mapa de dengue** (Leaflet) com camada de bairros, escala de cores, *popup* com nome e contagem, e controle de camadas.
*   **Painel de qualidade** com indicadores de completude por variavel e alertas automaticos para campos com >30% de ignorados/brancos.
*   **Comparador entre agravos** na pagina inicial com seletor de variavel (ano, sexo, faixa etaria, raca/cor, gestacao).
*   **Filtro global de periodo** na barra lateral que sincroniza todas as abas.

---

## Estrutura do repositorio

```
arboviroses-campos/
├── app.R                        # Entrada modular do Shiny
├── app_arboviroses.R            # App monolitico legado (referencia funcional)
├── R/
│   ├── app_legacy_original.R    # Copia fiel do original pre-modularizacao
│   ├── dados.R                  # Leitura de caches, fallback e estruturas de dados
│   ├── graficos.R               # Funcoes de visualizacao (graficos, qualidade, downloads)
│   ├── mapas.R                  # Malha de bairros, correspondencia espacial, Leaflet
│   ├── packages.R               # Verificacao e carregamento de dependencias
│   ├── server.R                 # Logica reativa do Shiny
│   ├── ui.R                     # Interface do usuario (UI completa)
│   └── utils.R                  # Auditoria, validacao, logs e utilidades
├── scripts/
│   ├── 01_ingestao.R            # Download/leitura de dados brutos (dengue, zika, chikungunya)
│   ├── 02_limpeza.R             # Padronizacao de variaveis e datas
│   ├── 03_agregacao.R           # Agregacao por ano, sexo, idade, raca/cor, gestacao etc.
│   ├── 04_mapas.R               # Preparacao de bases espaciais
│   ├── 05_validacao.R           # Auditoria de totais, completude, bairros nao mapeados
│   ├── 06_export_app.R          # Exportacao dos .rds finais para data/app_cache/
│   ├── update_data.R            # Orquestrador do pipeline completo
│   ├── deploy.R                 # Publicacao no shinyapps.io com protecao contra conta antiga
│   └── fix_encoding_text.R      # Correcao de encoding em textos do projeto
├── data/
│   ├── app_cache/               # Caches processados usados pelo app (versionados)
│   ├── audit/                   # Relatorios automaticos de auditoria (versionados)
│   ├── raw/                     # Dados brutos baixados (nao versionados)
│   ├── interim/                 # Dados limpos intermediarios (nao versionados)
│   └── processed/               # Agregados processados (nao versionados)
├── tests/testthat/              # Testes automatizados (11 testes, 0 falhas)
├── www/style.css                # Estilos incrementais (design visual principal preservado)
├── .github/workflows/           # CI/CD (GitHub Actions)
├── renv.lock                    # Ambiente R reprodutivel
├── LICENSE                      # MIT License
└── README.md
```

---

## Reproducibilidade e portabilidade

O projeto foi concebido para documentar de forma transparente **como o painel foi construido**, oferecendo rastreabilidade completa desde a ingestao dos dados ate a publicacao. Nao se espera que terceiros executem o pipeline integralmente em suas maquinas (pois os dados brutos nao sao publicados e as planilhas locais sao restritas a vigilancia municipal). Contudo, toda a arquitetura de software, os criterios metodologicos, as funcoes de agregacao e os codigos de visualizacao estao disponiveis para **inspecao, auditoria, reproducao conceitual e reuso em contextos analogos**.

Para restaurar o ambiente R e explorar o codigo:

```r
renv::restore()
```

Para executar o pipeline de atualizacao (requer planilhas locais em `dados_sinan_campos/`):

```r
# Sem downloads externos (usa planilhas e caches locais)
source("scripts/update_data.R")

# Com downloads via microdatasus (requer acesso a internet)
Sys.setenv(ARBOVIROSES_DOWNLOAD = "true")
source("scripts/update_data.R")
```

Para iniciar o app localmente:

```r
shiny::runApp()
```

---

## Auditoria e metadados

O pipeline gera automaticamente:

| Arquivo | Conteudo |
|:---|---|
| `data/app_cache/metadata.json` | Data de atualizacao, fontes, anos, versao do pipeline, observacoes metodologicas |
| `data/audit/totais_por_ano_agravo.csv` | Casos confirmados, obitos e ignorados por ano e agravo |
| `data/audit/completude_variaveis.csv` | Percentual de campos ignorados/brancos por variavel, agravo e ano |
| `data/audit/bairros_nao_mapeados.csv` | Bairros da planilha sem correspondencia na malha geobr/IBGE |
| `data/audit/relatorio_auditoria.html` | Relatorio HTML consolidado com alertas metodologicos |

---

## Limitacoes

*   **Notificacao nao e incidencia real.** Subnotificacao, atraso de encerramento, barreiras de acesso ao sistema de saude e mudancas de definicao de caso afetam a contagem. Os numeros representam *registros notificados*, nao o total de casos ocorridos.
*   **Incompletude diferencial.** Campos como escolaridade e raca/cor tem proporcoes elevadas de ignorados/brancos: as distribuicoes observadas nesses recortes devem ser interpretadas com cautela e sempre acompanhadas do indicador de completude.
*   **Incidencia aproximada.** O denominador populacional provem do IBGE/SIDRA (tabela 6579) e pode nao estar disponivel para todos os anos; quando ausente, replica-se o ultimo valor conhecido, o que introduz imprecisao na taxa.
*   **Malha territorial desatualizada.** A malha de bairros do IBGE data de 2010. Mudancas na delimitacao ou nomenclatura de bairros desde entao podem gerar desencontros com os registros das planilhas de notificacao.
*   **Chikungunya em fluxo de migracao.** Diferentemente de dengue e Zika (que ja possuem registros individuais cacheados), os dados de chikungunya estao em processo de migracao para o pipeline via microdatasus/SINAN-CHIKUNGUNYA. Ate a validacao completa com download, utiliza-se fallback estatico pre-validado.
*   **Associacoes descritivas, nao causais.** Todos os graficos e tabelas sao descritivos. Padroes observados nao estabelecem nexo causal e devem ser ponto de partida para investigacoes complementares.

---

## Perspectivas

*   Integracao de **modelos de aprendizado de maquina interpretavel** para identificar combinacoes de fatores associadas a maior risco de notificacao e obito.
*   Ampliacao da cobertura temporal para **series historicas longas** (2010+), permitindo analise de tendencias de longo prazo.
*   Inclusao de **dados climaticos e entomologicos** (indice de infestacao, precipitacao, temperatura) para correlacao ecologica.
*   Expansao da analise espacial para **chikungunya e Zika**, condicionada a disponibilidade de registros por bairro.
*   **Validacao externa** dos achados com a vigilancia municipal e com outros sistemas de informacao (SIM, SIH).
*   Migracao para **modelo de nowcasting** para antecipacao de cenarios de curto prazo.

---

## Declaracao de uso de Inteligencia Artificial

Em conformidade com a **Portaria CNPq no 2.664, de 2 de marco de 2026**, declara-se que as seguintes ferramentas de inteligencia artificial foram utilizadas como suporte durante o desenvolvimento deste projeto:

| Ferramenta | Proposito |
|:---|---|
| DeepSeek-v4-pro | Estruturacao do pipeline de dados, refatoracao de codigo R/Shiny |
| ChatGPT-5.5 | Revisao textual, documentacao tecnica, estruturacao do README |
| Claude Sonnet 4.6 | Auditoria de codigo, diagnostico de codigo morto, testes automatizados |
| Grok | Analise exploratoria inicial, sugestoes de visualizacao |

Todas as saidas geradas por IA foram **revisadas, validadas e adaptadas pelos autores humanos** antes da incorporacao ao projeto. As analises epidemiologicas, a interpretacao dos resultados e as decisoes metodologicas sao de responsabilidade exclusiva da equipe de pesquisa. Nenhum dado pessoal ou sensivel foi submetido a estas ferramentas.

---

## Creditos

| Autor | Afiliacao | Contribuicao | Lattes |
|:---|---|---|:---|
| **Ryan de Paulo Santos** | Bacharelando em Enfermagem, IFF Guarus | Desenvolvimento do aplicativo Shiny, arquitetura do pipeline de dados, analise das visualizacoes, versionamento e publicacao | [Lattes](http://lattes.cnpq.br/7503796642571978) |
| **Brenda Velasco Moreira** | Bacharelanda em Enfermagem, IFF Guarus | Leitura epidemiologica, organizacao cientifica dos dados, revisao dos indicadores | [Lattes](http://lattes.cnpq.br/2823380252102590) |
| **Mirella Guimaraes Lourenco de Souza** | Bacharelanda em Enfermagem, IFF Guarus | Interpretacao dos indicadores de arboviroses, analise critica dos resultados | [Lattes](http://lattes.cnpq.br/1000563712788651) |
| **Marcelly Rangel de Mello** | Bacharelanda em Enfermagem, IFF Guarus | Analise, revisao e comunicacao dos resultados, validacao cientifica | [Lattes](http://lattes.cnpq.br/9803145019193571) |
| **Profa. Dra. Karla Rangel Ribeiro** | Docente e Orientadora, IFF Guarus | Concepcao do projeto, orientacao metodologica, articulacao entre vigilancia em saude e pesquisa aplicada | [Lattes](http://lattes.cnpq.br/6725528158895476) |

**Financiamento:** Conselho Nacional de Desenvolvimento Cientifico e Tecnologico (CNPq), projeto de iniciacao cientifica.

**Contato para correspondencia:** [ryan.paulo@gsuite.iff.edu.br](mailto:ryan.paulo@gsuite.iff.edu.br)

---

## Licenca

Este projeto esta licenciado sob a **MIT License**. Consulte o arquivo [LICENSE](LICENSE) para o texto completo.

(c) 2026 Ryan de Paulo Santos, Brenda Velasco Moreira, Mirella Guimaraes Lourenco de Souza, Marcelly Rangel de Mello, Karla Rangel Ribeiro.
