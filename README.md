# Arboviroses em Campos dos Goytacazes

Aplicativo Shiny para visualizacao, auditoria e comunicacao de indicadores de dengue, zika e chikungunya em Campos dos Goytacazes, RJ.

App publicado: <https://arbovirosescamposiff.shinyapps.io/arboviroses/>

## Fontes dos Dados

- SINAN/SVS, via `microdatasus` quando `ARBOVIROSES_DOWNLOAD=true`.
- Planilhas locais `DENGUEYYYY.xlsx` para componentes historicos e bairros.
- IBGE/SIDRA para denominadores populacionais quando cacheados.
- Malha de bairros via `geobr`, preparada previamente pelo pipeline.

Os dados brutos e `dados_sinan_campos/outputs` nao devem ser publicados. O app publicado usa apenas caches processados em `data/app_cache/`.

## Estrutura

- `app.R`: entrada modular do Shiny.
- `R/`: funcoes de dados, graficos, mapas, UI, servidor e utilitarios.
- `www/style.css`: estilos incrementais; o desenho visual principal foi preservado.
- `scripts/`: pipeline de ingestao, limpeza, agregacao, mapas, validacao, exportacao e deploy.
- `data/app_cache/`: arquivos processados leves usados pelo app.
- `data/audit/`: tabelas e relatorio automatico de auditoria.
- `tests/testthat/`: testes minimos do app.
- `renv.lock`: pacotes diretos usados pelo projeto.

## Restaurar Ambiente

```r
renv::restore()
```

Se o `renv::snapshot()` completo for necessario, rode em uma sessao local estavel. Este projeto inclui um `renv.lock` minimo com os pacotes diretos do app/pipeline.

## Rodar Localmente

```r
shiny::runApp()
```

O app nao baixa dados no startup. Se algum cache final estiver ausente, rode o pipeline antes.

## Atualizar Dados

Sem downloads externos, usando caches/planilhas locais:

```r
source("scripts/update_data.R")
```

Com downloads via `microdatasus`:

```r
Sys.setenv(ARBOVIROSES_DOWNLOAD = "true")
source("scripts/update_data.R")
```

A etapa `01_ingestao.R` inclui dengue, zika e chikungunya, com chikungunya configurada para `SINAN-CHIKUNGUNYA`.

## Publicar no shinyapps.io

Configure a conta localmente, sem gravar segredos no repositorio:

```r
rsconnect::setAccountInfo(name = "arbovirosescamposiff", token = "<TOKEN>", secret = "<SECRET>")
```

Depois publique:

```r
source("scripts/deploy.R")
```

O script verifica contas do `rsconnect`, evita publicar na conta antiga, envia somente arquivos necessarios e tenta validar a URL final com HTTP 200.

## Auditoria

O pipeline gera:

- `data/app_cache/metadata.json`
- `data/audit/totais_por_ano_agravo.csv`
- `data/audit/completude_variaveis.csv`
- `data/audit/bairros_nao_mapeados.csv`
- `data/audit/relatorio_auditoria.html`

## Limitacoes

- Dados de vigilancia podem conter atraso, revisoes e campos ignorados/brancos.
- Incidencia por 100 mil depende de denominador populacional confiavel.
- A correspondencia por bairro depende de grafia, padronizacao e disponibilidade de poligonos na malha.
- O app preserva o design atual; melhorias visuais devem ser incrementais.

## Licenca

Definir antes de publicacao ampla do repositorio.
