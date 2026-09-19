# Registro de alterações — Mapa de Dengue por Bairro

**Projeto:** Padrões temporais e perfil sociodemográfico de dengue, chikungunya e Zika em Campos dos Goytacazes (2020–2025)  
**Data:** 2026-08-11  
**Autor das alterações:** Ryan Paulo Santos (com assistência de IA)

---

## 1. Objetivo

Substituir o mapa de bairros baseado exclusivamente na malha do `geobr` (IBGE 2010) — que **não cobre todos os bairros** de Campos dos Goytacazes — por uma solução com cobertura completa usando **setores censitários + Leaflet interativo**.

---

## 2. Arquivos modificados

| Arquivo | Tipo de alteração |
|---|---|
| `R/packages.R` | Adicionados `geobr`, `terra`, `tidyterra`, `ggrepel` às dependências |
| `R/mapas.R` | +7 novas funções; `normalizar_chave_bairro` reescrita |
| `R/dados.R` | `normalizar_bairro` reescrita com normalização robusta |
| `R/server.R` | Novo `renderLeaflet` com setores + pontos; removido código legado de correspondência |
| `R/graficos.R` | `painel_correspondencia_bairros` atualizado para tabela de geolocalização |
| `scripts/04_mapas.R` | Adicionado cache de setores censitários + pré-geocodificação |
| `scripts/06_export_app.R` | Adicionada cópia de novos caches |
| `data/app_cache/` | Novos arquivos: `setores_censitarios_campos_2010.rds`, `bairros_geocoded.csv`, `dengue_bairros_campos_v1.rds` (reconstruído) |
| `data/audit/` | Novo: `totais_por_bairro.csv` |

### Arquivos novos criados (scripts auxiliares)

| Script | Finalidade |
|---|---|
| `scripts/rebuild_bairros.R` | Reconstrução dos dados de bairro com normalização |
| `scripts/geocode_bairros.R` | Geocodificação de bairros via Nominatim |
| `scripts/geocode_top100.R` | Geocodificação apenas dos 100 bairros prioritários |
| `scripts/gerar_mapa_preview.R` | Geração do mapa Leaflet para preview |
| `scripts/catalogar_bairros.R` | Catálogo de variações de grafia |
| `scripts/check_coords.R` | Verificação de coordenadas |
| `scripts/check_xlsx.R` | Inspeção das planilhas fonte |
| `scripts/estatisticas_mapa.R` | Estatísticas de cobertura |

---

## 3. Normalização de nomes de bairros

### Problema
As planilhas `DENGUEYYYY.xlsx` (fornecidas pela Subsecretaria de Vigilância Epidemiológica) continham **884 variações de grafia** para nomes de bairros. Exemplos:

- `PQ AURORA` vs `PARQUE AURORA` vs `Parque Aurora`
- `JD CARIOCA` vs `JARDIM CARIOCA`
- `ALPHAVILLE` vs `ALFAVILLE` vs `ALPHVILLE 2`
- `GOYTACASES` vs `GOYTACAZES` vs `Goitacazes`
- `VILA MANHAES` vs `VILA MANHHAES` vs `VILA MANAHES`
- `NOVO JOCKEY` vs `PQ JOCKEY CLUB` vs `PARQUE JOQUEI CLUB`

### Solução
Função `normalizar_bairro_v2` (em `scripts/rebuild_bairros.R`) e atualização de `normalizar_bairro` em `R/dados.R` e `normalizar_chave_bairro` em `R/mapas.R`:

1. Conversão para ASCII (remoção de acentos)
2. `toupper` para uniformizar caixa
3. Remoção de pontuação
4. Expansão de abreviações: `PQ` → `PARQUE`, `JD` → `JARDIM`, `VL` → `VILA`
5. Correção de grafias específicas: `ALPHAVILE` → `ALPHAVILLE`, `JOQUEI` → `JOCKEY`, `GOYTACASES` → `GOITACAZES`, `MANHHAES` → `MANHAES`, `CARIOVA` → `CARIOCA`, etc.
6. Normalização de numerais romanos: `II` → `2`, `III` → `3`

### Resultado
- **884** nomes originais → **826** nomes normalizados (58 variantes eliminadas)
- "Alto Da Boa Vista" (que o usuário procurava) foi encontrado e normalizado corretamente

---

## 4. Pipeline de dados espaciais

### Fonte da malha
- **Setores censitários do IBGE 2010** via `geobr::read_census_tract(code_tract = 3301009)`
- **665 feições** com cobertura **100% do município**
- Cache em `data/app_cache/setores_censitarios_campos_2010.rds`

### Obtenção de coordenadas dos bairros

Duas fontes, em ordem de prioridade:

1. **Centroides geobr** — para bairros que existem na malha de bairros do IBGE 2010 (≈82 bairros)
2. **Geocodificação Nominatim/OSM** — para os demais, via API `nominatim.openstreetmap.org` com cache em `data/app_cache/bairros_geocoded.csv` (≈100 bairros prioritários)

### Validação espacial
Coordenadas obtidas são validadas contra o polígono do município (`sf::st_within`). Pontos fora dos limites de Campos são **descartados automaticamente**.

---

## 5. Mapa Leaflet interativo

### Função principal
`mapa_leaflet_setores_pontos()` em `R/mapas.R`

### Camadas
| Camada | Descrição |
|---|---|
| **Base clara** (CartoDB.Positron) | Tile layer padrão |
| **OpenStreetMap** | Alternativa de tile layer |
| **Setores censitários** | Polígonos cinza claro, baixa opacidade |
| **Dengue por bairro** | Círculos proporcionais a √Casos, paleta de vermelhos, popups com nome e contagem |

### Controles
- Seletor de camadas (base + overlay)
- Escala em km
- Popups e labels ao passar o mouse

---

## 6. Resultados

### Dados reconstruídos
| Métrica | Valor |
|---|---|
| Total de registros | 37.752 |
| Bairros únicos (normalizados) | 826 |
| Bairros com coordenadas | 131 |
| Cobertura de casos | **89,5%** (33.790 / 37.752) |
| Bairros sem coordenadas | 695 (maioria < 5 casos) |

### Top 20 bairros por casos (2020–2025)
| Bairro | Casos |
|---|---|
| Parque Jockey Club | 2.572 |
| Goitacazes | 1.971 |
| Centro | 1.939 |
| Travessao | 1.565 |
| Farol De Sao Tome | 1.553 |
| Parque Aurora | 1.157 |
| Parque Penha | 1.023 |
| Parque Guarus | 905 |
| Parque Turf Club | 817 |
| Tocos | 797 |
| Donana | 703 |
| Jardim Carioca | 697 |
| Parque Prazeres | 690 |
| Parque Eldorado | 584 |
| Parque Leopoldina | 583 |
| Parque Rosario | 570 |
| Parque California | 568 |
| Parque Santa Rosa | 553 |
| Parque Esplanada | 517 |
| Tapera | 500 |

### Bairros importantes ainda sem coordenadas
(Precisam de coordenadas manuais para atingir >95% de cobertura)

| Bairro | Casos |
|---|---|
| Parque Aeroporto | 421 |
| Parque Calabouco | 398 |
| Parque Lebret | 156 |
| Parque Tropical | 142 |
| Jardim Ceasa | 116 |
| Parque Jardim Carioca | 111 |

---

## 7. Como continuar amanhã

### Para adicionar coordenadas manuais dos bairros faltantes:
Editar o arquivo `data/app_cache/bairros_geocoded.csv` e adicionar linhas com `lat` e `lon` para cada bairro.

### Para rodar o app localmente:
```r
shiny::runApp()
```

### Para atualizar o renv (já que novos pacotes foram adicionados):
```r
renv::snapshot()
```

### Para visualizar o mapa atual:
Abrir `mapa_dengue_leaflet_preview.html` no navegador.

### Scripts disponíveis:
```r
source("scripts/rebuild_bairros.R")     # Reconstruir dados de bairros
source("scripts/geocode_top100.R")      # Geocodificar top 100 bairros
source("scripts/gerar_mapa_preview.R")  # Gerar preview HTML do mapa
```

---

## 8. Notas técnicas

- O pacote `tidygeocoder` causava **segfault no R 4.6.0** e foi substituído por chamada direta à API Nominatim via `jsonlite::fromJSON()`
- O pacote `tidyterra` permanece instalado mas **não é mais usado para o mapa final** (apenas `terra` para conversão de SpatVector)
- O `ggrepel` permanece instalado caso se queira reativar o mapa estático ggplot2
- A geocodificação via Nominatim tem limite de ~1 requisição/segundo; a geocodificação completa dos 800+ bairros levaria ~15 minutos
