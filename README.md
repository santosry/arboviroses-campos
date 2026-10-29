# IC — Arboviroses (Campos dos Goytacazes)

Repositório-mãe que organiza o projeto de iniciação científica sobre arboviroses
(dengue, chikungunya e Zika) em Campos dos Goytacazes (RJ).

[![Licença: MIT](https://img.shields.io/badge/licen%C3%A7a-MIT-21918C.svg)](LICENSE)
[![FAIR](https://img.shields.io/badge/princ%C3%ADpios-FAIR-3B528B.svg)](#licença-citação-e-reuso-fair)
[![Acessibilidade](https://img.shields.io/badge/cores-viridis-FDE725.svg)](arboviroses-campos/README.md#acessibilidade-e-visualização)

---

## 📁 Estrutura

| Pasta | Descrição |
|---|---|
| [`arboviroses-campos/`](arboviroses-campos/) | **Projeto A — Painel Shiny.** Análise descritiva (sazonalidade, perfil sociodemográfico, qualidade dos dados e distribuição espacial) das arboviroses 2020–2025. Repo git em `github.com/santosry/arboviroses-campos`. |
| [`conep-2026/`](conep-2026/) | **Projeto B — CONEPE 2026.** Análise estatística da associação entre dengue e temperatura (dados INMET × SINAN). |
| [`_backup/`](_backup/) | Backups de versões antigas (clone do repo do app). |

## 🚀 Links

- **App publicado:** <https://arbovirosescamposiff.shinyapps.io/arboviroses/>
- **Repositório:** <https://github.com/santosry/arboviroses-campos>
- **Instituição:** Instituto Federal Fluminense (IFF), Campus Campos Guarus · CNPq

---

## Licença, citação e reuso (FAIR)

O código deste repositório está sob **licença MIT** (consulte [`LICENSE`](LICENSE)). Os indicadores agregados e as figuras podem ser reutilizados com atribuição à equipe e à fonte original (SINAN/SVS e IBGE/SIDRA). Dados brutos individuais e planilhas locais da vigilância **não** são publicados por restrições de privacidade e governança.

| Princípio | Como é atendido |
|:---|:---|
| **Findable** | README, `CITATION.cff`, metadados em `data/cache_app/metadata.json` e documentação metodológica. |
| **Accessible** | Aplicativo público, repositório aberto, caches agregados e licença MIT. |
| **Interoperable** | CSV/JSON, nomes de variáveis padronizados e módulos R reutilizáveis. |
| **Reusable** | Licença aberta, testes automatizados, scripts de atualização e limites metodológicos explícitos. |

### Como citar

> SANTOS, R. P.; MOREIRA, B. V.; SOUZA, M. G. L.; MELLO, M. R.; RIBEIRO, K. R. **Painel de Inteligência Epidemiológica para Arboviroses em Campos dos Goytacazes (2020-2025)**. Instituto Federal Fluminense, Campus Campos Guarus, 2026. Disponível em: <https://github.com/santosry/arboviroses-campos>. Licença MIT.

### Guia rápido

```bash
git clone https://github.com/santosry/arboviroses-campos.git
cd arboviroses-campos/arboviroses-campos
Rscript -e 'renv::restore()'
Rscript -e 'shiny::runApp()'
```

> **Aviso:** os dados são notificações do SINAN/SVS, não incidência real. Subnotificação, atraso de encerramento e incompletude afetam os indicadores. As análises são descritivas e não estabelecem causalidade.
