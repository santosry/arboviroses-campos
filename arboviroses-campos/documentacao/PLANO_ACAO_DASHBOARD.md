# Plano de ação priorizado — Painel de Arboviroses

**Versão:** 0.2.0 · **Data:** 2026-10-28 · **Escopo:** implementação imediata (sem machine learning)

Este plano organiza as mudanças em blocos de impacto e esforço, com indicadores de verificação. A implementação foi feita diretamente no app Shiny modular (`app.R`, `R/`, `www/`, `scripts/`, `.github/workflows/`).

---

## Bloco A — Essencial, alto impacto e baixo custo

### A1. Disclaimers formais e guardrails de comunicação

- **O que foi feito**
  - Banner global persistente em todas as abas (`global-disclaimer`) com os limites de uso, ausência de causalidade e recomendação de leitura da aba Métodos.
  - Nota metodológica reutilizável (`aviso_uso_dados()`) no topo de **todas** as abas: Início, Chikungunya, Dengue, Zika, Avaliação, Métodos e Equipe.
  - Rodapé persistente com fonte, licença, repositório e contato.
  - Mensagem explícita de que notificação não é incidência real e que os dados não substituem avaliação clínica individual.
- **Onde está**
  - `R/ui.R` (helper, banner, notas por aba, rodapé).
  - `www/style.css` e bloco CSS adicional em `R/ui.R`.
- **Indicador:** 100% das abas com aviso legal/metodológico explícito.
- **Status:** concluído.

### A2. Licença aberta, citação e princípios FAIR

- **O que foi feito**
  - Licença MIT preservada em `LICENSE` (raiz e app).
  - `CITATION.cff` com autores, versão, licença e links.
  - READMEs (raiz e app) com badges, seção “Licença, citação e reuso (FAIR)”, guia rápido de reuso e limites de dados.
  - Documentação do que pode ser reutilizado: código e indicadores agregados sim; dados individuais e planilhas locais não.
  - Metadados atualizados em `data/cache_app/metadata.json` (versão 0.2.0, benchmarks, observações).
- **Indicador:** repositório indexado com licença aberta, citação e nota FAIR explícita.
- **Status:** concluído.

---

## Bloco B — Importante, esforço médio

### B3. Comparação com médias do Estado do RJ e do Brasil

- **O que foi feito**
  - Cache `data/cache_app/benchmarks_uf_br.csv` com casos confirmados, população e incidência por 100 mil para **RJ e Brasil**, 2020–2025, nos três agravos.
  - Linhas tracejadas adicionais nos gráficos de incidência (Plotly e figuras de publicação), com legenda, *hover* e nota de escopo.
  - Script `scripts/08_benchmarks.R` para atualização via `microdatasus` (SINAN nacional, filtro de residência RJ e total Brasil) e denominadores IBGE/SIDRA.
  - Auditoria em `data/audit/benchmarks_uf_br_auditoria.csv`.
- **Definições**
  - Dengue: `CLASSI_FIN` 10, 11 e 12 (dengue, sinais de alarme e dengue grave).
  - Chikungunya: `CLASSI_FIN` 13 (confirmado).
  - Zika: `CLASSI_FIN` 1 (confirmado).
  - População: estimativas IBGE/SIDRA; Censo 2022 para 2022–2023.
- **Indicador:** gráficos de evolução temporal com pelo menos um parâmetro externo de comparação.
- **Status:** concluído.

### B4. Avaliação de usabilidade e co-design

- **O que foi feito**
  - Nova aba **Avaliação** com formulário estruturado:
    - perfil, instituição/território, e-mail opcional e tempo de uso;
    - oito itens Likert (1–5) cobrindo navegação, clareza, utilidade, confiança, limites, documentação, acessibilidade de cores e recomendação;
    - comentários abertos e consentimento.
  - Registro local em `logs/07_feedback.csv`.
  - Envio automático já configurado com FormSubmit (`https://formsubmit.co/ajax/ryan.paulo@gsuite.iff.edu.br`), com ativação única por e-mail; endpoint sobrescrevível por `ARBOVIROSES_FEEDBACK_ENDPOINT`.
  - Fallback de e-mail pré-preenchido e CSV para envio manual.
  - Cálculo de média Likert e download das respostas da sessão.
- **Indicador:** relatório quantitativo de satisfação e usabilidade (média Likert + comentários).
- **Status:** concluído. O primeiro envio exige ativação única do FormSubmit no e-mail de destino. O app tenta o POST pelo navegador com `no-cors` (duas tentativas) e mantém fallback por e-mail/CSV caso o serviço esteja indisponível.

### B5. Automação semestral da carga de dados

- **O que foi feito**
  - Workflow `.github/workflows/atualizacao-semestral.yml` com cron `0 6 1 1,7 *` (janeiro e julho) e disparo manual.
  - Restaura `renv`, executa `scripts/update_data.R`, `scripts/08_benchmarks.R`, testes e publica artefatos de auditoria.
  - Commit automático dos caches e relatórios atualizados, com `permissions: contents: write`.
  - App continua sem download no startup: lê apenas caches versionados.
- **Indicador:** workflow versionado e execução agendada a cada seis meses.
- **Status:** concluído.

---

## Melhorias transversais de visualização e acessibilidade

1. **Paleta viridis** centralizada em `R/utils.R` (`paleta_viridis()`, `CORES_AGRAVOS`, `CORES_SEXO`, `CORES_QUALIDADE`, referências RJ/Brasil).
2. **Gráficos e mapas** migrados para viridis: séries, incidência, sexo, raça/cor, escolaridade, gestação, faixa etária, qualidade, comparador, mapa Leaflet, mapa geobr e figuras de publicação.
3. **Codificação redundante:** rótulos, legendas, posição e texto de apoio; a cor não é o único canal de informação.
4. **Layout mais limpo:** menos sombras, bordas suaves, mais espaço em branco, tipografia e legendas padronizadas, foco visível para teclado.
5. **Notas de benchmark** junto aos gráficos de incidência e seção metodológica dedicada na aba Métodos.
6. **Testes automatizados** cobrindo benchmarks, gráfico comparado e paleta (53 testes, 0 falhas).

---

## Fora do escopo desta versão

- Machine learning / nowcasting / modelos preditivos.
- Atualização dos dados municipais sem executar o pipeline com `ARBOVIROSES_DOWNLOAD=true` (a fase 2 está implementada, mas depende do download prévio).
- Envio de e-mail por SMTP próprio (o formulário usa FormSubmit/endpoint HTTP ou e-mail pré-preenchido).
- Publicação de dados individuais ou planilhas locais restritas.
- Expansão de mapas por bairro para chikungunya e Zika (depende de dados locais).

---

## Critérios de verificação

| Item | Como verificar |
|:---|:---|
| Avisos em todas as abas | Abrir o app e navegar pelas sete abas; banner e nota devem estar visíveis. |
| FAIR/licença | Conferir `README.md`, `LICENSE`, `CITATION.cff` e `data/cache_app/metadata.json`. |
| Benchmarks | Abrir Dengue/Chikungunya/Zika e verificar linhas tracejadas RJ/Brasil e nota de escopo. |
| Feedback | Responder ao formulário, verificar mensagem de status e download CSV; configurar endpoint para envio automático. |
| Automação | Executar o workflow manualmente em Actions e conferir artefatos/commit. |
| Acessibilidade | Inspecionar cores dos gráficos e navegar por teclado; validar contraste e presença de rótulos. |
