
# 📍 pbc_pop: Análise Espacial e Populacional do Boto-Cinza em Cananéia (SP)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Este repositório reúne o código, os dados e a metodologia para a análise espacial e populacional do boto-cinza (*Sotalia guianensis*) no estuário de Cananéia, litoral sul de São Paulo. O projeto combina dados de marcação e recaptura, posicionamento geográfico e análise de uso de habitat, com o objetivo de apoiar a conservação da espécie e a gestão da área.

---

## 🎯 Objetivos principais

- Estimar parâmetros populacionais (ex.: abundância, taxas de sobrevivência) com base em um esforço contínuo de campo.
- Analisar padrões de uso espacial da população, incluindo áreas de maior ocorrência (hotspots).
- Produzir mapas temáticos e outputs replicáveis para relatórios técnicos e artigos científicos.

---

## 📂 Estrutura do repositório

```
pbc_pop/
├── 01_dados/              # Bases brutas (marcações, recapturas, effort, shapefiles, etc.)
├── 02_scripts/            # Scripts R para limpeza, análise e visualização
├── 03_resultados/         # Saídas intermediárias (ex.: RDS, CSVs processados)
├── 04_figuras/            # Mapas e gráficos finais para publicação
├── 05_relatorios/         # Relatórios Markdown, LaTeX, ou Word
└── README.md              # Este arquivo
```

---

## 🛠️ Principais ferramentas utilizadas

- **Linguagem:** R
- **Pacotes principais:**  
  `dplyr`, `tidyr`, `sf`, `ggplot2`, `RMark`, `spatstat`, `raster`, `leaflet`, entre outros.
- **Ambiente de análise:** R + MARK

---

## 🧭 Fluxo de trabalho

1. **Importação e limpeza de dados**  
   Scripts para padronização de planilhas de campo e shapefiles.

2. **Análise populacional (Capture-Recapture / SECR)**  
   Modelagem de abundância e densidade com métodos espacialmente explícitos.

3. **Análise de uso de espaço**  
   Geração de mapas de densidade kernel, polígonos de áreas de uso (50%, 95%) e hotspots.

4. **Visualização e relatórios finais**  
   Produção de gráficos, tabelas e mapas para divulgação científica e técnica.

---

## 📌 Status atual

✅ Estruturação das bases de dados  
✅ Scripts de limpeza e preparação  
✅ Modelagem populacional preliminar  
✅ Análises espaciais em andamento  
🔜 Integração completa dos outputs em relatório reprodutível

---

## 🧑‍💻 Como reproduzir

> **Pré-requisitos:** R ≥ 4.x e RStudio

Clone o repositório:

```bash
git clone https://github.com/eric-medeiros/pbc_pop.git
```

Abra o projeto `.Rproj` e execute os scripts na sequência indicada dentro da pasta `/02_scripts/`.

---

## 📄 Licença

Este projeto está licenciado sob os termos da licença MIT. Veja o arquivo [`LICENSE`](LICENSE) para mais detalhes.

---

## 📬 Contato

Eric Medeiros  
[LinkedIn](https://www.linkedin.com/in/eric-medeiros-289a80246/) | [GitHub](https://github.com/eric-medeiros)
