# A concretagem de 500 m³

**STT0631 · Logística de Obras Civis** — simulação em aula

> Amanhã serão lançados **500 m³ de concreto** na obra do projeto do
> curso. De qual usina ele vem, quantas viagens são necessárias, e quantos
> caminhões precisam estar girando para que a laje não "pegue" no meio?

Cinco etapas. Cada uma tem **uma pergunta** e **um produto**. Copie, cole, rode.

| | Pergunta | Produto |
|---|---|---|
| 1 | Onde está a obra? | um ponto no mapa |
| 2 | Onde estão as usinas de concreto? | uma lista de candidatas |
| 3 | Qual a distância pela rede viária? | tabela reta × rede |
| 4 | Quais atendem à NBR 7212? | o corte dos 90 minutos |
| 5 | Quantas viagens e quantos caminhões? | a ficha da concretagem |

Script completo: **[`aula-pratica.R`](aula-sfnetworks.R)**


Pacotes de dados abertos usados: `cnefetools` (endereços),
`geocodebr` (endereço ↔ coordenada) e `geobr` (malha urbana) — todos do IBGE
e do IPEA, todos offline depois do primeiro download.


![cnefetools](https://pedreirajr.github.io/cnefetools/logo.svg)
![geobr](https://raw.githubusercontent.com/ipea/geobr/refs/heads/master/r-package/man/figures/geobr_logo_y.png)
---
[Material da aula](aula-sfnetworks.md)
---
