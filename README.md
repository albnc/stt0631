# A concretagem de 500 m³

**STT0631 · Logística de Obras Civis** — simulação em aula

> Na terça-feira serão lançados **500 m³ de concreto** na obra do projeto do
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

Script completo: **[`aula-pratica.R`](aula-pratica.R)**

### Os roteiros, em sequência

| Roteiro | Distância | Tempo de viagem | Quando |
|---|---|---|---|
| [`aula-6-passos.R`](aula-6-passos.R) | linha reta, partida em cidade × rodovia | fator declarado, **por trecho** | **primeira aula** — CNEFE, geocodebr, geobr |
| **[Três usinas, quatro tempos](aula-sfnetworks.md)** · [`.R`](aula-sfnetworks.R) | rede viária (`sfnetworks`) | **medido** no grafo, com restrição de veículo | **segunda aula** — a página para copiar e colar |
| [`aula-pratica.R`](aula-pratica.R) | rede viária (`sfnetworks`) | medido, + frota e número de viagens | extensão: as 500 m³ |

O segundo não é uma versão melhor do primeiro: é o primeiro com os **fatores
substituídos por medidas**. A circuidade deixa de ser um número escolhido e
passa a ser um número diferente para cada usina, medido na rede.

Pacotes de dados abertos usados no primeiro: `cnefetools` (endereços),
`geocodebr` (endereço ↔ coordenada) e `geobr` (malha urbana) — todos do IBGE
e do IPEA, todos offline depois do primeiro download.

---

## Antes de começar

```r
pkgs <- c("cnefetools", "sf", "dplyr", "stringr", "janitor", "osmdata",
          "sfnetworks", "tidygraph", "mapview", "ggplot2")
if (length(setdiff(pkgs, rownames(installed.packages()))))
  install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))
```

### Os parâmetros — mude aqui, e só aqui

```r
MUNI       <- 3548906      # código IBGE — São Carlos/SP
OBRA_LON   <- -47.9297     # longitude do canteiro  <-- TROCAR
OBRA_LAT   <- -22.0060     # latitude  do canteiro  <-- TROCAR

VOLUME_M3  <- 500          # volume a lançar
CAP_M3     <- 8            # capacidade do caminhão-betoneira
T_CARGA    <- 10           # min · carregamento na usina
T_DESCARGA <- 15           # min · descarga no canteiro
V_MEDIA    <- 30           # km/h · betoneira carregado em via urbana
JANELA_MIN <- 90           # min · ABNT NBR 7212

UTM <- 31983               # SIRGAS 2000 / UTM 23S — para medir em metros
```

---

## Etapa 1 · Onde está a obra?

**Produto:** um ponto no mapa.

```r
obra <- st_sfc(st_point(c(OBRA_LON, OBRA_LAT)), crs = 4326) |>
  st_sf(geometry = _) |>
  mutate(nome = "Canteiro") |>
  st_transform(UTM)

mapview(obra, col.regions = "black", layer.name = "Obra")
```

Tudo o que vem a seguir é medido a partir deste ponto.

---

## Etapa 2 · Onde estão as usinas de concreto?

**Produto:** uma lista de candidatas, para conferir.

O CNEFE 2022 traz todos os endereços do município, com coordenada. A primeira
execução baixa o arquivo do IBGE — alguns minutos. Depois é instantâneo.

```r
cnefe <- read_cnefe(MUNI, output = "sf", cache = TRUE) |> clean_names()
```

O CNEFE **não tem CNAE**. A atividade econômica aparece como texto livre, no
nome do estabelecimento — então "buscar por atividade" aqui é buscar por
palavra.

```r
candidatas <- cnefe |>
  filter(cod_especie %in% c(3, 6, 8),                 # não-domicílios
         str_detect(toupper(dsc_estabelecimento),
                    "CONCRET|USINA|PREMOLD|PRE-MOLD|ARGAMASSA")) |>
  transmute(nome = dsc_estabelecimento) |>
  st_transform(UTM)

nrow(candidatas)
st_drop_geometry(candidatas)

mapview(candidatas, col.regions = "orange", layer.name = "Candidatas") +
  mapview(obra, col.regions = "black", layer.name = "Obra")
```

> **Isto é uma hipótese, não um cadastro.** Tem falso positivo (uma loja de
> "concertos" de celular) e falso negativo (a usina cujo nome não traz nenhuma
> dessas palavras). Confira a lista antes de seguir.

### A lista conferida

Depois de checar — no próprio CNEFE ou pegando a coordenada no Google Maps —
preencha e rode. Esta lista substitui as candidatas:

```r
usinas <- tibble::tribble(
  ~nome,                ~lon,        ~lat,
  "Usina A",            -47.9000,    -21.9800,
  "Usina B",            -47.8700,    -22.0300,
  "Usina C",            -47.9500,    -21.9600
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(UTM)
```

Enquanto a lista real não existir, o script segue com as candidatas.

---

## Etapa 3 · Qual a distância pela rede viária, e não em linha reta?

**Produto:** tabela reta × rede, e o desvio entre as duas.

Baixa a malha viária que cobre a obra e as usinas, com folga de 3 km:

```r
bb <- rbind(obra["nome"], usinas["nome"]) |>
  st_buffer(3000) |> st_union() |> st_transform(4326) |> st_bbox()

vias <- opq(bb, timeout = 300) |> add_osm_feature("highway") |> osmdata_sf()
```

**Geometria não é topologia.** No OpenStreetMap duas vias podem se cruzar no
mapa sem compartilhar um nó: há cruzamento na tela e nenhuma conexão para o
computador. `to_spatial_subdivision()` cria o nó em cada cruzamento real.

```r
rede <- as_sfnetwork(st_transform(vias$osm_lines, UTM), directed = FALSE) |>
  convert(to_spatial_subdivision, .clean = TRUE) |>
  convert(to_components, .select = 1, .clean = TRUE) |>
  activate("edges") |> mutate(km = as.numeric(edge_length()) / 1000)
```

Um Dijkstra na sua máquina — sem servidor, sem chave de API:

```r
km_rede <- st_network_cost(rede, obra, usinas, weights = "km") |> as.numeric()
km_reta <- as.numeric(st_distance(usinas, obra)) / 1000

tabela <- usinas |>
  st_drop_geometry() |>
  mutate(km_reta    = round(km_reta, 1),
         km_rede    = round(km_rede, 1),
         circuidade = round(km_rede / km_reta, 2)) |>
  filter(is.finite(km_rede)) |>
  arrange(km_rede)

tabela
```

> A **circuidade** é o preço da realidade. Se ela der 1,35, cada quilômetro
> medido na régua custa 1,35 km de caminhão — e um orçamento de frete feito em
> linha reta subestima a quilometragem em 35%.

A rota da usina mais próxima:

```r
rota <- st_network_paths(rede, obra, usinas[which.min(km_rede), ], weights = "km")
geom_rota <- rede |> activate("edges") |> st_as_sf() |>
  slice(unlist(rota$edge_paths[[1]]))

mapview(geom_rota, color = "#156082", layer.name = "Rota") +
  mapview(usinas, col.regions = "orange") +
  mapview(obra, col.regions = "black")
```

---

## Etapa 4 · Quais usinas atendem ao limite de tempo da NBR 7212?

**Produto:** o corte dos 90 minutos.

A norma conta **do início da mistura ao fim da descarga**. Então o relógio é
`carregamento + viagem + descarga`:

```r
tabela <- tabela |>
  mutate(min_viagem = round(km_rede / V_MEDIA * 60),
         min_total  = T_CARGA + min_viagem + T_DESCARGA,
         atende     = min_total <= JANELA_MIN)

tabela

cat("\nUsinas que atendem à janela de", JANELA_MIN, "min:",
    sum(tabela$atende), "de", nrow(tabela), "\n")
```

```r
ggplot(tabela, aes(reorder(nome, -min_total), min_total, fill = atende)) +
  geom_col() +
  geom_hline(yintercept = JANELA_MIN, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#196B24", `FALSE` = "#C00000"),
                    name = "Atende") +
  labs(x = NULL, y = "Carregamento + viagem + descarga (min)",
       title = "O corte da NBR 7212",
       subtitle = paste0("Linha tracejada: ", JANELA_MIN, " min")) +
  theme_minimal(base_size = 12)
```

> Esse número é o tamanho real do seu mercado fornecedor. Se for 1 ou 2, você
> não tem poder de negociação — tem dependência.
>
> A NBR 7212 não é uma regra de projeto: é uma **restrição de roteirização**.
> Norma técnica e problema de otimização são a mesma coisa vista de dois
> lugares diferentes.

---

## Etapa 5 · Quantas viagens e quantos caminhões os 500 m³ exigem?

**Produto:** a ficha da concretagem.

```r
escolhida <- tabela |> filter(atende) |> slice_min(min_total, n = 1)

n_viagens   <- ceiling(VOLUME_M3 / CAP_M3)
ciclo       <- T_CARGA + escolhida$min_viagem + T_DESCARGA + escolhida$min_viagem
n_caminhoes <- ceiling(ciclo / T_DESCARGA)
duracao_h   <- n_viagens * T_DESCARGA / 60

cat(sprintf(
"  Usina             : %s
  Viagem            : %.1f km · %d min
  Dentro da janela  : %d min de %d

  Volume            : %d m³
  Viagens           : %d  (betoneira de %d m³)
  Ciclo do caminhão : %d min
  Frota necessária  : %d caminhões em rodízio
  Duração           : %.1f h de lançamento contínuo
",
  escolhida$nome, escolhida$km_rede, escolhida$min_viagem,
  escolhida$min_total, JANELA_MIN,
  VOLUME_M3, n_viagens, CAP_M3, ciclo, n_caminhoes, duracao_h))
```

### A conta da frota

O **gargalo é a descarga**, porque só um caminhão descarrega por vez. Para não
haver fila nem espera na calha, um caminhão precisa chegar a cada
`T_DESCARGA` minutos — e isso exige:

```
frota = ciclo ÷ tempo de descarga
```

onde `ciclo = carregamento + ida + descarga + volta`.

A duração total não depende da frota, e sim do número de viagens:
`duração = viagens × tempo de descarga`.

```r
if (duracao_h > 8)
  cat("ATENÇÃO: a concretagem não cabe em um turno de 8 h.\n",
      "Opções: aumentar a frota, usar duas usinas, ou dividir em etapas.\n")
```

---

## Para discutir

1. **Dobrar a frota reduz a duração pela metade?** Teste. *(Não reduz: o
   gargalo é a descarga, não o transporte — frota a mais só gera fila.)*
2. Se a usina mais próxima parar, qual é a segunda? Quanto custa trocar?
3. Em que ponto a distância deixa de ser problema de frete e passa a ser
   problema de norma?
4. **O que este modelo não sabe:** mão única, restrição de peso nas vias,
   congestionamento, horário de circulação de caminhão, chuva.

> Modelo bom não é o que acerta. É o que sabe onde erra.
