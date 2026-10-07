# Três usinas, uma obra, quatro tempos

**STT0631 · Logística de Obras Civis** — Escola de Engenharia de São Carlos, USP

> A obra do projeto vai receber concreto usinado. Três usinas da cidade são
> candidatas. **Qual delas chega dentro da janela da NBR 7212** — e por qual
> caminho?

A resposta não é uma distância. É um tempo, e o tempo depende de três coisas
que a régua não vê: o traçado da rede, o veículo que roda nela, e as vias onde
esse veículo **não pode** entrar.

Este roteiro mede as três, uma por vez. Tudo roda na sua máquina — Dijkstra
local, sem servidor de roteirização, sem chave de API, sem cota.

Script completo: **[`aula-sfnetworks.R`](aula-sfnetworks.R)**

---

## Os quatro tempos

| Coluna | O que é | A diferença mede |
|---|---|---|
| `min_reta` | linha reta, 40 km/h | o chute da planilha |
| `min_auto` | rede viária, carro | **preço da rede** — a geometria |
| `min_cam` | rede viária, caminhão | **preço do veículo** |
| `min_restr` | rede **permitida**, caminhão | **preço da regra** |

```
preco_rede      = min_auto  − min_reta
preco_veiculo   = min_cam   − min_auto
preco_restricao = min_restr − min_cam
```

Um fator único soma as três causas num número só, e você não sabe qual manda.
Separadas, cada uma aponta para uma decisão diferente: traçado é problema de
rota, veículo é problema de frota, regra é problema de norma.

---

## Antes de começar

```r
pkgs <- c("cnefetools", "sf", "dplyr", "tidyr", "stringr", "osmdata",
          "sfnetworks", "tidygraph", "ggplot2", "mapview")
if (length(setdiff(pkgs, rownames(installed.packages()))))
  install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))
```

Duas coisas são baixadas na primeira execução e ficam em cache:

- o **CNEFE 2022** do município (IBGE) — alguns minutos
- a **malha viária** do OpenStreetMap no recorte da análise — segundos

Depois disso, tudo é local.

### Os parâmetros — mude aqui, e só aqui

```r
MUNI    <- 3548906           # código IBGE — São Carlos/SP
REF_LON <- -47.903152        # endereço da obra
REF_LAT <- -22.016216

IDX_USINAS  <- c(3128, 6978, 13204)   # as três usinas, por posição em `estab`
COD_USINAS  <- NULL                   # depois: os códigos do CNEFE (ver passo 3)

FOLGA_KM <- 3                # folga do recorte do OSM além dos pontos
PESO_T   <- 30               # t · betoneira de 8 m³ carregado, com tara

V_RETA <- 40                 # km/h · o chute da planilha, em linha reta

T_CARGA    <- 10             # min · carregamento na usina
T_DESCARGA <- 15             # min · descarga no canteiro
JANELA     <- 90             # min · ABNT NBR 7212

UTM <- 31983                 # SIRGAS 2000 / UTM 23S — para medir em metros
```

A mesma via, dois veículos. Caminhão carregado **não é carro lento**: é mais
lento em cada classe, e muito mais nas estreitas, onde manobra e conflito
pesam. Duas tabelas lado a lado deixam a hipótese visível — e auditável.

```r
V_AUTO_CL <- c(motorway = 70, motorway_link = 50, trunk = 65, trunk_link = 45,
               primary  = 50, primary_link  = 40, secondary = 45,
               secondary_link = 35, tertiary = 40, tertiary_link = 30,
               unclassified = 30, residential = 20, living_street = 10,
               service = 10, road = 30)

V_CAM_CL  <- c(motorway = 60, motorway_link = 40, trunk = 55, trunk_link = 35,
               primary  = 40, primary_link  = 30, secondary = 35,
               secondary_link = 25, tertiary = 30, tertiary_link = 22,
               unclassified = 22, residential = 15, living_street =  8,
               service =  8, road = 22)
```

> **Regra deste roteiro:** `tab` nasce na ordem de `usinas` e **nunca** é
> reordenada. Ordenar é coisa de exibição — use `arrange()` só dentro de um
> `print`. Reordenar a tabela e depois atribuir uma coluna por posição não dá
> erro: dá resposta errada.

---

## Passo 1 · Carregar os estabelecimentos

```r
cnefe <- read_cnefe(code_muni = MUNI, output = "sf", cache = TRUE) |>
  st_transform(UTM)

count(st_drop_geometry(cnefe), COD_ESPECIE)
```

As oito espécies de endereço do CNEFE:

| | | | |
|---|---|---|---|
| 1 domicílio particular | 2 coletivo | 3 agropecuário | 4 ensino |
| 5 saúde | **6 outras finalidades** | **7 em construção** | 8 religioso |

Domicílio não compra concreto. Ficamos com 3 a 8.

```r
estab <- cnefe |> filter(COD_ESPECIE %in% 3:8)

COL <- grep("ESTABELEC", names(estab), value = TRUE)[1]
estab$NOME <- toupper(as.character(st_drop_geometry(estab)[[COL]]))

nrow(cnefe); nrow(estab)
```

O nome do estabelecimento é **texto livre** — o CNEFE não tem CNAE. Guardamos
em maiúsculas uma vez, e o `grep` acha a coluna mesmo se o IBGE mudar o nome
exato dela.

---

## Passo 2 · Identificar a obra — espécie 7

```r
obras <- estab |> filter(COD_ESPECIE == 7)
ref   <- st_sfc(st_point(c(REF_LON, REF_LAT)), crs = 4326) |> st_transform(UTM)
obra  <- obras[which.min(st_distance(obras, ref)), ]

cat(sprintf("  canteiros de espécie 7 no município : %d
  distância do endereço ao canteiro   : %.0f m\n",
  nrow(obras), as.numeric(st_distance(obra, ref))))
```

A espécie 7 é um **cadastro georreferenciado de canteiros de obra do Brasil
inteiro**, público desde 2022 e quase não usado. O endereço da obra serve de
referência; o script faz o *snap* para o canteiro mais próximo.

> Se a distância impressa passar de ~100 m, o canteiro do CNEFE não é a sua
> obra — é o vizinho. Nesse caso use `obra <- st_sf(geometry = ref)` e siga.

---

## Passo 3 · Identificar as três usinas — espécie 6

```r
usinas <- if (is.null(COD_USINAS)) estab[IDX_USINAS, ] else
  estab |> filter(COD_UNICO_ENDERECO %in% COD_USINAS)

usinas <- usinas |> select(COD_UNICO_ENDERECO, NOME)
stopifnot(nrow(usinas) == 3, !anyDuplicated(usinas$NOME))

print(st_drop_geometry(usinas))

mapview(usinas, col.regions = "orange", layer.name = "Usinas") +
  mapview(obra, col.regions = "black", layer.name = "Obra")
```

**Copie os `COD_UNICO_ENDERECO` impressos para `COD_USINAS`.** Posição em
`estab` muda se o IBGE revisar o arquivo; o código único não. Análise que não
se reproduz amanhã não é análise.

---

## Passo 4 · A distância em linha reta — o piso

```r
tab <- usinas |>
  st_drop_geometry() |>
  mutate(km_reta  = as.numeric(st_distance(usinas, obra)) / 1000,
         min_reta = km_reta / V_RETA * 60)

tab |> arrange(km_reta)
```

Nenhum caminhão percorre esta distância. Mas a realidade só pode ser **pior**
que ela, nunca melhor — por isso serve de piso, e não de estimativa.

---

## Passo 5 · A rede viária como grafo

Recorte do OpenStreetMap que cobre a obra e as três usinas, com folga:

```r
bb <- rbind(obra["NOME"], usinas["NOME"]) |>
  st_buffer(FOLGA_KM * 1000) |> st_union() |> st_transform(4326) |> st_bbox()

osm <- opq(bb, timeout = 300) |> add_osm_feature("highway") |> osmdata_sf()
```

Tag ausente no recorte não é erro — é `NA`:

```r
pega <- function(x, nm)
  if (nm %in% names(x)) as.character(x[[nm]]) else rep(NA_character_, nrow(x))

L <- osm$osm_lines
L$classe <- as.character(L$highway)
L$mw     <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", pega(L, "maxweight"))))
L$hgv    <- pega(L, "hgv")
```

### As duas restrições

```r
vias <- L |>
  filter(classe %in% names(V_AUTO_CL)) |>        # fora calçada, ciclovia, trilha
  mutate(v_auto = V_AUTO_CL[classe],
         v_cam  = V_CAM_CL[classe],
         # (1) o que o DADO proíbe: placa mapeada no OpenStreetMap
         bloq_tag   = (!is.na(mw) & mw < PESO_T) | (!is.na(hgv) & hgv == "no"),
         # (2) o que a SUA REGRA proíbe: decisão de engenharia, não dado
         bloq_regra = classe == "living_street") |>
  st_transform(UTM)

cat("\n  trechos no recorte :", nrow(vias),
    "\n  bloqueados por TAG :", sum(vias$bloq_tag),
    "\n  bloqueados por REGRA:", sum(vias$bloq_regra), "\n")

count(st_drop_geometry(vias), classe, sort = TRUE)
```

**`bloq_tag`** só é `TRUE` quando existe atributo no OSM dizendo isso —
`maxweight` abaixo das 30 t, ou `hgv = no`. É a restrição **verificável**: tem
autor, histórico de edição, e você pode conferir no Street View. Numa defesa
técnica, é o que você **cita**. O problema é a cobertura: em via urbana
brasileira, `maxweight` é raro.

**`bloq_regra`** é `TRUE` porque **você** afirmou que uma betoneira de 30 t não
entra numa `living_street`. Ninguém mapeou nada. É **completa** — vale para
toda a classe, sem lacuna — e é **opinião**. Numa defesa técnica, é o que você
precisa **justificar**.

Por que separar, se a rede restrita daria o mesmo resultado? Porque fundidas
numa coluna só você perde a capacidade de responder **por que** um trecho está
fora. Essa é a única diferença — e é a que importa num laudo.

```r
table(vias$bloq_tag, vias$bloq_regra, dnn = c("tag", "regra"))
```

| | regra **não** bloqueia | regra bloqueia |
|---|---|---|
| **tag não bloqueia** | trafegável — ou o dado é omisso | **só a sua hipótese** |
| **tag bloqueia** | **só o dado** — placa mapeada | consenso |

- **nenhum dos dois** — você assume que passa. É onde mora o risco de falso
  "liberado": a maior parte da rede cai aqui por omissão do OSM, não por prova.
- **só o dado** — confiança máxima; cite a fonte.
- **só a regra** — tem de aparecer no relatório com a sua assinatura.
- **os dois** — redundante, e por isso **útil como calibração**: é a sua única
  amostra para testar a regra contra a realidade mapeada. Se a regra nunca
  coincide com as tags, ela provavelmente está apontada para a classe errada.

> **Ausência de restrição no dado não é ausência de restrição no mundo.** Se
> 99% das vias não têm peso máximo mapeado, isso não libera 30 t — só diz que
> ninguém mapeou.

### Do `sf` para o grafo

**Geometria não é topologia.** No OpenStreetMap duas vias podem se cruzar na
tela sem compartilhar um nó: há cruzamento na imagem e **nenhuma conexão** para
o computador. `to_spatial_subdivision()` cria o nó em cada cruzamento real.

```r
maior_comp <- function(rede) {
  g  <- rede |> activate("nodes") |> mutate(cmp = group_components())
  tb <- table(st_as_sf(g, "nodes")$cmp)
  g |> filter(cmp == as.integer(names(tb)[which.max(tb)]))
}

monta <- function(x) {
  as_sfnetwork(x, directed = FALSE) |>
    convert(to_spatial_subdivision, .clean = TRUE) |>
    maior_comp() |>
    activate("edges") |>
    mutate(km       = as.numeric(edge_length()) / 1000,
           min_auto = km / v_auto * 60,
           min_cam  = km / v_cam  * 60)
}

rede   <- monta(vias)                                      # tudo
rede_r <- monta(vias |> filter(!bloq_tag, !bloq_regra))    # só o permitido

rede
cat(sprintf("\n  arestas: %d na rede livre, %d na restrita (-%.1f%%)\n",
  igraph::ecount(rede), igraph::ecount(rede_r),
  100 * (1 - igraph::ecount(rede_r) / igraph::ecount(rede))))
```

`maior_comp()` fica com o maior componente conexo. Uma rede OSM recortada tem
dezenas de pedaços soltos; se a análise cair num deles, **todos** os custos
voltam `Inf` e você procura o erro no lugar errado.

---

## Passo 6 · Os quatro tempos

```r
custo <- function(rede, w) as.numeric(st_network_cost(rede, obra, usinas,
                                                      weights = w))

tab <- tab |>
  mutate(km_rede    = custo(rede,   "km"),
         min_auto   = custo(rede,   "min_auto"),
         min_cam    = custo(rede,   "min_cam"),
         min_restr  = custo(rede_r, "min_cam"),
         circuidade = km_rede / km_reta,
         preco_rede      = min_auto  - min_reta,
         preco_veiculo   = min_cam   - min_auto,
         preco_restricao = min_restr - min_cam)

tab |>
  select(NOME, km_reta, km_rede, circuidade,
         min_reta, min_auto, min_cam, min_restr) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(min_restr)

cat(sprintf("\n  circuidade MEDIDA, por usina : %s\n",
  paste(round(tab$circuidade, 2), collapse = " · ")))
```

Aqui está o ganho sobre o modelo de fatores: a **circuidade deixa de ser um
número escolhido** e passa a ser medida, uma por usina. Um fator médio erra
justamente no caso extremo — que é sempre o que decide a concretagem.

> `Inf` em `min_restr` **não é bug: é a resposta.** Significa que o caminhão
> não chega sem violar a restrição que você declarou. Uma restrição que
> desconecta a rede está dizendo algo — sobre a cidade, ou sobre a sua regra.

---

## Passo 7 · Desenhar as rotas

```r
rota_sf <- function(rede, w) {
  p <- st_network_paths(rede, obra, usinas, weights = w)
  a <- rede |> activate("edges") |> st_as_sf()
  do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
    e <- unlist(p$edge_paths[[i]])
    if (!length(e)) return(NULL)
    g <- a[e, "km"]; g$usina <- usinas$NOME[i]; g
  }))
}

rotas_livre <- rota_sf(rede,   "min_cam")
rotas_restr <- rota_sf(rede_r, "min_cam")

mapview(rotas_restr, zcol = "usina", layer.name = "Rota do caminhão") +
  mapview(usinas, col.regions = "orange", layer.name = "Usinas") +
  mapview(obra, col.regions = "black", layer.name = "Obra")
```

A usina que mais sofre com a restrição, com as **duas** rotas no mesmo mapa:

```r
i <- which.max(tab$preco_restricao)
nome_i <- tab$NOME[i]

mapview(rotas_livre[rotas_livre$usina == nome_i, ], color = "#156082",
        layer.name = paste("livre ·", round(tab$min_cam[i]), "min")) +
  mapview(rotas_restr[rotas_restr$usina == nome_i, ], color = "#E97132",
        layer.name = paste("restrita ·", round(tab$min_restr[i]), "min")) +
  mapview(obra, col.regions = "black")
```

Mesma origem, mesmo destino, caminhos diferentes — porque as **regras**
mudaram. Nenhuma explicação verbal faz esse trabalho.

---

## Passo 8 · Comparar e cortar pela NBR 7212

A norma conta **do início da mistura ao fim da descarga**. Então o relógio é
`carregamento + viagem + descarga`:

```r
tab <- tab |> mutate(ciclo  = T_CARGA + min_restr + T_DESCARGA,
                     atende = ciclo <= JANELA)

tab |>
  select(NOME, min_reta, preco_rede, preco_veiculo, preco_restricao,
         min_restr, ciclo, atende) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(min_restr)

cat(sprintf("\n  janela da NBR 7212          : %d min
  descontada carga e descarga : %d min para a viagem
  usinas que atendem          : %d de %d\n",
  JANELA, JANELA - T_CARGA - T_DESCARGA, sum(tab$atende, na.rm = TRUE), nrow(tab)))
```

```r
ORDEM <- c("1 · reta", "2 · rede, carro", "3 · rede, caminhão",
           "4 · rede permitida")

longo <- tab |>
  transmute(NOME,
            `1 · reta`            = min_reta,
            `2 · rede, carro`     = min_auto,
            `3 · rede, caminhão`  = min_cam,
            `4 · rede permitida`  = min_restr) |>
  pivot_longer(-NOME, names_to = "modelo", values_to = "min") |>
  mutate(modelo = factor(modelo, levels = ORDEM))

ggplot(longo, aes(x = min, y = reorder(NOME, min), fill = modelo)) +
  geom_col(position = position_dodge(width = .8), width = .75) +
  geom_vline(xintercept = JANELA - T_CARGA - T_DESCARGA,
             linetype = "dashed", linewidth = .7) +
  scale_fill_manual(values = c("#BBD3DE", "#8FB3C4", "#156082", "#E97132")) +
  labs(x = "Tempo de viagem (min)", y = NULL, fill = NULL,
       title = "A mesma viagem, quatro vezes",
       subtitle = paste0("cada barra acrescenta uma causa: a rede, o veículo, ",
                         "a regra\ntracejado: ", JANELA - T_CARGA - T_DESCARGA,
                         " min — o que resta da janela de ", JANELA,
                         " min após carga e descarga")) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
```

A NBR 7212 não é regra de projeto: lida assim, é uma **restrição de
roteirização**. Norma técnica e problema de otimização são a mesma coisa vista
de dois lugares diferentes.

---

## Passo 9 · Teste N-1 — e se o trecho crítico fechar?

```r
p    <- st_network_paths(rede_r, obra, usinas, weights = "min_cam")
uso  <- table(unlist(p$edge_paths))
crit <- as.integer(names(uso)[which.max(uso)])

arestas <- rede_r |> activate("edges") |> st_as_sf()
rede_n1 <- rede_r |> activate("edges") |> slice(-crit) |> maior_comp()

tab <- tab |> mutate(min_n1    = custo(rede_n1, "min_cam"),
                     atende_n1 = T_CARGA + min_n1 + T_DESCARGA <= JANELA)

tab |> select(NOME, min_restr, min_n1, atende, atende_n1) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

mapview(arestas[crit, ], color = "red", lwd = 6, layer.name = "Trecho crítico") +
  mapview(rotas_restr, zcol = "usina") +
  mapview(obra, col.regions = "black")

cat(sprintf("\n  trecho crítico usado por %d das %d rotas
  usinas que atendiam : %d   ·   depois de fechar o trecho : %d\n",
  max(uso), nrow(usinas),
  sum(tab$atende, na.rm = TRUE), sum(tab$atende_n1, na.rm = TRUE)))
```

O script acha a aresta que as três rotas mais compartilham, apaga, e recalcula.
Se alguma usina sai da janela, a obra tem um **ponto único de falha viário** —
e isso é cláusula de contrato, não engenharia de tráfego.

---

## Para discutir

1. **A circuidade medida bateu com o 1,35 que escolhemos?** Onde errou mais?
2. **Dos três preços — rede, veículo, regra — qual foi o maior?** A resposta
   muda a conversa: um é problema de traçado, outro de frota, outro de norma.
3. **`preco_restricao` igual a zero:** a restrição não morde, ou o OSM não a
   mapeou? As duas hipóteses dão a mesma coluna e decisões opostas.
4. **No teste N-1**, se alguma usina sai da janela, a obra tem um fornecedor
   viável e um trecho de via como ponto único de falha.
5. **Por que não um serviço de roteirização** (OSRM, Google, r5r)? Um serviço
   responde *"qual a rota"*. Só um grafo na sua máquina deixa você abrir a
   rede, ler o atributo de cada trecho, **proibir a passagem de um veículo** e
   **arrancar uma ponte** para ver o que acontece. Nenhum deles tem modo
   "caminhão", nem argumento de peso, altura ou número de eixos.

### O que este modelo não sabe

Declarar o limite é parte da competência que a disciplina ensina.

- A rede é montada **não dirigida** — mão única foi ignorada.
- Não há restrição de conversão nem raio de giro.
- Restrição de caminhão costuma valer em **faixas horárias**; o modelo é estático.
- A velocidade é **de projeto**, não observada — não há congestionamento.
- O CNEFE é uma fotografia de 2022, e obra é fenômeno volátil.
- `hgv = destination` significa "pesado só com destino local" — uma betoneira
  que vai *para* aquela obra não é proibida, mas quem atravessa é. O modelo
  estático não distingue passagem de destino.

> **Modelo bom não é o que acerta. É o que sabe onde erra.**

---

## Dados

| Fonte | O que entrega | Pacote |
|---|---|---|
| [CNEFE 2022 — IBGE](https://www.ibge.gov.br/estatisticas/sociais/populacao/38734-cadastro-nacional-de-enderecos-para-fins-estatisticos.html) | 111 milhões de endereços com coordenada, espécie e nome | [`cnefetools`](https://cran.r-project.org/package=cnefetools) |
| [OpenStreetMap](https://www.openstreetmap.org) | rede viária com as placas de restrição | [`osmdata`](https://cran.r-project.org/package=osmdata) |
| — | a rede como grafo: matriz, rota, N-1 | [`sfnetworks`](https://cran.r-project.org/package=sfnetworks) + [`tidygraph`](https://cran.r-project.org/package=tidygraph) |

Material didático sob [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.pt-br).
Verifique sempre a **edição vigente** da ABNT NBR 7212.
