# ==============================================================================
# STT0631 · Logística de Obras Civis — prática em aula
# Escola de Engenharia de São Carlos · USP
#
# TRÊS USINAS, UMA OBRA, QUATRO TEMPOS
# Do endereço à rota medida na rede viária, com restrição de veículo.
#
#   1. Carregar os estabelecimentos          CNEFE 2022 / IBGE
#   2. Identificar a obra                    espécie 7
#   3. Identificar as três usinas            espécie 6
#   4. Medir a distância em linha reta       o piso
#   5. Montar a rede viária como GRAFO       osmdata + sfnetworks
#   6. Medir os quatro tempos                reta · rede · caminhão · restrito
#   7. Desenhar as rotas                     o que o número não mostra
#   8. Comparar e cortar pela NBR 7212       a decisão
#   9. Teste N-1                             e se o trecho crítico fechar?
#
# Tudo roda na sua máquina: Dijkstra local, sem servidor e sem chave de API.
# A diferença entre este arquivo e o passo 6 do anterior é que aqui NADA é
# fator escolhido — a circuidade é medida, e a restrição é uma aresta apagada.
# ==============================================================================

pkgs <- c("cnefetools", "sf", "dplyr", "tidyr", "stringr", "osmdata",
          "sfnetworks", "tidygraph", "ggplot2", "mapview")
if (length(setdiff(pkgs, rownames(installed.packages()))))
  install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))


## ─── PARÂMETROS · mude aqui, e só aqui ───────────────────────────────────────

MUNI    <- 3548906           # código IBGE — São Carlos/SP
REF_LON <- -47.903152        # Av. Comendador Alfredo Maffei, 1310
REF_LAT <- -22.016216

# As três usinas conferidas em aula, por posição em `estab`.
IDX_USINAS  <- c(3128, 6978, 13204)
# Índice do CNEFE muda se o IBGE revisar o arquivo. Depois de rodar uma vez,
# copie os códigos impressos no passo 3 para cá e troque a linha de seleção.
COD_USINAS  <- NULL          # ex.: c("3548906...", "3548906...", "3548906...")

FOLGA_KM <- 3                # folga do recorte do OSM além dos pontos
PESO_T   <- 30               # t · betoneira de 8 m³ carregado, com tara

# Velocidade por classe de via — a MESMA via, dois veículos diferentes.
# Caminhão carregado não é carro lento: é mais lento em cada classe, e muito
# mais nas estreitas, onde manobra e conflito pesam.
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

V_RETA <- 40                 # km/h · o chute da planilha, em linha reta

T_CARGA    <- 10             # min · carregamento na usina
T_DESCARGA <- 15             # min · descarga no canteiro
JANELA     <- 90             # min · ABNT NBR 7212 (conferir edição vigente)

UTM <- 31983                 # SIRGAS 2000 / UTM 23S — para medir em metros

# REGRA DESTE ARQUIVO: `tab` nasce na ordem de `usinas` e NUNCA é reordenada.
# Ordenar é coisa de exibição: use arrange() só dentro de um print.


# ==============================================================================
# PASSO 1 · Carregar os estabelecimentos
# ==============================================================================

cnefe <- read_cnefe(code_muni = MUNI, output = "sf", cache = TRUE) |>
  st_transform(UTM)

count(st_drop_geometry(cnefe), COD_ESPECIE)

# 1 domicílio particular · 2 coletivo · 3 agropecuário · 4 ensino · 5 saúde
# 6 OUTRAS FINALIDADES · 7 EM CONSTRUÇÃO · 8 religioso
estab <- cnefe |> filter(COD_ESPECIE %in% 3:8)

COL <- grep("ESTABELEC", names(estab), value = TRUE)[1]
estab$NOME <- toupper(as.character(st_drop_geometry(estab)[[COL]]))

nrow(cnefe); nrow(estab)


# ==============================================================================
# PASSO 2 · Identificar a obra — espécie 7
# ==============================================================================

obras <- estab |> filter(COD_ESPECIE == 7)
ref   <- st_sfc(st_point(c(REF_LON, REF_LAT)), crs = 4326) |> st_transform(UTM)
obra  <- obras[which.min(st_distance(obras, ref)), ]

cat(sprintf("  canteiros de espécie 7 no município : %d
  distância do endereço ao canteiro   : %.0f m\n",
  nrow(obras), as.numeric(st_distance(obra, ref))))

# Se essa distância passar de ~100 m, o canteiro do CNEFE não é a sua obra —
# é o vizinho. Nesse caso use `obra <- st_sf(geometry = ref)` e siga.


# ==============================================================================
# PASSO 3 · Identificar as três usinas — espécie 6
# ==============================================================================

usinas <- if (is.null(COD_USINAS)) estab[IDX_USINAS, ] else
  estab |> filter(COD_UNICO_ENDERECO %in% COD_USINAS)

usinas <- usinas |> select(COD_UNICO_ENDERECO, NOME)
stopifnot(nrow(usinas) == 3, !anyDuplicated(usinas$NOME))

# Copie estes códigos para COD_USINAS e a seleção fica reproduzível.
print(st_drop_geometry(usinas))

mapview(usinas, col.regions = "orange", layer.name = "Usinas") +
  mapview(obra, col.regions = "black", layer.name = "Obra")


# ==============================================================================
# PASSO 4 · A distância em linha reta — o piso
# ==============================================================================

tab <- usinas |>
  st_drop_geometry() |>
  mutate(km_reta  = as.numeric(st_distance(usinas, obra)) / 1000,
         min_reta = km_reta / V_RETA * 60)

tab |> arrange(km_reta)

# Nenhum caminhão percorre esta distância. Mas a realidade só pode ser pior
# que ela, nunca melhor — por isso serve de piso, e não de estimativa.


# ==============================================================================
# PASSO 5 · A rede viária como grafo
# ==============================================================================

bb <- rbind(obra["NOME"], usinas["NOME"]) |>
  st_buffer(FOLGA_KM * 1000) |> st_union() |> st_transform(4326) |> st_bbox()

osm <- opq(bb, timeout = 300) |> add_osm_feature("highway") |> osmdata_sf()

# Tag ausente no recorte não é erro: é NA.
pega <- function(x, nm)
  if (nm %in% names(x)) as.character(x[[nm]]) else rep(NA_character_, nrow(x))

L <- osm$osm_lines
L$classe <- as.character(L$highway)
L$mw     <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", pega(L, "maxweight"))))
L$hgv    <- pega(L, "hgv")

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

# O cruzamento das duas restrições. Cada quadrante pede uma ação diferente:
#   nenhum dos dois -> trafegável, OU o dado é omisso (é a maior parte)
#   só a tag        -> placa mapeada: confiança máxima, cite a fonte
#   só a regra      -> sua hipótese: tem de ir ao relatório assinada
#   os dois         -> redundante, e por isso útil para CALIBRAR a regra
table(vias$bloq_tag, vias$bloq_regra, dnn = c("tag", "regra"))

# Se quase nenhum trecho tem maxweight, isso NÃO libera 30 t — só diz que
# ninguém mapeou. Ausência de restrição no dado não é ausência no mundo.
# Por isso bloq_regra existe separado: ali você assume a autoria da hipótese.

## ─── do sf para o grafo ──────────────────────────────────────────────────────
## GEOMETRIA NÃO É TOPOLOGIA: no OSM duas vias podem se cruzar na tela sem
## compartilhar um nó. Para o computador, não há conexão nenhuma.
## to_spatial_subdivision() cria o nó em cada cruzamento real.

maior_comp <- function(rede) {
  g <- rede |> activate("nodes") |> mutate(cmp = group_components())
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


# ==============================================================================
# PASSO 6 · Os quatro tempos
#
# Cada coluna isola UMA causa. É isso que um fator único não consegue fazer.
#
#   min_reta   -> reta,  velocidade única      o chute da planilha
#   min_auto   -> rede,  carro                 o custo da GEOMETRIA da rede
#   min_cam    -> rede,  caminhão              o custo do VEÍCULO
#   min_restr  -> rede permitida, caminhão     o custo da REGRA
# ==============================================================================

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

cat(sprintf("\n  circuidade MEDIDA, por usina : %s
  (no arquivo anterior era 1,35 para todas, escolhido no slide)\n",
  paste(round(tab$circuidade, 2), collapse = " · ")))

# Inf em min_restr não é bug: é a resposta. O caminhão não chega sem violar a
# restrição declarada. Restrição que desconecta a rede está dizendo algo —
# sobre a cidade, ou sobre a sua regra.


# ==============================================================================
# PASSO 7 · Desenhar as rotas
# ==============================================================================

rota_sf <- function(rede, w) {
  p <- st_network_paths(rede, obra, usinas, weights = w)
  a <- rede |> activate("edges") |> st_as_sf()
  do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
    e <- unlist(p$edge_paths[[i]])
    if (!length(e)) return(NULL)
    g <- a[e, "km"]; g$usina <- usinas$NOME[i]; g
  }))
}

rotas_livre  <- rota_sf(rede,   "min_cam")
rotas_restr  <- rota_sf(rede_r, "min_cam")

mapview(rotas_restr, zcol = "usina", layer.name = "Rota do caminhão") +
  mapview(usinas, col.regions = "orange", layer.name = "Usinas") +
  mapview(obra, col.regions = "black", layer.name = "Obra")

# A usina que mais sofre com a restrição: as duas rotas no mesmo mapa.
i <- which.max(tab$preco_restricao)
nome_i <- tab$NOME[i]

mapview(rotas_livre[rotas_livre$usina == nome_i, ], color = "#156082",
        layer.name = paste("livre ·", round(tab$min_cam[i]), "min")) +
  mapview(rotas_restr[rotas_restr$usina == nome_i, ], color = "#E97132",
        layer.name = paste("restrita ·", round(tab$min_restr[i]), "min")) +
  mapview(obra, col.regions = "black")

# Mesma origem, mesmo destino, caminhos diferentes — porque as REGRAS mudaram.
# Nenhuma explicação verbal faz este trabalho.


# ==============================================================================
# PASSO 8 · Comparar e cortar pela NBR 7212
# ==============================================================================

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


# ==============================================================================
# PASSO 9 · Teste N-1 — e se o trecho crítico fechar?
# ==============================================================================

# Qual aresta é mais usada pelas três rotas ao mesmo tempo?
p    <- st_network_paths(rede_r, obra, usinas, weights = "min_cam")
uso  <- table(unlist(p$edge_paths))
crit <- as.integer(names(uso)[which.max(uso)])

arestas  <- rede_r |> activate("edges") |> st_as_sf()
rede_n1  <- rede_r |> activate("edges") |> slice(-crit) |> maior_comp()

tab <- tab |> mutate(min_n1 = custo(rede_n1, "min_cam"),
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


# ==============================================================================
# PARA DISCUTIR
#
# 1. A circuidade medida bateu com o 1,35 que escolhemos? Onde errou mais?
#    Fator médio erra justamente no caso extremo — que é o que decide a obra.
#
# 2. Dos três preços — rede, veículo, regra — qual foi o maior? A resposta
#    muda a conversa: um é problema de traçado, outro de frota, outro de norma.
#
# 3. preco_restricao igual a zero: a restrição não morde, ou o OSM não a
#    mapeou? As duas hipóteses dão a mesma coluna e decisões opostas.
#
# 4. No teste N-1, se alguma usina sai da janela, a obra tem UM fornecedor
#    viável e um trecho de via como ponto único de falha. Isso é contrato,
#    não engenharia de tráfego.
#
# 5. O que esta rede ainda não sabe: mão única (montamos não dirigida),
#    restrição de conversão, raio de giro, horário de circulação de caminhão,
#    congestionamento e chuva.
#
# Modelo bom não é o que acerta. É o que sabe onde erra.
# ==============================================================================
