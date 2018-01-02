require(rvest)
require(httr)
require(dplyr)
require(purrr)
require(silgelib)
require(tidyverse)
require(stringr)
require(grid)
require(gridExtra)

regioes <- c(
  "Acre" = 12, 
  "Alagoas"= 27, 
  "Amapá" = 16, 
  "Amazonas" = 13, 
  "Bahia" = 29,
  "Ceará" = 23, 
  "Distrito Federal" = 53, 
  "Espírito Santo" = 32, 
  "Goiás" = 52, 
  "Maranhão" = 21, 
  "Mato Grosso" = 51, 
  "Mato Grosso do Sul" = 50, 
  "Minas Gerais" = 31, 
  "Pará" = 15, 
  "Paraíba" = 25, 
  "Paraná" = 41, 
  "Pernambuco" = 26, 
  "Piauí" = 22, 
  "Rio de Janeiro" = 33, 
  "Rio Grande do Norte" = 24, 
  "Rio Grande do Sul" = 43, 
  "Rondônia" = 11, 
  "Roraima" = 14, 
  "Santa Catarina" = 42, 
  "São Paulo" = 35, 
  "Sergipe" = 28, 
  "Tocantins" = 17
)

  
faixas <- seq(1930, 2010, by = 10)

sexos <- c("m", "f")

get_names <- function(regiao, faixa, sexo) {
  url <- "https://servicodados.ibge.gov.br/api/v1/censos/nomes/faixa?qtd=1000000&regiao="
  url <- paste(url, regiao, "&sexo=", sexo, "&faixa=", faixa, sep = "")
  con_utf8(GET(url)) %>%
  jsonlite::fromJSON(TRUE, flatten = TRUE)
}
nomes = list()
i = 0
for(regiao in regioes) {
  for(faixa in faixas) {
    for(sexo in sexos) {
      i = i + 1
      nomes[[i]] <- get_names(regiao = regiao, faixa = faixa, sexo = sexo)
    }
  }
}

nomes_m <- do.call("rbind.data.frame", sapply(regioes, function(x) get_names(x, sexo = "m"), simplify = F))
nomes_f <- do.call("rbind.data.frame", sapply(regioes, function(x) get_names(x, sexo = "f"), simplify = F))

nomes_test <- do.call("rbind.data.frame", nomes)
nomes_test$estado <- names(regioes[match(nomes_test$regiao, regioes)])

nomes_counts <- nomes_test %>%
  group_by(estado, nome) %>%
  summarise(freq = sum(freq)) %>%
  mutate(freq = freq/sum(freq)) %>%
  ungroup() %>%
  spread(nome, freq, fill = 0) %>%
  as.data.frame()

rownames(nomes_counts) <- nomes_counts$estado
nomes_counts <- nomes_counts[,-1]
 
tt <- data.frame(tt$x)
tt$estados <- rownames(tt) 
tt %>%
  ggplot(aes(x = PC1, y = PC2, label = estados)) + 
  geom_text()

 mutate(nchar = nchar(nome)) %>%
  mutate(consoantes = str_count(nome, "[^AEIOU]")) %>%
  mutate(vogais = str_count(nome, "[AEIOU]")) %>%
  mutate(ratio = consoantes/vogais)

nomes_counts %>%
  filter(str_detect(nome, "ON$"))

nomes_test <- nomes_test %>%
  mutate(decada = case_when(
    faixa == "<1930" ~ "-1930",
    faixa == "<1940" ~ "1931-40",
    faixa == "<1950" ~ "1941-50",
    faixa == "<1960" ~ "1951-60",
    faixa == "<1970" ~ "1961-70",
    faixa == "<1980" ~ "1971-80",
    faixa == "<1990" ~ "1981-90",
    faixa == "<2000" ~ "1991-00",
    faixa == "<2010" ~ "2001-10"
   ))
nomes_test %>%
  group_by(nome) %>%
  summarise(freq = sum(freq), nchar = nchar(nome)[1], consoantes) %>%
  arrange(desc(freq), nchar)

nomes_freq_f <- nomes_test %>%
  filter(sexo == "f") %>%
  group_by(faixa, nome) %>%
  summarise(freq = sum(freq)) %>%
  mutate(freq = freq/sum(freq)) %>%
  arrange(desc(freq))

p_1930 <- 
nomes_test %>%
  filter(faixa %in% c("<1930", "<1940"), sexo == "f") %>%
  group_by(nome) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  mutate(nome = str_to_title(nome)) %>%
  mutate(freq = freq/sum(freq) * 1000, nome = reorder(nome, freq)) %>%
  top_n(20) %>%
  ggplot(aes(x = nome, y = freq)) +
  geom_bar(stat = "identity") +
  labs(y = "Usos a cada 1000 mulheres", x = "Nome", caption = "") + 
  ggtitle("Nomes mais comuns antigamente", subtitle = "Nomes mais comuns entre mulheres nascidas até 1940") + 
  coord_flip() +
  theme_plex() + 
  theme(panel.grid.major.x = element_line()) +
  theme(
    # axis.title = element_text(family="IBMPlexSans-Bold"),
    panel.grid.major.x = element_line(), 
    plot.caption = element_text(size = 8, colour = "darkgrey"),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )

p_1990 <- 
nomes_test %>%
  filter(faixa %in% c("<2000", "<2010"), sexo == "f") %>%
  group_by(nome) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  mutate(nome = str_to_title(nome)) %>%
  mutate(freq = freq/sum(freq) * 1000, nome = reorder(nome, freq)) %>%
  top_n(20) %>%
  ggplot(aes(x = nome, y = freq)) +
  geom_bar(stat = "identity") +
  labs(y = "Usos a cada 1000 mulheres", x = NULL, caption = "Fonte: IBGE") + 
  ggtitle("Nomes mais comuns hoje", subtitle = "Nomes mais comuns entre mulheres nascidas após 1990") + 
  coord_flip() + 
  theme_plex() + 
  theme(
    # axis.title = element_text(family="IBMPlexSans-Bold"),
    panel.grid.major.x = element_line(), 
    plot.caption = element_text(size = 8, colour = "darkgrey"),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )

p_nomes <- grid.arrange(p_1930, p_1990, ncol = 2)
ggsave(p_nomes, file = "~/Desktop/p_nomes.png", width = 11, height = 7)

top_20_1930 <- nomes_test %>%
  filter(faixa %in% c("<1930", "<1940"), sexo == "f") %>%
  group_by(nome) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  mutate(freq = freq/sum(freq) * 1000) %>%
  top_n(20) 

top_20_1990 <- nomes_test %>%
  filter(faixa %in% c("<2000", "<2010"), sexo == "f") %>%
  group_by(nome) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  mutate(freq = freq/sum(freq) * 1000) %>%
  top_n(20) 

p_usos <- nomes_test %>%
  filter(sexo == "f") %>%
  group_by(decada, nome) %>%
  summarise(freq = sum(freq)) %>%
  mutate(freq = freq/sum(freq)) %>%
  filter(nome %in% top_20_1930$nome) %>%
  mutate(nome = reorder(str_to_title(nome), -top_20_1930$freq[match(nome, top_20_1930$nome)])) %>%
  ggplot(aes(x = decada, y = freq * 1000, group = 1)) +
  geom_line() + 
  labs(x = "Década", y = "Usos a cada 1000 mulheres", caption = "Fonte: IBGE") + 
  ggtitle("Nomes ao longo das décadas", subtitle = "Frequência de uso dos vinte nomes femininos mais comuns entre pessoas nascidas até 1940") + 
  facet_wrap(~nome, ncol = 5, scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    strip.text = element_text(family = "IBMPlexSans-Medium"),
    plot.subtitle = element_text(family="IBMPlexSans", size = 10),
    plot.title = element_text(family="IBMPlexSans-Bold", size = 12),
    plot.caption = element_text(size = 8, colour = "darkgrey")
    ) 

ggsave(p_usos, file = "~/Desktop/p_usos_nomes.png", width = 10, height = 6)


p_usos_1990 <- nomes_test %>%
  filter(sexo == "f") %>%
  group_by(decada, nome) %>%
  summarise(freq = sum(freq)) %>%
  mutate(freq = freq/sum(freq)) %>%
  filter(nome %in% top_20_1990$nome) %>%
  ungroup() %>%
  mutate(nome = reorder(str_to_title(nome), -top_20_1990$freq[match(nome, top_20_1990$nome)])) %>%
  ggplot(aes(x = decada, y = freq * 1000, group = 1)) +
  geom_line() + 
  labs(x = "Década", y = "Usos a cada 1000 mulheres", caption = "Fonte: IBGE") + 
  ggtitle("Nomes ao longo das décadas", subtitle = "Frequência de uso dos vinte nomes femininos mais comuns entre pessoas nascidas após 1990") + 
  facet_wrap(~nome, ncol = 5, scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    strip.text = element_text(family = "IBMPlexSans-Medium"),
    plot.subtitle = element_text(family="IBMPlexSans", size = 10),
    plot.title = element_text(family="IBMPlexSans-Bold", size = 12),
    plot.caption = element_text(size = 8, colour = "darkgrey")
  ) 

ggsave(p_usos_1990, file = "~/Desktop/p_usos_nomes_1990.png", width = 10, height = 6)

nomes_freq_f %>%
  filter(nome %in% top_20_decada30_f$nome) %>%
  ggplot(aes(x = faixa, y = nome, fill = freq)) + 
  geom_tile()

sons <- nomes_test %>%
  filter(str_detect(nome, "SON$"))

sons <- nomes_test %>%
  filter(sexo == "m") %>%
  group_by(decada, nome) %>%
  summarise(freq = sum(freq)) %>%
  mutate(freq = freq/sum(freq)) %>%
  filter(str_detect(nome, "SON$")) %>%
  arrange(desc(freq))

nomes_test %>%
  filter(sexo == "m") %>%
  group_by(decada, estado) %>%
  summarise(freq_son = sum(freq[str_detect(nome, "SON$")])/sum(freq)) %>%
  ggplot(aes(x = decada, y = freq_son, group = 1)) + 
  geom_line() +
  facet_wrap(~estado)

sons %>%
  group_by(faixa) %>%
  summarise(sum_freq = sum(freq)) %>%
  ggplot(aes(x = faixa, y = sum_freq)) +
  geom_line(aes(group = 1)) +
  theme_bw()

sons %>% 
  count(faixa) %>%
  ggplot(aes(x = faixa, y = n)) +
  geom_line(aes(group = 1))

sons %>%
  group_by(faixa) %>%
  summarise(nchar = mean(str_length(nome))) %>%
  ggplot(aes(x = faixa, y = nchar, group = 1)) + 
  geom_line()


            