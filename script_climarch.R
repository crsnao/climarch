library(sf)
library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(fuzzyjoin)
library(stringdist)
library(writexl)

museus_lista_raw <- read.csv('patrimonio_climatico/lista_dos_museus_brasileiros_2026-01-26T21_32_54.485528625Z.csv')

inund15_raw <- st_read('patrimonio_climatico/inundações2015.gpkg')
plot(st_geometry(inund15_raw))

# Função para remover o estado entre parênteses
remover_estado_parenteses <- function(texto) {
  if (is.na(texto)) return(texto)
  
  # Remove tudo entre parênteses (incluindo os parênteses)
  texto_limpo <- str_remove_all(texto, "\\s*\\([^)]+\\)")
  
  # Remove espaços extras no final
  texto_limpo <- trimws(texto_limpo)
  
  # Converte para minúsculas para padronizar
  texto_limpo <- tolower(texto_limpo)
  
  return(texto_limpo)
}
museus_lista_raw$município_clean <- sapply(museus_lista_raw$Município, remover_estado_parenteses)

# Para separar os nomes dos estados e dos municípios
inund15 <- inund15_raw %>%
  separate(col = "NOME", 
           into = c("município_clean", "estado"), 
           sep = "/")

inund15$município_clean <- tolower(inund15$município_clean)

# Transformar o nome dos municípios para fazer o join
criar_problemas_obrigatorios <- function(df, coluna) {
  df_prob <- df
  textos <- as.character(df[[coluna]])
  
  # Padrões de caracteres acentuados (em diferentes encodings)
  padroes_acentos <- c(
    # Acentos agudos
    "é", "á", "í", "ó", "ú",
    "É", "Á", "Í", "Ó", "Ú",
    # Acentos circunflexos
    "â", "ê", "ô",
    "Â", "Ê", "Ô",
    # Tils
    "ã", "õ",
    "Ã", "Õ",
    # Cedilha
    "ç", "Ç",
    # Crases
    "à", "è", "ì", "ò", "ù",
    "À", "È", "Ì", "Ò", "Ù"
  )
  
  # Para cada texto
  for (i in seq_along(textos)) {
    texto <- textos[i]
    
    # Verificar se tem algum caractere acentuado
    tem_acento <- any(sapply(padroes_acentos, function(p) {
      grepl(p, texto, fixed = TRUE)
    }))
    
    # Se tem acento, FORÇAR problema
    if (tem_acento && nchar(texto) > 0) {
      # Substituir TODOS os caracteres acentuados por �
      for (padrao in padroes_acentos) {
        texto <- gsub(padrao, "�", texto, fixed = TRUE)
      }
      
      # Adicionalmente, se o texto contém "taubat" ou "petr"
      if (grepl("taubat", texto, ignore.case = TRUE)) {
        texto <- gsub("taubat", "taubat�", texto, ignore.case = TRUE)
      }
      if (grepl("petr", texto, ignore.case = TRUE)) {
        texto <- gsub("petr", "petr�", texto, ignore.case = TRUE)
      }
    }
    
    textos[i] <- texto
  }
  
  df_prob[[coluna]] <- textos
  return(df_prob)
}

# Testar 
museus_lista_prob <- criar_problemas_obrigatorios(museus_lista_raw, "município_clean")

# Remove � e padroniza os erros
remove_problems <- function(x) {
  x %>%
    # Remove replacement character
    gsub("�", "", ., fixed = TRUE) %>%
    # Remove acentos
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    # Padroniza
    tolower() %>%
    trimws() %>%
    gsub("[^a-z0-9\\s]", "", .) %>%
    gsub("\\s+", " ", .)
}

inund15$município_clean <- remove_problems(inund15$município_clean)
museus_lista_prob$município_clean <- remove_problems(museus_lista_prob$município_clean)

# Join simples
result <- inner_join(inund15, museus_lista_prob, by = "município_clean")

# Verificar mismatches linha por linha
result_check <- result %>%
  mutate(
    sao_iguais = estado == Estado,
    diferenca = ifelse(estado != Estado, 
                       paste(estado, "!=", Estado), 
                       NA)
  )

write_xlsx(result_check, "patrimonio_climatico_inund.xlsx")

# Para fazer a análise por região:
reg <- tribble(
  ~estado, ~regiao, ~regiao_abrev,
  "AC", "Norte", "N",
  "AL", "Nordeste", "NE",
  "AP", "Norte", "N",
  "AM", "Norte", "N",
  "BA", "Nordeste", "NE",
  "CE", "Nordeste", "NE",
  "DF", "Centro-Oeste", "CO",
  "ES", "Sudeste", "SE",
  "GO", "Centro-Oeste", "CO",
  "MA", "Nordeste", "NE",
  "MT", "Centro-Oeste", "CO",
  "MS", "Centro-Oeste", "CO",
  "MG", "Sudeste", "SE",
  "PA", "Norte", "N",
  "PB", "Nordeste", "NE",
  "PR", "Sul", "S",
  "PE", "Nordeste", "NE",
  "PI", "Nordeste", "NE",
  "RJ", "Sudeste", "SE",
  "RN", "Nordeste", "NE",
  "RS", "Sul", "S",
  "RO", "Norte", "N",
  "RR", "Norte", "N",
  "SC", "Sul", "S",
  "SP", "Sudeste", "SE",
  "SE", "Nordeste", "NE",
  "TO", "Norte", "N"
)

# Juntar com seus dados
patclim_reg <- patclim %>%
  left_join(reg, by = "estado")

names(patclim)
patclim$CLASSE <- factor(patclim$CLASSE, 
                         levels = c("Muito alto", "Alto", "Medio", "Baixo", "Muito baixo", "Dado indisponivel"),
                         ordered = TRUE)

plot(density(patclim$VALOR, na.rm = TRUE), )
mean(patclim$VALOR, na.rm = T)

boxplot(patclim_reg$VALOR ~ patclim_reg$regiao_abrev)

# Tabela para o resumo
tabela_resumo <- patclim_reg %>%
  group_by(regiao) %>%
  summarise(
    n_museus = n(),
    alto_risco = sum(CLASSE %in% c("Muito alto", "Alto")),
    porcentagem = paste0(round(n_museus/nrow(patclim_reg)*100, 1), "%"),
    porcentagem_alto = paste0(round(alto_risco/n_museus*100, 1), "%"),
    risco_medio = mean(VALOR, na.rm = TRUE)
  ) %>%
  arrange(regiao)

print(tabela_resumo)
write_xlsx(tabela_resumo, "patrimonio_climatico_inund_analise_reg.xlsx")