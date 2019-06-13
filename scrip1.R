install.packages("readxl")
library("dplyr")
install.packages("")
library("ggplot2")
library("readxl")

base1 %>%
  group_by(Ano_Eleicao) %>%
  summarise(Max = max(total_despesa_candidato),
            Min = min(total_despesa_candidato),
            Media = mean(total_despesa_candidato),
            DP = sd(total_despesa_candidato),
            CV = (DP/Media)*100)


base1 %>%
  group_by(Ano_Eleicao) %>%
  summarise(transportes = mean(despesas_transporte),
            publicidade = mean(despesa_publicidade),
            pesquisas = mean(despesas_pesquisas_eleitorais),
            multas = mean(despesas_multas_eleitorais),
            comite = mean(despesas_comite),
            pessoal = mean(despesas_com_pessoal),
            doaçao = mean(despesas_doacoe_outros_cand_comi_part),
            nao_especificado = mean(despesas_no_especificado),
            impostos = mean(despesas_impost_encargos),
            reembolso = mean(despesas_reembolso_eleitores))

graficos <- data.frame(media_gastos_2008 = c(15160, 31178, 1040, 92.4, 2164, 13824, 2440, 33414),
                       media_gastos_2012 = c(17892, 45850, 87.0, 332, 61638, 1331, 14554, 4004),
                       nome = c("transportes", "publicidade", "pesquisas", "multas", "comite", "pessoal", "doacao", "nao especificado"),
                       stringsAsFactors = FALSE)

graficos1 <- data.frame(media_gastos_2008 = c(33141, 31178, 15160, 13824, 2440, 2164, 1040, 92.4),
                        nome = c("não especificado", "publicidade", "trasnportes", "pessoal", "doação", "cômite", "pesquisas", "multas"),
                       stringsAsFactors = FALSE)

graficos%>%
  ggplot(aes(nome, media_gastos_2008)) +
  geom_bar(stat = "identity") + 
  coord_flip()

graficos%>%
  ggplot(aes(nome, media_gastos_2012)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  xlab( )

reorder_size <- function(graficos) {
  factor(media_gastos_2008, levels = names(sort(table(graficos), decreasing=T)))
}

graficos

