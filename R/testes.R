#' Testar Normalidade
#'
#' Esta função realiza testes de normalidade nos dados e fornece recomendações baseadas nos resultados.
#'
#' @param dados Um vetor numérico de dados.
#' @return Resultados do teste de normalidade.
#' @export
#'
#' @examples
#' testar_normalidade(c(1, 2, 3, 4, 5))
# Função para testar normalidade
testar_normalidade <- function(dados) {
  if(!require(nortest)) install.packages("nortest", dependencies=TRUE)
  if(!require(MASS)) install.packages("MASS", dependencies=TRUE)
  
  library(nortest)
  library(MASS)
  
  cat("Escolha o teste de normalidade:\n")
  cat("1: Teste de Shapiro-Wilk\n")
  cat("2: Teste de Anderson-Darling\n")
  cat("3: Teste de Lilliefors (Kolmogorov-Smirnov)\n")
  
  escolha <- as.integer(readline(prompt = "Digite o número do teste desejado: "))
  
  resultado <- switch(escolha,
                      '1' = shapiro.test(dados),
                      '2' = ad.test(dados),
                      '3' = lillie.test(dados),
                      stop("Escolha inválida. Por favor, escolha 1, 2 ou 3."))
  
  print(resultado)
  
  if (resultado$p.value < 0.05) {
    cat("\nOs dados não são normalmente distribuídos (p-valor < 0.05).\n")
    cat("Recomendação: Considere usar métodos não paramétricos.\n")
  } else {
    cat("\nOs dados são normalmente distribuídos (p-valor >= 0.05).\n")
    cat("Recomendação: Pode prosseguir com métodos paramétricos.\n")
  }
}

#' Testar Homocedasticidade
#'
#' Esta função realiza testes de homocedasticidade nos dados e fornece recomendações baseadas nos resultados.
#'
#' @param dados Um vetor numérico de dados.
#' @param grupos Um vetor de fatores que representam os grupos dos dados.
#' @return Resultados do teste de homocedasticidade.
#' @export
#'
#' @examples
#' testar_homoscedasticidade(c(1, 2, 3, 4, 5), factor(c(1, 1, 2, 2, 2)))
# Função para testar homocedasticidade
testar_homoscedasticidade <- function(dados, grupos) {
  if(!require(car)) install.packages("car", dependencies=TRUE)
  if(!require(lmtest)) install.packages("lmtest", dependencies=TRUE)
  
  library(car)
  library(lmtest)
  
  cat("Escolha o teste de homocedasticidade:\n")
  cat("1: Teste de Bartlett\n")
  cat("2: Teste de Levene\n")
  cat("3: Teste de Fligner-Killeen\n")
  
  escolha <- as.integer(readline(prompt = "Digite o número do teste desejado: "))
  
  resultado <- switch(escolha,
                      '1' = bartlett.test(dados, grupos),
                      '2' = leveneTest(dados ~ grupos),
                      '3' = fligner.test(dados, grupos),
                      stop("Escolha inválida. Por favor, escolha 1, 2 ou 3."))
  
  print(resultado)
  
  if (resultado$p.value < 0.05) {
    cat("\nAs variâncias não são iguais (p-valor < 0.05).\n")
    cat("Recomendação: Considere usar métodos que não assumem homocedasticidade.\n")
  } else {
    cat("\nAs variâncias são iguais (p-valor >= 0.05).\n")
    cat("Recomendação: Pode prosseguir com métodos que assumem homocedasticidade.\n")
  }
}
