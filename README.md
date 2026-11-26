# Escolaridade e rendimento por hora no Brasil

Neste trabalho de Econometria I, analisamos a relação entre anos de estudo e rendimento por hora entre pessoas ocupadas de 25 a 64 anos. Usamos os microdados da PNAD Contínua do quarto trimestre de 2025. O relatório apresenta os filtros usados para chegar à amostra de 182.871 observações e explica como interpretamos os resultados.

## Como rodar a análise

Abra o arquivo R que acompanha o relatório em uma sessão nova do RStudio. Defina como diretório de trabalho a pasta onde quer salvar os resultados e clique em Source para rodar o script inteiro. Na primeira execução, deixe a conexão com a internet ativa para baixar os microdados e instalar os pacotes que estiverem faltando. A fonte é o [IBGE, na pasta de microdados da PNAD Contínua de 2025](https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/2025/).

O script mostra os resultados no Console, salva três gráficos de resíduos na pasta graficos_diagnostico e usa os pacotes flextable e officer para gerar a tabela de regressões em Word.
