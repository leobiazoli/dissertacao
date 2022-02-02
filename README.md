## Modelos quase-experimentais na avaliação de impactos econômicos de desastres sociotécnicos

**Resumo:** A inferência causal tem o objetivo de avaliar o efeito de alguma causa potencial em algum resultado. Nesse sentido, destacam-se os modelos quase-experimentais *Propensity Score Matching (PSM)* e controle sintético, que são técnicas de avaliação não experimental. Esses modelos quase-experimentais utilizam informações de um conjunto de unidades que não participaram de um tratamento para identificar o que teria acontecido às unidades participantes se não participassem da intervenção. Assim, neste trabalho foram aplicados os modelos quase-experimentais, PSM e controle sintético, com o objetivo de avaliar o impacto econômico do rompimento da barragem do Fundão na região de Mariana-MG. Estes modelos foram aplicados também com o uso de análise de agrupamento para utilizar municípios controle semelhantes às unidades tratadas e analisar as diferenças nos resultados obtidos. Os dados analisados foram retirados de diversas fontes e tratam-se de indicadores socioeconômicos que caracterizam os municípios atingidos pelo rompimento da barragem do Fundão. Os resultados obtidos pelo PSM, para avaliar algum efeito no PIB real *per capita* dos 35 municípios impactados, não revelaram nenhum efeito de tratamento médio significativo nos anos 2016, 2017 e 2018. Ao analisar os resultados obtidos no PSM com a análise de agrupamento, foi encontrado uma diferença maior entre as médias dos grupos tratado e controle, porém não foi significativo também ao nível de 5\% de significância. O controle sintético também foi utilizado para avaliação do impacto do rompimento da barragem de Fundão no PIB real *per capita* no município de Mariana, em que foram traçadas as trajetórias sintéticas da variável resposta com e sem a análise de agrupamento. Os resultados obtidos no controle sintético, sem a análise de agrupamento, não demonstraram ser um bom contrafactual para a unidade tratada, município de Mariana. O controle sintético sem o agrupamento considerou todos os municípios mineiros não impactados pelo desastre sociotécnico como potenciais controle e isso, além de afetar as estimativas, levou a um tempo computacional de mais de 6 horas. Ao considerar a análise de agrupamento no controle sintético, o contrafactual obtido contou com um ajuste satisfatório e com redução no tempo computacional em mais de 600\%.

<br>

**Notas:** as análises realizadas foram desenvolvidas na linguagem de programação *R*, as quais encontram-se disponíveis neste *Github*.
