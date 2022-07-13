---
title: "Crescimento Logístico"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Entendendo o modelo

### Premissas:

- Há recursos finitos que são limitantes ao crescimento da população, o que impede seu crescimento exponencial.
- A capacidade de suporte ($K$) é constante. Assumimos que a disponibilidade de recursos não se altera no ambiente.
- Denso dependência linear – o quanto cada indivíduo adicional diminui a taxa de crescimento per capita da população é proporcional à porção ocupada da capacidade de suporte. Quanto mais indivíduos, menor a contribuição individual deles para o crescimento da população.

Lembre-se que a taxa de crescimento do modelo exponencial é definida por: 

$$dN/dt = rN$$
Ou seja, em determinado ponto da curva, a variação instantânea ou inclinação (o quão rápido a população cresce ou decresce naquele momento) é definida por $rN$.

## Capacidade de suporte

Se a população possui um número máximo suportado pelo ambiente (capacidade de suporte – $K$), o crescimento populacional desacelera conforme a população se aproxima de $K$, de forma proporcional à ocupação.

$$dN/dt = r N \left( \frac{K-N}{N} \right)$$

<center>
que é igual a:
</center>


$$dN/dt = r N \left( 1- \frac{N}{K} \right)$$
$\frac{N}{K}$ é simplesmente a porção de “espaços” já ocupados por indivíduos na população. De K espaços (tamanho máximo da população), N já estão ocupados por indivíduos.

Ao diminuirmos de $1$, temos a porção ainda não ocupada da população.

Quanto menor for $\frac{N}{K}$ (ou seja, quando menor for N, pois K é constante), maior (mais próximo a $1$) será $1- \frac{N}{K}$, termo multiplicado por $rN$ (o quanto a população cresceria exponencialmente sem a limitação por recursos) e maior será a contribuição de cada um dos $N$ indivíduos para adicionar mais indivíduos à população.

Assim, vemos que o crescimento logístico é o mesmo que o exponencial multiplicado por uma variável – a porção **desocupada** ($1- \frac{N}{K}$) da capacidade de suporte.

Em outras palavras: o crescimento logístico da população é o crescimento exponencial multiplicado pela proporção da capacidade de suporte ainda não preenchida. 


### Exemplo: 

Por exemplo: quando $20\%$ da capacidade de suporte está preenchida, $rN$ é multiplicado por $0,8$ (porque $1 – 0,2=0,8$) e temos naquele ponto $80\%$ do valor de $rN$ (o quanto a população cresceria, naquele ponto, no modelo exponencial).

Se $N/K = 1$, ou seja, quando $N = K$, o tamanho da população atingiu a capacidade de suporte), . Assim, $rN$ é multiplicado por $0$, e a população não cresce.

Lembrete: $dn/dt$ diz respeito apenas à inclinação da curva a cada ponto, o quão rápido a população está crescendo. Não é aplicado para calcular quantos indivíduos teríamos no tempo $t$, que seria $Nt$.

O cálculo de $Nt$ existe para o modelo de crescimento logístico, mas está fora do escopo porque a partir de certo tempo, $Nt$ se fixa em $K$.

Se estiver curioso, lá vai:

$N_t = \frac{K} {1+[(K-N_0)/N_0]e^{-rt}}$

## Ponto de inflexão

[IMAGEM]

O ponto de inflexão se localiza na altura $K/2$ e corresponde ao ponto em que a velocidade de crescimento da população, $dN/dt$, é máxima. É o ponto mais íngreme da curva, após o qual a velocidade de crescimento para de aumentar e começa a decair.

Para entender, vamos lembrar que $r$ e $K$ são constantes no modelo. O que varia é o valor de N e consequentemente, $1- \frac{N}{K}$ também varia, diminuindo conforme $N$ aumenta.

A velocidade de crescimento da população $\frac{dN}{dt} = rN \left( 1- \frac{N}{K} \right)$ a princípio aumenta conforme $N$ aumenta, mas ao mesmo tempo é freada conforme $\left( 1- \frac{N}{K} \right)$ aumenta, pois o preenchimento da capacidade de suporte diminui a contribuição de cada indivíduo para o crescimento da população (taxa de crescimento per capita). 

Bem no início da curva, com valores de N próximos a 0 (e antes do ponto de inflexão), o termo $\left( 1- \frac{N}{K} \right)$ assume os maiores valores (perto de $1$, pois $N/K$ ainda é próximo de $0$). Isso significa que a população cresce em velocidade próxima a $rN$, como seria no modelo exponencial sem limitação por recursos. A taxa de crescimento *per capita* é a maior possível, pois cada indivíduo contribui para o crescimento da população perto do quanto contribuiria sem o freio da denso-dependência. No entanto, $N$ ainda é muito baixo e a população como um todo começa crescendo devagar ($dN/dt$ é pequeno e a curva é pouco íngreme).

No ponto de inflexão ($N = K/2$), temos a combinação de $N$ que faz com que $dN/dt$ (a velocidade de crescimento da população) atinja o maior valor possível:

$N = K/2$

$\left( 1- \frac{N}{K} \right) = 1 - \frac{1}{2} = \frac{1}{2} $

$\frac{dN}{dt}max = r \left( \frac{K}{2} \right) \left( \frac{1}{2} \right)$

$\frac{dN}{dt}max = r \left( \frac{K}{4} \right)$


Depois disso, com mais da metade da capacidade de suporte preenchida, a velocidade de crescimento volta a cair.


Perceba na imagem abaixo a diferença da taxa de crescimento da população como um todo e da contribuição *per capita* dos indivíduos conforme a população aumenta em direção à capacidade de suporte.

[IMAGEM/GRÁFICO]

No primeiro gráfico temos o número de indivíduos adicionados à população em pontos ao longo do tempo. Mostra o quão rápido a **população** como um todo cresce. Repare a presença do ponto de inflexão no centro do gráfico.


No segundo temos a contribuição (em número de indivíduos adicionados) *per capita* dos indivíduos daquela população. Conforme o tempo passa e a população cresce, o efeito de denso-dependência reduz o quanto cada um contribui por meio da reprodução.

## A capacidade de suporte no ambiente

Na natureza, a resposta da população ao preenchimento da capacidade de suporte não é imediata e o número de indivíduos $N$ tende a oscilar ao redor de $K$, nunca atingindo o equilíbrio.

Quando a população está se aproximando de $K$, tem-se aumento da competição intraespecífica por recursos, sejam eles alimentos, espaço etc. Devido a isso, a taxa de natalidade per capita $b$ diminui (indivíduos produzem menos filhotes, seja por menor disponibilidade energética ou por falta de lugar para construir ninhos, por exemplo) e a taxa de mortalidade per capita $d$ aumenta.

Além disso, o próprio valor de $K$ na natureza não é constante, pois as variáveis ambientais (como a disponibilidade de alimento) frequentemente oscilam.

Lembrando que $r$ é a taxa líquida resultante das taxas de mortalidade e natalidade per capta ($r = b – d$), que diminui conforme a natalidade diminui e a mortalidade aumenta. 

Sendo assim, na prática $r$ tem valores cada vez mais baixos conforme $N$ se aproxima de $K$ (que também pode oscilar no decorrer do tempo) e valores negativos quando $N$ ultrapassa $K$ (taxa de mortalidade se torna maior que a de natalidade), o que causa o decréscimo da população.

Mas lembre-se que os valores de $r$ e $K$ utilizados no modelo não mudam, se considera o valor do $r$ máximo como constante e o efeito da denso-dependência é aplicado pelo termo $\left( 1- \frac{N}{K} \right)$.
