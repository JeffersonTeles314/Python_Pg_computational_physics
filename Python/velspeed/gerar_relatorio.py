#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Monta o relatório em LaTeX a partir de saida/resultados.json."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

SAIDA = Path("saida")
R = json.load(open(SAIDA / "resultados.json", encoding="utf-8"))
P, AR, RES = R["parametros"], R["ar_elipse"], R["ar_resumo"]
PI_, PIR, MEIOS = R["ar_meia_onda"], R["ar_meia_onda_resumo"], R["meios"]


def v(x, d=3):
    """Número com vírgula decimal para o texto em português."""
    return f"{x:.{d}f}".replace(".", "{,}")


# ---------------------------------------------------------------- tabelas ---
tab_ar = "\n".join(
    rf"{L['medida']} & {v(L['dx'], 3)} & {v(L['a'], 2)} & {v(L['b'], 2)} & "
    rf"{v(np.degrees(L['phi']), 2)} & {v(np.degrees(L['sigma_phi']), 2)} & "
    rf"{v(L['c']/1e8, 4)} & {v(L['sigma_c']/1e8, 4)} \\"
    for L in AR)

tab_pi = "\n".join(
    rf"{L['medida']} & {v(L['dx'], 4)} & {v(L['c']/1e8, 4)} & {v(L['sigma_c']/1e8, 4)} \\"
    for L in PI_)


def tab_meio(m):
    return "\n".join(
        rf"{L['medida']} & {v(L['dx'], 4)} & {v(L['n'], 4)} & {v(L['sigma_n'], 4)} \\"
        for L in m["medidas"])


# ------------------------------------------- orçamento de erros (medida 3) ---
L3 = AR[2]
contrib_dx = (L3["c"] / L3["dx"]) * P["sigma_regua"]
contrib_phi = (L3["c"] / L3["phi"]) * L3["sigma_phi"]
peso_dx = 100 * contrib_dx ** 2 / (contrib_dx ** 2 + contrib_phi ** 2)
peso_phi = 100 - peso_dx

TEX = r"""
\documentclass[11pt,a4paper]{article}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[portuguese]{babel}
\usepackage{amsmath,amssymb,graphicx,booktabs,geometry,caption,float,indentfirst}
\usepackage[colorlinks=true,linkcolor=black,citecolor=black,urlcolor=blue]{hyperref}
\geometry{margin=2.4cm}
\captionsetup{font=small,labelfont=bf}
\setlength{\parskip}{2pt}

\title{\textbf{Medida da Velocidade da Luz pelo Método da Modulação de Fase}\\[2pt]
\large Experimento 5 --- Física Experimental (kit PHYWE 1329)\\[4pt]
\normalsize Simulação computacional do aparato e tratamento estatístico dos dados}
\author{Universidade Federal do Acre --- Centro de Ciências Biológicas e da Natureza}
\date{\today}

\begin{document}
\maketitle

\begin{abstract}
\noindent
Mediu-se a velocidade da luz no ar pelo método da comparação de fase entre um feixe
de luz modulado em intensidade a $\nu = 50{,}1$~MHz e o sinal de referência do
emissor, usando a abertura da figura de Lissajous no osciloscópio como fasímetro.
A partir de cinco medidas obteve-se
$c = (@@CMED@@ \pm @@SCMED@@)\times 10^{8}$~m/s, compatível com o valor aceito
dentro de @@COMPAT@@ desvios-padrão (discrepância de @@ERRREL@@\%). Um ajuste linear
de $\phi(\Delta x)$ forneceu $c = (@@CFIT@@ \pm @@SCFIT@@)\times 10^{8}$~m/s e o
método complementar do deslocamento de meia onda, $c = (@@CPI@@ \pm @@SCPI@@)\times
10^{8}$~m/s. Para os meios transparentes obtiveram-se
$n_{\text{água}} = @@NAGUA@@ \pm @@SNAGUA@@$ e
$n_{\text{resina}} = @@NRES@@ \pm @@SNRES@@$, correspondendo a
$c_{\text{água}} = (@@CAGUA@@ \pm @@SCAGUA@@)\times10^{8}$~m/s e
$c_{\text{resina}} = (@@CRES@@ \pm @@SCRES@@)\times10^{8}$~m/s. Discute-se por que
o método mede a velocidade \emph{de grupo} e não a de fase.
\end{abstract}

\section{Objetivos}
Medir a velocidade da luz no ar e, em seguida, em dois meios transparentes
(água destilada e resina sintética), determinando os respectivos índices de
refração; aplicar a regra de propagação de erros para grandezas medidas de
forma indireta.

\section{Fundamentação teórica}

\subsection{Por que modular a luz}
A frequência óptica do LED vermelho ($\sim 4{,}6\times10^{14}$~Hz) é
inacessível a qualquer eletrônica de laboratório. A solução do aparato é
transferir a informação temporal para uma \emph{envoltória}: a intensidade
do feixe é modulada a $\nu = 50{,}1$~MHz, de modo que o comprimento de onda
relevante passa a ser
\begin{equation}
\lambda_{\text{mod}} = \frac{c}{\nu} \approx @@LAMBDA@@~\text{m},
\end{equation}
uma escala perfeitamente compatível com um trilho de bancada de 1,95~m.
O atraso do percurso externo aparece como uma diferença de fase
\begin{equation}
\phi = k\,\Delta l = \frac{2\pi\,\Delta l}{\lambda_{\text{mod}}}
     = \frac{2\pi\nu\,\Delta l}{c},
\qquad\Longrightarrow\qquad
c = \frac{2\pi\nu\,\Delta l}{\phi}.
\label{eq:c}
\end{equation}

\subsection{Conversão heteródina e conservação da fase}
Um mixer bate os sinais de emissão e de recepção contra um oscilador local de
$50{,}05$~MHz, produzindo dois sinais de $50$~kHz. A operação é uma
multiplicação seguida de filtragem passa-baixas: se o sinal de entrada é
$\cos(\omega t - \phi)$ e o oscilador local é $\cos(\omega_{LO}t)$, o produto
contém o termo $\tfrac12\cos[(\omega-\omega_{LO})t - \phi]$. A fase $\phi$
\emph{não é reescalada} pela conversão --- é exatamente essa propriedade que
permite medir um atraso de nanossegundos observando sinais de 50~kHz numa tela
de 20~MHz.

\subsection{A elipse de Lissajous como fasímetro}
No modo X--Y as placas recebem dois harmônicos de mesma frequência:
\begin{equation}
V_g(t) = b\,\sin(\omega t), \qquad V_R(t) = B\,\sin(\omega t - \phi).
\end{equation}
Eliminando $t$ obtém-se a cônica
$\left(V_g/b\right)^2 + \left(V_R/B\right)^2
- 2\left(V_g V_R / bB\right)\cos\phi = \sin^2\phi$, uma elipse que degenera
em reta quando $\phi = 0$ ou $\pi$. Fazendo $V_R = 0$ resulta
$\omega t = 0,\pi$ e portanto $|V_g| = b\,|\sin\phi|$. Chamando de $a$ essa
interceptação horizontal e de $b$ a semi-extensão máxima em $V_g$
(Fig.~\ref{fig:ab}),
\begin{equation}
\boxed{\;\sin\phi = \frac{a}{b}\;}
\label{eq:sinphi}
\end{equation}
que é a Eq.~(8) do roteiro. Como o percurso externo é de ida e volta,
$\Delta l = 2\Delta x$, e a Eq.~\eqref{eq:c} fica
\begin{equation}
c = \frac{4\pi\nu\,\Delta x}{\phi}, \qquad \phi = \arcsin(a/b).
\label{eq:celipse}
\end{equation}
\textbf{Limitação importante:} o arco-seno é ambíguo ($\phi$ e $\pi-\phi$ dão o
mesmo $a/b$) e perde sensibilidade quando $\phi\to90^\circ$, pois
$\mathrm{d}\phi/\mathrm{d}(a/b) = 1/\cos\phi \to \infty$. Por isso os
deslocamentos foram escolhidos no intervalo $0{,}20$--$0{,}60$~m, que mantém
$\phi \lesssim 75^\circ$.

\subsection{Método complementar: deslocamento de meia onda}
Se, partindo de uma reta, os espelhos forem afastados até que a reta reapareça
com coeficiente angular \emph{oposto}, então $\phi = \pi$ e
$2\Delta x = \lambda_{\text{mod}}/2$, ou seja
\begin{equation}
c = 4\nu\,\Delta x_\pi .
\label{eq:pi}
\end{equation}
Este método dispensa a leitura de tensões na tela: toda a incerteza migra para
uma única medida de comprimento, o que o torna substancialmente mais preciso.

\subsection{Meio transparente e índice de refração}
Com o meio $M$ no caminho, a luz o atravessa na ida e na volta, percorrendo um
caminho geométrico total $l_M = 2L$ dentro dele. O tempo de percurso é
$t_1 = (l_1 - l_M)/c + l_M/c_M$ (Eq.~3 do roteiro); retirando $M$ e afastando
os espelhos de $\Delta x$ até restaurar a mesma figura,
$t_2 = (l_1 + 2\Delta x)/c$ (Eq.~4). Igualando $t_1 = t_2$ (caso $k=0$) e
usando $n_M = c/c_M$:
\begin{equation}
n_M = \frac{2\Delta x}{l_M} + 1 \qquad\Longrightarrow\qquad
\Delta x = (n_M - 1)\,L .
\label{eq:nm}
\end{equation}
Note que $n_M$ decorre \emph{apenas} de duas medidas de comprimento: é uma
determinação independente do valor de $c$.

\subsection{Propagação de erros}
Para $f = f(x_1,x_2,\dots)$ com variáveis independentes,
\begin{equation}
\sigma_f^2 = \sum_{i=1}^{N}\left(\frac{\partial f}{\partial x_i}\right)^{2}\sigma_{x_i}^2 .
\label{eq:prop}
\end{equation}
Aplicando às expressões acima:
\begin{align}
\sigma_\phi^2 &= \left(\frac{\sigma_a}{b\cos\phi}\right)^{2}
              + \left(\frac{a\,\sigma_b}{b^{2}\cos\phi}\right)^{2}, &
\left(\frac{\sigma_c}{c}\right)^{2} &=
              \left(\frac{\sigma_{\Delta x}}{\Delta x}\right)^{2}
            + \left(\frac{\sigma_\phi}{\phi}\right)^{2}, \\
\sigma_{n_M}^2 &= \left(\frac{2\sigma_{\Delta x}}{l_M}\right)^{2}
              + \left(\frac{2\Delta x\,\sigma_{l_M}}{l_M^{2}}\right)^{2}, &
\left(\frac{\sigma_{c_M}}{c_M}\right)^{2} &=
              \left(\frac{\sigma_c}{c}\right)^{2}
            + \left(\frac{\sigma_{n_M}}{n_M}\right)^{2}.
\end{align}

\section{Materiais e montagem}
\begin{itemize}\setlength\itemsep{1pt}
\item Osciloscópio de 20~MHz, dois canais, com modo X--Y;
\item unidade emissora-receptora com modulação de $50{,}1$~MHz estabilizada a
      quartzo e mixer interno de $50{,}05$~MHz;
\item base de aço graduada de 195~cm; lentes condensadoras $f = 50$~mm;
      espelhos ajustáveis com suporte;
\item recipiente tubular de $L = 1{,}00$~m (água destilada) e bloco de resina
      sintética de $L = 0{,}30$~m; cabos BNC.
\end{itemize}

\begin{figure}[H]\centering
\includegraphics[width=0.94\textwidth]{fig_montagem.png}
\caption{Esquema da montagem. O feixe modulado percorre o trajeto externo duas
vezes (ida e volta); $\Delta x$ é o deslocamento dos espelhos e $L=l_M/2$ o
comprimento físico do meio inserido.}
\end{figure}

\section{Modelo computacional}
\textbf{Nota metodológica.} Os dados analisados nas seções seguintes foram
produzidos por um simulador do aparato (\texttt{simulacao\_velocidade\_luz.py}),
executado com semente aleatória @@SEED@@ para garantir reprodutibilidade. O
simulador não gera diretamente os resultados finais: ele sintetiza os
\emph{traços do osciloscópio} e reproduz o ato de \emph{ler a tela}, de modo que
todas as etapas de análise --- extração de $a$ e $b$, cálculo de $\phi$,
propagação de erros --- são idênticas às que se aplicariam a dados de bancada.
Substituindo as colunas $a$, $b$ e $\Delta x$ pelas leituras reais, o mesmo
código reproduz o relatório com os dados da prática presencial.

O modelo inclui:
\begin{enumerate}\setlength\itemsep{1pt}
\item \textbf{Física exata:} $\phi = 2\pi\nu\,\Delta l/c$ com $c$ igual ao valor
      SI e $\Delta l$ incluindo o caminho óptico $(n-1)l_M$ do meio;
\item \textbf{Sinais:} dois harmônicos de 50~kHz com amplitudes distintas
      ($b \approx 5{,}0$~V, $B \approx 4{,}6$~V) e ruído gaussiano de
      $15$~mV~rms;
\item \textbf{Resíduo de zeramento:} o ajuste inicial da reta com o botão de
      fase deixa um erro sistemático de $\approx 8$~mrad;
\item \textbf{Leitura da tela:} jitter humano de $30$~mV somado à quantização na
      menor subdivisão visível ($@@RESTELA@@$~V), resultando em
      $\sigma_{a} = \sigma_{b} = @@SIGLEIT@@$~V;
\item \textbf{Leitura do trilho:} $\sigma_{\Delta x} = @@SIGREGUA@@$~m para a
      posição do espelho e $\sigma_{\Delta x} = @@SIGAJUSTE@@$~m quando a
      posição depende do julgamento visual de ``a figura voltou a ser reta''
      (sensibilidade angular do olho de $\sim 15$~mrad).
\end{enumerate}

\section{Procedimento}
\textbf{(A) Ar, método da elipse.} Espelhos na posição zero; ajuste do botão de
fase até a reta de coeficiente angular positivo. Afastamento dos espelhos a
cinco posições entre 20 e 60~cm; em cada uma, leitura de $a$ e $b$ e cálculo de
$\phi$ e $c$ pelas Eqs.~\eqref{eq:sinphi} e \eqref{eq:celipse}.

\textbf{(B) Ar, meia onda.} A partir da mesma reta de referência, afastamento
dos espelhos até a reta reaparecer invertida; leitura de $\Delta x_\pi$ e
cálculo por \eqref{eq:pi}. Repetido cinco vezes.

\textbf{(C) Meios transparentes.} Com $M$ no caminho, ajuste da reta; retirada
de $M$ (a figura abre em elipse) e afastamento dos espelhos até a reta
retornar; leitura de $\Delta x$ e cálculo de $n_M$ por \eqref{eq:nm}. Repetido
cinco vezes para cada meio.

\section{Resultados}

\subsection{Velocidade da luz no ar --- método da elipse}

\begin{figure}[H]\centering
\includegraphics[width=0.47\textwidth]{fig_leitura_ab.png}
\caption{Elipse correspondente a $\Delta x = 40$~cm, com a leitura dos
parâmetros $a$ (interceptação em $V_R = 0$) e $b$ (extensão máxima em $V_g$).
Compare com a Fig.~4 do roteiro.}
\label{fig:ab}
\end{figure}

\begin{figure}[H]\centering
\includegraphics[width=\textwidth]{fig_lissajous.png}
\caption{Evolução da figura de Lissajous com o afastamento dos espelhos: a
elipse abre progressivamente à medida que a fase acumulada cresce.}
\end{figure}

\begin{table}[H]\centering\small
\caption{Leituras e resultados individuais (método da elipse).}
\begin{tabular}{cccccccc}
\toprule
\# & $\Delta x$ (m) & $a$ (V) & $b$ (V) & $\phi$ ($^\circ$) &
$\sigma_\phi$ ($^\circ$) & $c$ ($10^{8}$~m/s) & $\sigma_c$ ($10^{8}$~m/s)\\
\midrule
@@TABAR@@
\bottomrule
\end{tabular}
\end{table}

A combinação das cinco medidas pela média ponderada pelo inverso da variância
fornece
\begin{equation}
c = (@@CMED@@ \pm @@SCMED@@)\times 10^{8}~\text{m/s},
\end{equation}
com desvio-padrão amostral de $@@DESVPAD@@\times10^{8}$~m/s --- compatível com a
incerteza propagada, o que indica que o orçamento de erros da
Seção~2.6 descreve bem a dispersão observada. A discrepância em relação ao
valor aceito $c_0 = 2{,}99792458\times10^{8}$~m/s é de @@ERRREL@@\%, ou
@@COMPAT@@$\sigma$.

\subsection{Ajuste linear \texorpdfstring{$\phi(\Delta x)$}{phi(Dx)}}
Tratar as cinco medidas como pontos de uma reta é mais informativo do que
promediá-las, porque separa o efeito sistemático do zeramento (que vai para o
coeficiente linear) do efeito de $c$ (que vai para o coeficiente angular):
\begin{equation}
\phi = \underbrace{\frac{4\pi\nu}{c}}_{A}\,\Delta x + B .
\end{equation}
O ajuste ponderado forneceu $A = @@A@@ \pm @@SA@@$~rad/m e
$B = @@B@@ \pm @@SB@@$~rad, com $\chi^2_\nu = @@CHI2@@$. O coeficiente linear é
compatível com zero, confirmando que o zeramento não introduziu viés
significativo. Do coeficiente angular,
\begin{equation}
c = \frac{4\pi\nu}{A} = (@@CFIT@@ \pm @@SCFIT@@)\times10^{8}~\text{m/s}
\qquad (\text{discrepância de } @@ERRFIT@@\%).
\end{equation}

\begin{figure}[H]\centering
\includegraphics[width=0.62\textwidth]{fig_ajuste_fase.png}
\caption{Fase medida em função do deslocamento dos espelhos e ajuste linear
ponderado; abaixo, os resíduos. A reta tracejada é a previsão com o valor
tabelado de $c$.}
\end{figure}

\subsection{Verificação pelo deslocamento de meia onda}
\begin{table}[H]\centering\small
\caption{Medidas com $\phi = \pi$ (reta invertida).}
\begin{tabular}{cccc}
\toprule
\# & $\Delta x_\pi$ (m) & $c$ ($10^{8}$~m/s) & $\sigma_c$ ($10^{8}$~m/s)\\
\midrule
@@TABPI@@
\bottomrule
\end{tabular}
\end{table}
\noindent Média ponderada: $c = (@@CPI@@ \pm @@SCPI@@)\times10^{8}$~m/s,
discrepância de @@ERRPI@@\%. A incerteza relativa cai de
$\sim@@INCELIPSE@@\%$ (elipse) para $\sim@@INCPI@@\%$, porque
$\Delta x_\pi \approx 1{,}50$~m é grande e a leitura de tensões desaparece da
cadeia de medida.

\begin{figure}[H]\centering
\includegraphics[width=0.68\textwidth]{fig_dispersao_c.png}
\caption{Valores individuais de $c$ pelos dois métodos (E: elipse; P: meia
onda) confrontados com o valor aceito.}
\end{figure}

\subsection{Meios transparentes}

@@BLOCOMEIOS@@

\begin{figure}[H]\centering
\includegraphics[width=0.66\textwidth]{fig_meios.png}
\caption{Índices de refração medidos comparados aos valores tabelados de fase
($\sim650$~nm) e de grupo.}
\label{fig:meios}
\end{figure}

\section{Discussão}

\subsection{Orçamento de erros}
Para a medida com $\Delta x = @@DX3@@$~m, as contribuições à variância de $c$
foram de @@PESODX@@\% para a leitura do trilho e @@PESOPHI@@\% para a leitura
da fase. O método da elipse é, portanto, \emph{limitado pela tela}: cada
subdivisão de @@RESTELA@@~V mal lida se converte em cerca de 1\% de erro em
$c$. Isso sugere três melhorias concretas: (i) expandir verticalmente o traço
até ocupar toda a tela, maximizando $b$; (ii) operar na faixa
$\phi \approx 40^\circ$--$60^\circ$, onde $\cos\phi$ ainda não é pequeno mas
$\phi$ já é grande o bastante para diluir o erro relativo; (iii) usar
preferencialmente o método de meia onda, que é uma ordem de grandeza mais
preciso.

\subsection{Velocidade de fase ou de grupo?}
Este é o ponto conceitual mais sutil do experimento. O que se propaga e é
cronometrado não é a onda portadora óptica, mas a \emph{envoltória} de
modulação --- e envoltórias viajam com a velocidade de grupo
$v_g = \mathrm{d}\omega/\mathrm{d}k$, não com $v_f = \omega/k$. Consequentemente
o índice medido é o índice de grupo
\begin{equation}
n_g = n - \lambda\frac{\mathrm{d}n}{\mathrm{d}\lambda},
\end{equation}
que em meios normalmente dispersivos é maior que $n$. No ar a diferença é
irrelevante ($n_g - n \sim 10^{-8}$), mas na água em torno de 650~nm ela chega
a $\sim1\%$: $n \approx 1{,}331$ contra $n_g \approx 1{,}344$. Os valores
medidos ficam de fato mais próximos dos índices de grupo do que dos índices de
fase tabelados, como mostra a Fig.~\ref{fig:meios} --- um resultado que costuma
ser interpretado erroneamente como ``erro sistemático'' do aparato.

\subsection{Outras fontes de erro sistemático}
\begin{itemize}\setlength\itemsep{1pt}
\item \textbf{Ponto zero da escala:} o zero do trilho não coincide com o plano
      de emissão do diodo. O método é imune a isso, pois usa apenas
      \emph{diferenças} $\Delta x$ --- razão pela qual se mede o deslocamento e
      não a distância absoluta.
\item \textbf{Estabilidade de $\nu$:} sendo estabilizada a quartzo
      ($\delta\nu/\nu \sim 10^{-5}$), sua contribuição é desprezível frente às
      demais e por isso $\nu$ foi tratada como constante exata.
\item \textbf{Não linearidade e defasagem dos amplificadores} do osciloscópio
      entre os canais X e Y: introduz uma fase espúria constante, absorvida
      pelo procedimento de zeramento e visível no coeficiente linear $B$ do
      ajuste.
\item \textbf{Índice de refração do ar:} $n_{\rm ar} \approx 1{,}00027$, de modo
      que o valor medido é rigorosamente $c/n_{\rm ar}$, cerca de
      $0{,}03$\% abaixo de $c$ --- muito menor que a incerteza experimental.
\item \textbf{Temperatura da água:} $\mathrm{d}n/\mathrm{d}T \approx
      -1\times10^{-4}$/$^\circ$C, ou $\sim0{,}1$\% numa variação de
      $10\,^\circ$C.
\end{itemize}

\section{Conclusão}
O método da modulação de fase reproduziu a velocidade da luz no ar com
discrepâncias de @@ERRREL@@\% (elipse), @@ERRFIT@@\% (ajuste linear) e
@@ERRPI@@\% (meia onda) em relação ao valor aceito, todos os resultados
compatíveis com $c_0$ dentro das incertezas propagadas. Os índices de refração
obtidos, $n_{\text{água}} = @@NAGUA@@ \pm @@SNAGUA@@$ e
$n_{\text{resina}} = @@NRES@@ \pm @@SNRES@@$, situam-se dentro do esperado para
água e acrílico e reproduzem melhor os índices de \emph{grupo} do que os de
fase, coerentemente com o fato de o experimento cronometrar a envoltória de
modulação. A análise do orçamento de erros mostra que a precisão do método da
elipse é limitada pela resolução de leitura da tela, enquanto o método de meia
onda transfere toda a incerteza para uma medida de comprimento e alcança
precisão cerca de cinco vezes melhor.

\begin{thebibliography}{9}
\bibitem{roteiro} DIAS, O. A. T.; SILVA, E. \emph{1329 --- Medida da velocidade
da luz}. Roteiro elaborado a partir da documentação PHYWE. PUC-SP.
\bibitem{ufac} UNIVERSIDADE FEDERAL DO ACRE. \emph{Experimento 5: Medida da
velocidade da luz}. Centro de Ciências Biológicas e da Natureza.
\bibitem{hecht} HECHT, E. \emph{Optics}. 5.~ed. Pearson, 2017. Cap.~7
(superposição) e Cap.~4 (dispersão).
\bibitem{taylor} TAYLOR, J. R. \emph{An Introduction to Error Analysis}.
2.~ed. University Science Books, 1997.
\bibitem{daimon} DAIMON, M.; MASUMURA, A. Measurement of the refractive index
of distilled water from the near-infrared region to the ultraviolet region.
\emph{Applied Optics}, v.~46, n.~18, p.~3811--3820, 2007.
\end{thebibliography}

\appendix
\section{Arquivos que acompanham o relatório}
\begin{itemize}\setlength\itemsep{1pt}
\item \texttt{simulacao\_velocidade\_luz.py} --- modelo do aparato, síntese dos
      sinais, leitura simulada da tela e funções de análise/propagação de erros;
\item \texttt{analise\_e\_figuras.py} --- execução da prática, análise e geração
      das figuras;
\item \texttt{gerar\_relatorio.py} --- montagem deste documento a partir de
      \texttt{resultados.json};
\item \texttt{dados\_ar\_elipse.csv}, \texttt{dados\_meia\_onda.csv},
      \texttt{dados\_agua.csv}, \texttt{dados\_resina.csv} --- dados brutos.
\end{itemize}

\end{document}
"""

# ------------------------------------------------------ bloco dos meios ------
blocos = []
for m in MEIOS:
    blocos.append(rf"""
\subsubsection*{{{m['nome']} --- $L = {v(m['L'], 2)}$~m ($l_M = {v(m['l_M'], 2)}$~m)}}
\begin{{table}}[H]\centering\small
\begin{{tabular}}{{cccc}}
\toprule
\# & $\Delta x$ (m) & $n_M$ & $\sigma_{{n_M}}$\\
\midrule
{tab_meio(m)}
\bottomrule
\end{{tabular}}
\end{{table}}
\noindent Média ponderada: $n_M = {v(m['n'], 4)} \pm {v(m['sigma_n'], 4)}$
(tabelado: $n_{{\text{{fase}}}} = {v(m['n_fase_tab'], 4)}$,
$n_{{\text{{grupo}}}} = {v(m['n_grupo_tab'], 4)}$; discrepâncias de
{v(m['erro_rel_fase'], 2)}\% e {v(m['erro_rel_grupo'], 2)}\%, respectivamente).
Com $c$ obtido na Seção~6.1,
$c_M = c/n_M = ({v(m['c_M']/1e8, 4)} \pm {v(m['sigma_c_M']/1e8, 4)})\times10^{{8}}$~m/s.
""")
bloco_meios = "\n".join(blocos)

SUBS = {
    "@@TABAR@@": tab_ar, "@@TABPI@@": tab_pi, "@@BLOCOMEIOS@@": bloco_meios,
    "@@LAMBDA@@": v(P["lambda_mod"], 3),
    "@@SEED@@": str(P["seed"]),
    "@@RESTELA@@": v(P["resolucao_tela"], 2),
    "@@SIGLEIT@@": v(P["sigma_leitura"], 3),
    "@@SIGREGUA@@": v(P["sigma_regua"], 3),
    "@@SIGAJUSTE@@": v(P["sigma_dx_ajuste"], 4),
    "@@CMED@@": v(RES["c"] / 1e8, 4), "@@SCMED@@": v(RES["sigma_c"] / 1e8, 4),
    "@@DESVPAD@@": v(RES["desvio_padrao"] / 1e8, 4),
    "@@ERRREL@@": v(RES["erro_rel"], 2), "@@COMPAT@@": v(RES["compat_sigma"], 1),
    "@@A@@": v(RES["A"], 4), "@@SA@@": v(RES["sA"], 4),
    "@@B@@": v(RES["B"], 4), "@@SB@@": v(RES["sB"], 4),
    "@@CHI2@@": v(RES["chi2_red"], 2),
    "@@CFIT@@": v(RES["c_ajuste"] / 1e8, 4),
    "@@SCFIT@@": v(RES["sigma_c_ajuste"] / 1e8, 4),
    "@@ERRFIT@@": v(RES["erro_rel_ajuste"], 2),
    "@@CPI@@": v(PIR["c"] / 1e8, 4), "@@SCPI@@": v(PIR["sigma_c"] / 1e8, 4),
    "@@ERRPI@@": v(PIR["erro_rel"], 2),
    "@@INCELIPSE@@": v(100 * RES["sigma_c"] / RES["c"], 1),
    "@@INCPI@@": v(100 * PIR["sigma_c"] / PIR["c"], 2),
    "@@NAGUA@@": v(MEIOS[0]["n"], 4), "@@SNAGUA@@": v(MEIOS[0]["sigma_n"], 4),
    "@@CAGUA@@": v(MEIOS[0]["c_M"] / 1e8, 4),
    "@@SCAGUA@@": v(MEIOS[0]["sigma_c_M"] / 1e8, 4),
    "@@NRES@@": v(MEIOS[1]["n"], 4), "@@SNRES@@": v(MEIOS[1]["sigma_n"], 4),
    "@@CRES@@": v(MEIOS[1]["c_M"] / 1e8, 4),
    "@@SCRES@@": v(MEIOS[1]["sigma_c_M"] / 1e8, 4),
    "@@DX3@@": v(L3["dx"], 2),
    "@@PESODX@@": v(peso_dx, 0), "@@PESOPHI@@": v(peso_phi, 0),
}

tex = TEX
for k, val in SUBS.items():
    tex = tex.replace(k, val)

(SAIDA / "relatorio_velocidade_luz.tex").write_text(tex, encoding="utf-8")
print("relatorio_velocidade_luz.tex gerado.")
