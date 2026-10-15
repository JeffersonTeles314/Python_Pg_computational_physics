#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
=============================================================================
 SIMULAÇÃO DO EXPERIMENTO 5 — MEDIDA DA VELOCIDADE DA LUZ
 Universidade Federal do Acre / kit PHYWE 1329
=============================================================================

O aparelho modula a intensidade de um LED vermelho em nu = 50,1 MHz. O feixe
percorre uma distância 2x até um par de espelhos e retorna ao fotodiodo. Os
sinais de emissão e de recepção são batidos (mixer) com um oscilador de
50,05 MHz, gerando dois sinais de 50 kHz que PRESERVAM a diferença de fase
acumulada pela luz no percurso externo. No modo X-Y do osciloscópio esses dois
sinais harmônicos de mesma frequência produzem uma figura de Lissajous
(elipse), cuja abertura mede a fase.

    phi = k*Dl = 2*pi*nu*Dl / c            (Eq. 1 do roteiro)
    c   = 2*pi*nu*Dl / phi                 (Eq. 2)
    sin(phi) = a/b                         (Eq. 8, ver Fig. 4)
    n_M = 2*Dx/l_M + 1                     (Eq. 7)   [l_M = caminho TOTAL no meio]
    sigma_f^2 = sum_i (df/dx_i)^2 sigma_i^2 (Eq. 10)

Este módulo (i) modela fisicamente o aparelho, (ii) sintetiza os traços do
osciloscópio com ruído, (iii) simula a LEITURA do estudante na tela (com
quantização da escala) e (iv) executa a análise de dados completa, incluindo
propagação de erros. Dados reais podem substituir os simulados: basta passar
as leituras (a, b, Dx) para as funções de análise da seção ANÁLISE.

Autor: gerado para a prática de Física Experimental — J.
=============================================================================
"""

from __future__ import annotations

import csv
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

# ----------------------------------------------------------------------------
# 1. CONSTANTES FÍSICAS E PARÂMETROS DO APARELHO
# ----------------------------------------------------------------------------

C_ACEITO = 299_792_458.0          # m/s — valor exato (SI)
NU_MOD = 50.1e6                   # Hz  — frequência de modulação (quartzo)
NU_LO = 50.05e6                   # Hz  — oscilador local do mixer
F_BAT = NU_MOD - NU_LO            # Hz  — 50 kHz exibidos no osciloscópio
LAMBDA_MOD = C_ACEITO / NU_MOD    # m   — "comprimento de onda" da modulação (~5,98 m)

TRILHO_UTIL = 1.95                # m   — base de aço graduada


@dataclass
class Meio:
    """Meio transparente M inserido no caminho do feixe."""
    nome: str
    L_fisico: float               # m — comprimento físico do meio
    n_grupo: float                # índice de grupo (o que a modulação enxerga)
    n_fase: float                 # índice de fase (tabelado, ~650 nm)
    passagens: int = 2            # a luz atravessa M na ida e na volta

    @property
    def l_M(self) -> float:
        """Caminho total percorrido DENTRO do meio (l_M da Eq. 3 do roteiro)."""
        return self.passagens * self.L_fisico

    @property
    def sigma_lM(self) -> float:
        """Incerteza em l_M (trena/escala: 2 mm por face, propagado)."""
        return self.passagens * 0.002


# Índices de grupo em ~650 nm (LED vermelho). O método de modulação mede a
# velocidade de GRUPO, por isso n_grupo != n_fase (ver discussão no relatório).
AGUA = Meio("Água destilada", L_fisico=1.000, n_grupo=1.3444, n_fase=1.3314)
RESINA = Meio("Resina sintética (acrílico)", L_fisico=0.300, n_grupo=1.5100, n_fase=1.4890)


@dataclass
class Bancada:
    """Imperfeições reais da bancada e limitações de leitura."""
    amp_x: float = 5.0            # V — amplitude do canal X (V_g, emissão)
    amp_y: float = 4.6            # V — amplitude do canal Y (V_R, recepção)
    ruido_v: float = 0.015        # V — ruído rms nos traços
    offset_fase: float = 0.008    # rad — resíduo do "zeramento" com o botão de fase
    resolucao_tela: float = 0.10  # V — menor subdivisão lida na tela (1 V/div)
    jitter_leitura: float = 0.030 # V — imprecisão humana adicional na leitura
    sigma_regua: float = 0.002    # m — leitura da escala do trilho
    sensib_reta: float = 0.015    # rad — menor desvio da reta que o olho detecta
    seed: int = 20260813
    rng: np.random.Generator = field(init=False, repr=False)

    def __post_init__(self):
        self.rng = np.random.default_rng(self.seed)

    @property
    def sigma_leitura(self) -> float:
        """Incerteza combinada de uma leitura de tensão na tela (quantização + humano)."""
        return float(np.hypot(self.resolucao_tela / np.sqrt(12.0), self.jitter_leitura))

    @property
    def sigma_dx_ajuste(self) -> float:
        """Incerteza em Dx dominada pelo julgamento visual da reta/elipse."""
        s_visual = C_ACEITO * self.sensib_reta / (4 * np.pi * NU_MOD)
        return float(np.hypot(s_visual, self.sigma_regua))


# ----------------------------------------------------------------------------
# 2. MODELO FÍSICO DO APARELHO
# ----------------------------------------------------------------------------

class AparelhoVelocidadeLuz:
    """Gera os sinais de 50 kHz que aparecem nos canais X e Y do osciloscópio."""

    def __init__(self, bancada: Bancada, c_verdadeiro: float = C_ACEITO):
        self.bancada = bancada
        self.c = c_verdadeiro

    # -- física ---------------------------------------------------------------
    def fase_do_percurso(self, dx: float, meio: Meio | None = None) -> float:
        """
        Fase acumulada em relação à configuração de referência (espelhos no zero,
        figura ajustada para reta). Deslocar os espelhos de dx acrescenta 2*dx de
        caminho; inserir o meio M acrescenta (n-1)*l_M de caminho ÓPTICO.
        """
        dl = 2.0 * dx
        if meio is not None:
            dl += (meio.n_grupo - 1.0) * meio.l_M
        return 2.0 * np.pi * NU_MOD * dl / self.c

    def dx_equivalente(self, meio: Meio) -> float:
        """Deslocamento que reproduz, sem o meio, o mesmo caminho óptico de M."""
        return (meio.n_grupo - 1.0) * meio.l_M / 2.0

    # -- sinais ---------------------------------------------------------------
    def tracar_lissajous(self, dx: float, meio: Meio | None = None,
                         n_pontos: int = 6000, n_periodos: float = 2.0):
        """Retorna (t, V_g, V_R) — o que efetivamente é desenhado na tela."""
        b = self.bancada
        phi = self.fase_do_percurso(dx, meio) + b.offset_fase
        t = np.linspace(0.0, n_periodos / F_BAT, n_pontos)
        w = 2.0 * np.pi * F_BAT
        v_g = b.amp_x * np.sin(w * t) + b.rng.normal(0, b.ruido_v, t.size)
        v_r = b.amp_y * np.sin(w * t - phi) + b.rng.normal(0, b.ruido_v, t.size)
        return t, v_g, v_r


class Osciloscopio:
    """Simula a leitura dos parâmetros a e b da elipse (Fig. 4 do roteiro)."""

    def __init__(self, bancada: Bancada):
        self.bancada = bancada

    @staticmethod
    def _cruzamentos_x_em_y_zero(v_g: np.ndarray, v_r: np.ndarray) -> np.ndarray:
        """|V_g| nos pontos em que a elipse corta o eixo horizontal (V_R = 0)."""
        s = np.signbit(v_r)
        idx = np.where(s[:-1] != s[1:])[0]
        xs = []
        for i in idx:
            f = v_r[i] / (v_r[i] - v_r[i + 1])          # interpolação linear
            xs.append(abs(v_g[i] + f * (v_g[i + 1] - v_g[i])))
        return np.asarray(xs)

    def _ler(self, valor: float) -> float:
        """Aplica jitter humano e quantiza na menor subdivisão da tela."""
        b = self.bancada
        v = valor + b.rng.normal(0, b.jitter_leitura)
        return float(np.round(v / b.resolucao_tela) * b.resolucao_tela)

    def medir_a_b(self, v_g: np.ndarray, v_r: np.ndarray) -> tuple[float, float]:
        """a = semi-interceptação horizontal (V_R=0); b = semi-extensão máxima em V_g."""
        a_bruto = float(np.mean(self._cruzamentos_x_em_y_zero(v_g, v_r)))
        b_bruto = float(np.mean(np.sort(np.abs(v_g))[-30:]))   # média do topo, robusta a ruído
        return self._ler(a_bruto), self._ler(b_bruto)


# ----------------------------------------------------------------------------
# 3. ANÁLISE: FASE, VELOCIDADE E PROPAGAÇÃO DE ERROS (Eq. 10)
# ----------------------------------------------------------------------------

def fase_da_elipse(a: float, b: float, sigma_leitura: float) -> tuple[float, float]:
    """phi = arcsin(a/b), com sigma_phi por propagação de erros."""
    if a / b > 0.97:
        import warnings
        warnings.warn("a/b > 0,97: phi próximo de 90°, leitura degenerada e "
                      "arco-seno ambíguo. Reduza o deslocamento dos espelhos.")
    r = np.clip(a / b, -1.0, 1.0)
    phi = float(np.arcsin(r))
    cos_phi = max(np.cos(phi), 1e-9)
    dphi_da = 1.0 / (b * cos_phi)
    dphi_db = -a / (b ** 2 * cos_phi)
    sigma_phi = float(np.hypot(dphi_da * sigma_leitura, dphi_db * sigma_leitura))
    return phi, sigma_phi


def velocidade_por_fase(dx: float, phi: float, sigma_dx: float,
                        sigma_phi: float) -> tuple[float, float]:
    """c = 2*pi*nu*(2 dx)/phi  e sigma_c pela Eq. (10)."""
    c = 4.0 * np.pi * NU_MOD * dx / phi
    dc_ddx = c / dx
    dc_dphi = -c / phi
    sigma_c = float(np.hypot(dc_ddx * sigma_dx, dc_dphi * sigma_phi))
    return float(c), sigma_c


def velocidade_por_meia_onda(dx_pi: float, sigma_dx: float) -> tuple[float, float]:
    """Método PHYWE: reta -> reta invertida (phi = pi) => 2 dx = lambda/2 => c = 4 nu dx."""
    c = 4.0 * NU_MOD * dx_pi
    return float(c), float(4.0 * NU_MOD * sigma_dx)


def indice_refracao(dx: float, meio: Meio, sigma_dx: float) -> tuple[float, float]:
    """n_M = 2 dx / l_M + 1  (Eq. 7), com propagação de erros."""
    n = 2.0 * dx / meio.l_M + 1.0
    dn_ddx = 2.0 / meio.l_M
    dn_dlM = -2.0 * dx / meio.l_M ** 2
    sigma_n = float(np.hypot(dn_ddx * sigma_dx, dn_dlM * meio.sigma_lM))
    return float(n), sigma_n


def velocidade_no_meio(c: float, sigma_c: float, n: float,
                       sigma_n: float) -> tuple[float, float]:
    """c_M = c / n_M, com propagação de erros."""
    c_m = c / n
    sigma = float(np.hypot(sigma_c / n, c * sigma_n / n ** 2))
    return float(c_m), sigma


def media_ponderada(valores, sigmas) -> tuple[float, float]:
    """Média ponderada pelo inverso da variância (estimador de mínima variância)."""
    v = np.asarray(valores, float)
    s = np.asarray(sigmas, float)
    w = 1.0 / s ** 2
    m = float(np.sum(w * v) / np.sum(w))
    return m, float(1.0 / np.sqrt(np.sum(w)))


def ajuste_linear(x, y, sy):
    """Ajuste ponderado y = A x + B; retorna (A, sA, B, sB, chi2_reduzido)."""
    x, y, sy = map(lambda v: np.asarray(v, float), (x, y, sy))
    w = 1.0 / sy ** 2
    S, Sx, Sy = w.sum(), (w * x).sum(), (w * y).sum()
    Sxx, Sxy = (w * x * x).sum(), (w * x * y).sum()
    D = S * Sxx - Sx ** 2
    A = (S * Sxy - Sx * Sy) / D
    B = (Sxx * Sy - Sx * Sxy) / D
    sA, sB = np.sqrt(S / D), np.sqrt(Sxx / D)
    resid = (y - A * x - B) / sy
    chi2_red = float((resid ** 2).sum() / max(len(x) - 2, 1))
    return float(A), float(sA), float(B), float(sB), chi2_red


# ----------------------------------------------------------------------------
# 4. EXECUÇÃO DA PRÁTICA (as "cinco vezes" pedidas no roteiro)
# ----------------------------------------------------------------------------

class PraticaExperimental:
    def __init__(self, bancada: Bancada | None = None):
        self.bancada = bancada or Bancada()
        self.aparelho = AparelhoVelocidadeLuz(self.bancada)
        self.osc = Osciloscopio(self.bancada)

    # --- 5.1 velocidade da luz no ar (método da elipse) ----------------------
    def serie_no_ar(self, deslocamentos=(0.200, 0.300, 0.400, 0.500, 0.600)):
        """
        Os deslocamentos são escolhidos de modo que phi permaneça abaixo de ~75°:
        como sin(phi) = a/b, valores de phi próximos de 90° tornam a leitura
        degenerada (a -> b) e a inversão do arco-seno ambígua (phi e pi - phi).
        """
        linhas = []
        for i, dx_nominal in enumerate(deslocamentos, start=1):
            # o espelho é posicionado na marca desejada com erro de régua
            dx_real = dx_nominal + self.bancada.rng.normal(0, self.bancada.sigma_regua)
            _, v_g, v_r = self.aparelho.tracar_lissajous(dx_real)
            a, b = self.osc.medir_a_b(v_g, v_r)
            phi, s_phi = fase_da_elipse(a, b, self.bancada.sigma_leitura)
            c, s_c = velocidade_por_fase(dx_nominal, phi,
                                         self.bancada.sigma_regua, s_phi)
            linhas.append(dict(medida=i, dx=dx_nominal, a=a, b=b,
                               phi=phi, sigma_phi=s_phi, c=c, sigma_c=s_c,
                               traco=(v_g, v_r)))
        return linhas

    # --- verificação independente: deslocamento de meia onda (phi = pi) ------
    def serie_meia_onda(self, n_medidas=5):
        dx_teorico = C_ACEITO / (4.0 * NU_MOD)     # ~1,496 m
        linhas = []
        for i in range(1, n_medidas + 1):
            dx = dx_teorico + self.bancada.rng.normal(0, self.bancada.sigma_dx_ajuste)
            c, s_c = velocidade_por_meia_onda(dx, self.bancada.sigma_dx_ajuste)
            linhas.append(dict(medida=i, dx=dx, c=c, sigma_c=s_c))
        return linhas

    # --- 5.2 velocidade da luz em meio transparente -------------------------
    def serie_meio(self, meio: Meio, n_medidas=5):
        dx_teorico = self.aparelho.dx_equivalente(meio)
        linhas = []
        for i in range(1, n_medidas + 1):
            dx = dx_teorico + self.bancada.rng.normal(0, self.bancada.sigma_dx_ajuste)
            n, s_n = indice_refracao(dx, meio, self.bancada.sigma_dx_ajuste)
            linhas.append(dict(medida=i, dx=dx, n=n, sigma_n=s_n))
        return linhas


# ----------------------------------------------------------------------------
# 5. SAÍDA: CSV COM OS "DADOS BRUTOS"
# ----------------------------------------------------------------------------

def salvar_csv(caminho: Path, linhas, colunas):
    with open(caminho, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(colunas)
        for L in linhas:
            w.writerow([f"{L[k]:.6g}" if isinstance(L[k], float) else L[k]
                        for k in colunas])


if __name__ == "__main__":
    from analise_e_figuras import main
    main()
