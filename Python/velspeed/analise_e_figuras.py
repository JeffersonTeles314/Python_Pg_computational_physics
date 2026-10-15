#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Execução da prática virtual: roda a simulação, analisa os dados, gera as
figuras do relatório e grava resultados.json / *.csv.
"""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from simulacao_velocidade_luz import (
    AGUA, C_ACEITO, LAMBDA_MOD, NU_MOD, RESINA, Bancada, PraticaExperimental,
    ajuste_linear, media_ponderada, salvar_csv, velocidade_no_meio,
)

SAIDA = Path("saida")
SAIDA.mkdir(exist_ok=True)

plt.rcParams.update({
    "font.size": 10, "axes.grid": True, "grid.alpha": 0.3,
    "figure.dpi": 160, "savefig.bbox": "tight", "axes.axisbelow": True,
})

AZUL, VERMELHO, VERDE, CINZA = "#1f4e79", "#c0392b", "#1e8449", "#555555"


# ----------------------------------------------------------------------------
# FIGURAS
# ----------------------------------------------------------------------------

def fig_montagem():
    fig, ax = plt.subplots(figsize=(8.4, 2.9))
    ax.set_axis_off()
    ax.add_patch(plt.Rectangle((0.2, 1.4), 1.5, 1.1, fc="#dfe6ee", ec=AZUL, lw=1.4))
    ax.text(0.95, 1.95, "Osciloscópio\n(modo X–Y)", ha="center", va="center", fontsize=8)
    ax.add_patch(plt.Rectangle((2.1, 1.2), 2.0, 1.5, fc="#eef3f8", ec=AZUL, lw=1.4))
    ax.text(3.1, 2.35, "Unidade emissora-receptora", ha="center", fontsize=7.5)
    ax.text(3.1, 1.95, r"$\nu=50{,}1$ MHz", ha="center", fontsize=8)
    ax.text(3.1, 1.60, "mixer → 50 kHz", ha="center", fontsize=7.5)
    ax.annotate("", xy=(2.1, 2.2), xytext=(1.7, 2.2),
                arrowprops=dict(arrowstyle="<->", color=AZUL))
    # feixe de ida e volta
    ax.annotate("", xy=(8.3, 2.15), xytext=(4.1, 2.15),
                arrowprops=dict(arrowstyle="->", color=VERMELHO, lw=1.6))
    ax.annotate("", xy=(4.1, 1.55), xytext=(8.3, 1.55),
                arrowprops=dict(arrowstyle="->", color=VERMELHO, lw=1.6))
    ax.plot([8.4, 8.4], [1.35, 2.35], color=CINZA, lw=4)
    ax.plot([9.1, 9.1], [1.35, 2.35], color=CINZA, lw=4, alpha=0.45, ls="--")
    ax.text(8.75, 2.62, "espelhos", ha="center", fontsize=8)
    ax.annotate("", xy=(9.1, 1.05), xytext=(8.4, 1.05),
                arrowprops=dict(arrowstyle="<->", color=VERDE, lw=1.4))
    ax.text(8.75, 0.80, r"$\Delta x$", ha="center", color=VERDE, fontsize=10)
    # meio M
    ax.add_patch(plt.Rectangle((5.6, 1.45), 1.6, 0.8, fc="#cfe8ff", ec=AZUL, alpha=0.7))
    ax.text(6.4, 2.55, r"meio $M$ ($l_M/2$)", ha="center", fontsize=8)
    ax.annotate("", xy=(7.2, 1.18), xytext=(5.6, 1.18),
                arrowprops=dict(arrowstyle="<->", color=AZUL, lw=1.1))
    ax.set_xlim(0, 10); ax.set_ylim(0.5, 2.9)
    fig.savefig(SAIDA / "fig_montagem.png"); plt.close(fig)


def fig_leitura_ab(traco, a, b, phi):
    v_g, v_r = traco
    fig, ax = plt.subplots(figsize=(4.6, 4.4))
    ax.plot(v_g, v_r, color=AZUL, lw=1.1)
    ax.axhline(0, color="k", lw=0.9); ax.axvline(0, color="k", lw=0.9)
    ax.plot([0, a], [-1.35, -1.35], color=VERMELHO, marker="|", lw=1.6)
    ax.text(a / 2, -1.15, "$a$", color=VERMELHO, ha="center", fontsize=12)
    ax.plot([0, b], [-2.3, -2.3], color=VERDE, marker="|", lw=1.6)
    ax.text(b / 2, -2.1, "$b$", color=VERDE, ha="center", fontsize=12)
    ax.vlines(a, -1.35, 0, color=VERMELHO, ls=":", lw=1.2)
    ax.vlines(b, -2.3, max(v_r), color=VERDE, ls=":", lw=1.2)
    ax.set_xlabel("$V_g$ (canal X) [V]"); ax.set_ylabel("$V_R$ (canal Y) [V]")
    ax.set_title(rf"$\sin\phi = a/b \Rightarrow \phi = {np.degrees(phi):.1f}^\circ$",
                 fontsize=10)
    ax.set_aspect("equal"); ax.set_xlim(-6.2, 6.2); ax.set_ylim(-6.2, 6.2)
    fig.savefig(SAIDA / "fig_leitura_ab.png"); plt.close(fig)


def fig_lissajous(linhas):
    fig, axs = plt.subplots(1, len(linhas), figsize=(12.4, 2.9), sharey=True)
    for ax, L in zip(axs, linhas):
        v_g, v_r = L["traco"]
        ax.plot(v_g, v_r, color=AZUL, lw=0.9)
        ax.axhline(0, color="k", lw=0.7); ax.axvline(0, color="k", lw=0.7)
        ax.set_title(rf"$\Delta x={L['dx']*100:.0f}$ cm" "\n"
                     rf"$\phi={np.degrees(L['phi']):.1f}^\circ$", fontsize=9)
        ax.set_xlim(-6.5, 6.5); ax.set_ylim(-6.5, 6.5)
        ax.set_aspect("equal"); ax.set_xlabel("$V_g$ [V]")
    axs[0].set_ylabel("$V_R$ [V]")
    fig.savefig(SAIDA / "fig_lissajous.png"); plt.close(fig)


def fig_ajuste(linhas, A, sA, B, sB):
    dx = np.array([L["dx"] for L in linhas])
    phi = np.array([L["phi"] for L in linhas])
    s = np.array([L["sigma_phi"] for L in linhas])
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(5.6, 4.6), sharex=True,
                                   gridspec_kw=dict(height_ratios=[3, 1]))
    xx = np.linspace(0, 0.9, 50)
    ax1.plot(xx, A * xx + B, color=VERMELHO, lw=1.4,
             label=rf"ajuste: $A={A:.4f}\pm{sA:.4f}$ rad/m")
    ax1.plot(xx, 4 * np.pi * NU_MOD / C_ACEITO * xx, color=CINZA, ls="--", lw=1.2,
             label="previsão com $c$ tabelado")
    ax1.errorbar(dx, phi, yerr=s, fmt="o", color=AZUL, ms=5, capsize=3,
                 label="medidas")
    ax1.set_ylabel(r"$\phi$ [rad]"); ax1.legend(fontsize=8)
    ax2.errorbar(dx, phi - (A * dx + B), yerr=s, fmt="o", color=AZUL, ms=4, capsize=3)
    ax2.axhline(0, color=VERMELHO, lw=1.2)
    ax2.set_xlabel(r"$\Delta x$ [m]"); ax2.set_ylabel("resíduo\n[rad]", fontsize=8)
    fig.savefig(SAIDA / "fig_ajuste_fase.png"); plt.close(fig)


def fig_dispersao_c(linhas, c_med, s_med, linhas_pi):
    fig, ax = plt.subplots(figsize=(6.4, 3.4))
    idx = np.arange(1, len(linhas) + 1)
    ax.errorbar(idx, [L["c"] / 1e8 for L in linhas],
                yerr=[L["sigma_c"] / 1e8 for L in linhas],
                fmt="o", color=AZUL, capsize=3, label="método da elipse")
    idx2 = np.arange(len(linhas) + 1, len(linhas) + len(linhas_pi) + 1)
    ax.errorbar(idx2, [L["c"] / 1e8 for L in linhas_pi],
                yerr=[L["sigma_c"] / 1e8 for L in linhas_pi],
                fmt="s", color=VERDE, capsize=3, label=r"método $\phi=\pi$")
    ax.axhline(C_ACEITO / 1e8, color=VERMELHO, lw=1.3, label="valor aceito")
    ax.axhspan((c_med - s_med) / 1e8, (c_med + s_med) / 1e8, color=AZUL, alpha=0.12,
               label="média ponderada (elipse) $\\pm\\ \\sigma$")
    ax.set_xlabel("nº da medida"); ax.set_ylabel(r"$c$ [$10^8$ m/s]")
    ax.legend(fontsize=8, ncol=2)
    ax.set_xticks(np.concatenate([idx, idx2]))
    ax.set_xticklabels([f"E{i}" for i in idx] + [f"P{i}" for i in range(1, len(idx2) + 1)])
    fig.savefig(SAIDA / "fig_dispersao_c.png"); plt.close(fig)


def fig_meios(res_meios):
    fig, ax = plt.subplots(figsize=(6.0, 3.2))
    nomes = [r["nome"] for r in res_meios]
    y = np.arange(len(nomes))
    ax.errorbar([r["n"] for r in res_meios], y, xerr=[r["sigma_n"] for r in res_meios],
                fmt="o", color=AZUL, capsize=4, ms=6, label="medido (índice de grupo)")
    ax.scatter([r["n_grupo_tab"] for r in res_meios], y + 0.13, marker="D",
               color=VERDE, s=32, label="tabelado: grupo")
    ax.scatter([r["n_fase_tab"] for r in res_meios], y - 0.13, marker="v",
               color=VERMELHO, s=36, label="tabelado: fase (~650 nm)")
    ax.set_yticks(y); ax.set_yticklabels(nomes, fontsize=9)
    ax.set_xlabel("índice de refração $n_M$"); ax.legend(fontsize=8, loc="lower right")
    fig.savefig(SAIDA / "fig_meios.png"); plt.close(fig)


# ----------------------------------------------------------------------------
# PRÁTICA + ANÁLISE
# ----------------------------------------------------------------------------

def main():
    bancada = Bancada()
    pratica = PraticaExperimental(bancada)
    R: dict = {"parametros": {
        "nu": NU_MOD, "lambda_mod": LAMBDA_MOD, "c_aceito": C_ACEITO,
        "sigma_leitura": bancada.sigma_leitura, "sigma_regua": bancada.sigma_regua,
        "sigma_dx_ajuste": bancada.sigma_dx_ajuste,
        "resolucao_tela": bancada.resolucao_tela, "seed": bancada.seed,
    }}

    # ---------- 5.1 ar, método da elipse ----------
    ar = pratica.serie_no_ar()
    c_med, s_med = media_ponderada([L["c"] for L in ar], [L["sigma_c"] for L in ar])
    desvio_pad = float(np.std([L["c"] for L in ar], ddof=1))
    A, sA, B, sB, chi2 = ajuste_linear([L["dx"] for L in ar], [L["phi"] for L in ar],
                                       [L["sigma_phi"] for L in ar])
    c_fit = 4 * np.pi * NU_MOD / A
    s_c_fit = c_fit * sA / A

    R["ar_elipse"] = [{k: L[k] for k in
                       ("medida", "dx", "a", "b", "phi", "sigma_phi", "c", "sigma_c")}
                      for L in ar]
    R["ar_resumo"] = dict(c=c_med, sigma_c=s_med, desvio_padrao=desvio_pad,
                          erro_rel=abs(c_med - C_ACEITO) / C_ACEITO * 100,
                          compat_sigma=abs(c_med - C_ACEITO) / s_med,
                          A=A, sA=sA, B=B, sB=sB, chi2_red=chi2,
                          c_ajuste=c_fit, sigma_c_ajuste=s_c_fit,
                          erro_rel_ajuste=abs(c_fit - C_ACEITO) / C_ACEITO * 100)

    # ---------- verificação: deslocamento de meia onda ----------
    pi_ = pratica.serie_meia_onda()
    c_pi, s_pi = media_ponderada([L["c"] for L in pi_], [L["sigma_c"] for L in pi_])
    R["ar_meia_onda"] = pi_
    R["ar_meia_onda_resumo"] = dict(c=c_pi, sigma_c=s_pi,
                                    dx_medio=float(np.mean([L["dx"] for L in pi_])),
                                    erro_rel=abs(c_pi - C_ACEITO) / C_ACEITO * 100)

    # ---------- 5.2 meios transparentes ----------
    res_meios = []
    for meio in (AGUA, RESINA):
        linhas = pratica.serie_meio(meio)
        n, s_n = media_ponderada([L["n"] for L in linhas], [L["sigma_n"] for L in linhas])
        c_m, s_cm = velocidade_no_meio(c_med, s_med, n, s_n)
        res_meios.append(dict(
            nome=meio.nome, L=meio.L_fisico, l_M=meio.l_M,
            medidas=linhas, n=n, sigma_n=s_n, c_M=c_m, sigma_c_M=s_cm,
            n_grupo_tab=meio.n_grupo, n_fase_tab=meio.n_fase,
            erro_rel_grupo=abs(n - meio.n_grupo) / meio.n_grupo * 100,
            erro_rel_fase=abs(n - meio.n_fase) / meio.n_fase * 100,
            dx_medio=float(np.mean([L["dx"] for L in linhas])),
        ))
    R["meios"] = res_meios

    # ---------- figuras ----------
    fig_montagem()
    fig_lissajous(ar)
    fig_leitura_ab(ar[2]["traco"], ar[2]["a"], ar[2]["b"], ar[2]["phi"])
    fig_ajuste(ar, A, sA, B, sB)
    fig_dispersao_c(ar, c_med, s_med, pi_)
    fig_meios(res_meios)

    # ---------- arquivos ----------
    salvar_csv(SAIDA / "dados_ar_elipse.csv", ar,
               ["medida", "dx", "a", "b", "phi", "sigma_phi", "c", "sigma_c"])
    salvar_csv(SAIDA / "dados_meia_onda.csv", pi_, ["medida", "dx", "c", "sigma_c"])
    for r in res_meios:
        nome = "agua" if "gua" in r["nome"] else "resina"
        salvar_csv(SAIDA / f"dados_{nome}.csv", r["medidas"],
                   ["medida", "dx", "n", "sigma_n"])
    with open(SAIDA / "resultados.json", "w", encoding="utf-8") as f:
        json.dump(R, f, indent=2, ensure_ascii=False)

    # ---------- resumo no terminal ----------
    print("=" * 74)
    print("PRÁTICA VIRTUAL — MEDIDA DA VELOCIDADE DA LUZ".center(74))
    print("=" * 74)
    print(f"nu = {NU_MOD/1e6:.1f} MHz   lambda_mod = {LAMBDA_MOD:.3f} m")
    print("\n[5.1] AR — método da elipse (5 medidas)")
    print(f"{'Dx(m)':>7}{'a(V)':>7}{'b(V)':>7}{'phi(°)':>9}{'c(1e8 m/s)':>13}{'±':>10}")
    for L in ar:
        print(f"{L['dx']:>7.3f}{L['a']:>7.2f}{L['b']:>7.2f}"
              f"{np.degrees(L['phi']):>9.2f}{L['c']/1e8:>13.4f}{L['sigma_c']/1e8:>10.4f}")
    print(f"  média ponderada: c = ({c_med/1e8:.4f} ± {s_med/1e8:.4f})e8 m/s "
          f"| erro {R['ar_resumo']['erro_rel']:.2f}% | {R['ar_resumo']['compat_sigma']:.1f} sigma")
    print(f"  ajuste phi(Dx):  c = ({c_fit/1e8:.4f} ± {s_c_fit/1e8:.4f})e8 m/s "
          f"| chi2_red = {chi2:.2f}")
    print(f"\n[verif.] phi = pi: <Dx> = {R['ar_meia_onda_resumo']['dx_medio']:.4f} m -> "
          f"c = ({c_pi/1e8:.4f} ± {s_pi/1e8:.4f})e8 m/s "
          f"| erro {R['ar_meia_onda_resumo']['erro_rel']:.2f}%")
    print("\n[5.2] MEIOS TRANSPARENTES")
    for r in res_meios:
        print(f"  {r['nome']}: <Dx> = {r['dx_medio']:.4f} m  l_M = {r['l_M']:.2f} m")
        print(f"     n = {r['n']:.4f} ± {r['sigma_n']:.4f}  (grupo tab. {r['n_grupo_tab']:.4f}, "
              f"fase tab. {r['n_fase_tab']:.4f})")
        print(f"     c_M = ({r['c_M']/1e8:.4f} ± {r['sigma_c_M']/1e8:.4f})e8 m/s")
    print("\nArquivos gravados em:", SAIDA.resolve())
    return R


if __name__ == "__main__":
    main()
