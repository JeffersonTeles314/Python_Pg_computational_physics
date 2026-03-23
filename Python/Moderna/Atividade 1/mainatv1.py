import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import linregress

t = np.array([
0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
60
])

A = np.array([
1.0000, 0.9259, 0.8573, 0.7938, 0.7350, 0.6806, 0.6302, 0.5835, 0.5403, 0.5000,
0.4630, 0.4287, 0.3970, 0.3677, 0.3405, 0.3153, 0.2920, 0.2704, 0.2500, 0.2315,
0.2144, 0.1985, 0.1838, 0.1702, 0.1575, 0.1458, 0.1350, 0.1250, 0.1158, 0.1072,
0.0992, 0.0919, 0.0851, 0.0788, 0.0730, 0.0676, 0.0625, 0.0579, 0.0536, 0.0496,
0.0459, 0.0425, 0.0394, 0.0365, 0.0338, 0.0313, 0.0290, 0.0268, 0.0248, 0.0230,
0.0213, 0.0197, 0.0182, 0.0169, 0.0156, 0.0145, 0.0134, 0.0124, 0.0115, 0.0106,
0.0098
])

# 2. Linearizar
ln_A = np.log(A)

# 3. Regressão Linear
slope, intercept, r_value, p_value, std_err = linregress(t, ln_A)

lambd = -slope
meia_vida = np.log(2) / lambd

print(f"Constante de desintegração (lambda): {lambd:.4f} meses⁻¹")
print(f"Meia-vida calculada: {meia_vida:.2f} meses")

# 4. Curva ajustada para comparar com os dados originais
A_ajustada = np.exp(intercept + slope * t)

# 5. Graficos com e sem escala logaritmica
fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# Sem logaritmo (escala linear)
axes[0].plot(t, A, "o", label="Dados observados", color="tab:blue", markersize=4)
axes[0].plot(t, A_ajustada, "-", label="Ajuste exponencial", color="tab:red")
axes[0].set_title("Desintegracao sem log (escala linear)")
axes[0].set_xlabel("Tempo (meses)")
axes[0].set_ylabel("Atividade A")
axes[0].grid(True, alpha=0.3)
axes[0].legend()

# Com logaritmo (linearizacao em ln(A))
axes[1].plot(t, ln_A, "o", label="ln(A) observado", color="tab:green", markersize=4)
axes[1].plot(t, intercept + slope * t, "-", label="Regressao linear", color="tab:orange")
axes[1].set_title("Desintegracao com log (ln(A))")
axes[1].set_xlabel("Tempo (meses)")
axes[1].set_ylabel("ln(A)")
axes[1].grid(True, alpha=0.3)
axes[1].legend()

fig.suptitle("Comparacao: versao sem e com logaritmo", fontsize=12)
fig.tight_layout()
plt.show()

