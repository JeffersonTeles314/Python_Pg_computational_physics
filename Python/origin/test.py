import numpy as np
from scipy.optimize import curve_fit
from scipy.special import voigt_profile
import matplotlib.pyplot as plt

def ler_dados_txt(caminho):
    """
    Lê um arquivo .txt contendo duas colunas numéricas.
    Retorna arrays numpy: x, y
    """
    import numpy as np

    dados = []
    with open(caminho, "r", encoding="utf-8") as f:
        for linha in f:
            linha = linha.strip()

            # Ignora linhas vazias ou comentários
            if not linha or linha.startswith("#"):
                continue

            partes = linha.split()

            # Ignora linhas que não têm duas colunas numéricas
            if len(partes) < 2:
                continue

            try:
                x_val = float(partes[0])
                y_val = float(partes[1])
                dados.append([x_val, y_val])
            except:
                # Se não conseguir converter para float, ignora a linha
                continue

    dados = np.array(dados)
    return dados[:, 0], dados[:, 1]

x, y = ler_dados_txt("data.txt")

# -------------------------------
# 2. Definição da soma de N Voigts
# -------------------------------
def multi_voigt(x, *params):
    """
    params = [amp1, cen1, sigma1, gamma1, amp2, cen2, sigma2, gamma2, ...]
    """
    n_voigts = (len(params) - 2) // 4   # cada Voigt tem 4 parâmetros
    y = np.zeros_like(x, dtype=float)

    
    for i in range(n_voigts):
        if i == 0:
            baseline = params[0]
            angulo = params[1]
            amp   = params[2]
            cen   = params[3]
            sigma = params[4]
            gamma = params[5]
        else:
            amp   = params[4*i + 2]
            cen   = params[4*i + 3]
            sigma = params[4*i + 4]
            gamma = params[4*i + 5]

        y += amp * voigt_profile(x - cen, sigma, gamma)

    y += baseline + angulo * x

    return y

# -------------------------------
# 3. Escolha de quantos Voigts usar
# -------------------------------
N_VOIGTS = 3   # altere para 1, 2, 3, ...

# chute inicial dos parâmetros (amp, centro, sigma, gamma)
initial_params = [50, 1, 100, 30, 0.05, 0.05, 250, 35, 0.05, 0.05, 100, 43.5, 0.1, 0.1]
# for i in range(N_VOIGTS):
#     initial_params += [20, 26.5 + 0.2*i, 0.05, 0.05]

# -------------------------------
# 4. Ajuste não-linear
# -------------------------------
params_opt, params_cov = curve_fit(multi_voigt, x, y, p0=initial_params, maxfev=100000)

print("\nParâmetros ajustados:")
print(f"Baseline = {params_opt[0]:.4f}, Angulo = {params_opt[1]:.4f}")
for i in range(N_VOIGTS):
    amp, cen, sigma, gamma = params_opt[4*i+2:4*i+6]
    print(f"Voigt {i+1}: amp = {amp:.4f}, centro = {cen:.4f}, sigma = {sigma:.4f}, gamma = {gamma:.4f}")

# -------------------------------
# 5. Plot do resultado
# -------------------------------
x_fit = np.linspace(min(x), max(x), 400)
y_fit = multi_voigt(x_fit, *params_opt)

plt.scatter(x, y, color="black", label="Dados")
plt.plot(x_fit, y_fit, label=f"Ajuste com {N_VOIGTS} Voigts")

plt.xlabel(r"2\theta (°)")
plt.ylabel("Intensidade")
plt.legend()
plt.show()
