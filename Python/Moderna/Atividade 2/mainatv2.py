import matplotlib.pyplot as plt
import numpy as np

import pandas as pd

# Constantes Físicas
MH = 1.007825  # Massa do átomo de Hidrogênio (u)
MN = 1.008665  # Massa do Nêutron (u)
C2 = 931.5     # Conversão u para MeV (Velocidade da luz ao quadrado)

# Dados: (Nome, Símbolo, Prótons Z, Massa A, Massa Atômica Experimental u)
# Selecionamos os isótopos mais estáveis ou abundantes para cada elemento
elementos_data = [
    ("Boro", "B", 5, 11, 11.009305),
    ("Carbono", "C", 6, 12, 12.000000),
    ("Oxigênio", "O", 8, 16, 15.994915),
    ("Cálcio", "Ca", 20, 40, 39.962591),
    ("Manganês", "Mn", 25, 55, 54.938045),
    ("Ferro", "Fe", 26, 56, 55.934937),
    ("Zinco", "Zn", 30, 64, 63.929142),
    ("Arsênio", "As", 33, 75, 74.921596),
    ("Prata", "Ag", 47, 107, 106.905097),
    ("Estanho", "Sn", 50, 120, 119.902195),
    ("Iodo", "I", 53, 127, 126.904473),
    ("Térbio", "Tb", 65, 159, 158.925347),
    ("Irídio", "Ir", 77, 193, 192.962926),
    ("Ouro", "Au", 79, 197, 196.966569),
    ("Chumbo", "Pb", 82, 208, 207.976652),
    ("Bismuto", "Bi", 83, 209, 208.980399),
    ("Urânio", "U", 92, 238, 238.050788),
    ("Plutônio", "Pu", 94, 244, 244.064204)
]

def calcular_energias(dados):
    resultados = []
    for nome, sym, z, a, massa in dados:
        n = a - z  # Número de nêutrons
        eb = (z * MH + n * MN - massa) * C2
        eb_por_a = eb / a
        resultados.append({
            "Elemento": nome,
            "Isótopo": f"{sym}-{a}",
            "Z": z,
            "A": a,
            "Energia de Ligação (MeV)": round(eb, 2),
            "Energia/Nucleon (MeV/nuc)": round(eb_por_a, 4)
        })
    return pd.DataFrame(resultados)

# Gerar Tabela
df_resultados = calcular_energias(elementos_data)

# Ordenar por número de massa crescente
df_resultados = df_resultados.sort_values(by="A").reset_index(drop=True)

# Definição pedida: energia de ligação por núcleon = E_B / A
df_resultados["E_B/A (MeV/nucleon)"] = (
    df_resultados["Energia de Ligação (MeV)"] / df_resultados["A"]
)

print(df_resultados.to_string(index=False))

# Exportar para CSV (opcional)
df_resultados.to_csv("energia_ligacao_calculada.csv", index=False)

# Gráfico de E_B/A em função de A (crescente)
plt.figure(figsize=(10, 6))
plt.plot(
    df_resultados["A"],
    df_resultados["E_B/A (MeV/nucleon)"],
    marker="o",
    linewidth=1.8,
    color="tab:blue"
)
plt.title("Energia de Ligação por Núcleon vs Número de Massa")
plt.xlabel("Número de Massa (A)")
plt.ylabel("E_B/A (MeV/núcleon)")
plt.grid(True, linestyle="--", alpha=0.4)
plt.tight_layout()
plt.show()
