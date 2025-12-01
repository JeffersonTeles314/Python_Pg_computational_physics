import numpy as np
from scipy.optimize import fsolve

G = 6.67430e-11
M = 5.972e24
mu = G * M  # parâmetro gravitacional da Terra

# Condições iniciais (ex: órbita circular a 400km)
r0_vec = np.array([6771e3, 0.0])  # raio da Terra + 400km
v0_vec = np.array([0.0, 7.67e3])  # velocidade circular

r0 = np.linalg.norm(r0_vec)
v0 = np.linalg.norm(v0_vec)

h_vec = np.cross(r0_vec, v0_vec)
h = np.linalg.norm(h_vec)

epsilon = v0**2 / 2 - mu / r0
a = -mu / (2 * epsilon)
e = np.sqrt(1 - h**2 / (a * mu))
n = np.sqrt(mu / a**3)

# Função para resolver a equação de Kepler
def solve_kepler(M, e):
    func = lambda E: E - e * np.sin(E) - M
    E0 = M if e < 0.8 else np.pi
    E_solution = fsolve(func, E0)
    return E_solution[0]

# Tempo após o qual queremos a posição (ex: 10 min)
t = 600  # segundos
M_t = n * t
E_t = solve_kepler(M_t, e)

# Coordenadas no referencial orbital
r = a * (1 - e * np.cos(E_t))
x = a * (np.cos(E_t) - e)
y = a * np.sqrt(1 - e**2) * np.sin(E_t)

print(f"Posição após {t} segundos: x = {x:.2f} m, y = {y:.2f} m, r = {r:.2f} m")