import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

# Constantes
G = 6.67430e-11           # Constante gravitacional [m^3/kg/s^2]
M = 5.972e24              # Massa da Terra [kg]
R = 6371e3                # Raio da Terra [m]

# Equações diferenciais do movimento
def movimento(t, y):
    x, y_, vx, vy = y
    r = np.sqrt(x**2 + y_**2)
    ax = -G * M * x / r**3
    ay = -G * M * y_ / r**3
    return [vx, vy, ax, ay]

# Condições iniciais
h0 = 200e3        # Altitude inicial menor             # Altitude inicial [m]
x0, y0 = R + h0, 0
vx0, vy0 = 0, 7200        # Velocidade inicial [m/s]

# Critério de colisão
def impacto(t, y): 
    x, y_ = y[0], y[1]
    r = np.sqrt(x**2 + y_**2)
    return r - R
impacto.terminal = True
impacto.direction = -1   # Só detecta quando se aproxima

# Integração
sol = solve_ivp(movimento, [0, 50000], [x0, y0, vx0, vy0],
                events=impacto, rtol=1e-8, atol=1e-10)

# Resultado
print(f"Tempo até colisão: {sol.t_events[0][0]:.1f} s")

# Visualização
plt.plot(sol.y[0]/1e3, sol.y[1]/1e3)
plt.gca().set_aspect('equal')
plt.xlabel('x [km]')
plt.ylabel('y [km]')
plt.title('Órbita até colisão')
plt.grid()
plt.show()