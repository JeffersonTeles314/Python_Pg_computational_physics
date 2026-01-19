import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# -----------------------------
# Parâmetros físicos
# -----------------------------
L = 1.0          # comprimento da corda
c = 1.0          # velocidade da onda
A = 0.1          # amplitude

# -----------------------------
# Domínio espacial
# -----------------------------
Nx = 200
x = np.linspace(0, L, Nx)

# -----------------------------
# Tempo
# -----------------------------
dt = 0.02
t_max = 10

# -----------------------------
# Solução analítica da onda
# -----------------------------
def u(x, t):
    return A * np.sin(np.pi * x / L) * np.cos(np.pi * c * t / L)

# -----------------------------
# Configuração do gráfico
# -----------------------------
fig, ax = plt.subplots()
line, = ax.plot(x, u(x, 0), lw=2)

ax.set_xlim(0, L)
ax.set_ylim(-A*1.2, A*1.2)
ax.set_xlabel("x")
ax.set_ylabel("u(x, t)")
ax.set_title("Corda Vibrante – Equação da Onda 1D")

# -----------------------------
# Função de animação
# -----------------------------
def animate(frame):
    t = frame * dt
    line.set_ydata(u(x, t))
    return line,

# -----------------------------
# Criar animação
# -----------------------------
ani = FuncAnimation(
    fig,
    animate,
    frames=int(t_max / dt),
    interval=20,
    blit=True
)



ani.save('corda_vibrante.gif', writer='pillow', fps=30)

print("GIF salvo com sucesso!")
plt.show()

