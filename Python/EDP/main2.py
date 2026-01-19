import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# -----------------------------
# Parâmetros físicos
# -----------------------------
L = 1.0
c = 1.0
N = 20            # número de modos
A0 = 0.1

# -----------------------------
# Discretização espacial
# -----------------------------
Nx = 500
x = np.linspace(0, L, Nx)

# -----------------------------
# Condições iniciais
# -----------------------------
def f(x):   # deslocamento inicial
    return A0 * np.sin(4*np.pi * x / L)

def g(x):   # velocidade inicial
    return np.zeros_like(x)

# -----------------------------
# Frequências naturais
# -----------------------------
omega = np.array([n * np.pi * c / L for n in range(1, N+1)])

# -----------------------------
# Cálculo dos coeficientes
# -----------------------------
An = np.zeros(N)
Bn = np.zeros(N)

for n in range(1, N+1):
    An[n-1] = (2 / L) * np.trapz(
        f(x) * np.sin(n * np.pi * x / L), x
    )
    Bn[n-1] = (2 / (omega[n-1] * L)) * np.trapz(
        g(x) * np.sin(n * np.pi * x / L), x
    )

# -----------------------------
# Solução u(x,t)
# -----------------------------
def u(x, t):
    s = np.zeros_like(x)
    for n in range(1, N+1):
        s += (
            An[n-1] * np.cos(omega[n-1] * t) +
            Bn[n-1] * np.sin(omega[n-1] * t)
        ) * np.sin(n * np.pi * x / L)
    return s

# -----------------------------
# Animação
# -----------------------------
fig, ax = plt.subplots()
line, = ax.plot(x, u(x, 0), lw=2)

ax.set_xlim(0, L)
ax.set_ylim(-1.2*A0, 1.2*A0)
ax.set_xlabel("x")
ax.set_ylabel("u(x,t)")
ax.set_title("Corda Vibrante – Série de Fourier")

dt = 0.02

def animate(frame):
    t = frame * dt
    line.set_ydata(u(x, t))
    return line,

ani = FuncAnimation(
    fig,
    animate,
    frames=500,
    interval=20,
    blit=True
)

ani.save('corda_vibrante3.gif', writer='pillow', fps=30)

print("GIF salvo com sucesso!")
plt.show()
