import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

# Parâmetros físicos
G = 6.67430e-11
M = 5.972e24
mu = G * M

# Condições iniciais
r0 = np.array([7000e3, 0.0, 0.0])       # posição inicial (m)
v0 = np.array([0.0, 7.5e3, 1.0e3])      # velocidade inicial (m/s)

# Vetores orbitais
r = np.linalg.norm(r0)
v = np.linalg.norm(v0)
h_vec = np.cross(r0, v0)
h = np.linalg.norm(h_vec)
e_vec = (np.cross(v0, h_vec) / mu) - (r0 / r)
e = np.linalg.norm(e_vec)
epsilon = v**2 / 2 - mu / r
a = -mu / (2 * epsilon)

# Elementos orbitais
i = np.arccos(h_vec[2] / h)
n_vec = np.cross([0, 0, 1], h_vec)
n = np.linalg.norm(n_vec)
Omega = np.arccos(n_vec[0] / n) if n != 0 else 0
if n_vec[1] < 0: Omega = 2*np.pi - Omega
omega = np.arccos(np.dot(n_vec, e_vec)/(n*e)) if n != 0 else 0
if e_vec[2] < 0: omega = 2*np.pi - omega
nu_0 = np.arccos(np.dot(e_vec, r0)/(e*r))
if np.dot(r0, v0) < 0: nu_0 = 2*np.pi - nu_0

cosE0 = (e + np.cos(nu_0)) / (1 + e * np.cos(nu_0))
E0 = np.arccos(np.clip(cosE0, -1, 1))
if nu_0 > np.pi: E0 = 2*np.pi - E0
n_mean = np.sqrt(mu / a**3)

# Matrizes de rotação
def rot_z(angle): return np.array([
    [np.cos(angle), -np.sin(angle), 0],
    [np.sin(angle),  np.cos(angle), 0],
    [0, 0, 1]
])
def rot_x(angle): return np.array([
    [1, 0, 0],
    [0, np.cos(angle), -np.sin(angle)],
    [0, np.sin(angle),  np.cos(angle)]
])
R = rot_z(Omega) @ rot_x(i) @ rot_z(omega)

# Tempo total e resolução
t_max = 5400  # segundos (1.5 órbitas aproximadamente)
n_frames = 300
times = np.linspace(0, t_max, n_frames)

# Função para resolver Kepler e obter posição e velocidade no tempo t
def position_velocity(t):
    M_t = E0 - e * np.sin(E0) + n_mean * t
    kepler = lambda E: E - e * np.sin(E) - M_t
    E = fsolve(kepler, M_t)[0]
    nu = 2 * np.arctan2(np.sqrt(1 + e) * np.sin(E / 2),
                        np.sqrt(1 - e) * np.cos(E / 2))
    r_mag = a * (1 - e * np.cos(E))
    x_orb = r_mag * np.cos(nu)
    y_orb = r_mag * np.sin(nu)
    pos_orb = np.array([x_orb, y_orb, 0.0])
    pos_3D = R @ pos_orb

    # velocidade no plano orbital
    vx_orb = -np.sqrt(mu * a) / r_mag * np.sin(E)
    vy_orb = np.sqrt(mu * a) / r_mag * np.sqrt(1 - e**2) * np.cos(E)
    v_orb = np.array([vx_orb, vy_orb, 0.0])
    vel_3D = R @ v_orb

    return pos_3D, vel_3D

# Preparar figura
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')

# Terra
earth_radius = 6371e3
u, v = np.mgrid[0:2*np.pi:40j, 0:np.pi:20j]
x_sphere = earth_radius * np.cos(u) * np.sin(v)
y_sphere = earth_radius * np.sin(u) * np.sin(v)
z_sphere = earth_radius * np.cos(v)
ax.plot_surface(x_sphere, y_sphere, z_sphere, color='lightblue', alpha=0.6)

# Elementos da animação
traj, = ax.plot([], [], [], 'r-', lw=1, label='trajetória')
sat_point, = ax.plot([], [], [], 'ro', label='satélite')
vel_arrow = ax.quiver(0,0,0,0,0,0, color='green', label='velocidade')

# Histórico da trajetória
traj_data = []

# Configurar eixos
ax.set_xlim(-1.5e7, 1.5e7)
ax.set_ylim(-1.5e7, 1.5e7)
ax.set_zlim(-1.5e7, 1.5e7)
ax.set_xlabel('X (m)')
ax.set_ylabel('Y (m)')
ax.set_zlabel('Z (m)')
ax.set_title('Animação da órbita em 3D')
ax.legend()
ax.set_box_aspect([1, 1, 1])

# Função de atualização
def update(frame):
    t = times[frame]
    pos, vel = position_velocity(t)
    traj_data.append(pos)
    data = np.array(traj_data)
    traj.set_data(data[:, 0], data[:, 1])
    traj.set_3d_properties(data[:, 2])

    sat_point.set_data([pos[0]], [pos[1]])
    sat_point.set_3d_properties([pos[2]])

    # Vetor velocidade
    scale = 200
    vel_arrow.remove()
    new_arrow = ax.quiver(pos[0], pos[1], pos[2],
                          vel[0], vel[1], vel[2],
                          length=1, normalize=True, color='green')
    globals()['vel_arrow'] = new_arrow
    return traj, sat_point, vel_arrow


ani = FuncAnimation(fig, update, frames=n_frames, interval=50, blit=False)
plt.show()