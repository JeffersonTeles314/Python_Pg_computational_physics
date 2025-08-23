import math
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from mpl_toolkits.mplot3d import Axes3D

# Constante μ0 (permeabilidade magnética do vácuo)
MU0 = 4 * math.pi * 1e-7  # T m / A

def calcular_campo_magnetico(corrente, raio, distancia):
    """Calcula o campo magnético no eixo da espira usando a Lei de Biot-Savart"""
    denominador = 2 * (raio**2 + distancia**2) ** 1.5
    Bz = (MU0 * corrente * raio**2) / denominador
    return Bz

def gerar_espira(raio, num_pontos=100):
    """Gera pontos para plotar a espira circular no plano XY"""
    theta = np.linspace(0, 2*np.pi, num_pontos)
    x = raio * np.cos(theta)
    y = raio * np.sin(theta)
    z = np.zeros_like(theta)
    return x, y, z

def plotar_sistema(corrente, raio, distancia, Bz):
    """Gera o gráfico 3D da espira, do ponto P e do campo magnético"""
    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(111, projection='3d')
    
    # Configurações do gráfico
    ax.set_title(f'Campo Magnético de uma Espira Circular\n(I = {corrente} A, R = {raio} m, d = {distancia} m)', fontsize=14)
    ax.set_xlabel('X (m)', fontsize=12)
    ax.set_ylabel('Y (m)', fontsize=12)
    ax.set_zlabel('Z (m)', fontsize=12)
    ax.grid(True)
    
    # Gerar e plotar a espira
    x_espira, y_espira, z_espira = gerar_espira(raio)
    ax.plot(x_espira, y_espira, z_espira, 'b-', linewidth=2.5, label='Espira')
    ax.plot([0], [0], [0], 'bo', markersize=8)  # Centro da espira
    
    # Plotar o ponto P
    ax.plot([0], [0], [distancia], 'ro', markersize=8, label='Ponto P')
    
    # Calcular tamanho da seta proporcional ao campo
    max_axis = max(raio, abs(distancia)) * 1.8
    fator_seta = max_axis / 8
    
    # Determinar cor e direção da seta baseado no sentido do campo
    cor_seta = 'green' if corrente >= 0 else 'purple'
    direcao = 1 if corrente >= 0 else -1
    
    # Plotar o vetor do campo magnético
    ax.quiver(0, 0, distancia, 0, 0, direcao * fator_seta, 
              color=cor_seta, arrow_length_ratio=0.1, linewidth=2.5, 
              label='Campo Magnético')
    
    # Adicionar texto informativo
    texto_campo = f'$B_z$ = {abs(Bz):.2e} T\n'
    texto_campo += f'Direção: {"+z" if corrente >= 0 else "-z"}'
    ax.text2D(0.05, 0.95, texto_campo, transform=ax.transAxes, fontsize=12,
              bbox=dict(facecolor='white', alpha=0.8))
    
    # Configurar limites dos eixos
    ax.set_xlim([-max_axis, max_axis])
    ax.set_ylim([-max_axis, max_axis])
    ax.set_zlim([-max_axis, max_axis])
    
    # Adicionar elementos visuais
    ax.plot([0, 0], [0, 0], [-max_axis, max_axis], 'k--', alpha=0.3)  # Linha do eixo
    ax.legend(loc='upper right')
    
    plt.tight_layout()
    plt.show()

def animar_campo(raio, corrente):
    """Gera uma animação do campo magnético ao longo do eixo Z"""
    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(111, projection='3d')
    
    # Configurações iniciais
    max_dist = raio * 3
    distancias = np.linspace(-max_dist, max_dist, 100)
    max_axis = max_dist * 1.2
    
    # Gerar espira
    x_espira, y_espira, z_espira = gerar_espira(raio)
    ax.plot(x_espira, y_espira, z_espira, 'b-', linewidth=2.5)
    ax.plot([0], [0], [0], 'bo', markersize=8)
    
    # Configurar eixos
    ax.set_xlim([-max_axis, max_axis])
    ax.set_ylim([-max_axis, max_axis])
    ax.set_zlim([-max_axis, max_axis])
    ax.set_title(f'Variação do Campo Magnético ao Longo do Eixo Z\n(I = {corrente} A, R = {raio} m)', fontsize=14)
    ax.set_xlabel('X (m)', fontsize=12)
    ax.set_ylabel('Y (m)', fontsize=12)
    ax.set_zlabel('Z (m)', fontsize=12)
    
    # Elemento inicial do campo
    ponto = ax.plot([0], [0], [distancias[0]], 'ro')[0]
    seta = ax.quiver(0, 0, distancias[0], 0, 0, 0, color='green')
    
    def update(frame):
        """Atualiza a posição e o campo para cada frame"""
        d = frame
        Bz = calcular_campo_magnetico(corrente, raio, d)
        
        # Atualizar ponto
        ponto.set_data([0], [0])
        ponto.set_3d_properties([d])
        
        # Calcular tamanho da seta proporcional ao campo
        fator_seta = max_axis / 8 * np.sign(Bz)
        
        # Atualizar seta
        seta.remove()
        new_arrow = ax.quiver(0, 0, d, 0, 0, fator_seta, 
                              color='green' if Bz >= 0 else 'purple',
                              arrow_length_ratio=0.1, linewidth=2.5)
        return ponto, new_arrow
    
    # Criar animação
    ani = FuncAnimation(fig, update, frames=distancias, blit=False, interval=50)
    
    plt.tight_layout()
    plt.show()
    return ani

def main():
    # Entrada de dados pelo usuário
    print("="*50)
    print("Simulação do Campo Magnético de uma Espira Circular")
    print("="*50)
    
    corrente = float(input("\nDigite a corrente na espira (A): "))
    raio = float(input("Digite o raio da espira (m): "))
    distancia = float(input("Digite a distância do centro da espira ao ponto P (m): "))
    
    # Calcular campo magnético
    Bz = calcular_campo_magnetico(corrente, raio, distancia)
    
    # Exibir resultados
    print("\n" + "="*50)
    print("Resultados:")
    print(f"• Campo magnético no ponto P: {abs(Bz):.4e} T")
    print(f"• Direção: Eixo Z {'positivo' if Bz >= 0 else 'negativo'}")
    print(f"• Sentido: {'Anti-horário' if corrente >= 0 else 'Horário'} pela regra da mão direita")
    
    # Plotar sistema
    plotar_sistema(corrente, raio, distancia, Bz)
    
    # Opção para gerar animação
    if input("\nDeseja gerar uma animação ao longo do eixo? (s/n): ").lower() == 's':
        animar_campo(raio, corrente)

if __name__ == "__main__":
    main()