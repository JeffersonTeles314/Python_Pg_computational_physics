module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
        volt = (x**2.0d0)/4.0d0
    end     function

real*8 function func_g(x, e, a)
    real*8, intent(in) :: x, e, a
    func_g = (2.0d0 * (e - volt(x, a))) / hbar2_2m
end function

real*8 function numerov_sobe(e, a, x_ini, match)
    real*8, intent(in) :: e, a, x_ini, match
    real*8 :: x_atual, h, h2, h2_12
    real*8 :: y_prev, y_curr, y_next
    real*8 :: g_prev, g_curr, g_next
    real*8 :: derivada

    h = 0.01d0
    h2 = h**2
    h2_12 = h2 / 12.0d0
    
    ! 1. Condições Iniciais no "infinito" à esquerda
    x_atual = x_ini
    y_prev = 0.0d0
    y_curr = 1.0d-8 ! Pequeno valor para iniciar a integração

    ! 2. Loop de integração até o Match Point
    ! Precisamos parar um passo antes do match para calcular o y_next no ponto exato
    do while (x_atual + h .LE. match + (h/2.0d0))
        
        ! Calculamos os valores de g(x) para os três pontos
        ! Lembre-se: func_g agora deve ser (2m/hbar2)*(E - V)
        g_prev = func_g(x_atual, e, a)
        g_curr = func_g(x_atual + h, e, a)
        g_next = func_g(x_atual + 2.0d0*h, e, a)

        ! 3. Fórmula de Numerov corrigida para: y_next
        ! y_next*(1 + g_next*h^2/12) = 2*y_curr*(1 - 5*g_curr*h^2/12) - y_prev*(1 + g_prev*h^2/12)
        
        y_next = (2.0d0 * y_curr * (1.0d0 - 5.0d0 * h2_12 * g_curr) - &
                  y_prev * (1.0d0 + h2_12 * g_prev)) / (1.0d0 + h2_12 * g_next)

        ! Atualização das variáveis para o próximo passo
        y_prev = y_curr
        y_curr = y_next
        x_atual = x_atual + h
    end do

    ! 4. Cálculo da Derivada Logarítmica no Match Point
    ! Usando a fórmula que mantém a precisão de quarta ordem
    ! Aqui, y_curr é a função no match point, y_prev e y_next são vizinhos
    
    derivada = (y_next - y_prev) / (2.0d0 * h)
    ! (Opcional) Adicionar correção de curvatura h^2/12 se quiser precisão extrema:
    ! derivada = derivada + h2_12 * (g_prev*y_prev - g_next*y_next)

    numerov_sobe = derivada / y_curr

end function

real*8 function numerov_desce(e, a, x_ini, match)
    real*8, intent(in) :: e, a, x_ini, match
    real*8 :: x_atual, h, h2_12
    real*8 :: y_prev, y_curr, y_next
    real*8 :: g_prev, g_curr, g_next
    real*8 :: derivada

    ! Definimos h negativo pois estamos vindo da direita para a esquerda
    h = -0.01d0
    h2_12 = (h**2) / 12.0d0
    
    ! 1. Condições Iniciais no "infinito" à direita (x_ini deve ser positivo, ex: 10.0)
    x_atual = x_ini
    y_prev = 0.0d0
    y_curr = 1.0d-8 ! Chute inicial para começar a integração

    ! 2. Loop de integração descendo até o Match Point
    ! O critério .GE. garante que paremos ao atingir o ponto de encontro vindo da direita
    do while (x_atual + h .GE. match - (abs(h)/2.0d0))
        
        ! Calculamos g(x) para os três pontos (atrás, atual, frente no sentido da integração)
        g_prev = func_g(x_atual, e, a)
        g_curr = func_g(x_atual + h, e, a)
        g_next = func_g(x_atual + 2.0d0*h, e, a)

        ! 3. Fórmula de Numerov (Iteração para y_next)
        y_next = (2.0d0 * y_curr * (1.0d0 - 5.0d0 * h2_12 * g_curr) - &
                  y_prev * (1.0d0 + h2_12 * g_prev)) / (1.0d0 + h2_12 * g_next)

        ! Atualização das variáveis para o próximo passo
        y_prev = y_curr
        y_curr = y_next
        x_atual = x_atual + h
    end do

    ! 4. Cálculo da Derivada Logarítmica no Match Point
    ! Importante: Como h é negativo, a ordem (y_next - y_prev) / (2*h) 
    ! mantém o sinal correto da derivada física.
    derivada = (y_next - y_prev) / (2.0d0 * h)

    ! Retorna a derivada logarítmica (psi' / psi)
    numerov_desce = derivada / y_curr

end function
end module

program edos
    use meu_modulo
    implicit none
    
    real*8 :: x_inicial, x_final, a, match_pt
    real*8 :: valsobe, valdesce, comp_atual, comp_anterior
    real*8 :: e_atual, delta_e, e_inicial, tol_e
    integer :: bissec_iterations

    ! --- Configurações Iniciais ---
    a = 3.0d0           ! Parâmetro do potencial
    x_inicial = -7.0d0  ! Longe o suficiente para psi -> 0
    x_final = 7.0d0     ! Longe o suficiente para psi -> 0
    match_pt = 0.0d0    ! Ponto de encontro (origem)
    
    e_inicial = 3.5d0   ! Começar de uma energia baixa
    delta_e = 0.05d0    ! Passo de varredura inicial
    tol_e = 1.0d-10     ! Tolerância de precisão
    e_atual = e_inicial
    bissec_iterations = 0
    
    open(13, file="results.txt")
    open(14, file="ratio.txt")
    
    ! --- Cálculo do primeiro ponto de erro ---
    ! Note: passamos (E, A, Início, Match)
    valsobe = numerov_sobe(e_atual, a, x_inicial, match_pt)
    valdesce = numerov_desce(e_atual, a, x_final, match_pt)
    comp_atual = valsobe - valdesce

    write(*,*) "Iniciando busca de autovalores..."
    write(*,*) "E atual | Erro (Match)"

    ! ===== LOOP DE BUSCA E BISSEÇÃO =====
    10 continue
    
    comp_anterior = comp_atual
    e_atual = e_atual + delta_e
    
    ! Calcula novo erro no match point
    valsobe = numerov_sobe(e_atual, a, x_inicial, match_pt)
    valdesce = numerov_desce(e_atual, a, x_final, match_pt)
    comp_atual = valsobe - valdesce
    
    write(14, "(I5, 3F15.8)") bissec_iterations, e_atual, valsobe, valdesce
    write(*, "(F12.8, E15.4)") e_atual, comp_atual

    ! Se houver troca de sinal, o autovalor está entre e_atual-delta_e e e_atual
    if (comp_anterior * comp_atual .LT. 0.0d0) then
        ! Se o erro for muito discrepante (assíntota), pode não ser um autovalor
        ! mas para o oscilador harmônico, a troca de sinal costuma ser segura.
        
        e_atual = e_atual - delta_e   ! Volta para o passo anterior
        comp_atual = comp_anterior     ! Restaura o erro anterior
        delta_e = delta_e / 2.0d0      ! Refina o passo (bisseção)
    end if

    bissec_iterations = bissec_iterations + 1

    ! Critério de parada: precisão alcançada
    if (dabs(delta_e) .GT. tol_e) then
        if (bissec_iterations .LT. 10000) goto 10
    endif

    ! --- Resultado Final ---
    write(*,*) "------------------------------------"
    write(*,*) "Energia encontrada: ", e_atual
    write(*,*) "Iteracoes: ", bissec_iterations
    
    write(13,*) "Energia: ", e_atual
    write(13,*) "Iteracoes: ", bissec_iterations

    close(13)
    close(14)

end program edos

