import sympy as sp

def resolver_euler_lagrange(lagrangiana_str, variavel_generalizada='q'):
    # Definir símbolos
    t = sp.Symbol('t')
    q = sp.Function(variavel_generalizada)(t)
    dq = q.diff(t)

    # Criar símbolo temporário para facilitar substituições
    q_sym = sp.Symbol(variavel_generalizada)
    dq_sym = sp.Symbol(f'd{variavel_generalizada}')

    # Interpretar string como expressão simbólica
    L_expr = sp.sympify(lagrangiana_str)

    # Substituir variáveis simbólicas por funções do tempo
    L = L_expr.subs({q_sym: q, dq_sym: dq})

    # Derivar a Lagrangiana
    dL_dq = sp.diff(L, q)
    dL_ddq = sp.diff(L, dq)
    dt_dL_ddq = sp.diff(dL_ddq, t)

    # Equação de Euler-Lagrange
    EL_eq = sp.simplify(dt_dL_ddq - dL_dq)

    # Exibir resultados
    print(f"Lagrangiana L = {L}")
    print(f"\ndL/dq = {dL_dq}")
    print(f"dL/ddq = {dL_ddq}")
    print(f"d/dt(dL/ddq) = {dt_dL_ddq}")
    print(f"\nEquação de Euler-Lagrange:\n{EL_eq} = 0")

    return EL_eq

# === Exemplo de uso ===
m, k = sp.symbols('m k')
lagrangiana = "(1/2)*m*(dq)**2 - (1/2)*k*q**2"

resolver_euler_lagrange(lagrangiana)
