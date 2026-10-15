"""
Naive Bayes implementado do zero (apenas NumPy).

Contém:
  - GaussianNB    : atributos contínuos (verossimilhança gaussiana)
  - MultinomialNB : contagens / frequências (texto, bag-of-words)
  - Bag of words simples para o exemplo de texto

Teoria em uma linha:
    P(y | x) ∝ P(y) * ∏_j P(x_j | y)
A hipótese "naive" é justamente a independência condicional entre os
atributos dado a classe. Trabalhamos em log para evitar underflow:
    log P(y | x) ∝ log P(y) + Σ_j log P(x_j | y)
"""

import numpy as np


# ----------------------------------------------------------------------
# 1) Naive Bayes Gaussiano — atributos contínuos
# ----------------------------------------------------------------------
class GaussianNB:
    """
    Assume P(x_j | y) ~ Normal(mu_{j,y}, sigma²_{j,y}).

    Parâmetros
    ----------
    var_smoothing : float
        Fração da maior variância global somada a todas as variâncias,
        para estabilidade numérica quando um atributo é constante na classe.
    """

    def __init__(self, var_smoothing=1e-9):
        self.var_smoothing = var_smoothing

    def fit(self, X, y):
        X = np.asarray(X, dtype=float)
        y = np.asarray(y)

        self.classes_ = np.unique(y)
        n_classes, n_features = len(self.classes_), X.shape[1]

        self.theta_ = np.zeros((n_classes, n_features))   # médias
        self.var_ = np.zeros((n_classes, n_features))     # variâncias
        self.log_prior_ = np.zeros(n_classes)

        epsilon = self.var_smoothing * X.var(axis=0).max()

        for i, c in enumerate(self.classes_):
            Xc = X[y == c]
            self.theta_[i] = Xc.mean(axis=0)
            self.var_[i] = Xc.var(axis=0) + epsilon
            self.log_prior_[i] = np.log(len(Xc) / len(X))

        return self

    def _joint_log_likelihood(self, X):
        """log P(y) + Σ_j log P(x_j | y) para cada classe. Shape: (n, n_classes)."""
        X = np.asarray(X, dtype=float)
        jll = np.zeros((X.shape[0], len(self.classes_)))

        for i in range(len(self.classes_)):
            # log da densidade normal, somado sobre os atributos
            log_det = -0.5 * np.sum(np.log(2.0 * np.pi * self.var_[i]))
            quad = -0.5 * np.sum(((X - self.theta_[i]) ** 2) / self.var_[i], axis=1)
            jll[:, i] = self.log_prior_[i] + log_det + quad

        return jll

    def predict_log_proba(self, X):
        jll = self._joint_log_likelihood(X)
        # normalização por log-sum-exp (estável numericamente)
        log_norm = _logsumexp(jll, axis=1, keepdims=True)
        return jll - log_norm

    def predict_proba(self, X):
        return np.exp(self.predict_log_proba(X))

    def predict(self, X):
        return self.classes_[np.argmax(self._joint_log_likelihood(X), axis=1)]

    def score(self, X, y):
        return float(np.mean(self.predict(X) == np.asarray(y)))


# ----------------------------------------------------------------------
# 2) Naive Bayes Multinomial — contagens (texto)
# ----------------------------------------------------------------------
class MultinomialNB:
    """
    Assume P(x_j | y) ~ proporção do termo j dentro da classe y,
    com suavização de Laplace/Lidstone (alpha) para evitar prob. zero.

    Parâmetros
    ----------
    alpha : float
        alpha = 1.0 -> Laplace; 0 < alpha < 1 -> Lidstone.
    """

    def __init__(self, alpha=1.0):
        self.alpha = alpha

    def fit(self, X, y):
        X = np.asarray(X, dtype=float)
        y = np.asarray(y)

        self.classes_ = np.unique(y)
        n_classes, n_features = len(self.classes_), X.shape[1]

        count = np.zeros((n_classes, n_features))
        self.log_prior_ = np.zeros(n_classes)

        for i, c in enumerate(self.classes_):
            Xc = X[y == c]
            count[i] = Xc.sum(axis=0)
            self.log_prior_[i] = np.log(len(Xc) / len(X))

        smoothed = count + self.alpha
        # log P(termo j | classe i)
        self.feature_log_prob_ = np.log(smoothed) - np.log(
            smoothed.sum(axis=1, keepdims=True)
        )
        return self

    def _joint_log_likelihood(self, X):
        X = np.asarray(X, dtype=float)
        return X @ self.feature_log_prob_.T + self.log_prior_

    def predict_log_proba(self, X):
        jll = self._joint_log_likelihood(X)
        return jll - _logsumexp(jll, axis=1, keepdims=True)

    def predict_proba(self, X):
        return np.exp(self.predict_log_proba(X))

    def predict(self, X):
        return self.classes_[np.argmax(self._joint_log_likelihood(X), axis=1)]

    def score(self, X, y):
        return float(np.mean(self.predict(X) == np.asarray(y)))


# ----------------------------------------------------------------------
# Utilitários
# ----------------------------------------------------------------------
def _logsumexp(a, axis=None, keepdims=False):
    a_max = np.max(a, axis=axis, keepdims=True)
    out = np.log(np.sum(np.exp(a - a_max), axis=axis, keepdims=True))
    out += a_max
    return out if keepdims else np.squeeze(out, axis=axis)


class BagOfWords:
    """Vetorizador mínimo: texto -> vetor de contagens."""

    def fit(self, docs):
        vocab = sorted({t for d in docs for t in self._tokens(d)})
        self.vocab_ = {t: i for i, t in enumerate(vocab)}
        return self

    def transform(self, docs):
        X = np.zeros((len(docs), len(self.vocab_)))
        for i, d in enumerate(docs):
            for t in self._tokens(d):
                j = self.vocab_.get(t)
                if j is not None:
                    X[i, j] += 1
        return X

    def fit_transform(self, docs):
        return self.fit(docs).transform(docs)

    @staticmethod
    def _tokens(doc):
        return "".join(c.lower() if c.isalnum() else " " for c in doc).split()


def train_test_split(X, y, test_size=0.3, seed=42):
    rng = np.random.default_rng(seed)
    idx = rng.permutation(len(X))
    n_test = int(len(X) * test_size)
    te, tr = idx[:n_test], idx[n_test:]
    return X[tr], X[te], y[tr], y[te]


def matriz_confusao(y_true, y_pred, classes):
    cm = np.zeros((len(classes), len(classes)), dtype=int)
    pos = {c: i for i, c in enumerate(classes)}
    for t, p in zip(y_true, y_pred):
        cm[pos[t], pos[p]] += 1
    return cm


# ----------------------------------------------------------------------
# Demonstração
# ----------------------------------------------------------------------
if __name__ == "__main__":
    # --- Exemplo 1: dados contínuos (3 classes gaussianas sintéticas) -----
    rng = np.random.default_rng(0)
    centros = np.array([[0.0, 0.0], [4.0, 4.0], [0.0, 5.0]])
    X = np.vstack([rng.normal(c, 1.2, size=(200, 2)) for c in centros])
    y = np.repeat([0, 1, 2], 200)

    Xtr, Xte, ytr, yte = train_test_split(X, y)
    gnb = GaussianNB().fit(Xtr, ytr)

    print("=== GaussianNB (dados sintéticos) ===")
    print(f"Acurácia treino : {gnb.score(Xtr, ytr):.4f}")
    print(f"Acurácia teste  : {gnb.score(Xte, yte):.4f}")
    print("Matriz de confusão (linhas=real, colunas=predito):")
    print(matriz_confusao(yte, gnb.predict(Xte), gnb.classes_))
    print("Probabilidades para [2, 2]:", np.round(gnb.predict_proba([[2, 2]])[0], 4))

    # --- Exemplo 2: classificação de texto (spam vs. normal) -------------
    docs = [
        "ganhe dinheiro rapido clique aqui",
        "promocao imperdivel ganhe premio agora",
        "voce ganhou um premio clique no link",
        "credito facil dinheiro na hora clique",
        "reuniao de equipe amanha as dez horas",
        "segue em anexo o relatorio do projeto",
        "podemos remarcar a reuniao para sexta",
        "obrigado pelo envio do relatorio final",
    ]
    rotulos = np.array(["spam"] * 4 + ["normal"] * 4)

    bow = BagOfWords()
    Xtxt = bow.fit_transform(docs)
    mnb = MultinomialNB(alpha=1.0).fit(Xtxt, rotulos)

    novos = [
        "clique aqui e ganhe dinheiro agora",
        "envio o relatorio da reuniao de amanha",
    ]
    Xnovo = bow.transform(novos)
    pred = mnb.predict(Xnovo)
    prob = mnb.predict_proba(Xnovo)

    print("\n=== MultinomialNB (texto) ===")
    print(f"Vocabulário: {len(bow.vocab_)} termos | classes: {list(mnb.classes_)}")
    for texto, p, pr in zip(novos, pred, prob):
        conf = pr[list(mnb.classes_).index(p)]
        print(f"  {texto!r} -> {p} (confiança {conf:.3f})")
