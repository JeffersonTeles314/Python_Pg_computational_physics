
k = int(input())
a = [1, 2, 3, 4, 5]
# a = list(map(int, input().split()))
best = 0

l = 0 # início da janela
s = 0 # soma da janela atual

for r in range(len(a)): # r = fim da janela
    s += a[r] # adiciona o novo elemento à janela
    while s > k:
        s -= a[l]
        l += 1
    best = max(best, r-l+1)

print(best)