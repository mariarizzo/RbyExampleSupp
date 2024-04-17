# Prussian horsekick data
k = c(0, 1, 2, 3, 4)
x = c(109, 65, 22, 3, 1)
p = x / sum(x)        #relative frequencies
print(p)

r = sum(k * p)   #mean
v = sum(x * (k - r)^2) / 199  #variance
print(r)
print(v)
f = dpois(k, r)
print(cbind(k, p, f))
