# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np

array = np.zeros([15,500], dtype = int)
i = 0
# Copier/coller les listes affichées dans 
# l'invite de commande ACTR sans les parenthèse

s = ""
s = s.split()
array[i,:] = s[:]
i += 1

s = ""
s = s.split()
array[i,:] = s[:]
i += 1

s = ""
s = s.split()
array[i,:] = s[:]
i += 1

# Copier/coller les blocs de 4 lignes pour faire des meilleurs moyennes

array = np.mean(array, axis=0)

plt.plot(array)
plt.show()