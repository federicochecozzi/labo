# -*- coding: utf-8 -*-
"""
Created on Wed Aug 24 16:31:15 2022

@author: tiama
"""

import  numpy as np
import  time
import matplotlib.pyplot as plt

np.random.seed(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#que hace qyt tiros libres
def vec_ftirar(prob, qty):
  return sum(np.random.rand(qty, len(prob)) < prob) 

#defino los jugadores
#probs = [0.7,0.6]

t0 = time.time()

resultados = np.zeros(10000)

for i in range(10000):
    for j in range(100):
        resultados[i] += vec_ftirar([0.6],10)
    resultados[i] /= 100
    

t1 = time.time()
print(t1-t0)

plt.hist(x=resultados, bins='auto', color='#0504aa',
                            alpha=0.7, rwidth=0.85)

