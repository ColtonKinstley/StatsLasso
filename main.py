import pandas as pd
import sklearn as sk
import matplotlib.pyplot as plt
import numpy as np


def nSampleFromY(beta, n, sd):
    p = len(beta)
    designMatrix = np.array([])
    matrix = 
    for i in range(p):
        designMatrix[i] = np.random.normal(0,1)*beta[i] 
    y = sum(designMatrix) + np.random.normal(0,sd)
        


#do some stuff

