

# Se procede a hacer funciones las cuales permitan hacer lo que se desea
# para el ejercicio 1

# Se importan las librerías necesarias para el ejercicio 1 y 2
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import pandas as pd
import numpy as np
import time
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.manifold import MDS
from scipy.spatial.distance import euclidean
import time
import matplotlib.pyplot as plt


class ejercicio_1:
    def __init__(self, df):
        scaler = StandardScaler().fit(df)
        
        # Normalizamos los datos
        self.__datos = scaler.transform(df)
    
    
    @property
    def datos(self):
        return self.__datos
    
    @datos.setter
    def datos(self, df):
        self.__datos = df

    # Función para multistart de k-means con inicialización aleatoria (método similar a Forgy)
    def kmeans_multistart(self, n_clusters, n_runs):
        X = self.__datos

        mejor_inercia = float('inf')
        mejores_etiquetas = None
        inercia_promedio = 0
        inercia = []
        tiempo_transcurrido = []
        total_iterations = 0
        
        
        for _ in range(n_runs):
            
            kmeans = KMeans(n_clusters=n_clusters, init='random', n_init=1)  # Simulando Forgy con init='random' y n_init=1
            # Guardamos el tiempo de ejecución
            start_time = time.time()
            kmeans.fit(X)
            tiempo_transcurrido.append(time.time() - start_time)
            
            # Guardamos el número de iteraciones
            total_iterations += kmeans.n_iter_
            
            # Guardamos la inercia promedio
            inercia_promedio += kmeans.inertia_
            
            # Guardamos las inercias de cada ejecución
            inercia.append(kmeans.inertia_)
            

            if kmeans.inertia_ < mejor_inercia:
                mejor_inercia = kmeans.inertia_
                mejores_etiquetas = kmeans.labels_
                i = 1
            
            elif kmeans.inertia_ == mejor_inercia:
                etiquetas_indices = [np.where(kmeans.labels_ == label)[0][0] for label in np.unique(kmeans.labels_)]
                reemplazo = {x:y for x,y in zip(kmeans.labels_[etiquetas_indices], mejores_etiquetas[etiquetas_indices])}
                serie = pd.Series(kmeans.labels_)
                serie = serie.replace(reemplazo)

                if np.array_equal(mejores_etiquetas, serie):
                    i += 1

        
        inercia_promedio /= n_runs
        iteraciones_promedio = total_iterations / n_runs

        tau = i/n_runs*100

        self.inercia = inercia
        self.tiempo_transcurrido = tiempo_transcurrido
        return {
            'mejor_inercia': mejor_inercia,
            'mejores_etiquetas': mejores_etiquetas,
            'inercia_promedio': inercia_promedio,
            'tiempo_transcurrido': tiempo_transcurrido,
            'iteraciones_promedio': iteraciones_promedio,
            'tau': tau, 
            'inertia': inercia
        }
    

    def plot_comportamiento_locales(self):
        
        sns.histplot(self.inercia, bins=30)
        plt.title('Distribución de la inercia')
        plt.xlabel('Inercia')
        plt.ylabel('Frecuencia')
        plt.show()


    def plot_comportamiento_tiempos(self):       
        sns.histplot(self.tiempo_transcurrido)
        plt.title('Distribución de los tiempos de ejecución')
        plt.xlabel('Tiempo de ejecución')
        plt.ylabel('Frecuencia')
        plt.show()


    


