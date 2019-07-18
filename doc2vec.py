# -*- coding: utf-8 -*-
"""
Created on Mon May 13 11:29:11 2019

@author: jnumpaque
"""

from gensim.models import doc2vec
from collections import namedtuple
import pandas as pd
import gensim
    
def doc2vec_model(texto, long_vector, freq_min, semilla = 1234, ventana = 300):
    # Entrenando
    docs = []
    analyzedDocument = namedtuple('AnalyzedDocument', 'words tags')
        
    for i, text in enumerate(texto):
        words = text.split()
        tags = [i]
        docs.append(analyzedDocument(words, tags))
    
    # Entrenando el modelo (vector longitud 50)
    model = doc2vec.Doc2Vec(docs, vector_size = long_vector, min_count = freq_min, window = ventana, seed = semilla, workers = 1, negative = 5)
       
    return(model)
    
def doc2vec_predict(modelo, texto):
    
    Doc2Vec=[]
    for i in range(0, len(texto)):
        tokenizado = gensim.utils.simple_preprocess(texto[i])
        Doc2Vec.append(modelo.infer_vector(tokenizado))
    
    p=pd.DataFrame(Doc2Vec)
    
    return(p)