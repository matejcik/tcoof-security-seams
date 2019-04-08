#%%
%matplotlib inline

import myresults
from myresults import read_csv, box_graph

import matplotlib
import matplotlib.pyplot as plt

#%%

myresults.RESULT_PATH = "../results/2019-04-08"

sat_data = read_csv("more-rooms-than-projects-sat", 1e-6)
norm_data = read_csv("more-rooms-than-projects", 1e-6)

#%%


def subselect(df):
    df = df[~df.failed][["hungry", "nsec"]]
    g = df.groupby("hungry")
    return [s for _, s in g["nsec"]]


sat = subselect(sat_data)
norm = subselect(norm_data)

xes = [i for i in range(5, 5 + len(sat))]

#%%

colors = ["#e41a1c", "#377eb8", "#4daf4a", "#984ea3"]

labels = ["Satisfiability problem", "Optimization problem"]

_, ax = box_graph([sat, norm], labels, colors, xes)
ax.set_ylabel("Computation time (ms)")
ax.set_xlabel("Number of hungry workers")
plt.show()

#%%

import numpy as np
import pandas as pd

cells = {
    "phase": np.int32,
    "factories": np.int32,
    "workers": np.int32,
    "late": np.float32,
    "i": np.int32,
    "time": np.int64,
}

filename = "../../trust/data/1.log"


df = pd.read_csv(filename, header=None, names=cells.keys(), dtype=cells)

#%%

data = df[df.late == 0.1]
subsel = data[["workers", "time"]].groupby("workers")["time"]

xes, boxes = zip(*subsel)

filtered_boxes = [b if len(b) == 100 else [] for b in boxes]

box_graph([filtered_boxes], ["hi"], colors, xes)
plt.show()

#%%
