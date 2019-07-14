#%%
import resultlib
from resultlib import make_colors, box_graph

resultlib.set_result_path("2019-07-12")

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline

COLUMNS = {
    "projects": np.int32,
    "lunch_n": np.int32,
    "lunch_cap": np.int32,
    "work_n": np.int32,
    "work_cap": np.int32,
    "workers": np.int32,
    "hungry": np.int32,
    "full": str,
    "lunchtime": str,
}


def read_csv(label, scaling_factor=1e-6):
    df = resultlib.read_csv(label, COLUMNS, scaling_factor)
    df.full = df.full == "true"
    df.lunchtime = df.lunchtime == "true"
    return df

#%%
data = read_csv("badsolver-growingprojects")

#%%
proj = data.projects.unique()
for n in proj:
    s = data[data.projects == n].nsec
    s = s.sort_values(ascending=True).reset_index(drop=True)
    s.plot(label=str(n))

#%%

fig, ax = resultlib.prepare_graph()

ax.errorbar(linedata.index, linedata.values, yerr=lineerr.values, fmt="o", label="helo")
plt.xticks(linedata.index)
plt.legend(loc="upper left")
plt.tight_layout()
plt.show()

#%%
