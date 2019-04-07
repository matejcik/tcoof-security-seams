#%%

import matplotlib

# matplotlib.use("Agg")

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

import csv


#%%

filename = "../results/2019-04-07/workercount-simple.log"


def read_csv(fn):
    cols = {
        "projects": np.int32,
        "lunch_n": np.int32,
        "lunch_cap": np.int32,
        "work_n": np.int32,
        "work_cap": np.int32,
        "workers": np.int32,
        "hungry": np.int32,
        "preassigned": np.int32,
        "lunchtime": str,
        "i": np.int32,
        "time": str,
    }

    df = pd.read_csv(
        fn, header=None, names=cols.keys(), dtype=cols, skipinitialspace=True
    )

    df.lunchtime = df.lunchtime == "true"
    df["failed"] = df.time == "FAIL"

    NS_TIME = int(30e9)
    df["nsec"] = df.time.replace("FAIL", NS_TIME).astype(np.float64) * 1e-6

    return df


#%%

df = read_csv("../results/190401/lunch-perf.log")
# ===== filter data for more-projects-than-rooms
data = df[df.projects == 7][["lunch_n", "hungry", "failed", "nsec"]]

#%%

data = data[data.hungry < 30]

lunchroom_counts = data.lunch_n.unique()
worker_counts = data.hungry.unique()
boxes = []
for n in lunchroom_counts:
    boxdata = data[(data.lunch_n == n) & (~data.failed)][["hungry", "nsec"]]
    grouping = boxdata.groupby("hungry")
    series = [s for _, s in grouping["nsec"]]
    series += [[]] * (len(worker_counts) - len(series))
    print(len(series), len(worker_counts))
    boxes.append((n, series))

#%%

colors = ["#e41a1c", "#377eb8", "#4daf4a", "#984ea3"]


def set_box_color(bp, color):
    plt.setp(bp["boxes"], color=color)
    plt.setp(bp["whiskers"], color=color)
    plt.setp(bp["caps"], color=color)
    plt.setp(bp["medians"], color=color)


fig = plt.figure(figsize=(7, 5), dpi=300)
ax = fig.add_subplot(111)

group_size = len(lunchroom_counts)
step_size = group_size + 1

for idx, (n, series) in enumerate(boxes):
    bp = ax.boxplot(
        series,
        positions=[i * step_size + idx for i in range(len(worker_counts))],
        showfliers=False,
        widths=0.6,
    )
    color = colors[idx]
    set_box_color(bp, color)
    plt.plot([], c=color, label=f"{n} lunchrooms")

plt.legend()
plt.xticks(
    [(x * step_size) + group_size / 2 - 0.5 for x in range(len(worker_counts))],
    worker_counts,
)
ax.set_xlabel("Hello")
plt.xlim(-1, step_size * len(worker_counts))
plt.show()
#%%

fig = plt.figure(figsize=(7, 5), dpi=300)
ax = fig.add_subplot(111)
# ax.xaxis.grid(True, linestyle="-", which="major", color="lightgrey", alpha=0.5)
# ax.yaxis.grid(True, linestyle="-", which="major", color="lightgrey", alpha=0.5)

nl = df[["workers", "nsec"]]
g = nl.groupby("workers")

keys, series = zip(*g["nsec"])
ax.boxplot(series, showfliers=False)
ax.set_xticklabels(keys)
plt.xticks(rotation=90)
plt.show()

#%%

g = nl.groupby("workers")
