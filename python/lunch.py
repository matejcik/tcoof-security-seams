import resultlib
from resultlib import make_colors, box_graph

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

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


def plot_morerooms():
    files = ["morerooms-" + s for s in ("optimizing", "satisfying", "onebyone")]
    headers = ["Optimizing solver", "Satisfying solver", "One-by-one solver"]
    datasets = []
    labels = []
    colors = make_colors(len(files))

    for ifn, header in zip(files, headers):
        data = read_csv(ifn)
        subsel = data[data.success & (data.hungry <= 25)]
        subsel = subsel[["hungry", "nsec"]]
        grouping = subsel.groupby("hungry")["nsec"]
        series = [s for _, s in grouping]

        datasets.append(series)
        labels.append(header)

    maxticks = max(len(d) for d in datasets)
    x_ticks = range(5, maxticks + 5)

    fig, ax = box_graph(datasets, labels, colors, x_ticks)
    ax.set_xlabel("Number of workers")

    fig.tight_layout()
    plt.savefig("workers-morerooms.pdf")
    plt.close()


def plot_simple():
    data = read_csv("workercount-simple")
    projects = data.projects.unique()
    colors = make_colors(len(projects))
    labels = [f"{n} projects" for n in projects]
    x_ticks = []
    datasets = []

    for n in projects:
        subsel = data[data.projects == n][["workers", "nsec"]]
        grouping = subsel.groupby("workers")["nsec"]
        ticks, series = zip(*grouping)
        if len(ticks) > len(x_ticks):
            x_ticks = ticks
        datasets.append(series)

    fig, ax = box_graph(datasets, labels, colors, x_ticks)
    ax.set_xlabel("Number of workers")

    fig.tight_layout()
    plt.savefig("workers-simple.pdf")
    plt.close()


def plot_onebyone():
    data = read_csv("oneworker-params")
    data = data[data.success]
    x_ticks = data.lunch_n.unique()
    project_counts = data.projects.unique()
    colors = make_colors(len(project_counts))
    labels = [f"{n} projects" for n in project_counts]
    datasets = []

    for n in project_counts:
        subsel = data[data.projects == n][["lunch_n", "nsec"]]
        grouping = subsel.groupby("lunch_n")["nsec"]
        series = [s for _, s in grouping]
        datasets.append(series)

    fig, ax = box_graph(datasets, labels, colors, x_ticks)
    ax.set_xlabel("Number of lunch rooms")

    plt.yticks([0, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 20000, 25000, 30000])

    fig.tight_layout()
    plt.savefig("workers-oneworker.pdf")
    plt.close()


def main():
    plot_simple()
    plot_morerooms()
    plot_onebyone()


if __name__ == "__main__":
    main()
