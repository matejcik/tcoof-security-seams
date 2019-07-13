import resultlib
from resultlib import make_colors, box_graph

resultlib.set_result_path("2019-07-12")

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
    "fill": np.int32,
    "lunchtime": str,
}


def read_csv(label, scaling_factor=1e-6):
    df = resultlib.read_csv(label, COLUMNS, scaling_factor)
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


def plot_moreprojects():
    data = read_csv("moreprojects-optimizing")
    lunchrooms = data.lunch_n.unique()
    colors = make_colors(len(lunchrooms))
    labels = [f"{n} lunchrooms" for n in lunchrooms]
    x_ticks = list(range(5, 41))
    datasets = []

    for n in lunchrooms[:2]:
        subsel = data[data.lunch_n == n][["hungry", "nsec"]]
        # if n == 2:
        #     subsel.nsec *= 50
        grouping = subsel.groupby("hungry")["nsec"]
        series = [s for _, s in grouping]

        datasets.append(series)

    # maxticks = max(len(d) for d in datasets)
    # x_ticks = range(5, maxticks + 5)

    fig, ax = box_graph(datasets, labels, colors, x_ticks, rotation="vertical")
    ax.set_xlabel("Number of workers")

    fig.tight_layout()
    plt.savefig("workers-moreprojects.pdf")
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

    fig, ax = resultlib.prepare_graph()

    for label, color, n in zip(labels, colors, project_counts):
        subsel = data[data.projects == n][["lunch_n", "nsec"]]
        grouping = subsel.groupby("lunch_n")["nsec"]
        linedata = grouping.mean()
        lineerr = grouping.std()
        ax.errorbar(
            linedata.index,
            linedata.values,
            yerr=lineerr,
            fmt="o",
            label=label,
            color=color,
        )

    plt.xticks(x_ticks, rotation="45")
    plt.legend(loc="upper left")
    ax.set_xlabel("Number of lunch rooms")
    fig.tight_layout()
    plt.savefig("workers-oneworker.pdf")
    plt.close()


def plot_blabla():
    data = read_csv("blabla")
    fills = [0]
    projects = data.projects.unique()
    colors = make_colors(len(projects) * len(fills))
    labels = [f"{n} projects (fill {f})" for f in fills for n in projects]
    x_ticks = list(range(5, 31))
    datasets = []
    for f in fills:
        for n in projects:
            subsel = data[data.hungry <= 30]
            subsel = subsel[subsel.projects == n]
            subsel = subsel[subsel.fill == f]
            subsel = subsel[["hungry", "nsec"]]
            grouping = subsel.groupby("hungry")["nsec"]
            series = [s for _, s in grouping]
            datasets.append(series)

    fig, ax = box_graph(datasets, labels, colors, x_ticks, rotation="vertical")
    ax.set_xlabel("Number of workers")

    fig.tight_layout()
    plt.savefig("blabla.pdf")
    plt.close()


def main():
    plot_blabla()
    # plot_simple()
    # plot_morerooms()
    # plot_onebyone()
    # plot_moreprojects()


if __name__ == "__main__":
    main()
