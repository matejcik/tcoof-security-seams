import resultlib
from resultlib import make_colors, box_graph

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.optimize import curve_fit

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
    data = read_csv("morerooms-optimizing")
    data = data[["hungry", "nsec"]][data.hungry < 16]
    grouping = data.groupby("hungry")["nsec"]

    xticks, series = zip(*grouping)

    fig, ax = resultlib.prepare_graph()
    ax.boxplot(series, showfliers=False, positions=xticks)
    # plt.xticks(range(1, len(series) + 1), xticks)

    def exp(x, a, b, c):
        return a * np.exp(b * x) + c

    meds = grouping.median()
    popt, _ = curve_fit(exp, meds.index, meds.values)

    fitx = np.linspace(xticks[0], xticks[-1], 100)
    fity = exp(fitx, *popt)
    plt.plot(fitx, fity, "-")

    ax.set_xlabel("Number of workers")
    fig.tight_layout()
    plt.savefig("workers-morerooms.pdf")
    plt.close()


def plot_moreprojects():
    data = read_csv("moreprojects")
    data = data[(data.hungry <= 40) & data.success]
    # projects = data.projects.unique()
    projects = [5, 6, 7, 8, 9]
    labels = [f"{n} projects" for n in projects]
    x_ticks = data[data.projects == projects[0]].hungry.unique()
    datasets = []

    fig, ax = resultlib.prepare_graph()

    for n, label in zip(projects, labels):
        subsel = data.copy()

        # clear outliers after the expected cutoff point
        maxhungry = n * 4 + 2
        subsel.loc[subsel.hungry > maxhungry, "nsec"] = 0

        subsel = subsel[subsel.projects == n][["hungry", "nsec"]]
        grouping = subsel.groupby("hungry")["nsec"]
        series = [s for _, s in grouping]

        line = grouping.median()
        ax.plot(line.index, line.values, "-", label=label)

        datasets.append(series)

    # maxticks = max(len(d) for d in datasets)
    # x_ticks = range(5, maxticks + 5)

    # fig, ax = box_graph(datasets, labels, colors, x_ticks, rotation="vertical")
    ax.set_xlabel("Number of workers")
    plt.legend()
    plt.xticks(x_ticks, rotation="vertical")
    # plt.yscale("log")

    fig.tight_layout()
    plt.savefig("workers-moreprojects.pdf")
    plt.close()


def plot_simple():
    data = read_csv("workercount-simple")
    projects = data.projects.unique()
    fmts = {5: "-o", 15: "-v", 50: "-x"}

    fig, ax = resultlib.prepare_graph()

    for n in projects:
        subsel = data[data.projects == n][["workers", "nsec"]]
        grouping = subsel.groupby("workers")["nsec"]
        line = grouping.median()
        lineerr = grouping.std()
        ax.errorbar(
            line.index, line.values, yerr=lineerr, fmt=fmts[n], label=f"{n} projects"
        )

    plt.legend()
    plt.xticks(line.index, rotation="45")
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

    def poly(x, a, b, e):
        return a * x ** e + b

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

        popt, _ = curve_fit(poly, linedata.index, linedata.values)

        fitx = np.linspace(x_ticks[0], x_ticks[-1], 100)
        fity = poly(fitx, *popt)
        plt.plot(fitx, fity, "-", color=colors[1])

    plt.xticks(x_ticks, rotation="45")
    plt.legend(loc="upper left")
    ax.set_xlabel("Number of lunch rooms")
    fig.tight_layout()
    plt.savefig("workers-oneworker.pdf")
    plt.close()


def plot_badsolver():
    data = read_csv("badsolver-growingprojects")
    groups = data.groupby("projects")["nsec"]

    projects = list(range(4, 31, 4))

    fig, ax = resultlib.prepare_graph()
    for p in projects:
        series = groups.get_group(p)
        series = series.sort_values(ascending=True).reset_index(drop=True)
        ax.plot(series.index, series, "-", label=f"{p} projects")

    perc_ticks = list(range(0, 101, 10))
    plt.xticks([p * 5 for p in perc_ticks], perc_ticks)
    ax.set_xlabel("Percentile")
    plt.legend()

    fig.tight_layout()
    plt.savefig("badsolver.pdf")
    plt.close()


def main():
    plot_simple()
    plot_onebyone()
    plot_morerooms()
    plot_moreprojects()
    plot_badsolver()


if __name__ == "__main__":
    main()
