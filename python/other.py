import resultlib

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

import seaborn as sns

COLUMNS = {"timeout": np.int32}


def plot_timeouts():
    data = resultlib.read_csv("timelimits", COLUMNS)
    data.timeout //= 1000
    grouping = data.groupby("timeout")["utility"]
    xticks, series = zip(*grouping)

    fig, ax = resultlib.prepare_graph()
    ax.boxplot(series, showfliers=False, positions=xticks)

    filtered = data[data.success].groupby("timeout")["utility"]
    meds = filtered.median()
    ax.plot(meds.index, meds.values, "o")

    def log(x, a, b):
        return a + b * np.log(x)

    popt, _ = curve_fit(log, meds.index, meds.values)

    fitx = np.linspace(xticks[0], xticks[-1], 100)
    fity = log(fitx, *popt)
    plt.plot(fitx, fity, "-", color=resultlib.make_colors(4)[3])

    ax.set_xlabel("Time limit (s)")
    ax.set_ylabel("Total utility")

    fig.tight_layout()
    plt.savefig("timelimits.pdf")
    plt.close()


def plot_simulation():
    data = resultlib.read_csv("simulated", {"iter": np.int32})
    data = data["nsec"]

    fig = plt.figure(figsize=(7, 5), dpi=300)
    ax = fig.add_subplot(111)
    ax.yaxis.grid(True, linestyle="-", which="major", color="lightgrey", alpha=0.5)

    # hist = data["nsec"].sort_values(ascending=True).reset_index(drop=True)
    # ax.plot(hist)
    # sns.distplot(data["nsec"], hist=True, kde=True)
    logbins = np.geomspace(data.min(), data.max(), 50)
    ax.hist(data, bins=logbins)
    ax.set_xlabel("Computation time (ms)")
    ax.set_ylabel("Number of samples")
    plt.xscale("log")
    plt.yscale("log")
    ax.xaxis.grid(False)

    fig.tight_layout()
    plt.savefig("simulation.pdf")
    plt.close()


def main():
    plot_timeouts()
    plot_simulation()


if __name__ == "__main__":
    main()
