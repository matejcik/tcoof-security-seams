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
    flt = data[data.success]
    grouping = flt.groupby("timeout")["utility"]
    xticks, series = zip(*grouping)

    fig, ax = resultlib.prepare_graph()
    plt.xticks(rotation="vertical")
    ax.boxplot(series, showfliers=False, positions=xticks)

    def log(x, a, b):
        return a + b * np.log(x)

    meds = grouping.median()
    popt, _ = curve_fit(log, meds.index, meds.values)

    fitx = np.linspace(xticks[0], xticks[-1], 100)
    fity = log(fitx, *popt)
    ax.plot(fitx, fity, "-")

    color = "#d62728"
    ax2 = ax.twinx()
    ax2.set_yticks(list(range(0, 101, 10)))
    ax2.set_ylim(0, 100)
    perc = 100 - grouping.count()
    ax2.plot(perc.index, perc.values, "--", color=color)

    ax2.set_ylabel("Failed attempts (out of 100)")
    ax2.tick_params(axis="y", labelcolor=color)

    plt.xlim(0.5, 30.5)

    ax.set_xlabel("Time limit (s)")
    ax.set_ylabel("Total utility")

    fig.tight_layout()
    plt.savefig("timelimits.pdf")
    plt.close()


def plot_simulation():
    data = resultlib.read_csv("simulated", {"iter": np.int32})
    data = data["nsec"]

    print(data[data > 100].count())

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
