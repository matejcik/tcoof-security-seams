import resultlib

resultlib.set_result_path("2019-07-14")

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

COLUMNS = {"timeout": np.int32}


def plot_timeouts():
    data = resultlib.read_csv("timelimits", COLUMNS)
    data.timeout //= 1000
    data = data[data.success & (data.timeout <= 30)]
    subsel = data[["timeout", "utility"]]
    grouping = subsel.groupby("timeout")["utility"]
    xticks, series = zip(*grouping)

    fig, ax = resultlib.prepare_graph()
    ax.boxplot(series, showfliers=False, positions=xticks)

    def log(x, a, b):
        return a + b * np.log(x)

    meds = grouping.median()
    popt, _ = curve_fit(log, meds.index, meds.values)

    fitx = np.linspace(xticks[0], xticks[-1], 100)
    fity = log(fitx, *popt)
    plt.plot(fitx, fity, "-")

    ax.set_xlabel("Time limit (s)")
    ax.set_ylabel("Total utility")

    fig.tight_layout()
    plt.savefig("timelimits.pdf")
    plt.close()


if __name__ == "__main__":
    plot_timeouts()
