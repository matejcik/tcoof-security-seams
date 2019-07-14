import resultlib
from resultlib import make_colors, box_graph

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# fmt: off
COLUMNS = {
    "integers": np.int32,
    "booleans": np.int32,
    "constraints": np.int32,
}
# fmt: on

FILES = ["integers", "booleans", "constraints"]
HEADERS = ["Integers", "Logicals", "Constraints (x100)"]
FORMATS = ["-o", "-v", "-x"]


def read_csv(label, scaling_factor=1e-6):
    return resultlib.read_csv(label, COLUMNS, scaling_factor)


def plot_variables():
    fig, ax = resultlib.prepare_graph()

    for ifn, header, fmt in zip(FILES, HEADERS, FORMATS):
        data = read_csv(ifn)
        subsel = data[data.success]
        subsel = subsel[[ifn, "nsec"]]
        if ifn == "constraints":
            subsel.constraints /= 100
        grouping = subsel.groupby(ifn)["nsec"]
        line = grouping.mean()
        ax.plot(line.index, line.values, fmt, label=header)

    x_ticks = range(500, 10001, 500)
    plt.xticks(x_ticks, rotation="vertical")

    # fig, ax = box_graph(datasets, labels, colors, x_ticks, "vertical")
    ax.set_xlabel("Number of elements")

    plt.legend()
    fig.tight_layout()
    plt.savefig("variables.pdf")
    plt.close()


def plot_variablemem():
    fig, ax = resultlib.prepare_graph()

    for ifn, header, fmt in zip(FILES, HEADERS, FORMATS):
        data = read_csv(ifn)
        data.memory *= 1e-6
        subsel = data[data.success]
        subsel = subsel[[ifn, "memory"]]
        if ifn == "constraints":
            subsel.constraints /= 100
        grouping = subsel.groupby(ifn)["memory"]
        line = grouping.max()
        ax.plot(line.index, line.values, fmt, label=header)

    x_ticks = range(500, 10001, 500)
    plt.xticks(x_ticks, rotation="vertical")

    ax.set_ylabel("Peak memory usage (MB)")
    ax.set_xlabel("Number of elements")

    plt.legend()
    fig.tight_layout()
    plt.savefig("variablemem.pdf")
    plt.close()


def main():
    plot_variables()
    plot_variablemem()


if __name__ == "__main__":
    main()
