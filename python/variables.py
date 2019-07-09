import resultlib
from resultlib import make_colors, box_graph

resultlib.set_result_path("2019-07-09")

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


def read_csv(label, scaling_factor=1e-6):
    return resultlib.read_csv(label, COLUMNS, scaling_factor)


def plot_variables():
    files = ["integers", "booleans", "constraints"]
    headers = ["IntVar count", "BoolVar count", "Constraint count"]
    datasets = []
    labels = []
    colors = make_colors(len(files))

    for ifn, header in zip(files, headers):
        data = read_csv(ifn)
        subsel = data[data.success]
        subsel = subsel[[ifn, "nsec"]]
        grouping = subsel.groupby(ifn)["nsec"]
        series = [s for _, s in grouping]

        datasets.append(series)
        labels.append(header)

    maxticks = max(len(d) for d in datasets)
    x_ticks = range(5, maxticks + 5)

    fig, ax = box_graph(datasets, labels, colors, x_ticks)
    ax.set_xlabel("Number of elements")

    fig.tight_layout()
    plt.savefig("variables.pdf")
    plt.close()


def main():
    plot_variables()


if __name__ == "__main__":
    main()
