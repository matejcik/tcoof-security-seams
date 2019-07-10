import os
import colorsys

import matplotlib

matplotlib.use("Agg")

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

HERE = os.path.dirname(__file__)
RESULT_PATH = os.path.abspath(os.path.join(HERE, "..", "results", "final"))


def set_result_path(lastcmp):
    global RESULT_PATH
    RESULT_PATH = os.path.abspath(os.path.join(HERE, "..", "results", lastcmp))


set_result_path("final")

# fmt: off
COLUMNS_COMMON = {
    "i": np.int32,
    "success": str,
    "nsec": np.int64,
    "utility": np.int32,
}
# fmt: on

NS_TIME = int(30e9)


def read_csv(label, columns, scaling_factor=1e-6):
    filename = os.path.join(RESULT_PATH, label + ".log")
    all_columns = columns.copy()
    all_columns.update(COLUMNS_COMMON)
    df = pd.read_csv(
        filename,
        header=None,
        names=all_columns.keys(),
        dtype=all_columns,
        skipinitialspace=True,
    )

    df.success = df.success == "true"
    df.nsec *= scaling_factor

    return df


def make_colors(n):
    f = 1 / n
    return [colorsys.hsv_to_rgb(i * f, 0.9, 0.76) for i in range(n)]


def box_graph(datasets, labels, colors, x_ticks, rotation=None):
    """
    Datasets is a list of lists of series: the outer list is the number of "lines"
    (i.e., different measure types that will be shown per each tick).
    The inner list has an element for each data point (each x-ticks), and is a series
    of values from which a single candlebar is drawn.

    `colors` is a list of hex/rgb colors, same length as `datasets`

    `x_ticks` is a list of values on the X axis, same length as elements of `datasets`
    """

    def setcolor(bp, color):
        plt.setp(bp["boxes"], color=color)
        plt.setp(bp["whiskers"], color=color)
        plt.setp(bp["caps"], color=color)
        plt.setp(bp["medians"], color=color)

    fig = plt.figure(figsize=(7, 5), dpi=300)
    ax = fig.add_subplot(111)

    assert len(datasets) <= len(colors)
    assert len(datasets) <= len(labels)
    for dataset in datasets:
        if len(dataset) < len(x_ticks):
            dataset.extend([[]] * (len(x_ticks) - len(dataset)))

    group_size = len(datasets)
    step_size = group_size + 1
    x_size = len(x_ticks)

    for idx, (series, label, color) in enumerate(zip(datasets, labels, colors)):
        bp = ax.boxplot(
            series,
            positions=[i * step_size + idx for i in range(x_size)],
            showfliers=False,
        )
        setcolor(bp, color)
        plt.plot([], c=color, label=label)

    plt.legend()
    plt.xticks(
        [(x * step_size) + group_size / 2 - 0.5 for x in range(x_size)],
        x_ticks,
        rotation=rotation,
    )
    plt.xlim(-1, step_size * x_size)

    ax.set_ylabel("Computation time (ms)")
    ax.yaxis.grid(True, linestyle="-", which="major", color="lightgrey", alpha=0.5)

    return fig, ax
