import os

import matplotlib

matplotlib.use("Agg")

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

HERE = os.path.dirname(__file__)
RESULT_PATH = os.path.abspath(os.path.join(HERE, "..", "results", "2019-04-07"))

COLUMNS = {
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

NS_TIME = int(30e9)


def read_csv(label, scaling_factor):
    filename = os.path.join(RESULT_PATH, label + ".log")
    df = pd.read_csv(
        filename,
        header=None,
        names=COLUMNS.keys(),
        dtype=COLUMNS,
        skipinitialspace=True,
    )

    df.lunchtime = df.lunchtime == "true"
    df["failed"] = df.time == "FAIL"
    df["nsec"] = df.time.replace("FAIL", NS_TIME).astype(np.int64)
    df["nsec"] *= scaling_factor

    return df


def box_graph(datasets, labels, colors, x_ticks):
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

    assert len(datasets) == len(colors)
    for dataset in datasets:
        if len(dataset) < len(x_ticks):
            dataset.extend([[]] * len(x_ticks) - len(dataset))

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
    plt.xticks([(x * step_size) + group_size / 2 - 0.5 for x in range(x_size)], x_ticks)
    plt.xlim(-1, step_size * x_size)
    fig.tight_layout()

    return fig, ax
