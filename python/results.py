import matplotlib

matplotlib.use("Agg")

import numpy as np
import matplotlib.pyplot as plt

import csv


def read_csv(filename, scaling_factor):
    with open(f"data/{filename}.log") as csv_file:
        data = []

        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            data.append(row)

        np_data = np.asarray(data, dtype=np.float32)
        np_data[:, 5] = np_data[:, 5] * scaling_factor

        return np_data


data = np.concatenate((read_csv("0", 1e-6), read_csv("1", 1)))

measure_phases = np.unique(data[:, 0])
factories_counts = np.unique(data[:, 1])
workers_per_workplace_counts = np.unique(data[:, 2])
workers_late_ratios = np.unique(data[:, 3])

# print(measure_phases)
# print(factories_counts)
# print(workers_per_workplace_counts)
# print(workers_late_ratios)


def set_box_color(bp, color):
    plt.setp(bp["boxes"], color=color)
    plt.setp(bp["whiskers"], color=color)
    plt.setp(bp["caps"], color=color)
    plt.setp(bp["medians"], color=color)


group_size = len(workers_late_ratios)
space_size = 1
step_size = group_size + space_size
colors = ["#e41a1c", "#377eb8", "#4daf4a", "#984ea3"]

factories_count = np.float32(1)

for measure_phase in measure_phases:

    expected_no_of_data_points = 10000 if measure_phase == 0 else 100

    fig = plt.figure(figsize=(7, 5), dpi=300)
    ax = fig.add_subplot(1, 1, 1)

    for workers_late_ratio_idx, (workers_late_ratio, color) in enumerate(
        zip(workers_late_ratios, colors)
    ):
        boxes = []

        for workers_per_workplace_count in workers_per_workplace_counts:
            data_selection = np.equal(
                data[:, 0:4],
                [
                    measure_phase,
                    factories_count,
                    workers_per_workplace_count,
                    workers_late_ratio,
                ],
            ).all(1)

            if np.sum(data_selection) == expected_no_of_data_points:
                boxes.append(data[data_selection, [5]])
            else:
                boxes.append([])

        bp = ax.boxplot(
            boxes,
            positions=np.array(range(len(workers_per_workplace_counts))) * step_size
            + workers_late_ratio_idx,
            showfliers=False,
            widths=0.6,
        )
        set_box_color(bp, color)

        ax.yaxis.grid(True, linestyle="-", which="major", color="lightgrey", alpha=0.5)

        workers_late_ratio_percentage = int(workers_late_ratio * 100)
        plt.plot([], c=color, label=f"{workers_late_ratio_percentage}% late workers")

    plt.legend()

    plt.xticks(
        [
            x + group_size / 2.0 - 0.5
            for x in range(0, step_size * len(workers_per_workplace_counts), step_size)
        ],
        [int(x) for x in workers_per_workplace_counts],
    )
    plt.xlim(-1, step_size * len(workers_per_workplace_counts) - space_size + 1)

    ax.set_xlabel("Workers in a shift (3 shifts in a factory)", fontsize=12)
    ax.set_ylabel("Computation time (ms)", fontsize=12)

    if measure_phase == 0:
        measure_phase_desc = "17 minutes before the start of the shift (notification about workers potentially late)"
    else:
        measure_phase_desc = (
            "13 minutes before the start of the shift (selection of standbys)"
        )

    # ax.set_title(measure_phase_desc)

    fig.tight_layout()

    plt.savefig(f"figures/{int(measure_phase)}.pdf")
    plt.close()

    # plt.show()
