from __future__ import annotations

import os
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
os.environ.setdefault("MPLCONFIGDIR", str(SCRIPT_DIR / ".matplotlib-cache"))

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import BoundaryNorm, ListedColormap, TwoSlopeNorm

OUTPUT_DIR = SCRIPT_DIR / "output"
FIGURES_DIR = SCRIPT_DIR / "figures"
AGGREGATE_LABELS = {"WBATE", "LBATE"}
EVAL_AXIS_BREAKS = [1, 5, 10, 15, 21, 25, 30, 35, 40]
EVAL_AXIS_LABELS = [f"b{i}" for i in EVAL_AXIS_BREAKS]
BW_SPECS = [("main", "bwmain"), ("itt", "bwitt")]
FIGURE_SPECS = [
    ("empapp_main_fuzzy", "Treatment effect", "Fuzzy"),
    ("empapp_main_itt", "Intention-to-treat effect", "ITT"),
    ("empapp_main_fs", "First stage", "FS"),
    ("empapp_main_itt0", "Control-side baseline outcome", "Control baseline"),
    ("empapp_covbal_itt", "Covariate balance ITT", "Cov. Bal."),
]


def clean_figures_dir() -> None:
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    for old in FIGURES_DIR.glob("*.png"):
        old.unlink()


def read_output_csv(file_name: str) -> pd.DataFrame:
    path = OUTPUT_DIR / file_name
    if not path.exists():
        raise FileNotFoundError(f"Missing output file: {path}. Run CTY_2026_JASA--empapp.py first.")
    return pd.read_csv(path)


def read_result_table(stem: str, suffix: str) -> pd.DataFrame:
    tab = read_output_csv(f"{stem}_{suffix}.csv")
    required = ["row", "b1", "b2", "estimate.p", "estimate.q", "p.value", "ci.lower", "ci.upper", "cb.lower", "cb.upper"]
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"{stem}_{suffix}.csv is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def read_percent_table(suffix: str) -> pd.DataFrame:
    tab = read_output_csv(f"empapp_main_percent_{suffix}.csv")
    required = ["row", "itt.percent", "fuzzy.percent"]
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"empapp_main_percent_{suffix}.csv is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def point_rows(tab: pd.DataFrame) -> pd.DataFrame:
    return tab.loc[~tab["row"].isin(AGGREGATE_LABELS)].copy()


def aggregate_value(tab: pd.DataFrame, name: str, column: str = "estimate.p") -> float:
    row = tab.loc[tab["row"] == name]
    if row.empty:
        return np.nan
    return float(row.iloc[0][column])


def save_png(fig: plt.Figure, file_name: str, width: float = 6, height: float = 5) -> None:
    fig.set_size_inches(width, height)
    fig.tight_layout()
    fig.savefig(FIGURES_DIR / file_name, dpi=300)
    plt.close(fig)


def figure_file(stem: str, kind: str, suffix: str) -> str:
    return f"{stem}_{kind}_{suffix}.png"


def plot_ylim(*arrays: object) -> tuple[float, float]:
    vals = np.concatenate([np.asarray(x, dtype=float).ravel() for x in arrays if x is not None])
    vals = vals[np.isfinite(vals)]
    if vals.size == 0:
        return -1.0, 1.0
    lo, hi = float(np.min(vals)), float(np.max(vals))
    if np.isclose(lo, hi):
        pad = max(0.02, abs(lo) * 0.08, 0.02)
    else:
        pad = max(0.02, (hi - lo) * 0.08)
    return lo - pad, hi + pad


def style_axes(ax: plt.Axes) -> None:
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def save_scatter_plot(dat: pd.DataFrame, eval_grid: pd.DataFrame, suffix: str) -> None:
    fig, ax = plt.subplots()
    control = dat["assignment"] == 0
    treated = dat["assignment"] == 1
    ax.scatter(dat.loc[control, "x.1"], dat.loc[control, "x.2"], s=3, c="indianred", alpha=0.18, marker="s", label="Control")
    ax.scatter(dat.loc[treated, "x.1"], dat.loc[treated, "x.2"], s=3, c="#104e8b", alpha=0.18, marker="o", label="Treatment")
    ax.plot([0, 0], [0, 55], color="0.35", lw=1.0, label="Boundary")
    ax.plot([0, 80], [0, 0], color="0.35", lw=1.0)
    ax.scatter(eval_grid["x.1"], eval_grid["x.2"], s=8, c="black", zorder=4)
    for idx in EVAL_AXIS_BREAKS:
        row = eval_grid.iloc[idx - 1]
        x = -3.2 if row["x.1"] == 0 else row["x.1"]
        y = row["x.2"] if row["x.1"] == 0 else -2.4
        ha = "right" if row["x.1"] == 0 else "center"
        va = "center" if row["x.1"] == 0 else "top"
        ax.text(x, y, f"b{idx}", ha=ha, va=va, fontsize=8)
    ax.set_xlim(-80, 100)
    ax.set_ylim(-40, 60)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    ax.legend(loc="upper left", bbox_to_anchor=(0.10, 0.98), frameon=True)
    style_axes(ax)
    save_png(fig, f"empapp_scatter_{suffix}.png")


def save_point_plot(tab: pd.DataFrame, stem: str, ylab: str, suffix: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    wbate = aggregate_value(tab, "WBATE")
    lbate = aggregate_value(tab, "LBATE")
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.scatter(x, pts["estimate.p"], s=12, color="black", label="Estimate", zorder=3)
    if np.isfinite(wbate):
        ax.axhline(wbate, color="#104e8b", lw=0.7, ls=":", label="WBATE")
    if np.isfinite(lbate):
        ax.axhline(lbate, color="firebrick", lw=0.7, ls="--", label="LBATE")
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts["estimate.p"], [wbate, lbate]))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, figure_file(stem, "pointest", suffix))


def save_percent_point_plot(tab: pd.DataFrame, value_col: str, ylab: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    aggregates = tab.loc[tab["row"].isin(AGGREGATE_LABELS)].copy()
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.scatter(x, pts[value_col], s=12, color="black", label="Estimate", zorder=3)
    for label, color, ls in [("WBATE", "#104e8b", ":"), ("LBATE", "firebrick", "--")]:
        row = aggregates.loc[aggregates["row"] == label]
        if not row.empty and np.isfinite(float(row.iloc[0][value_col])):
            ax.axhline(float(row.iloc[0][value_col]), color=color, lw=0.7, ls=ls, label=label)
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts[value_col], aggregates[value_col]))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, file_name)


def save_inference_plot(tab: pd.DataFrame, stem: str, ylab: str, suffix: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    band_lower = pts["cb.lower"].where(np.isfinite(pts["cb.lower"]), pts["ci.lower"])
    band_upper = pts["cb.upper"].where(np.isfinite(pts["cb.upper"]), pts["ci.upper"])
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.fill_between(x, band_lower, band_upper, color="#104e8b", alpha=0.14, label="95% CB")
    ax.errorbar(x, pts["estimate.p"], yerr=[pts["estimate.p"] - pts["ci.lower"], pts["ci.upper"] - pts["estimate.p"]], fmt="none", ecolor="black", elinewidth=0.35, capsize=1.5, label="95% CI")
    ax.scatter(x, pts["estimate.p"], s=10, color="black", label="Estimate", zorder=3)
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts["estimate.p"], pts["ci.lower"], pts["ci.upper"], band_lower, band_upper))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, figure_file(stem, "inference", suffix))


def save_effect_heatmap(tab: pd.DataFrame, eval_grid: pd.DataFrame, stem: str, label: str, suffix: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    values = pts["estimate.p"].to_numpy(float)
    fig, ax = plt.subplots()
    mid = float(np.nanmedian(values)) if np.isfinite(values).any() else 0.0
    norm = TwoSlopeNorm(vmin=float(np.nanmin(values)), vcenter=mid, vmax=float(np.nanmax(values))) if not np.isclose(np.nanmin(values), np.nanmax(values)) else None
    sc = ax.scatter(eval_grid["x.1"], eval_grid["x.2"], c=values, s=240, marker="s", cmap="RdBu_r", norm=norm, edgecolor="white", linewidth=0.4)
    for i, row in eval_grid.iterrows():
        ax.text(row["x.1"], row["x.2"], f"{i + 1:02d}", ha="center", va="center", fontsize=7)
    fig.colorbar(sc, ax=ax, label=label)
    ax.set_xlim(-10, 64)
    ax.set_ylim(-10, 42)
    ax.set_aspect(56 / 40)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    style_axes(ax)
    save_png(fig, figure_file(stem, "heatmap", suffix))


def save_pvalue_heatmap(tab: pd.DataFrame, eval_grid: pd.DataFrame, stem: str, label: str, suffix: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    pvals = pts["p.value"].to_numpy(float)
    bins = [0, 0.001, 0.01, 0.05, 0.1, 1.0]
    colors = ["#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf"]
    cmap = ListedColormap(colors)
    norm = BoundaryNorm(bins, cmap.N, clip=True)
    fig, ax = plt.subplots()
    sc = ax.scatter(eval_grid["x.1"], eval_grid["x.2"], c=np.clip(pvals, 0, 1), s=240, marker="s", cmap=cmap, norm=norm, edgecolor="white", linewidth=0.4)
    for i, row in eval_grid.iterrows():
        ax.text(row["x.1"], row["x.2"], f"{i + 1:02d}", ha="center", va="center", fontsize=7)
    cbar = fig.colorbar(sc, ax=ax, label=label, boundaries=bins)
    cbar.set_ticks([0.0005, 0.0055, 0.03, 0.075, 0.55])
    cbar.set_ticklabels(["0.000", "[0.001, 0.010)", "[0.010, 0.050)", "[0.050, 0.100)", ">= 0.100"])
    ax.set_xlim(-10, 64)
    ax.set_ylim(-10, 42)
    ax.set_aspect(56 / 40)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    style_axes(ax)
    save_png(fig, figure_file(stem, "heatmap_pval", suffix))


def write_result_figures(stem: str, ylab: str, heat_label: str, eval_grid: pd.DataFrame, suffix: str) -> None:
    tab = read_result_table(stem, suffix)
    save_point_plot(tab, stem, ylab, suffix)
    save_inference_plot(tab, stem, ylab, suffix)
    save_effect_heatmap(tab, eval_grid, stem, heat_label, suffix)
    save_pvalue_heatmap(tab, eval_grid, stem, heat_label, suffix)


def check_generated_figures(expected: list[str]) -> None:
    missing = [name for name in expected if not (FIGURES_DIR / name).exists()]
    if missing:
        raise FileNotFoundError(f"Missing expected figures: {', '.join(missing)}")
    too_small = [name for name in expected if (FIGURES_DIR / name).stat().st_size < 1000]
    if too_small:
        raise ValueError(f"Generated figure file(s) look empty: {', '.join(too_small)}")


def main() -> None:
    clean_figures_dir()
    eval_grid = read_output_csv("empapp_eval.csv")
    dat = read_output_csv("empapp_data.csv")
    for _, suffix in BW_SPECS:
        save_scatter_plot(dat, eval_grid, suffix)
        for stem, ylab, heat_label in FIGURE_SPECS:
            write_result_figures(stem, ylab, heat_label, eval_grid, suffix)
        percent_table = read_percent_table(suffix)
        save_percent_point_plot(percent_table, "itt.percent", "ITT / ITT.0 (%)", figure_file("empapp_main_percent_itt", "pointest", suffix))
        save_percent_point_plot(percent_table, "fuzzy.percent", "Fuzzy / ITT.0 (%)", figure_file("empapp_main_percent_fuzzy", "pointest", suffix))
    expected: list[str] = []
    for _, suffix in BW_SPECS:
        expected.append(f"empapp_scatter_{suffix}.png")
        for stem, _, _ in FIGURE_SPECS:
            expected.extend(figure_file(stem, kind, suffix) for kind in ["pointest", "inference", "heatmap", "heatmap_pval"])
        expected.append(figure_file("empapp_main_percent_itt", "pointest", suffix))
        expected.append(figure_file("empapp_main_percent_fuzzy", "pointest", suffix))
    check_generated_figures(expected)
    print("Empirical application figures complete.")
    print(f"Figures written to: {FIGURES_DIR.relative_to(SCRIPT_DIR)}")


if __name__ == "__main__":
    main()
