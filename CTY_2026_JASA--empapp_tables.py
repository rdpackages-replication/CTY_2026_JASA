from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = SCRIPT_DIR / "output"
TABLES_DIR = SCRIPT_DIR / "tables"
SMALL_ROWS = {"1", "5", "10", "15", "20", "25", "30", "35", "40"}
AGGREGATE_LABELS = {"WBATE", "LBATE"}

TABLE_SPECS = [
    ("empapp_main_fuzzy", "effect", True),
    ("empapp_main_itt", "effect", True),
    ("empapp_main_fs", "effect", True),
    ("empapp_main_itt0", "effect", True),
    ("empapp_covbal_itt", "effect", True),
    ("empapp_main_percent", "percent", False),
]
BW_SPECS = [("main", "bwmain"), ("itt", "bwitt")]


def clean_tables_dir() -> None:
    TABLES_DIR.mkdir(parents=True, exist_ok=True)
    for old in TABLES_DIR.glob("empapp_*.tex"):
        old.unlink()


def read_result_table(stem: str, kind: str, suffix: str) -> pd.DataFrame:
    path = OUTPUT_DIR / f"{stem}_{suffix}.csv"
    if not path.exists():
        raise FileNotFoundError(f"Missing output file: {path}. Run CTY_2026_JASA--empapp.py first.")
    tab = pd.read_csv(path)
    required = ["row", "h01", "h02", "h11", "h12", "N.Co", "N.Tr"]
    if kind == "effect":
        required += ["estimate.p", "p.value", "ci.lower", "ci.upper"]
    elif kind == "percent":
        required += ["itt.percent", "fuzzy.percent"]
    else:
        raise ValueError(f"Unknown table kind: {kind}")
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"{path} is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def is_aggregate(row_label: str) -> bool:
    return row_label in AGGREGATE_LABELS


def scalar(value: object) -> float:
    try:
        out = float(value)
    except (TypeError, ValueError):
        return np.nan
    return out


def fmt_num(value: object, digits: int = 3) -> str:
    x = scalar(value)
    if not np.isfinite(x):
        return ""
    return f"{x:.{digits}f}"


def fmt_pvalue(value: object) -> str:
    x = scalar(value)
    if not np.isfinite(x):
        return ""
    if x < 0.001:
        return "0.000"
    return f"{x:.3f}"


def fmt_interval(lower: object, upper: object) -> str:
    lo = scalar(lower)
    hi = scalar(upper)
    if not np.isfinite(lo) or not np.isfinite(hi):
        return ""
    return f"({fmt_num(lo)}, {fmt_num(hi)})"


def fmt_row_label(row_id: str) -> str:
    if row_id == "WBATE":
        return "$\\mathtt{WBATE}$"
    if row_id == "LBATE":
        return "$\\mathtt{LBATE}$"
    return f"$\\bb_{{{row_id}}}$"


def bandwidth_values(row: pd.Series) -> tuple[float, float]:
    if is_aggregate(str(row["row"])):
        return np.nan, np.nan
    h0 = np.array([scalar(row["h01"]), scalar(row["h02"])])
    h1 = np.array([scalar(row["h11"]), scalar(row["h12"])])
    if np.all(np.isfinite(h0)) and np.all(np.isfinite(h1)) and np.allclose(h0, h1):
        return float(h0[0]), float(h0[1])
    if np.all(np.isfinite(h0)):
        return float(h0[0]), float(h0[1])
    if np.all(np.isfinite(h1)):
        return float(h1[0]), float(h1[1])
    return np.nan, np.nan


def effective_sample_values(row: pd.Series) -> tuple[float, float]:
    if is_aggregate(str(row["row"])):
        return np.nan, np.nan
    return scalar(row["N.Co"]), scalar(row["N.Tr"])


def interval_limits(tab: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    if {"cb.lower", "cb.upper"}.issubset(tab.columns):
        use_cb = np.isfinite(pd.to_numeric(tab["cb.lower"], errors="coerce")) & np.isfinite(
            pd.to_numeric(tab["cb.upper"], errors="coerce")
        )
        lower = tab["ci.lower"].copy()
        upper = tab["ci.upper"].copy()
        lower.loc[use_cb] = tab.loc[use_cb, "cb.lower"]
        upper.loc[use_cb] = tab.loc[use_cb, "cb.upper"]
        return lower, upper
    return tab["ci.lower"], tab["ci.upper"]


def order_rows(tab: pd.DataFrame, keep_rows: set[str] | None = None) -> pd.DataFrame:
    aggregates = tab.loc[tab["row"].isin(AGGREGATE_LABELS)].copy()
    points = tab.loc[~tab["row"].isin(AGGREGATE_LABELS)].copy()
    if keep_rows is not None:
        points = points.loc[points["row"].isin(keep_rows)].copy()
    return pd.concat([points, aggregates], ignore_index=True)


def display_rows(tab: pd.DataFrame, keep_rows: set[str] | None = None) -> pd.DataFrame:
    out = order_rows(tab, keep_rows=keep_rows)
    lower, upper = interval_limits(out)
    rows = []
    for i, row in out.iterrows():
        h1, h2 = bandwidth_values(row)
        nco, ntr = effective_sample_values(row)
        rows.append(
            {
                "label": fmt_row_label(str(row["row"])),
                "h1": h1,
                "h2": h2,
                "N.Co": nco,
                "N.Tr": ntr,
                "estimate": scalar(row["estimate.p"]),
                "pvalue": scalar(row["p.value"]),
                "lower": scalar(lower.iloc[i]),
                "upper": scalar(upper.iloc[i]),
            }
        )
    return pd.DataFrame(rows)


def display_percent_rows(tab: pd.DataFrame) -> pd.DataFrame:
    out = order_rows(tab)
    rows = []
    for _, row in out.iterrows():
        h1, h2 = bandwidth_values(row)
        nco, ntr = effective_sample_values(row)
        rows.append(
            {
                "label": fmt_row_label(str(row["row"])),
                "h1": h1,
                "h2": h2,
                "N.Co": nco,
                "N.Tr": ntr,
                "itt": scalar(row["itt.percent"]),
                "fuzzy": scalar(row["fuzzy.percent"]),
            }
        )
    return pd.DataFrame(rows)


def latex_body(rows: pd.DataFrame) -> list[str]:
    lines: list[str] = []
    for _, row in rows.iterrows():
        if "WBATE" in row["label"] or "LBATE" in row["label"]:
            lines.append("  \\midrule")
        lines.append(
            "   "
            + row["label"]
            + " & "
            + fmt_num(row["h1"], 1)
            + " & "
            + fmt_num(row["h2"], 1)
            + " & "
            + fmt_num(row["N.Co"], 0)
            + " & "
            + fmt_num(row["N.Tr"], 0)
            + " & "
            + fmt_num(row["estimate"])
            + " & "
            + fmt_pvalue(row["pvalue"])
            + " & "
            + fmt_interval(row["lower"], row["upper"])
            + "\\\\"
        )
    return lines


def latex_percent_body(rows: pd.DataFrame) -> list[str]:
    lines: list[str] = []
    for _, row in rows.iterrows():
        if "WBATE" in row["label"] or "LBATE" in row["label"]:
            lines.append("  \\midrule")
        lines.append(
            "   "
            + row["label"]
            + " & "
            + fmt_num(row["h1"], 1)
            + " & "
            + fmt_num(row["h2"], 1)
            + " & "
            + fmt_num(row["N.Co"], 0)
            + " & "
            + fmt_num(row["N.Tr"], 0)
            + " & "
            + fmt_num(row["itt"])
            + " & "
            + fmt_num(row["fuzzy"])
            + "\\\\"
        )
    return lines


def write_lines(path: Path, lines: list[str]) -> None:
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_tabular(rows: pd.DataFrame, path: Path) -> None:
    write_lines(
        path,
        [
            "\\begin{tabular}{lccccccc}",
            "  \\toprule\\toprule",
            "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & Estimate & $p$-value & 95\\% CI \\\\",
            "  \\midrule",
            *latex_body(rows),
            "  \\bottomrule\\bottomrule",
            "\\end{tabular}",
        ],
    )


def write_percent_tabular(rows: pd.DataFrame, path: Path) -> None:
    write_lines(
        path,
        [
            "\\begin{tabular}{lcccccc}",
            "  \\toprule\\toprule",
            "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & ITT / ITT.0 (\\%) & Fuzzy / ITT.0 (\\%) \\\\",
            "  \\midrule",
            *latex_percent_body(rows),
            "  \\bottomrule\\bottomrule",
            "\\end{tabular}",
        ],
    )


def table_file(stem: str, suffix: str, small: bool = False) -> str:
    return f"{stem}{'_small' if small else ''}_{suffix}.tex"


def write_table_set(stem: str, kind: str, small: bool, suffix: str) -> None:
    tab = read_result_table(stem, kind, suffix)
    if kind == "percent":
        write_percent_tabular(display_percent_rows(tab), TABLES_DIR / table_file(stem, suffix))
        return
    write_tabular(display_rows(tab), TABLES_DIR / table_file(stem, suffix))
    if small:
        write_tabular(display_rows(tab, keep_rows=SMALL_ROWS), TABLES_DIR / table_file(stem, suffix, small=True))


def check_generated_tables(expected: list[str]) -> None:
    missing = [name for name in expected if not (TABLES_DIR / name).exists()]
    if missing:
        raise FileNotFoundError(f"Missing expected tables: {', '.join(missing)}")
    main_text = "\n".join((TABLES_DIR / table_file("empapp_main_fuzzy", suffix)).read_text(encoding="utf-8") for _, suffix in BW_SPECS)
    forbidden = ["Z value", "t-statistic", "P>|t|", "P>|z|", "CI/CB", "$<$0.001"]
    if any(label in main_text for label in forbidden):
        raise ValueError("Generated empirical table contains an obsolete label.")
    if "WBATE" not in main_text or "LBATE" not in main_text:
        raise ValueError("Generated empirical table is missing WBATE or LBATE.")
    if "$N_{\\mathrm{Co}}$" not in main_text or "$N_{\\mathrm{Tr}}$" not in main_text:
        raise ValueError("Generated empirical table is missing N.Co or N.Tr headers.")


def main() -> None:
    clean_tables_dir()
    for _, suffix in BW_SPECS:
        for stem, kind, small in TABLE_SPECS:
            write_table_set(stem, kind, small, suffix)
    expected: list[str] = []
    for _, suffix in BW_SPECS:
        expected.extend(table_file(stem, suffix) for stem, _, _ in TABLE_SPECS)
        expected.extend(table_file(stem, suffix, small=True) for stem, _, small in TABLE_SPECS if small)
    check_generated_tables(expected)
    print("Empirical application LaTeX tables complete.")
    print(f"Tables written to: {TABLES_DIR.relative_to(SCRIPT_DIR)}")


if __name__ == "__main__":
    main()
