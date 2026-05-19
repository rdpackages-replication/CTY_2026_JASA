from __future__ import annotations

import argparse
import os
from datetime import datetime
from pathlib import Path
from importlib.metadata import PackageNotFoundError, version

import numpy as np
import pandas as pd


from rd2d import rd2d


REQUIRED_COLUMNS = [
    "running_saber11",
    "running_sisben",
    "eligible_spp",
    "beneficiary_spp",
    "spadies_any",
    "icfes_educm1",
]


OUTCOME_SPECS = [
    {"prefix": "main", "outcome": "Y", "outputs": ["main", "itt", "fs", "itt.0"]},
    {"prefix": "covbal", "outcome": "Z", "outputs": ["itt"]},
]




def rd2d_version() -> str:
    try:
        return version("rd2d")
    except PackageNotFoundError:
        return "unknown"

BW_SPECS = [
    ("main", "bwmain"),
    ("itt", "bwitt"),
]


def load_spp_data(path: Path) -> pd.DataFrame:
    raw = pd.read_csv(path)
    missing = [name for name in REQUIRED_COLUMNS if name not in raw.columns]
    if missing:
        raise ValueError(f"spp.csv is missing columns: {', '.join(missing)}")

    spp = raw.loc[:, REQUIRED_COLUMNS].copy()
    spp.columns = ["x.1", "x.2", "assignment", "fuzzy", "Y", "Z"]
    expected_assignment = ((spp["x.1"] >= 0) & (spp["x.2"] >= 0)).astype(int)
    valid = spp["assignment"].notna()
    if not np.all(spp.loc[valid, "assignment"].to_numpy() == expected_assignment.loc[valid].to_numpy()):
        raise ValueError("eligible_spp does not match 1{x.1 >= 0 & x.2 >= 0}.")
    return spp


def make_eval_grid(neval: int = 40) -> pd.DataFrame:
    half = int(np.ceil(neval / 2))
    first = pd.DataFrame(
        {
            "x.1": np.zeros(half),
            "x.2": 40 - np.arange(half) * 40 / half,
        }
    )
    second = pd.DataFrame(
        {
            "x.1": np.arange(neval - half) * 56 / half,
            "x.2": np.zeros(neval - half),
        }
    )
    return pd.concat([first, second], ignore_index=True)


def prepare_outcome_data(spp: pd.DataFrame, outcome: str) -> pd.DataFrame:
    cols = ["x.1", "x.2", "assignment", "fuzzy", outcome]
    dat = spp.loc[:, cols].dropna().copy()
    dat = dat.rename(columns={outcome: "Y_now"})
    return dat


def run_fuzzy_fit(dat: pd.DataFrame, eval_grid: pd.DataFrame, bwparam: str, repp: int):
    return rd2d(
        dat["Y_now"].to_numpy(),
        dat[["x.1", "x.2"]].to_numpy(),
        dat["assignment"].to_numpy(),
        eval_grid.to_numpy(),
        fuzzy=dat["fuzzy"].to_numpy(),
        bwparam=bwparam,
        params_other="itt.0",
        params_cov=["main", "itt", "fs", "itt.0"],
        repp=repp,
    )


def summary_table(fit, output: str) -> pd.DataFrame:
    point_table = fit[output]
    summ = fit.summary(
        output=output,
        cbands=output,
        WBATE=np.ones(len(point_table)),
        LBATE=True,
    )
    table = summ.tables[output].copy()
    row = [str(i) for i in range(1, len(point_table) + 1)]
    row.extend(str(idx) for idx in table.index[len(point_table) :])
    table.insert(0, "row", row)
    return table


def output_id(prefix: str, output: str, suffix: str) -> str:
    output_clean = output.replace(".", "")
    if prefix == "main" and output_clean == "main":
        return f"empapp_main_fuzzy_{suffix}"
    parts = ["empapp", prefix]
    if prefix != output_clean:
        parts.append(output_clean)
    parts.append(suffix)
    return "_".join(parts)


def write_csv(frame: pd.DataFrame, path: Path) -> None:
    frame.to_csv(path, index=False, na_rep="")


def make_percent_table(output_dir: Path, suffix: str) -> pd.DataFrame:
    fuzzy = pd.read_csv(output_dir / f"empapp_main_fuzzy_{suffix}.csv")
    itt = pd.read_csv(output_dir / f"empapp_main_itt_{suffix}.csv")
    itt0 = pd.read_csv(output_dir / f"empapp_main_itt0_{suffix}.csv")
    if not (fuzzy["row"].equals(itt["row"]) and fuzzy["row"].equals(itt0["row"])):
        raise ValueError("Cannot construct percent table because summary rows do not align.")

    denom = itt0["estimate.p"]
    return pd.DataFrame(
        {
            "row": fuzzy["row"],
            "b1": fuzzy["b1"],
            "b2": fuzzy["b2"],
            "h01": fuzzy["h01"],
            "h02": fuzzy["h02"],
            "h11": fuzzy["h11"],
            "h12": fuzzy["h12"],
            "N.Co": fuzzy["N.Co"],
            "N.Tr": fuzzy["N.Tr"],
            "itt.percent": 100 * itt["estimate.p"] / denom,
            "fuzzy.percent": 100 * fuzzy["estimate.p"] / denom,
        }
    )


def run(data_path: Path, output_dir: Path, repp: int) -> list[str]:
    output_dir.mkdir(parents=True, exist_ok=True)
    for old in output_dir.glob("empapp_*.csv"):
        old.unlink()

    spp = load_spp_data(data_path)
    eval_grid = make_eval_grid()
    generated: list[str] = []

    write_csv(spp, output_dir / "empapp_data.csv")
    write_csv(eval_grid, output_dir / "empapp_eval.csv")

    metadata = pd.DataFrame(
        {
            "name": ["rd2d.version", "repp", "bwparams", "generated_at"],
            "value": [rd2d_version(), str(repp), "main=bwmain; itt=bwitt", datetime.now().strftime("%Y-%m-%d %H:%M:%S")],
        }
    )
    write_csv(metadata, output_dir / "empapp_metadata.csv")

    for bwparam, suffix in BW_SPECS:
        print(f"Running empirical application with bwparam='{bwparam}'.")
        for spec in OUTCOME_SPECS:
            dat = prepare_outcome_data(spp, spec["outcome"])
            fit = run_fuzzy_fit(dat, eval_grid, bwparam, repp)
            for output in spec["outputs"]:
                file_id = output_id(spec["prefix"], output, suffix)
                write_csv(summary_table(fit, output), output_dir / f"{file_id}.csv")
                generated.append(file_id)
        percent_id = f"empapp_main_percent_{suffix}"
        write_csv(make_percent_table(output_dir, suffix), output_dir / f"{percent_id}.csv")
        generated.append(percent_id)

    return generated


def main() -> None:
    os.chdir(Path(__file__).resolve().parent)
    parser = argparse.ArgumentParser()
    parser.add_argument("--data", type=Path, default=Path("spp.csv"))
    parser.add_argument("--output", type=Path, default=Path("output"))
    parser.add_argument("--repp", type=int, default=int(os.getenv("RD2D_EMP_REPP", "2000")))
    args = parser.parse_args()
    if args.repp <= 0:
        raise ValueError("--repp must be a positive integer.")

    generated = run(args.data, args.output, args.repp)
    print(f"CSV outputs written to: {args.output}")
    print(f"Generated {len(generated) + 3} CSV file(s).")


if __name__ == "__main__":
    main()


