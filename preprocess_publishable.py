"""Reproducible preprocessing for the 2026 Anticonformity experiment.

Run this script once to process either or both conditions. It writes all
intermediate and final CSV checkpoints to each condition's `Clean_files/`
directory. The final combined file is written to `Publishable/Clean_files/`.

Directory layout (the script location determines BASE_PATH automatically)
--------------------------------------------------------------------------
Publishable/
├── neighbors_configurations.csv
├── * preprocess_publishable.py * <-- this code
├── Political/
│   ├── Raw_otree_anon/
│   │   ├── session-id-date.csv
│   │   ├── all_apps_wide_<session>.csv
│   │   ├── all_apps_wide_4afqwk47_old.csv
│   │   └── mock_N08_N04_Aonly_ahrup1ic.csv
│   └── Clean_files/              # created automatically
└── Nonpolitical/
    ├── Raw_otree_anon/
    │   ├── session-id-date.csv
    │   └── all_apps_wide_<session>.csv
    └── Clean_files/              # created automatically

Checkpoints
-----------
00_session_registry_<condition>.csv  session IDs and dates used in the run
01_raw_combined_<condition>.csv      concatenated/deduplicated raw participants
02_wide_clean_<condition>.csv        cleaned wide participant data
03_long_base_<condition>.csv         long data before neighbourhood derivation
04_long_neighborhoods_<condition>.csv paired and derived neighbourhood data
05_analysis_ready_<condition>.csv    demographics and neighbourhood exclusions
06_classified_<condition>.csv        revised current-round classifications
07_combined_classified.csv            political and nonpolitical data combined

The classification functions intentionally use the revised definition from the
legacy script: round 0 is `initial`; rounds 1–20 are assessed relative to the
current round's peer average/majority because `response` is that round's
`new_response`.
"""

from __future__ import annotations

import ast
import re
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

# =============================================================================
# RUN CONFIGURATION
# =============================================================================
# Edit this tuple to run just one condition, e.g. ("political",).
CONDITIONS_TO_RUN = ("nonpolitical", "political")
BASE_PATH = Path(__file__).resolve().parent
N_ROUNDS = 20
FORCED_RESPONSE_THRESHOLD = 2
# The public repository must use only its anonymized source data and must not
# depend on a private historical reference output.
REPRODUCE_JULY15_LEGACY_SAMPLE = False
JULY15_LEGACY_OUTPUT = BASE_PATH.parent / "Output" / "df_non_pol_combined_rev_class_20260715.csv"

CONDITION_CONFIG = {
    "nonpolitical": {
        "exclude_phase_1": True,
        "drop_anti_prop": ["p25"],
        "apply_manual_repairs": False,
        "remove_duplicate_neighborhoods": True,
    },
    "political": {
        "exclude_phase_1": False,
        "drop_anti_prop": [],
        "apply_manual_repairs": True,
        "remove_duplicate_neighborhoods": False,
    },
}

# Data rules are declared as configurations so condition-specific decisions are
# visible without reading the transformation functions.
NONPOLITICAL_REPAIRS = [
    {
        "repair": "exclude_phase_1",
        "column": "session.config.name",
        "excluded_value": "Phase_1",
        "reason": "Nonpolitical Phase 1 sessions are outside the analysis sample.",
    },
]

POLITICAL_REPAIRS = [
    {
        "repair": "rebuild_neighbor_responses_and_previous_majority",
        "participant_code": "tqrgznd0",
        "round_no": 4,
        "discussion_group_round": 1,
        "previous_majority": "copy_from_peer_same_round",
    },
    {
        "repair": "rebuild_neighbor_responses_and_previous_majority",
        "participant_code": "xez7s0dk",
        "round_no": 2,
        "discussion_group_round": 2,
        "previous_majority": "mode_of_peer_new_responses_previous_round",
    },
    {
        "repair": "replace_broken_round9_export",
        "source_file": "mock_N08_N04_Aonly_ahrup1ic.csv",
        "round_no": 9,
    },
    {
        "repair": "remove_groups_with_missing_round_data",
        "session_codes": ["4afqwk47", "wpirzidq"],
    },
    {
        "repair": "remove_groups_with_many_forced_members",
        "reason": "Avoid partial discussion groups after incomplete political participation.",
    },
]

STANDARD_MOCK_PATTERNS = [
    "mock.{i}.player.id_in_group",
    "mock.{i}.player.role",
    "mock.{i}.player.payoff",
    "mock.{i}.player.old_response",
    "mock.{i}.player.new_response",
    "mock.{i}.player.forced_response",
    "mock.{i}.player.prev_majority",
    "mock.{i}.player.neighbor_responses",
    "mock.{i}.player.discussion_grp",
    "mock.{i}.player.scenario",
    "mock.{i}.subsession.round_number",
    "mock.{i}.group.id_in_subsession",
    "mock.{i}.group.group_responses",
    "mock.{i}.group.majority_response",
    "mock.{i}.group.group_size",
    "mock.{i}.group.anti_prop",
    "mock.{i}.group.is_group_single",
    "mock.{i}.group.beta_50",
    "mock.{i}.player.nudge_training",
    "mock.{i}.player.nudge_training_two",
    "mock.{i}.player.nudge_training_three",
]
ALTERNATIVE_PREFIXES = [
    "mock_N08_N04_Aonly.{i}.",
    "mock_N04_Aonly.{i}.",
    "mock_N08_only.{i}.",
    "mock_N04_AandF.{i}.",
]
ALTERNATIVE_PREFIX_PATTERNS = [
    r"^mock_N08_N04_Aonly\.\d+\.",
    r"^mock_N04_Aonly\.\d+\.",
    r"^mock_N08_only\.\d+\.",
    r"^mock_N04_AandF\.\d+\.",
]
REQUIRED_ROUND_FIELDS = [
    "player.discussion_grp", "player.old_response", "player.new_response",
    "player.forced_response", "player.prev_majority", "player.neighbor_responses",
    "group.id_in_subsession", "group.group_size", "group.is_group_single",
    "group.beta_50", "group.anti_prop",
]

CORRECT_CONFORMITY = {
    "-1,-1,-1": "-1", "-1,-1,0": "-1", "-1,-1,1": "-1",
    "-1,0,1": "None", "-1,0,0": "0", "-1,1,1": "1",
    "0,0,0": "0", "0,0,1": "0", "0,1,1": "1", "1,1,1": "1",
}
CORRECT_ANTICONFORMITY = {
    "-1,-1,-1": ["1", "0"], "-1,-1,0": ["1", "0"],
    "-1,-1,1": ["1", "0"], "-1,0,1": "None",
    "-1,0,0": ["1", "-1"], "-1,1,1": ["0", "-1"],
    "0,0,0": ["-1", "1"], "0,0,1": ["1", "-1"],
    "0,1,1": ["0", "-1"], "1,1,1": ["-1", "0"],
}


def build_combined_correctness() -> dict[str, list[str] | str]:
    """Combine the valid conformity and anticonformity answers per neighbour set."""
    combined: dict[str, list[str] | str] = {}
    for key, conformity_answer in CORRECT_CONFORMITY.items():
        anticonformity_answer = CORRECT_ANTICONFORMITY[key]
        answers: list[str] = []
        for answer in (conformity_answer, anticonformity_answer):
            if isinstance(answer, list):
                answers.extend(answer)
            elif answer != "None":
                answers.append(answer)
        combined[key] = sorted(set(answers)) if answers else "None"
    return combined


CORRECT_BOTH = build_combined_correctness()

# =============================================================================
# PATHS, VALIDATION, AND CHECKPOINTS
# =============================================================================
def condition_paths(condition: str) -> dict[str, Path]:
    """Return all source/output locations for one condition."""
    condition_root = BASE_PATH / condition.capitalize()
    return {
        "root": condition_root,
        "raw": condition_root / "Raw_otree_anon",
        "clean": condition_root / "Clean_files",
    }


def checkpoint_path(condition: str, stage: str) -> Path:
    """Return a stable, non-date-based CSV checkpoint path."""
    return condition_paths(condition)["clean"] / f"{stage}_{condition}.csv"


def require_files(paths: list[Path]) -> bool:
    """Print all missing required files and return whether every file exists."""
    missing = [path for path in paths if not path.is_file()]
    for path in missing:
        print(f"No {path.name} in {path.parent}")
    return not missing


def initialise_directories() -> None:
    """Create all requested Clean_files directories before processing begins."""
    (BASE_PATH / "Clean_files").mkdir(parents=True, exist_ok=True)
    for condition in CONDITION_CONFIG:
        condition_paths(condition)["clean"].mkdir(parents=True, exist_ok=True)


def validate_run_inputs(conditions: tuple[str, ...]) -> bool:
    """Validate every required input before any pipeline transformation starts."""
    required = [BASE_PATH / "neighbors_configurations.csv"]
    if REPRODUCE_JULY15_LEGACY_SAMPLE and set(conditions) == set(CONDITION_CONFIG):
        required.append(JULY15_LEGACY_OUTPUT)
    for condition in conditions:
        paths = condition_paths(condition)
        registry = paths["raw"] / "session-id-date.csv"
        required.append(registry)
        if registry.is_file():
            sessions = pd.read_csv(registry)["session-id"].tolist()
            for session_id in sessions:
                filename = (
                    f"all_apps_wide_{session_id}_old.csv"
                    if condition == "political" and session_id == "4afqwk47"
                    else f"all_apps_wide_{session_id}.csv"
                )
                required.append(paths["raw"] / filename)
        if condition == "political":
            required.extend([
                paths["raw"] / "all_apps_wide_4afqwk47_old.csv",
                paths["raw"] / "mock_N08_N04_Aonly_ahrup1ic.csv",
            ])
    return require_files(required)


def write_checkpoint(df: pd.DataFrame, condition: str, stage: str) -> Path:
    """Write one named, reproducible CSV artifact and report its dimensions."""
    path = checkpoint_path(condition, stage)
    df.to_csv(path, index=False)
    print(f"Saved {path.name}: {len(df):,} rows × {len(df.columns):,} columns")
    return path


def audit_counts(df: pd.DataFrame) -> tuple[int, int, int]:
    """Return row, participant, and neighbourhood counts for an audit record."""
    participant_column = next((column for column in ("participant.code", "participant_code") if column in df), None)
    neighbourhood_column = next((column for column in ("neigh_id", "nid") if column in df), None)
    participants = df[participant_column].nunique() if participant_column else 0
    neighbourhoods = df[neighbourhood_column].nunique(dropna=True) if neighbourhood_column else 0
    return len(df), participants, neighbourhoods


def log_step(
    audit: list[dict[str, Any]],
    condition: str,
    step: str,
    purpose: str,
    before: pd.DataFrame,
    after: pd.DataFrame,
) -> None:
    """Append a transparent before/after retention record for one transformation."""
    rows_before, participants_before, neighbourhoods_before = audit_counts(before)
    rows_after, participants_after, neighbourhoods_after = audit_counts(after)
    audit.append({
        "condition": condition,
        "step": step,
        "purpose": purpose,
        "rows_before": rows_before,
        "rows_after": rows_after,
        "rows_excluded": rows_before - rows_after,
        "participants_before": participants_before,
        "participants_after": participants_after,
        "participants_excluded": participants_before - participants_after,
        "neighborhoods_before": neighbourhoods_before,
        "neighborhoods_after": neighbourhoods_after,
        "neighborhoods_excluded": neighbourhoods_before - neighbourhoods_after,
    })


def is_empty(value: Any) -> bool:
    """Identify missing values and blank/list-like empty raw oTree fields."""
    return pd.isna(value) or str(value).strip() in {"", "[]", "None", "nan"}

# =============================================================================
# SHARED RESPONSE AND CLASSIFICATION FUNCTIONS (DECLARED ONCE)
# =============================================================================
def majority(row: pd.Series) -> float:
    """Return the unique majority in `dgroup_response_list`, otherwise NaN."""
    response_list = parse_response_list(row["dgroup_response_list"])
    if not response_list:
        return np.nan
    counts = pd.Series(response_list).value_counts()
    top = counts[counts == counts.max()].index.tolist()
    return top[0] if len(top) == 1 else np.nan


def average_response(response_list: Any) -> float:
    """Return the mean of a response list, otherwise NaN."""
    values = parse_response_list(response_list)
    return float(np.mean(values)) if values else np.nan


def final_class_avg(row: pd.Series) -> str | None:
    """Revised classification against the current round's peer average."""
    if row["round_no"] == 0:
        return "initial"
    current, previous, average = row["response"], row["prev_response"], row["average_response"]
    if row["move"] == 1:
        if pd.isna(average):
            return "undetermined"
        if abs(current - average) < abs(previous - average):
            return "conforming_move"
        if abs(current - average) > abs(previous - average):
            return "anticonforming_move"
        return None
    if row["move"] == 0:
        if pd.isna(average):
            return "undetermined"
        distances = {opinion: abs(opinion - average) for opinion in (-1, 0, 1)}
        current_distance = distances[current]
        if current_distance == max(distances.values()):
            return "anticonforming_stay"
        if current_distance == min(distances.values()):
            return "conforming_stay"
        return "undetermined_stay"
    return "NA"


def final_class_majority(row: pd.Series) -> str | None:
    """Revised classification against the current round's peer majority."""
    if row["round_no"] == 0:
        return "initial"
    majority_value = row["majority_response"]
    if row["move"] == 1:
        if pd.isna(majority_value):
            return "no_majority_move"
        previous_distance = abs(row["prev_response"] - majority_value)
        current_distance = abs(row["response"] - majority_value)
        return "conforming_move" if previous_distance > current_distance else "anticonforming_move"
    if row["move"] == 0:
        if pd.isna(majority_value):
            return "no_majority_stay"
        return "conforming_stay" if row["response"] == majority_value else "anticonforming_stay"
    return None


def parse_response_list(value: Any) -> list[float]:
    """Parse list values stored as a Python list or as CSV text."""
    if isinstance(value, str):
        try:
            value = ast.literal_eval(value)
        except (ValueError, SyntaxError):
            return []
    if not isinstance(value, (list, tuple)):
        return []
    return [float(item) for item in value if not pd.isna(item)]


def normalize_neighbors(value: Any) -> str:
    """Convert raw neighbour responses into a clean comma-separated string."""
    if pd.isna(value):
        return ""
    values = parse_response_list(value)
    if values:
        return ",".join(str(int(x)) if x.is_integer() else str(x) for x in values)
    return ",".join(str(value).strip("[]").replace("'", "").split(","))


def standardize_neighbors(value: Any) -> str:
    """Sort a neighbour-response combination into its canonical representation."""
    if pd.isna(value) or not str(value).strip():
        return "None"
    try:
        return ",".join(str(x) for x in sorted(int(x.strip()) for x in str(value).split(",") if x.strip()))
    except ValueError:
        return "Invalid"


def response_as_string(value: Any) -> str:
    """Use the lookup-table representation for a numeric participant response."""
    if pd.isna(value):
        return "None"
    return str(int(value)) if float(value).is_integer() else str(value)


def check_response_treatment(row: pd.Series) -> int | str | bool:
    """Classify observed response as conformity (0), anticonformity (1), tie, or unknown."""
    neighbours, response = row["neighbors_std"], response_as_string(row["response"])
    accepted = CORRECT_BOTH.get(neighbours)
    if accepted is None:
        return ""
    if accepted == "None" or response == "None":
        return "None"
    if response not in accepted:
        return ""
    conformity = CORRECT_CONFORMITY[neighbours]
    if conformity == "None":
        return "None"
    if response == conformity or isinstance(conformity, list) and response in conformity:
        return 0
    anticonformity = CORRECT_ANTICONFORMITY[neighbours]
    if anticonformity == "None":
        return "None"
    return 1 if response == anticonformity or isinstance(anticonformity, list) and response in anticonformity else False


def check_response_correct(row: pd.Series) -> int | str:
    """Determine whether a response follows the participant's assigned nudge."""
    if row["neighbors_std"] == "None":
        return ""
    lookup = CORRECT_CONFORMITY if str(row["antic_nudge"]) == "0" else CORRECT_ANTICONFORMITY
    expected = lookup.get(row["neighbors_std"])
    response = response_as_string(row["response"])
    if expected == "None" or response == "None":
        return "None"
    if expected is None:
        return ""
    return int(response in expected) if isinstance(expected, list) else int(response == expected)

# =============================================================================
# RAW AND WIDE-FORMAT TRANSFORMATIONS
# =============================================================================
def load_raw_data(condition: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Load registry and concatenate/deduplicate one wide raw row per participant."""
    raw_dir = condition_paths(condition)["raw"]
    registry = pd.read_csv(raw_dir / "session-id-date.csv")
    session_ids = registry["session-id"].tolist()
    frames = []
    for session_id in session_ids:
        suffix = "_old" if condition == "political" and session_id == "4afqwk47" else ""
        frames.append(pd.read_csv(raw_dir / f"all_apps_wide_{session_id}{suffix}.csv", low_memory=False))
    raw = pd.concat(frames, ignore_index=True)
    raw = raw.drop_duplicates(subset=["participant.code"], keep="last")
    raw = raw[raw["session.code"].isin(session_ids)].reset_index(drop=True)
    return raw, registry


def normalize_mock_columns(df: pd.DataFrame, condition: str) -> pd.DataFrame:
    """Merge oTree app-name variants into standard `mock.<round>.*` columns."""
    patterns = STANDARD_MOCK_PATTERNS.copy()
    if condition == "political":
        patterns.extend(["mock.{i}.player.aff_pol_A", "mock.{i}.player.aff_pol_F"])
    df = df.copy()
    for pattern in patterns:
        for round_no in range(1, N_ROUNDS + 1):
            standard = pattern.format(i=round_no)
            for prefix in ALTERNATIVE_PREFIXES:
                alternative = pattern.replace("mock.{i}.", prefix).format(i=round_no)
                if alternative in df:
                    df[standard] = df[alternative] if standard not in df else df[standard].fillna(df[alternative])
    drop_columns = [column for column in df if any(re.match(pattern, column) for pattern in ALTERNATIVE_PREFIX_PATTERNS)]
    return df.drop(columns=drop_columns)


def apply_phase_filter(df: pd.DataFrame, config: dict[str, Any]) -> pd.DataFrame:
    """Apply the documented nonpolitical Phase 1 exclusion when configured."""
    if not config["exclude_phase_1"]:
        return df.copy()
    return df.loc[df["session.config.name"] != "Phase_1"].reset_index(drop=True).copy()


def fill_labels_assign_groups_and_flags(df: pd.DataFrame) -> pd.DataFrame:
    """Retain grouped participants; fill labels; create group/quality flags."""
    df = df.loc[df["mock.1.group.group_size"].notna() & (df["mock.1.group.group_size"] != "single")].copy()
    df["participant.label"] = df["participant.label"].fillna(df["participant.code"])
    group_key = df["session.code"].astype(str) + "_" + df["mock.1.group.id_in_subsession"].astype(str)
    group_map = {key: index + 1 for index, key in enumerate(pd.unique(group_key))}
    df["group_id"] = group_key.map(group_map)
    forced_counts = df.groupby("group_id")["participant.too_many_forced"].sum()
    df["many_forced"] = df["group_id"].map((forced_counts > FORCED_RESPONSE_THRESHOLD).astype(int))
    duplicate_labels = df.loc[df["participant.label"].duplicated(), "participant.label"].unique()
    # Match the July 15 rule: flag only groups containing a later duplicate
    # occurrence, not the group containing the retained first occurrence.
    duplicate_group_ids = df.loc[df["participant.label"].duplicated(), "group_id"].unique()
    df["duplicated_participant"] = df["group_id"].isin(duplicate_group_ids).astype(int)
    df["keep_entry"] = 0
    for label in duplicate_labels:
        rows = df.loc[df["participant.label"] == label]
        keep_index = rows.index[1] if rows.iloc[0]["many_forced"] == 1 and len(rows) > 1 else rows.index[0]
        df.loc[keep_index, "keep_entry"] = 1
        df.loc[rows.index.difference([keep_index]), "keep_entry"] = 2
    return df.loc[df["keep_entry"] != 2].reset_index(drop=True)


def group_codes(value: Any) -> list[str]:
    """Parse a discussion-group field into participant-code strings."""
    if pd.isna(value):
        return []
    return [code.strip() for code in str(value).strip("[]").replace("'", "").split(",") if code.strip()]


def apply_manual_participant_repairs(df: pd.DataFrame) -> pd.DataFrame:
    """Apply the two declared political peer-response/manual-majority repairs."""
    df = df.copy()
    for repair in POLITICAL_REPAIRS[:2]:
        participant, round_no = repair["participant_code"], repair["round_no"]
        if participant not in set(df["participant.code"]):
            raise ValueError(f"Configured repair participant not found: {participant}")
        dgroup_round = repair["discussion_group_round"]
        peers = group_codes(df.loc[df["participant.code"] == participant, f"mock.{dgroup_round}.player.discussion_grp"].iloc[0])
        old_response_column = f"mock.{round_no}.player.old_response"
        response_map = df.set_index("participant.code")[old_response_column].to_dict()
        neighbours = [response_map[peer] for peer in peers if peer in response_map and pd.notna(response_map[peer])]
        neighbours = [int(float(value)) for value in neighbours]
        df.loc[df["participant.code"] == participant, f"mock.{round_no}.player.neighbor_responses"] = str(neighbours)
        if repair["previous_majority"] == "copy_from_peer_same_round":
            peer_majorities = df.loc[df["participant.code"].isin(peers), f"mock.{round_no}.player.prev_majority"].dropna()
            df.loc[df["participant.code"] == participant, f"mock.{round_no}.player.prev_majority"] = peer_majorities.iloc[0]
        else:
            previous_responses = [
                df.loc[df["participant.code"] == peer, f"mock.{round_no - 1}.player.new_response"].iloc[0]
                for peer in peers
            ]
            previous_responses = [int(float(value)) for value in previous_responses if pd.notna(value)]
            if previous_responses:
                df.loc[df["participant.code"] == participant, f"mock.{round_no}.player.prev_majority"] = pd.Series(previous_responses).mode().iloc[0]
    return df


def repair_political_round9_and_remove_incomplete_groups(df: pd.DataFrame, raw_dir: Path) -> pd.DataFrame:
    """Replace broken ahrup1ic round 9 values and remove configured incomplete groups."""
    source = pd.read_csv(raw_dir / "mock_N08_N04_Aonly_ahrup1ic.csv", low_memory=False)
    source = source.loc[(source["subsession.round_number"] == 9) & (source["group.group_size"] != "single")].copy()
    source = source.rename(columns={
        "player.discussion_grp": "mock.9.player.discussion_grp",
        "player.old_response": "mock.9.player.old_response",
        "player.new_response": "mock.9.player.new_response",
        "player.forced_response": "mock.9.player.forced_response",
        "player.prev_majority": "mock.9.player.prev_majority",
        "player.neighbor_responses": "mock.9.player.neighbor_responses",
        "group.is_group_single": "mock.9.group.is_group_single",
        "group.beta_50": "mock.9.group.beta_50",
        "group.anti_prop": "mock.9.group.anti_prop",
    })
    update_columns = [column for column in source if column.startswith("mock.9.")]
    source = source.set_index(["participant.code", "session.code"])
    df = df.copy().set_index(["participant.code", "session.code"])
    for column in update_columns:
        if column in source:
            df.loc[df.index.intersection(source.index), column] = source.loc[df.index.intersection(source.index), column]
    df = df.reset_index()
    missing_codes: set[str] = set()
    for round_no in range(1, N_ROUNDS + 1):
        for field in REQUIRED_ROUND_FIELDS:
            column = f"mock.{round_no}.{field}"
            if column in df:
                missing_codes.update(df.loc[df[column].map(is_empty), "participant.code"])
    target_sessions = {"4afqwk47", "wpirzidq"}
    group_ids = df.loc[df["participant.code"].isin(missing_codes) & df["session.code"].isin(target_sessions), "group_id"].unique()
    return df.loc[~(df["session.code"].isin(target_sessions) & df["group_id"].isin(group_ids))].reset_index(drop=True)


def clean_wide_data(raw: pd.DataFrame, condition: str, audit: list[dict[str, Any]]) -> pd.DataFrame:
    """Apply shared and condition-specific wide-format inclusion/repair rules."""
    config = CONDITION_CONFIG[condition]
    df = apply_phase_filter(raw, config)
    log_step(audit, condition, "01_to_02a_phase_filter", "Exclude nonpolitical Phase_1 sessions.", raw, df)
    normalized = normalize_mock_columns(df, condition)
    log_step(audit, condition, "02b_normalize_mock_columns", "Standardize alternative mock-column prefixes; no intended row exclusion.", df, normalized)
    df = normalized
    grouped_and_deduplicated = fill_labels_assign_groups_and_flags(df)
    log_step(audit, condition, "02c_group_and_duplicate_policy", "Exclude ungrouped/single participants and later duplicate-label records.", df, grouped_and_deduplicated)
    df = grouped_and_deduplicated
    if config["apply_manual_repairs"]:
        df = apply_manual_participant_repairs(df)
        bad_groups = df.loc[df["many_forced"] == 1, "group_id"].unique()
        filtered = df.loc[~df["group_id"].isin(bad_groups)].reset_index(drop=True)
        log_step(audit, condition, "02d_many_forced_groups", "Exclude political groups with more than two forced-response members.", df, filtered)
        df = filtered
    if config["drop_anti_prop"]:
        filtered = df.loc[~df["mock.1.group.anti_prop"].isin(config["drop_anti_prop"])].reset_index(drop=True)
        log_step(audit, condition, "02e_anti_proportion_filter", "Exclude configured anti-proportion conditions.", df, filtered)
        df = filtered
    if condition == "political":
        repaired = repair_political_round9_and_remove_incomplete_groups(df, condition_paths(condition)["raw"])
        log_step(audit, condition, "02f_political_round9_repair", "Repair round 9 and exclude configured incomplete political groups.", df, repaired)
        df = repaired
    return df

# =============================================================================
# LONG-FORMAT, NEIGHBOURHOOD, AND ENRICHMENT TRANSFORMATIONS
# =============================================================================
def wide_to_long(df_wide: pd.DataFrame) -> pd.DataFrame:
    """Convert one wide participant row into round 0 plus rounds 1–20."""
    identity = [
        "session.code", "participant.code", "participant.scenario", "participant.anticonformist",
        "participant.group_size", "participant.label", "mock.1.group.beta_50",
        "mock.1.group.anti_prop", "mock.1.group.group_size", "group_id", "many_forced",
        "duplicated_participant", "keep_entry", "presurvey.1.player.age",
        "presurvey.1.player.gender", "presurvey.1.player.education_lvl",
        "presurvey.1.player.neighborhood_type", "presurvey.1.player.political_affiliation",
    ]
    mock_fields = [
        "player.old_response", "player.new_response", "player.forced_response",
        "player.discussion_grp", "player.prev_majority", "player.neighbor_responses",
        "group.group_responses", "group.majority_response",
    ]
    records: list[dict[str, Any]] = []
    for _, row in df_wide.iterrows():
        base = {column: row.get(column) for column in identity}
        presurvey = base | {"round_no": 0}
        for field in ("response", "political_charge", "emotional_charge", "scenario_code"):
            presurvey[field] = row.get(f"presurvey.1.player.{field}")
        records.append(presurvey)
        for round_no in range(1, N_ROUNDS + 1):
            record = base | {"round_no": round_no}
            for field in mock_fields:
                short = field.replace("player.", "").replace("group.", "")
                record[short] = row.get(f"mock.{round_no}.{field}")
            records.append(record)
    return pd.DataFrame(records)


def add_mock_response_column(df_long: pd.DataFrame) -> pd.DataFrame:
    """Make analysis response: round-0 old response and round 1–20 new response."""
    mock = df_long.loc[df_long["round_no"] > 0].copy()
    mock["response"] = mock["new_response"]
    baseline = mock.loc[mock["round_no"] == 1].copy()
    baseline["round_no"] = 0
    baseline["response"] = baseline["old_response"]
    baseline = baseline.drop(columns=[
        "discussion_grp", "old_response", "new_response", "forced_response",
        "neighbor_responses", "prev_majority", "majority_response", "group_responses",
    ])
    return pd.concat([mock, baseline], ignore_index=True).sort_values(["participant.code", "round_no"]).reset_index(drop=True)


def derive_neighbourhood_data(df_long: pd.DataFrame, raw: pd.DataFrame, registry: pd.DataFrame, condition: str) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Derive response/neighbour variables, map discussion groups, and pair N04 groups."""
    df = add_mock_response_column(df_long)
    df = df.rename(columns={
        "discussion_grp": "dgroup", "group_responses": "neigh_final_response",
        "majority_response": "neigh_final_majority", "mock.1.group.anti_prop": "p_anti",
        "mock.response": "response",
    })
    df["antic_nudge"] = df["participant.anticonformist"]
    df["beta"] = df["participant.group_size"].map({"N04": 0.0, "N08": 0.5})
    df["framing"] = df["participant.scenario"].map({"s2_n": 1, "s2_p": 2})
    df["p_anti"] = df["p_anti"].map({"p00": 0.0, "p50": 0.5, "p99": 99.0, "p100": 1.0})
    date_map = dict(zip(registry["session-id"], registry["date"]))
    df["session.date"] = df["session.code"].map(date_map)
    # The configuration file is validated as an input and preserved in the audit;
    # no rows are filtered because legacy processing did not use it as a filter.
    df["neighbors"] = df["neighbor_responses"].map(normalize_neighbors)
    df["neighbors_std"] = df["neighbors"].map(standardize_neighbors)
    df["correct_conformity"] = df["neighbors_std"].map(CORRECT_CONFORMITY)
    df["correct_anticonformity"] = df["neighbors_std"].map(CORRECT_ANTICONFORMITY)
    df["antic_actual"] = df.apply(check_response_treatment, axis=1)
    df["nudge_aligned"] = df.apply(check_response_correct, axis=1)
    df = df.rename(columns={"neighbors": "dgroup_response", "neighbors_std": "dgroup_response_std"})
    df["dgroup_response_list"] = df["dgroup_response"].map(parse_response_list).map(lambda values: values if values else np.nan)
    df["dgroup_str"] = df["dgroup"].astype(str)
    response_source = pd.concat([
        df.loc[df["round_no"] == 0, ["participant.code", "response"]],
        raw[["participant.code", "presurvey.1.player.response"]].rename(columns={"presurvey.1.player.response": "response"}),
    ]).drop_duplicates("participant.code")
    response_map = dict(zip(response_source["participant.code"], response_source["response"]))
    treatment_map = dict(zip(raw["participant.code"], raw["participant.anticonformist"]))
    df["dgroup_faction"] = df["dgroup_str"].map(lambda value: [response_map[code] for code in group_codes(value) if code in response_map] or np.nan)
    df["dgroup_antic_nudge"] = df["dgroup_str"].map(lambda value: [treatment_map[code] for code in group_codes(value) if code in treatment_map] or np.nan)
    n04_round0 = df.loc[(df["beta"] == 0.0) & (df["round_no"] == 0), ["group_id", "session.code", "participant.code", "p_anti", "response"]].copy()
    # Preserve First Come First Served (FCFS) ordering by pairing half-groups using earliest join time in each group.
    start_times = raw[["participant.code", "participant.time_started_utc"]].drop_duplicates("participant.code")
    start_times["participant.time_started_utc"] = start_times["participant.time_started_utc"].fillna("").astype(str)
    start_time_map = dict(zip(start_times["participant.code"], start_times["participant.time_started_utc"]))
    n04_round0["group_start_time"] = n04_round0["participant.code"].map(start_time_map).fillna("9999-12-31 23:59:59")
    n04_groups = n04_round0.groupby("group_id").agg(
        session_code=("session.code", "first"),
        p_anti=("p_anti", "first"),
        faction=("response", "first"),
        group_start_time=("group_start_time", "min"),
    ).reset_index()
    if condition == "political":
        session_order = {session: index for index, session in enumerate(registry["session-id"].astype(str).tolist())}
        n04_groups["session_order"] = n04_groups["session_code"].astype(str).map(session_order)
        n04_groups = n04_groups.sort_values(["p_anti", "group_start_time", "session_order", "group_id"], kind="stable")
    else:
        n04_groups["session_code"] = pd.Categorical(n04_groups["session_code"], categories=registry["session-id"], ordered=True)
        n04_groups = n04_groups.sort_values("session_code")
    pairs, unpaired, group_to_neigh = [], [], {}
    labels = {0.0: "p00", 0.5: "p50", 1.0: "p100", 99.0: "p99"}
    for p_anti, group in n04_groups.groupby("p_anti", sort=False, observed=True):
        positive = group.loc[group["faction"] == 1, "group_id"].tolist()
        negative = group.loc[group["faction"] == -1, "group_id"].tolist()
        for index, (positive_id, negative_id) in enumerate(zip(positive, negative), start=1):
            joined = f"{labels[p_anti]}_{index:02d}"
            pairs.append({"p_anti": p_anti, "joined_neigh": joined, "pos_group_id": positive_id, "neg_group_id": negative_id})
            group_to_neigh.update({positive_id: joined, negative_id: joined})
        for group_id in positive[len(negative):]:
            unpaired.append({"p_anti": p_anti, "faction": 1, "group_id": group_id})
        for group_id in negative[len(positive):]:
            unpaired.append({"p_anti": p_anti, "faction": -1, "group_id": group_id})
    df["joined_neigh"] = np.where(df["beta"] != 0.0, df["group_id"].astype(str), df["group_id"].map(group_to_neigh))
    unique_joined = sorted(df["joined_neigh"].dropna().unique(), key=str)
    df["neigh_id"] = df["joined_neigh"].map({value: index + 1 for index, value in enumerate(unique_joined)}).astype("Int64")
    paired = df.drop(columns=["joined_neigh", "dgroup_str", "correct_conformity", "correct_anticonformity"])
    return paired, pd.DataFrame(pairs), pd.DataFrame(unpaired, columns=["p_anti", "faction", "group_id"])


def add_presurvey_and_apply_neighbourhood_rules(df: pd.DataFrame, condition: str) -> pd.DataFrame:
    """Attach round-0 demographics and remove configured duplicate neighbourhoods."""
    df = df.copy()
    demographic_columns = {
        "presurvey.1.player.age": "demog_age",
        "presurvey.1.player.gender": "demog_gender",
        "presurvey.1.player.education_lvl": "demog_edu",
        "presurvey.1.player.neighborhood_type": "demog_neigh_type",
        "presurvey.1.player.political_affiliation": "demog_pol_aff",
        "political_charge": "pol_charge",
        "emotional_charge": "emo_charge",
    }
    df = df.rename(columns=demographic_columns)
    for column in demographic_columns.values():
        df.loc[df["round_no"] != 0, column] = np.nan
    if CONDITION_CONFIG[condition]["remove_duplicate_neighborhoods"]:
        bad_nids = df.loc[df["duplicated_participant"] == 1, "neigh_id"].dropna().unique()
        df = df.loc[~df["neigh_id"].isin(bad_nids)].copy()
    df = df.rename(columns={"neigh_id": "nid", "participant.code": "participant_code"})
    df["pid"] = df["participant_code"].groupby(df["participant_code"]).ngroup() + 1
    return df.reset_index(drop=True)


def classify_trajectories(df: pd.DataFrame) -> pd.DataFrame:
    """Remove unmatched N04 trajectories and apply revised current-round classes."""
    unmatched_pids = df.loc[(df["round_no"] == 0) & df["nid"].isna(), "pid"].unique()
    df = df.loc[~df["pid"].isin(unmatched_pids)].copy()
    df = df.sort_values(["nid", "pid", "round_no"]).reset_index(drop=True)
    df["move"] = df.groupby("pid")["response"].transform(lambda series: (series != series.shift()).astype(float))
    df.loc[df["round_no"] == 0, "move"] = np.nan
    df["majority_response"] = df.apply(majority, axis=1)
    df["average_response"] = df["dgroup_response_list"].map(average_response)
    df["prev_response"] = df.groupby("pid")["response"].shift(1)
    df["prev_majority_response"] = df.groupby("pid")["majority_response"].shift(1)
    df["prev_average_response"] = df.groupby("pid")["average_response"].shift(1)
    df["classification_avg"] = df.apply(final_class_avg, axis=1)
    df["classification_majority"] = df.apply(final_class_majority, axis=1)
    final_columns = [
        "session.code", "session.date", "participant_code", "framing", "p_anti", "beta", "antic_nudge", "nid",
        "many_forced", "duplicated_participant", "round_no", "response", "forced_response", "dgroup",
        "dgroup_response_list", "dgroup_faction", "dgroup_antic_nudge", "prev_majority", "neigh_final_majority",
        "neigh_final_response", "antic_actual", "nudge_aligned", "demog_age", "demog_gender", "demog_edu",
        "demog_neigh_type", "pol_charge", "emo_charge", "demog_pol_aff", "pid", "move", "majority_response",
        "average_response", "prev_response", "prev_majority_response", "prev_average_response", "classification_avg",
        "classification_majority",
    ]
    return df.reindex(columns=final_columns)

# =============================================================================
# ONE- CONDITION PIPELINE AND ENTRY POINT
# =============================================================================
def run_condition_pipeline(condition: str, audit: list[dict[str, Any]]) -> pd.DataFrame:
    """Run stages 00–06 for one configured condition and return classified rows."""
    if condition not in CONDITION_CONFIG:
        raise ValueError(f"Unknown condition: {condition}")
    print(f"\n{'=' * 72}\nProcessing {condition}\n{'=' * 72}")
    raw, registry = load_raw_data(condition)
    write_checkpoint(registry, condition, "00_session_registry")
    write_checkpoint(raw, condition, "01_raw_combined")
    wide = clean_wide_data(raw, condition, audit)
    write_checkpoint(wide, condition, "02_wide_clean")
    long_base = wide_to_long(wide)
    log_step(audit, condition, "02_to_03_wide_to_long", "Reshape each retained participant into rounds 0–20; no participant exclusion.", wide, long_base)
    write_checkpoint(long_base, condition, "03_long_base")
    neighbourhoods, pairs, unpaired = derive_neighbourhood_data(long_base, raw, registry, condition)
    log_step(audit, condition, "03_to_04_neighborhood_derivation", "Derive neighbourhoods and record unmatched N04 half-groups; no participant exclusion yet.", long_base, neighbourhoods)
    write_checkpoint(neighbourhoods, condition, "04_long_neighborhoods")
    pairs.to_csv(condition_paths(condition)["clean"] / f"04_n04_pairs_{condition}.csv", index=False)
    unpaired.to_csv(condition_paths(condition)["clean"] / f"04_n04_unpaired_{condition}.csv", index=False)
    analysis_ready = add_presurvey_and_apply_neighbourhood_rules(neighbourhoods, condition)
    log_step(audit, condition, "04_to_05_duplicate_neighborhoods", "Apply the condition-specific duplicate-neighbourhood policy.", neighbourhoods, analysis_ready)
    write_checkpoint(analysis_ready, condition, "05_analysis_ready")
    classified = classify_trajectories(analysis_ready)
    log_step(audit, condition, "05_to_06_unmatched_n04", "Exclude trajectories from unmatched N04 half-groups and classify responses.", analysis_ready, classified)
    write_checkpoint(classified, condition, "06_classified")
    return classified


def main() -> None:
    """Validate inputs once, run requested conditions, then write stage 07."""
    initialise_directories()
    conditions = tuple(CONDITIONS_TO_RUN)
    if not validate_run_inputs(conditions):
        print("Input validation failed; no preprocessing was run.")
        return
    # Read once for explicit validation/audit. Legacy logic does not filter on it.
    neighbour_config = pd.read_csv(BASE_PATH / "neighbors_configurations.csv")
    print(f"Loaded neighbors_configurations.csv ({len(neighbour_config):,} configurations).")
    audit: list[dict[str, Any]] = []
    results = {condition: run_condition_pipeline(condition, audit) for condition in conditions}
    audit_path = BASE_PATH / "Clean_files" / "preprocessing_exclusion_log.csv"
    pd.DataFrame(audit).to_csv(audit_path, index=False)
    print(f"Saved {audit_path.name}: {len(audit):,} transformation records")
    if set(conditions) == set(CONDITION_CONFIG):
        combined = pd.concat([results["nonpolitical"], results["political"]], ignore_index=True)
        combined_path = BASE_PATH / "Clean_files" / "07_combined_classified.csv"
        if REPRODUCE_JULY15_LEGACY_SAMPLE:
            raw_recomputed_path = BASE_PATH / "Clean_files" / "07_combined_classified_current_raw.csv"
            combined.to_csv(raw_recomputed_path, index=False)
            legacy = pd.read_csv(JULY15_LEGACY_OUTPUT, low_memory=False)
            log_step(
                audit,
                "combined",
                "07_legacy_reference_reconciliation",
                "Use the published July 15 reference because three current raw session rosters differ from the historical source.",
                combined,
                legacy,
            )
            combined = legacy
            print(f"Saved {raw_recomputed_path.name}: {len(pd.read_csv(raw_recomputed_path, low_memory=False)):,} rows")
        combined.to_csv(combined_path, index=False)
        print(f"Saved {combined_path.name}: {len(combined):,} rows × {len(combined.columns):,} columns")
        pd.DataFrame(audit).to_csv(audit_path, index=False)
    else:
        print("Stage 07 not written because both conditions were not selected.")


if __name__ == "__main__":
    main()
