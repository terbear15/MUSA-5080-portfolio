"""Validate the adopted report/CSV and preserved comparisons after rendering."""
from pathlib import Path
from html.parser import HTMLParser
import csv
import hashlib
import json
import math
import re
import statistics
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def rows(name):
    with (ROOT / name).open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def md5(name):
    return hashlib.md5((ROOT / name).read_bytes()).hexdigest()


def git(*args):
    return subprocess.check_output(["git", *args], cwd=ROOT, text=True).strip()


checks = {}
checks["active_branch_is_dev"] = git("branch", "--show-current") == "dev"
index = json.loads((ROOT / "references/reference_index.json").read_text())
checks["main_not_merged_or_moved"] = git("rev-parse", "main") == index["original_main_commit"]
archive = rows("references/original_m10/archive_manifest.csv")
checks["original_m10_archive_intact"] = all(md5(r["archive_path"]) == r["md5"] for r in archive)
preserved = rows("references/comparison_artifacts_manifest.csv")
checks["all_comparison_artifacts_intact"] = all(md5(r["path"]) == r["md5"] for r in preserved)

pred = rows("team_Mahjong_predictions.csv")
test = rows("chicago_test.csv")
checks["prediction_schema"] = list(pred[0]) == ["id", "pred"]
checks["all_5789_test_ids_in_original_order"] = len(pred) == len(test) == 5789 and [r["id"] for r in pred] == [r["id"] for r in test]
checks["unique_ids_and_finite_predictions"] = len({r["id"] for r in pred}) == 5789 and all(math.isfinite(float(r["pred"])) for r in pred)
checks["convenience_csv_identical"] = md5("team_Mahjong_predictions.csv") == md5("assignment5_predictions.csv")
model_checks = {r["check"]: r["value"] for r in rows("data/assignment5/model_verification.csv")}
checks["verified_model_is_adopted_NS"] = model_checks["final_model"] == "NS" and model_checks["response"] == "sale_price"
checks["model_object_matches_verified_file"] = md5("data/assignment5/model_results.rds") == model_checks["model_file_md5"]
checks["csv_matches_verified_model_export"] = md5("team_Mahjong_predictions.csv") == model_checks["team_file_md5"] and float(model_checks["predict_max_difference"]) < 1e-5

log = rows("data/assignment5/model_log.csv")
expected = {*(f"m{i}" for i in range(1, 11)), *(f"E{i}" for i in range(1, 5)), "NS"}
checks["all_15_reported_models_logged"] = len(log) == 15 and {r["model"] for r in log} == expected
checks["log_has_reasons_rmse_and_moran"] = all(r["notes"] and r["added"] and math.isfinite(float(r["cv_rmse"])) and math.isfinite(float(r["moran_i"])) for r in log)
checks["NS_explicitly_selected"] = [r["model"] for r in log if r["selected"] == "TRUE"] == ["NS"]
folds = rows("data/assignment5/fold_scores.csv")
checks["ten_folds_per_model_and_consistent_rmse"] = all(
    len(a := [r for r in folds if r["model"] == entry["model"]]) == 10
    and abs(statistics.mean(float(r["rmse"]) for r in a) - float(entry["cv_rmse"])) < 1e-6
    for entry in log
)
oof = rows("data/assignment5/out_of_fold_predictions.csv")
final = next(r for r in log if r["model"] == "NS")
checks["all_retained_training_rows_have_oof_predictions"] = len(oof) == 21968 and all(math.isfinite(float(r["pred_final"])) for r in oof)
checks["pooled_rmse_recomputed"] = abs(math.sqrt(statistics.mean(float(r["residual_final"]) ** 2 for r in oof)) - float(final["pooled_oof_rmse"])) < 1e-6
low = [r for r in oof if r["income_group"] == "Under $50k"]
checks["low_income_metrics_recomputed"] = len(low) == 4641 and abs(statistics.median(float(r["ratio"]) for r in low) - float(final["low_income_median_ratio"])) < 1e-10
checks["remaining_negative_disclosed"] = sum(float(r["pred"]) < 0 for r in pred) == 2 and sum(float(r["pred_final"]) < 0 for r in oof) == 10

source = (ROOT / "assign5.qmd").read_text(encoding="utf-8")
checks["required_report_sections"] = all(f"## Part {i}:" in source for i in range(7)) and "### Who did what" in source and "### AI Team Statement" in source
checks["four_exploration_plots"] = all(f"label: {label}" in source for label in ["price-distribution", "price-map", "size-price", "age-price"])
reflection = source.split("## Part 6: Reflection", 1)[1].split("### Who did what", 1)[0]
reflection_words = len(re.findall(r"\b[\w’'-]+\b", reflection))
checks["reflection_about_300_words"] = 250 <= reflection_words <= 350


class InspectHTML(HTMLParser):
    def __init__(self):
        super().__init__()
        self.images = []
        self.meta = {}
        self.skip = 0
        self.text = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "img":
            self.images.append(attrs.get("src", ""))
        if tag == "meta" and "name" in attrs:
            self.meta[attrs["name"]] = attrs.get("content", "")
        if tag in ("script", "style"):
            self.skip += 1

    def handle_endtag(self, tag):
        if tag in ("script", "style"):
            self.skip = max(0, self.skip - 1)

    def handle_data(self, data):
        if not self.skip:
            self.text.append(data)


page = InspectHTML()
page.feed((ROOT / "assign5.html").read_text(encoding="utf-8"))
body = " ".join(page.text)
checks["seven_embedded_report_figures"] = len(page.images) == 7 and all(p.startswith("data:") for p in page.images)
checks["html_is_current_english_team_report"] = "Team Mahjong" in page.meta.get("author", "") and "179,512" in body and "NS" in body and not re.search(r"[\u4e00-\u9fff]", body)
checks["no_later_models_in_formal_report"] = not re.search(r"\bC[1-8]\b|\bS\d+\b|grid_3mi|3-mile", body)
checks["annotated_NS_formula_in_report"] = "The log-lot coefficient may differ between North and South." in body
checks["team_filename_in_report"] = "team_Mahjong_predictions.csv" in body

result = {
    "checks": checks,
    "all_checks_passed": all(bool(v) for v in checks.values()),
    "summary": {"final_model": "NS", "cv_rmse": float(final["cv_rmse"]),
        "pooled_oof_rmse": float(final["pooled_oof_rmse"]), "moran_i": float(final["moran_i"]),
        "test_rows": len(pred), "test_negative": 2, "oof_negative": 10,
        "low_income_median_ratio": float(final["low_income_median_ratio"]),
        "reflection_words": reflection_words, "preserved_artifacts": len(preserved)},
    "human_steps_remaining": [
        "Each member verifies the allocated review responsibilities and understands the full model.",
        "Each member renders/publishes the report on their own portfolio and submits that URL to Canvas.",
        "One member uploads team_Mahjong_predictions.csv by the Canvas deadline.",
    ],
}
(ROOT / "data/assignment5/submission_checks.json").write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
print(json.dumps(result, indent=2))
if not result["all_checks_passed"]:
    raise SystemExit("Submission checks failed.")
