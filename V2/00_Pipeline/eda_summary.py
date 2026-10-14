import csv, json, os, sys, uuid

ORDER = ["DHS 1998", "DHS 2003", "DHS 2008", "DHS 2014", "DHS 2022"]

def read(path):
    with open(path) as f:
        return list(csv.DictReader(f))

def fmt_n(x):
    return f"{int(round(float(x))):,}" if x not in (None, "", "NA") else "–"

def build_summary(eda_out):
    rec = {r["survey_id"]: r for r in read(os.path.join(eda_out, "KEN_records_by_round.csv"))}
    prev = {(r["survey_id"], r["population"]): r for r in read(os.path.join(eda_out, "KEN_direct_prevalence_by_round.csv"))}
    inf = {(r["survey_id"], r["reporter"]): r for r in read(os.path.join(eda_out, "KEN_infancy_reporting_by_round.csv"))}

    def p(sid, pop):
        r = prev.get((sid, pop))
        if not r: return "–"
        return f"{100*float(r['prevalence']):.1f} ({100*float(r['lower_ci']):.1f}–{100*float(r['upper_ci']):.1f})"

    t1 = ["| Survey | Women interviewed | Women with known status | Never heard (counted uncut) | Women cut | Prevalence, women 15–49 % (95% CI) | Daughters 0–14 | Daughters cut | Prevalence, girls 0–14 % (95% CI) | Men with attitude answer |",
          "|---|---:|---:|---:|---:|---|---:|---:|---|---:|"]
    for sid in ORDER + ["Total"]:
        r = rec[sid]
        daughters = fmt_n(r["daughters_0_14"]) if int(r["daughters_0_14"]) else "–"
        dcut = fmt_n(r["daughters_cut"]) if int(r["daughters_0_14"]) else "–"
        men = fmt_n(r["men_attitude_continue_or_stop"]) if int(r["men_attitude_continue_or_stop"]) else "–"
        label = f"**{sid}**" if sid == "Total" else sid
        t1.append(f"| {label} | {fmt_n(r['women_interviewed'])} | {fmt_n(r['status_known'])} | {fmt_n(r['never_heard_counted_uncut'])} | "
                  f"{fmt_n(r['women_cut'])} | {p(sid, 'Women 15-49') if sid != 'Total' else '–'} | {daughters} | {dcut} | "
                  f"{p(sid, 'Daughters 0-14') if sid != 'Total' else '–'} | {men} |")

    W, D, R = "Women: own cutting", "Mothers: daughters 0-14 (full roster)", "Mothers: most recently cut daughter"
    t2 = ["| Survey | Women cut | Women: \"during infancy\" | Women: age 0 | Women: age unknown | Women cut in infancy, % (weighted) | Daughters cut | Daughters: \"during infancy\" or age 0 | Daughters: age unknown | Daughters cut in infancy, % (weighted) |",
          "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
    totals = [0] * 8
    for sid in ORDER:
        w = inf.get((sid, W)); d = inf.get((sid, D)) or inf.get((sid, R))
        dlabel = "" if inf.get((sid, D)) else (" †" if d else "")
        if w:
            wv = [int(w["cut_cases"]), int(w["reported_during_infancy"]), int(w["reported_age_0"]), int(w["age_unknown"])]
            wcells = [f"{v:,}" for v in wv] + [f"{float(w['pct_infancy_weighted']):.1f}"]
            for k, v in enumerate(wv): totals[k] += v
        else:
            wcells = [fmt_n(rec[sid]["women_cut"]), "not asked", "not asked", "not asked", "–"]
            totals[0] += int(rec[sid]["women_cut"])
        if d:
            dv = [int(d["cut_cases"]), int(d["reported_during_infancy"]) + int(d["reported_age_0"]), int(d["age_unknown"])]
            dcells = [f"{v:,}{dlabel}" for v in dv] + [f"{float(d['pct_infancy_weighted']):.1f}{dlabel}"]
            for k, v in enumerate(dv): totals[4 + k] += v
        else:
            dcells = ["–"] * 4
        t2.append("| " + " | ".join([sid] + wcells + dcells) + " |")
    t2.append(f"| **Total** | {totals[0]:,} | {totals[1]:,} | {totals[2]:,} | {totals[3]:,} | – | {totals[4]:,} | {totals[5]:,} | {totals[6]:,} | – |")

    return "\n".join([
        "## 8. Summary of results",
        "",
        "Generated from the tables saved by this notebook (`03_Outputs/KEN/EDA/`). Counts are unweighted; percentages are survey-weighted.",
        "",
        "### 8.1 Records and prevalence by survey round",
        "",
        *t1,
        "",
        "### 8.2 Cutting declared at infancy, by survey round",
        "",
        "\"During infancy\" is the explicit answer code (DHS code 95, DHS 2008–09, 2014 and 2022); DHS 1998 has no such code. The infancy percentage counts infancy answers and reported ages of 0, and keeps \"don't know\" answers in the denominator. In Kenya very few women report cutting in infancy.",
        "",
        *t2,
        "",
        "† DHS 2008–09 records age at cutting only for the most recently cut daughter, so its daughter figures cover that daughter only. DHS 2003 did not ask women their own age at cutting.",
        *dont_know_section(eda_out),
    ])

def dont_know_section(eda_out):
    path = os.path.join(eda_out, "KEN_women_dont_know_by_round.csv")
    if not os.path.exists(path):
        return []
    rows = {r["survey_id"]: r for r in read(path)}
    def opt(n, pct, available):
        return f"{int(float(n)):,} ({float(pct):.1f}%)" if available else "not an answer option"
    lines = ["", "### 8.3 Women answering \"don't know\", by survey round", "",
             "| Survey | Women asked own status | Own status \"don't know\" (weighted %) | Women cut | Age at cutting \"don't know\" (weighted %) | Age missing or inconsistent |",
             "|---|---:|---:|---:|---:|---:|"]
    tot = [0, 0, 0, 0, 0]
    for sid in ORDER:
        r = rows[sid]
        sdk = int(float(r["status_dont_know"])); has_age = r["age_dont_know"] not in ("", "NA")
        status_cell = opt(sdk, r["status_dont_know_pct"], sdk > 0)
        age_cell = opt(r["age_dont_know"], r["age_dont_know_pct"], True) if has_age else "age not asked"
        lines.append(f"| {sid} | {int(float(r['women_asked_own_status'])):,} | {status_cell} | {int(float(r['women_cut'])):,} | "
                     f"{age_cell} | {int(float(r['age_missing_or_inconsistent'])):,} |")
        tot[0] += int(float(r["women_asked_own_status"])); tot[1] += sdk; tot[2] += int(float(r["women_cut"]))
        tot[3] += int(float(r["age_dont_know"])) if has_age else 0; tot[4] += int(float(r["age_missing_or_inconsistent"]))
    lines.append(f"| **Total** | {tot[0]:,} | {tot[1]:,} | {tot[2]:,} | {tot[3]:,} | {tot[4]:,} |")
    lines += ["", "Women with an unknown own status are excluded from prevalence and risk; cut women with an unknown age at cutting are kept in risk incidence with an estimated age (section 7.1)."]
    return lines

def insert_summary(nb_path, eda_out):
    nb = json.load(open(nb_path))
    text = build_summary(eda_out)
    cell = {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {},
            "source": [l + "\n" for l in text.split("\n")[:-1]] + [text.split("\n")[-1]]}
    cells = nb["cells"]
    existing = [i for i, c in enumerate(cells) if "".join(c["source"]).startswith("## 8. Summary of results")]
    if existing:
        cells[existing[0]] = cell
    else:
        notes = [i for i, c in enumerate(cells) if "".join(c["source"]).startswith("## 8. Notes for interpretation")]
        idx = notes[0] if notes else len(cells)
        cells.insert(idx, cell)
    for c in cells:
        s = "".join(c["source"])
        if s.startswith("## 8. Notes for interpretation"):
            s = s.replace("## 8. Notes for interpretation", "## 9. Notes for interpretation", 1)
            c["source"] = [l + "\n" for l in s.split("\n")[:-1]] + [s.split("\n")[-1]]
    json.dump(nb, open(nb_path, "w"), indent=1, ensure_ascii=False)

if __name__ == "__main__":
    insert_summary(sys.argv[1], sys.argv[2])
    print("summary inserted")
