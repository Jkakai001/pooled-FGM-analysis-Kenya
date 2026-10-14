import json, sys, uuid

NB = sys.argv[1]

def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]

def insert(nb_name, title, text, before_prefix=None):
    path = f"{NB}/{nb_name}"
    nb = json.load(open(path))
    cells = nb["cells"]
    cell = {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(text)}
    existing = [i for i, c in enumerate(cells) if "".join(c["source"]).startswith(title)]
    if existing:
        cells[existing[0]] = cell
    else:
        idx = len(cells)
        if before_prefix:
            hits = [i for i, c in enumerate(cells) if "".join(c["source"]).startswith(before_prefix)]
            if hits: idx = hits[0]
        cells.insert(idx, cell)
    json.dump(nb, open(path, "w"), indent=1, ensure_ascii=False)
    print("results inserted:", nb_name)

RISK = """## Results

Risk incidence pools **74,656 histories** from four DHS rounds (1998, 2008–09, 2014, 2022): 47,942
women reporting on themselves (13,702 cut) and 26,714 daughters aged 0–14 reported by their
mothers (1,428 cut). DHS 2003 did not ask age at cutting. All risks are survey-weighted
Kaplan–Meier estimates unless stated otherwise.

**1. Cutting happens mainly at ages 5–15.** Only 2–4% of cut women report cutting before age 5;
42–45% report ages 10–14 and 22–32% age 15 or older. In the hazard model, the annual odds of
being cut are 13 times those at ages 0–4 at ages 5–9, 52 times at 10–14 and 19 times at 15 and
older.

**2. Risk has fallen steeply across cohorts.** Women's risk of being cut by age 15 falls from
about 37–42% for women born in 1945–64 to 26.4% (1970–74), 15.9% (1985–89), 11.3% (1995–99)
and 8.5% (2005–09). In the hazard model, the odds of cutting fall steadily from the 1965–69 cohort
(0.68 relative to 1945–49) to 0.30 (1985–89), 0.16 (2000–04) and 0.09 (2010–14). No separate
change point was detected. Mothers report that 3–7% of daughters born since 1995 were cut by
age 10.

**3. Who is at risk.** Relative to Kikuyu, the annual odds of cutting are far higher among
Somali (20.3), Kisii (13.9) and Maasai (8.0) and higher among Kalenjin (2.2); they are very low
among Luo (0.01), Luhya (0.06) and Mijikenda/Swahili (0.22). Urban residence lowers the odds
(0.57). Relative to Central province, they are highest in North Eastern (4.1), Eastern (2.0) and
Nyanza (1.5) and lowest in Western (0.17).

**4. Geography.** North Eastern stands apart: among women born in 2005–09, smoothed risk is 72%
at ages 5–10 and a further 22% at 10–15; in every other province risk at ages 5–15 is under 15%.
Among daughters born in 2010–14, smoothed risk at ages 5–10 is 65% in North Eastern and under 5%
elsewhere.

**5. Validation.** The covariate-adjusted model (12A) agrees with province direct estimates
(correlation 0.87 for women and 0.96 for daughters).

### Sensitivity analysis (section 15B)

Checks on women's own reports use risk by age 15; the comparison with mothers' reports uses
risk by age 10, because daughters are observed only to age 14.

1. **The same cohort gives similar answers in different rounds.** Women born in 1975–79 report a
   risk by age 15 of 23.3% (DHS 1998), 23.4% (2008–09), 22.1% (2014) and 18.4% (2022); women
   born in 1965–69 report 30.8%, 31.6% and 31.5%. Reporting is stable up to DHS 2014; DHS 2022
   gives lower figures for cohorts born before 1985 (e.g. 17.8% against 24.6–30.5% for 1970–74),
   consistent with under-reporting since FGM was criminalised in 2011.
2. **Holding age at interview fixed (20–29),** risk by age 15 falls from 30.5% (1970–74) to 20.6%
   (1980–84), 11.8% (1990–94) and 9.5% (2000–04), the same path as the all-ages series. The
   decline is robust.
3. **Age at interview within cohort** shows no consistent pattern (born 1985–89: 15.5% at 15–24,
   15.7% at 25–34, 17.2% at 35–49).
4. **Women's own reports and mothers' reports** give similar risks by age 10 for girls born in
   2000–04 (5.9% and 6.6%); for 1995–99 mothers report less (4.2% against 7.8%), from a small
   number of older daughters.
5. **DHS vs MICS** is not possible nationally (Kenya has no national MICS); the subnational MICS
   agree with DHS for the same areas (section 15C and the prevalence notebook).
6. **Estimating vague and unknown ages** changes risk by age 15 by under 3 points for every
   cohort, because almost all Kenyan women give an exact age.

**Overall.** FGM in Kenya is mostly a practice of ages 5–15. Risk has fallen steadily for four
decades, from about 40% to under 10% by age 15, and the decline is robust to how and when women
report. Risk remains extremely high in North Eastern province (Somali communities) and is
concentrated among Somali, Kisii and Maasai girls.
"""

PREV = """## Results

Prevalence pools all five DHS rounds: 56,116 women aged 15–49 with a known FGM status and 26,714
daughters aged 0–14 from DHS 2014 and 2022. Kenya has no national MICS.

**1. National prevalence has fallen by more than half.** Among women 15–49 it fell from 37.7% in
DHS 1998 to 32.2% (2003), 27.1% (2008–09), 21.0% (2014) and 14.8% (2022). Among girls 0–14 it was
2.9% in 2014 and 1.6% in 2022. These figures reproduce the official DHS reports.

**2. North Eastern stands apart.** Pooling all rounds, model-based prevalence among women is 95.0%
in North Eastern, then 34.7% in Nyanza, 30.5% in Rift Valley and 29.3% in Eastern, and 1.7% in
Western. Among girls it is 35.4% in North Eastern and 4.0% in Nyanza, and under 2% elsewhere.

**3. Counties (DHS 2014 and 2022).** In 2022 prevalence among women is highest in Wajir (96.3%),
Mandera (95.9%), Marsabit (82.9%), Garissa (80.4%), Kisii (76.6%), Nyamira (74.1%) and Samburu
(73.3%). Among girls it is highest in Wajir (42.4%), Mandera (27.4%) and Garissa (23.9%).

**4. Prevalence has declined across birth cohorts.** Among women it falls from 56.5% for those
born in 1945–49 to 34.1% (1970–74), 19.1% (1985–89) and 8.9% (2005–09); the pooled average is 24.1%.

**5. Composition.** Relative to Kalenjin women, the odds of being cut are much higher among Kisii
(20.5), Maasai (10.0) and Somali (5.9) women and much lower among Luo, Luhya and Mijikenda/Swahili
women. Muslim women (4.8) and the poorest (1.8) have higher odds; secondary education (0.24) and
urban residence (0.77) lower odds.

**6. Consistency check.** The pooled model reproduces the direct province estimates (within 0.25
points). A weakly informative prior on the model coefficients keeps estimates stable for groups
with almost no cut girls.

**7. Subnational MICS (section 8A).** MICS and DHS agree for the same areas: Nyanza 36.5% (MICS
2011) against 32.4% (DHS 2014); Bungoma 2.1% against 1.2%, Kakamega 1.2% against 0.8% and Turkana
3.2% against 3.7% (MICS 2013–14 against DHS 2014). None of the differences is significant.
"""

ATT = """## Results

**1. Support for continuation has fallen.** Among women who have heard of FGM, support fell from
21.3% in DHS 1998 to 10.3% (2008–09), 6.3% (2014) and 4.4% (2022). Among men it fell from 9.5%
(2014) to 4.9% (2022); men were more supportive than women in 2014 (+3.2 points, p < 0.001) but not
in 2022 (+0.5 points, not significant).

**2. North Eastern.** Pooled support is 85.6% among women and 66.9% among men in North Eastern,
against 3–18% among women and 2–8% among men elsewhere (Nyanza women 18.0%). By county (2022),
women's support is highest in Mandera (90.5%), Wajir (81.2%) and Garissa (67.5%).

**3. Who supports continuation.** Women who have not been cut have far lower odds of support
(0.12). Relative to Kalenjin, support is higher among Somali women (7.1) and men (6.7) and Kisii
women (4.2); it is lower with secondary education (women 0.26, men 0.33).

**4. Subnational MICS (Part D).** MICS and DHS support agree in Bungoma, Kakamega and Turkana
(2–6%); in Nyanza support fell from 22.3% (MICS 2011) to 8.9% (DHS 2014).
"""

CMP = """## Results

**1. Province level.** Prevalence and support for continuation are strongly correlated across the
8 provinces (0.95–1.00), entirely because North Eastern combines very high prevalence and support.

**2. County level (DHS 2014 and 2022).** Across 47 counties, women's support correlates 0.93 with
girls' prevalence and 0.79 with women's prevalence; men's support 0.91 and 0.79. Women's and men's
support are themselves almost identical across counties (0.98), so their links with prevalence can
only be separated weakly: for girls' prevalence, women's support remains associated (+7.3 points
per standard deviation, p = 0.003) and men's adds nothing (p = 0.57).

**3. Cluster maps.** Clusters where most women support continuation are almost all in North
Eastern (97% of its clusters, against 0–10% elsewhere); for men, 84% of North Eastern clusters.
"""

insert("FGM_Risk_Incidence.ipynb", "## Results", RISK, "## 17. Final outputs")
insert("FGM_Prevalence_Incidence.ipynb", "## Results", PREV, "## Notes")
insert("FGM_Attitudinal_Incidence.ipynb", "## Results", ATT, None)
insert("FGM_Prevalence_Attitude_Comparison.ipynb", "## Results", CMP, "## Interpretation")
