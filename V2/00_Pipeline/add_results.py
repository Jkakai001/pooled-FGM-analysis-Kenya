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

Risk incidence pools **86,876 histories** from the four DHS rounds that recorded age at cutting
(1998–99, 2003, 2010, 2021): 53,617 women reporting on themselves (36,970 cut) and 33,259
daughters aged 0–14 reported by their mothers in DHS 2010 and 2021 (3,774 cut). MICS 2006 did
not ask age at cutting. All risks are survey-weighted Kaplan–Meier estimates unless stated
otherwise.

**1. Cutting happens throughout early and middle childhood, not only in infancy.** Pooling all
cohorts, women's conditional risk is 42.2% within ages 0–5, 37.5% within 5–10 among those
still uncut at 5, and 13.0% within 10–15. In the hazard model, the annual odds of being cut at
ages 5–9 are 1.21 times those at 0–4; they fall to 0.61 at 10–14 and 0.05 at 15+.

**2. Risk has fallen sharply for cohorts born since the late 1980s.** Women's own reports give
a risk of being cut by age 5 of 48–49% for those born in 1965–79, then 44.3% (1985–89),
41.1% (1990–94), 30.8% (1995–99), 23.0% (2000–04) and 17.4% (2005–09); risk by age 15 falls
from 82.1% (1965–69) to 60.3% (1990–94) and 37.0% (2000–04). Mothers' reports for daughters
give risk by age 5 of 20.7% (born 1995–99), 14.7% (2000–04), 10.6% (2005–09), 10.5%
(2010–14) and 8.2% (2015–19); girls born in 2020–24 are observed only to age 1 (1.3%). In the
hazard model, the odds of being cut rose to a peak of 1.44 (1965–74 cohorts, relative to
1945–49), then fell to 0.66 (1995–99), 0.36 (2005–09) and 0.33–0.36 (2010–19). No
statistically distinct change point was detected in the mean hazard over ages 0–14.

**3. Who is at risk.** Holding cohort, age and region constant, urban residence lowers the
annual odds of cutting slightly (odds ratio 0.92). Relative to Mossi, risk is higher among
Fulfulde/Peul (1.23) and lower among Lobi (0.73), Gourmantché and Gourounsi (0.77) and Bobo
(0.84). Relative to Boucle du Mouhoun, regional effects are highest in Hauts-Bassins (1.32),
Sahel (1.25, not significant) and Sud-Ouest (1.23), and lowest in Centre-Ouest (0.42),
Centre-Sud (0.64), Est (0.70) and Centre (0.74). Women's own reports carry twice the annual
odds of mothers' reports for daughters (odds ratio 2.01).

**4. Where recent cutting is concentrated.** Among women born in 2005–09, smoothed risk by
age 5 is highest in Sahel (34.2%), Hauts-Bassins (28.9%), Centre-Est (26.0%) and Sud-Ouest
(25.3%), and lowest in Centre-Ouest (7.3%) and Centre-Sud (11.6%). Among daughters born in
2015–19, Sahel stands out (32.6%), followed by Hauts-Bassins (15.3%) and Nord (14.4%); it is
lowest in Centre (3.6%), Est (3.7%) and Centre-Ouest (3.8%).

**5. Validation.** The pooled national trend reproduces `survfit()` exactly. The covariate-
adjusted model (12A) agrees with the regional direct estimates for risk by age 5 (correlation
0.73 for women, 0.89 for daughters; mean difference under 2 points).

### Sensitivity analysis (section 15B)

1. **The same cohort gives different risks in different rounds, depending on the
   questionnaire.** Women born in 1975–79 report a risk by age 5 of 37.7% in DHS 1998–99,
   52.3% in DHS 2003, 56.4% in DHS 2010 and 39.5% in DHS 2021. DHS 2003 and 2010 offered a
   "during infancy" answer (chosen by 41% and 46% of cut women), which is spread over ages
   0–4; DHS 1998–99 and 2021 had no such code and women gave an exact age, often later. The
   level of risk by age 5 therefore depends on the question, while the shape of the trend
   (rise to the 1970s cohorts, decline after the late 1980s) appears in every round.
2. **Holding age at interview fixed, the decline starts after the 1985–89 cohort.** Using only
   women interviewed at ages 20–29, risk by age 5 is 52.8% (1980–84) and 51.3% (1985–89), then
   39.8% (1990–94), 29.5% (1995–99) and 26.2% (2000–04). The recent decline is robust to age
   at interview.
3. **Age at interview within cohort** is confounded with questionnaire round: for women born
   in 1985–89, risk by age 5 is 46.6% reported at 15–24, 41.8% at 25–34 and 38.4% at 35–49.
4. **Mothers report less early cutting than young women report for themselves.** For girls
   born in 1995–99, 2000–04 and 2005–09, mothers' reports give 20.7%, 14.7% and 10.6%, while
   the same cohorts reporting for themselves give 30.8%, 23.0% and 17.4%. Much cutting happens
   after age 5, and women's own reports in DHS 2010 place many cases "during infancy" (spread
   over ages 0–4), while mothers give exact ages for their daughters.
5. **DHS vs MICS** cannot be compared: MICS 2006 did not ask age at cutting.
6. **Estimating vague and unknown ages matters for older cohorts.** Restricting to exact
   reported ages would put risk by age 5 at 28.3% instead of 48.9% for women born in 1965–69
   and 29.7% instead of 41.1% for 1990–94; for cohorts seen only in DHS 2021 (exact ages) and
   for daughters (0.1–0.4 points) the difference is negligible.

**Overall.** FGM in Kenya is a practice of early and middle childhood, with substantial
risk at ages 5–9. Risk peaked among women born in the 1960s and 1970s and has fallen steeply
for cohorts born since the late 1980s; mothers' reports show risk by age 5 falling from about
21% for daughters born in 1995–99 to about 8% for those born in 2015–19. The Sahel remains a
clear exception. Because the "during infancy" code changes the reported age, comparisons of
early-age risk between rounds should use the same questionnaire design or a fixed age at
interview.
"""

PREV = """## Results

Prevalence pools all five survey rounds: 60,825 women aged 15–49 with a known FGM status
(DHS 1998–99, 2003, 2010, 2021 and MICS 2006) and 33,259 daughters aged 0–14 from the two
rounds with a full daughter roster (DHS 2010, 2021).

**1. National prevalence fell after 2010.** Among women aged 15–49, prevalence was 72.1% in
DHS 1998–99, 76.6% in DHS 2003, 73.7% in MICS 2006 and 75.8% in DHS 2010, then fell to 56.1% in
DHS 2021. Among girls aged 0–14, it fell from 13.4% (DHS 2010) to 9.4% (DHS 2021). Girls'
prevalence is not lifetime risk, since many have not yet passed the ages at which cutting
occurs (up to about 10 in Kenya).

**2. Women's prevalence is high almost everywhere.** Pooling all rounds, model-based
prevalence among women is highest in Centre-Nord (79.6%), Nord (78.7%), Centre-Est (78.6%),
Hauts-Bassins (78.4%) and Plateau-Central (78.1%), and lowest in Centre-Ouest (44.6%), Est
(55.5%) and Centre (61.3%).

**3. Girls' prevalence is highest in the Sahel.** Among girls, prevalence is highest in Sahel
(30.3%), Nord (19.1%), Hauts-Bassins (18.3%) and Centre-Nord (13.6%), and lowest in Centre
(4.3%), Centre-Ouest (5.1%) and Est (5.2%).

**4. Prevalence has declined steeply across birth cohorts.** Among women, model-based
prevalence is 85–86% for those born in 1945–64, then 79.9% (1970–74), 71.1% (1980–84), 61.1%
(1990–94), 41.7% (2000–04) and 31.3% (2005–09, aged 15–16 when interviewed); the pooled
average is 69.6%. Among girls, it falls from 24.8% (born 1995–99) to 16.8% (2000–04),
10.0–10.5% (2005–14) and 5.5% (2015–19); girls born since 2015 are mostly younger than the
usual age of cutting.

**5. Composition.** Muslim women and girls have higher odds of being cut (1.69 and 2.05), as do
girls in traditionalist households (1.82). Secondary or higher education lowers the odds
(0.58 for women, 0.51 for girls). Senoufo (1.90) and Fulfulde/Peul (1.42) girls have higher
odds, and Lobi girls lower (0.45). Urban residence is associated with lower odds for girls
(0.63) but slightly higher odds for women (1.21). The survey-round terms and the "unknown"
wealth level (DHS 1998–99 and MICS 2006) are not separately identified, so their coefficients
are not interpreted; the prevalence estimates are unaffected.

**6. Validation.** The pooled model-based regional estimates agree with the pooled
design-based direct estimates (correlation 1.00; mean difference 0.05 points for women and
0.11 for girls).
"""

ATT = """## Results

**1. Women's support for continuing FGM has fallen steadily.** Among women who have heard of
FGM and answered "continue" or "stop", support for continuation fell from 24.3% in DHS
1998–99 to 18.7% (DHS 2003), 12.1% (MICS 2006), 9.4% (DHS 2010) and 8.7% (DHS 2021). Among men
it fell from 18.4% (1998–99) and 15.1% (2003) to 10.5% (2010), then rose to 12.7% (2021). DHS
1998–99 offered no "depends" answer.

**2. Men have overtaken women.** Men were less supportive than women in DHS 1998–99 (−5.9
points, p < 0.001) and DHS 2003 (−3.7 points, p = 0.002), did not differ in DHS 2010 (+1.2
points, p = 0.13), and were 4.0 points more supportive in DHS 2021 (p < 0.001).

**3. Where support is highest.** Pooling all rounds, women's support is highest in
Hauts-Bassins (22.5%), Sahel (20.5%), Nord (17.1%) and Cascades (15.1%), and lowest in Est
(6.2%), Centre (7.9%) and Centre-Sud (8.0%). Men's support is highest in Nord (20.7%),
Centre-Nord (15.5%) and Sahel (14.4%), and lowest in Centre-Ouest (7.2%) and Sud-Ouest (9.8%).
Men's and women's regional patterns are weakly correlated (0.33): men are more supportive in 8
of 13 regions, most in Centre (+5.6 points), Est (+5.5) and Centre-Sud (+5.4); women are more
supportive in Hauts-Bassins (+9.9), Sahel (+6.1), Cascades and Centre-Ouest (+3.9 each).

**4. Who supports continuation.** Uncut women have 85% lower odds of supporting continuation
(odds ratio 0.15). Support is higher among Muslim (1.70), traditionalist (2.26) and
other/no-religion (2.23) women and lower with secondary or higher education (0.33) and urban
residence (0.69). Among men, support is higher among Muslim (2.60), traditionalist (2.53) and
other/no-religion (4.84) men and lower with secondary or higher education (0.46).

**5. Birth cohort.** Among women, pooled support falls from about 18–21% (born 1950–59) to
13.8–14.2% (1965–84), 10.0% (1990–94) and 7.4–8.4% (2000–09). Among men it falls from 17.8%
(1950–54) to about 13% (1965–89) but does not fall further in the youngest cohorts (11.6–13.1%
for those born 1995–2009).

**6. Validation.** For both sexes, the pooled model-based regional estimates match the pooled
design-based direct estimates closely (correlation above 0.99; largest difference 0.3 points
for women and 0.6 points for men).
"""

CMP = """## Results

**1. Women's attitudes match where daughters are being cut.** Pooling all rounds, regional
support for continuation among women is strongly correlated with girls' prevalence (r = 0.81)
and less with women's own prevalence (r = 0.46). Hauts-Bassins and Sahel, where women's support
is highest, are among the three regions with the highest prevalence among girls.

**2. Men's attitudes are less tied to current practice.** Men's support is moderately
correlated with women's prevalence (r = 0.55) and with girls' prevalence (r = 0.48).

**3. Cluster maps.** Across 1,660 DHS clusters with women's attitudes and 1,654 with men's (DHS
1998–99 to 2021), clusters where most respondents support continuation (above 50%) are rare
(at most 4.5% of clusters in any region for women and 6.2% for men). Clusters where more than a
quarter of women support continuation are most common in Hauts-Bassins (30% of its clusters),
Sahel (26%), Boucle du Mouhoun (23%) and Nord (21%), and rarest in Sud-Ouest (2%) and Centre
(3%). For men, such clusters are most common in Nord (33%), Sahel (18%) and Plateau-Central
(18%), and rarest in Sud-Ouest (5%) and Centre-Ouest (6%).
"""

insert("FGM_Risk_Incidence.ipynb", "## Results", RISK, "## 17. Final outputs")
insert("FGM_Prevalence_Incidence.ipynb", "## Results", PREV, "## Notes")
insert("FGM_Attitudinal_Incidence.ipynb", "## Results", ATT, None)
insert("FGM_Prevalence_Attitude_Comparison.ipynb", "## Results", CMP, "## Interpretation")
