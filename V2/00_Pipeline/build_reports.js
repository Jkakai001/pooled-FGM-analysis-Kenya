const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell, ImageRun, AlignmentType,
  HeadingLevel, WidthType, BorderStyle, ShadingType, LevelFormat, Footer, PageNumber,
} = require("docx");

// Usage: node build_reports.js <reports folder> <figure folder (PNGs resized to 1800 px)>
const OUT = process.argv[2];
const FIG = process.argv[3] || path.join(__dirname, "fig");
fs.mkdirSync(OUT, { recursive: true });

const FONT = "Arial";
const BLUE = "1F3864";
const CONTENT_WIDTH = 9026; // A4 with 1-inch margins, in DXA

// ---------- helpers ----------
const p = (text, opts = {}) => new Paragraph({
  spacing: { after: 120, line: 276 },
  ...opts,
  children: (Array.isArray(text) ? text : [text]).map(t => typeof t === "string" ? new TextRun({ text: t, font: FONT, size: 22 }) : t),
});
const b = (text) => new TextRun({ text, bold: true, font: FONT, size: 22 });
const t = (text) => new TextRun({ text, font: FONT, size: 22 });
const h1 = (text) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 280, after: 140 }, children: [new TextRun({ text, font: FONT })] });
const h2 = (text) => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 200, after: 100 }, children: [new TextRun({ text, font: FONT })] });
const bullet = (runs) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80, line: 276 },
  children: (Array.isArray(runs) ? runs : [runs]).map(x => typeof x === "string" ? t(x) : x) });
const caption = (text) => new Paragraph({ spacing: { before: 60, after: 200 }, alignment: AlignmentType.LEFT,
  children: [new TextRun({ text, italics: true, font: FONT, size: 18, color: "555555" })] });

function figure(file, title, widthPx = 600) {
  const buf = fs.readFileSync(path.join(FIG, file));
  const w = buf.readUInt32BE(16), hgt = buf.readUInt32BE(20);
  return [
    new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 120, after: 40 },
      children: [new ImageRun({ type: "png", data: buf, transformation: { width: widthPx, height: Math.round(widthPx * hgt / w) },
        altText: { title, description: title, name: title } })] }),
    caption(title),
  ];
}

const border = { style: BorderStyle.SINGLE, size: 4, color: "BFBFBF" };
const borders = { top: border, bottom: border, left: border, right: border };
function table(headers, rows, widths, title, note) {
  const total = widths.reduce((a, c) => a + c, 0);
  const cell = (text, i, header, bold) => new TableCell({
    borders, width: { size: widths[i], type: WidthType.DXA },
    shading: header ? { fill: "D9E2F3", type: ShadingType.CLEAR, color: "auto" } : undefined,
    margins: { top: 50, bottom: 50, left: 90, right: 90 },
    children: [new Paragraph({ alignment: i === 0 ? AlignmentType.LEFT : AlignmentType.RIGHT,
      children: [new TextRun({ text: String(text), font: FONT, size: 18, bold: header || bold })] })],
  });
  const out = [];
  if (title) out.push(new Paragraph({ spacing: { before: 160, after: 60 }, keepNext: true,
    children: [new TextRun({ text: title, bold: true, font: FONT, size: 20, color: BLUE })] }));
  out.push(new Table({
    width: { size: total, type: WidthType.DXA }, columnWidths: widths,
    rows: [new TableRow({ tableHeader: true, children: headers.map((h, i) => cell(h, i, true)) }),
           ...rows.map(r => new TableRow({ children: r.map((v, i) => cell(v, i, false, r.bold)) }))],
  }));
  if (note) out.push(caption(note)); else out.push(new Paragraph({ spacing: { after: 120 }, children: [] }));
  return out;
}
const boldRow = (r) => Object.assign(r, { bold: true });

function titleBlock(title, subtitle) {
  return [
    new Paragraph({ spacing: { after: 60 }, children: [new TextRun({ text: title, bold: true, font: FONT, size: 40, color: BLUE })] }),
    new Paragraph({ spacing: { after: 80 }, children: [new TextRun({ text: subtitle, font: FONT, size: 24, color: "555555" })] }),
    new Paragraph({ spacing: { after: 240 }, border: { bottom: { style: BorderStyle.SINGLE, size: 8, color: BLUE, space: 4 } },
      children: [new TextRun({ text: "Kenya · DHS 1998–99 to 2021 and MICS 2006 · all survey rounds pooled", font: FONT, size: 20, color: "555555" })] }),
  ];
}

function doc(children, footerText) {
  return new Document({
    creator: "FGM analysis, Kenya (V2)",
    styles: {
      default: { document: { run: { font: FONT, size: 22 } } },
      paragraphStyles: [
        { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
          run: { size: 30, bold: true, font: FONT, color: BLUE }, paragraph: { spacing: { before: 280, after: 140 }, outlineLevel: 0 } },
        { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
          run: { size: 24, bold: true, font: FONT, color: BLUE }, paragraph: { spacing: { before: 200, after: 100 }, outlineLevel: 1 } },
      ],
    },
    numbering: { config: [{ reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT,
      style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] }] },
    sections: [{
      properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1440 } } },
      footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [
        new TextRun({ text: footerText + "  ·  page ", font: FONT, size: 16, color: "808080" }),
        new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 16, color: "808080" })] })] }) },
      children,
    }],
  });
}


// ======================================================================
// 1. RISK INCIDENCE
// ======================================================================
const risk = [
  ...titleBlock("FGM Risk Incidence, Kenya", "When girls are cut, how risk has changed across generations, and how sensitive the estimates are to reporting"),
  h1("1. Introduction"),
  p("Female genital mutilation (FGM) is almost always performed once, in childhood, and cannot be reversed. A woman's report of whether and at what age she was cut is therefore a record of what happened to her birth cohort, often decades before the survey. Risk incidence uses these reports to estimate the probability of being cut at each age, by birth cohort and by region."),
  p("This report pools every nationally representative Kenya survey that recorded age at cutting. Pooling gives larger samples and longer cohort coverage than any single survey, and observes each birth cohort several times, at different ages and with different questionnaires. That makes it possible to test how far the results depend on how and when women report; the sensitivity analysis is in section 4."),
  h1("2. Data"),
  p("Four Demographic and Health Surveys (DHS) record age at cutting: 1998–99, 2003, 2010 and 2021. The 2006 Multiple Indicator Cluster Survey (MICS) did not ask women their age at cutting and is used only for prevalence and attitudes. Two kinds of history are analysed:"),
  bullet([b("Women's own histories: "), t("women aged 15–49 in all four rounds, covering birth cohorts 1945–49 to 2005–09.")]),
  bullet([b("Daughters' histories: "), t("girls aged 0–14 reported by their mothers in DHS 2010 and 2021, the two rounds that list every daughter, covering birth cohorts 1995–99 to 2020–24.")]),
  p("Women who have never heard of FGM, and their daughters, are counted as not cut, following DHS convention. No cut case was dropped for lacking an exact age: answers of \"during infancy\" were spread over ages 0–4, and \"don't know\", missing or inconsistent ages over ages 0 to the current age, using the ages reported by women of the same survey and birth cohort who gave an exact year. DHS 1998–99 used a country-specific module, and its regions were reconstructed from province of residence."),
  ...table(
    ["Survey round", "Women's histories", "Women cut", "Exact age", "\"During infancy\"", "Age unknown", "Daughters' histories", "Daughters cut"],
    [["DHS 1998–99", "6,404", "4,684", "3,357", "–", "1,327", "–", "–"],
     ["DHS 2003", "12,475", "9,267", "4,612", "3,758", "897", "–", "–"],
     ["DHS 2010", "17,079", "12,897", "6,925", "5,917", "55", "17,152", "2,128"],
     ["DHS 2021", "17,659", "10,122", "10,122", "–", "0", "16,107", "1,646"],
     boldRow(["Total", "53,617", "36,970", "25,016", "9,675", "2,279", "33,259", "3,774"])],
    [1306, 1100, 1000, 1000, 1200, 1000, 1320, 1100],
    "Table 1. Records used for risk incidence, by survey round",
    "In total, 86,876 histories (40,744 cut). \"During infancy\" was an answer code only in DHS 2003 and 2010; DHS 1998–99 and 2021 recorded age 0 directly."),
  h1("3. Results"),
  h2("3.1 Cutting happens throughout early and middle childhood"),
  p("Pooling all cohorts, women's risk of being cut is 42.2% within ages 0–5, 37.5% within ages 5–10 among those still uncut at 5, and 13.0% within ages 10–15. In the discrete-time hazard model, the annual odds of being cut at ages 5–9 are 1.21 times those at ages 0–4; they fall to 0.61 times at 10–14 and 0.05 times at 15 and older. Figures 1 and 2 show the probability of remaining uncut by birth cohort; the fall continues well past age 5."),
  ...figure("Risk_01_Kaplan_Meier_KEN_kaplan_meier_self.png", "Figure 1. Probability of remaining FGM-free by age and birth cohort, women's own reports (all rounds pooled)"),
  ...figure("Risk_01_Kaplan_Meier_KEN_kaplan_meier_daughter.png", "Figure 2. Probability of remaining FGM-free by age and birth cohort, mothers' reports for daughters aged 0–14"),
  h2("3.2 Risk has fallen sharply for cohorts born since the late 1980s"),
  p("Women's own reports give a risk of being cut by age 5 of 48–49% for women born in 1965–79, then 41.1% for 1990–94, 30.8% for 1995–99, 23.0% for 2000–04 and 17.4% for 2005–09 (Figure 3). Risk by age 15 falls from 82.1% for women born in 1965–69 to 60.3% for 1990–94 and 37.0% for 2000–04. Mothers' reports for daughters give risk by age 5 of 20.7% for girls born in 1995–99, 14.7% for 2000–04, 10.6% for 2005–09, 10.5% for 2010–14 and 8.2% for 2015–19. In the hazard model, the odds of being cut peaked for the 1965–74 cohorts (1.44 times the 1945–49 cohort) and then fell to 0.66 (1995–99), 0.36 (2005–09) and 0.33–0.36 (2010–2019). No statistically distinct change point was detected."),
  ...figure("Risk_06_Trends_KEN_national_risk_trend_all_bands.png", "Figure 3. Risk of first FGM within each age band, by birth cohort and reporting source (pooled Kaplan–Meier)"),
  h2("3.3 Who is at risk"),
  ...table(["Factor (reference)", "Group", "Odds ratio", "95% CI"],
    [["Age (0–4)", "5–9", "1.21", "1.16–1.27"], ["", "10–14", "0.61", "0.57–0.65"], ["", "15+", "0.05", "0.04–0.06"],
     ["Residence (rural)", "Urban", "0.92", "0.87–0.98"],
     ["Ethnicity (Mossi)", "Fulfulde/Peul", "1.23", "1.09–1.39"], ["", "Senoufo", "1.05", "0.89–1.25"], ["", "Bissa", "0.96", "0.85–1.08"],
     ["", "Bobo", "0.84", "0.72–0.97"], ["", "Gourmantché", "0.77", "0.63–0.93"], ["", "Gourounsi", "0.77", "0.66–0.89"],
     ["", "Lobi", "0.73", "0.64–0.84"], ["", "Other", "0.77", "0.70–0.85"],
     ["Reporting source (daughter)", "Woman's own report", "2.01", "1.80–2.24"]],
    [2900, 2400, 1800, 1926], "Table 2. Survey-weighted discrete-time hazard model: annual odds of being cut",
    "Also adjusted for birth cohort and region. Relative to Boucle du Mouhoun, regional effects are highest in Hauts-Bassins (1.32) and Sud-Ouest (1.23), and lowest in Centre-Ouest (0.42), Centre-Sud (0.64), Est (0.70) and Centre (0.74). All histories are DHS, so no survey-programme term is needed."),
  p("Urban residence lowers the annual odds of cutting slightly. Risk is highest among Fulfulde/Peul and lowest among Lobi, Gourmantché and Gourounsi. Women's own reports carry twice the odds of mothers' reports for daughters, which is examined in section 4."),
  h2("3.4 Where recent cutting is concentrated"),
  p("Among women born in 2005–09, smoothed risk by age 5 is highest in Sahel (34.2%), Hauts-Bassins (28.9%), Centre-Est (26.0%) and Sud-Ouest (25.3%), and lowest in Centre-Ouest (7.3%) and Centre-Sud (11.6%) (Figure 4). Among daughters born in 2015–19, the Sahel stands out at 32.6%, followed by Hauts-Bassins (15.3%) and Nord (14.4%); risk is lowest in Centre (3.6%), Est (3.7%) and Centre-Ouest (3.8%) (Figure 5)."),
  ...figure("Risk_04_Risk_Maps_KEN_map_ages_0-5_self.png", "Figure 4. Smoothed risk of FGM by age 5 by region and birth cohort, women's own reports"),
  ...figure("Risk_04_Risk_Maps_KEN_map_ages_0-5_daughter.png", "Figure 5. Smoothed risk of FGM by age 5 by region and birth cohort, mothers' reports for daughters"),
  h2("3.5 Validation"),
  p("The pooled national estimates reproduce the survey-weighted Kaplan–Meier estimator exactly. The covariate-adjusted spatial model agrees with regional direct estimates for risk by age 5 (correlation 0.73 for women and 0.89 for daughters; mean difference under 2 percentage points)."),
  h1("4. Sensitivity analysis"),
  p("Risk incidence assumes that women recall, and are willing to report, whether and when they were cut. Because pooling observes each birth cohort several times, with different questionnaires and at different ages, we can test that assumption directly. All checks use the risk of being cut by age 5."),
  h2("4.1 The same cohort gives different risks depending on the questionnaire"),
  p("Women born in 1975–79 report a risk by age 5 of 37.7% in DHS 1998–99, 52.3% in DHS 2003, 56.4% in DHS 2010 and 39.5% in DHS 2021 (Table 3, Figure 6). Their childhoods did not change between surveys. The pattern follows the questionnaire: DHS 2003 and 2010 offered a \"during infancy\" answer, chosen by 41% and 46% of cut women and spread over ages 0–4, whereas DHS 1998–99 and 2021 had no such answer and women gave an exact age, often later in childhood. A within-cohort difference across rounds therefore reflects reporting, not risk. The shape of the trend, a rise to the cohorts born in the 1970s and a decline after the late 1980s, appears in every round."),
  ...table(["Birth cohort", "DHS 1998–99", "DHS 2003", "DHS 2010", "DHS 2021"],
    [["1960–64", "33.3", "43.8", "54.7", "–"], ["1965–69", "37.2", "46.4", "57.9", "–"], ["1970–74", "37.5", "47.9", "56.6", "40.3"],
     ["1975–79", "37.7", "52.3", "56.4", "39.5"], ["1980–84", "36.6", "50.5", "54.9", "37.4"], ["1985–89", "–", "42.2", "51.3", "36.4"],
     ["1990–94", "–", "–", "45.3", "35.7"], ["1995–99", "–", "–", "37.6", "29.5"]],
    [2226, 1700, 1700, 1700, 1700], "Table 3. Risk of FGM by age 5 (%), same birth cohort reported in successive surveys (women's own reports)"),
  ...figure("Risk_06_Trends_KEN_risk_0_5_by_round_and_cohort.png", "Figure 6. Risk of FGM by age 5 by birth cohort, estimated separately in each survey round"),
  h2("4.2 Holding age at interview fixed, the decline starts after the 1985–89 cohort"),
  p("Real change is best judged by comparing cohorts at the same age at interview. Using only women interviewed at ages 20–29, risk by age 5 is 52.8% for women born in 1980–84 and 51.3% for 1985–89, then falls to 39.8% (1990–94), 29.5% (1995–99) and 26.2% (2000–04) (Table 4, Figure 7). The recent decline is robust to age at interview."),
  h2("4.3 Mothers report less early cutting than young women report for themselves"),
  p("For girls born in 1995–2009, mothers' reports give a risk by age 5 of 10.6–20.7%, while the same cohorts reporting for themselves as young women give 17.4–30.8% (Table 4). Much cutting happens after age 5, and women's own reports in DHS 2010 place many cases \"during infancy\", spread over ages 0–4, while mothers give exact ages for their daughters. Both sources show the same steep decline across cohorts."),
  ...table(["Birth cohort", "Women, all ages at interview", "Women interviewed at 20–29 (95% CI)", "Mothers' reports for daughters (95% CI)"],
    [["1975–79", "48.4", "47.9 (45.3–50.4)", "–"], ["1980–84", "47.0", "52.8 (50.6–54.9)", "–"], ["1985–89", "44.3", "51.3 (48.6–53.8)", "–"],
     ["1990–94", "41.1", "39.8 (37.2–42.3)", "–"], ["1995–99", "30.8", "29.5 (27.3–31.6)", "20.7 (18.6–22.8)"],
     ["2000–04", "23.0", "26.2 (23.0–29.2)", "14.7 (13.0–16.4)"], ["2005–09", "17.4", "–", "10.6 (9.2–12.0)"],
     ["2010–14", "–", "–", "10.5 (9.0–12.0)"], ["2015–19", "–", "–", "8.2 (7.0–9.5)"]],
    [1700, 2200, 2563, 2563], "Table 4. Risk of FGM by age 5 (%) under alternative reporting assumptions"),
  ...figure("Risk_08_Sensitivity_Analysis_KEN_sensitivity_risk_by_age5.png", "Figure 7. Risk of FGM by age 5: all ages at interview, fixed age at interview (20–29), and mothers' reports"),
  h2("4.4 Further checks"),
  bullet([b("Age at interview within cohort. "), t("Age at interview is confounded with questionnaire round. For women born in 1985–89, risk by age 5 is 46.6% when reported at ages 15–24, 41.8% at 25–34 and 38.4% at 35–49.")]),
  bullet([b("DHS and MICS. "), t("A comparison is not possible, because MICS 2006 did not ask age at cutting.")]),
  bullet([b("Estimating vague and unknown ages. "), t("Using only women who gave an exact age would put risk by age 5 at 28.3% instead of 48.9% for women born in 1965–69, and 29.7% instead of 41.1% for 1990–94. For cohorts observed only in DHS 2021, where every age is exact, and for daughters (0.1–0.4 points), the difference is negligible.")]),
  h2("4.5 What the sensitivity analysis adds"),
  p("A single survey cannot separate real change from changes in how cutting is reported. Pooling shows that the level of early-age risk in Kenya depends strongly on whether the questionnaire offered a \"during infancy\" answer, so risk by age 5 should not be compared directly between DHS 2003–2010 and DHS 1998–99 or 2021. The decline itself is robust: it appears in every round, holds when age at interview is fixed, and is confirmed by mothers' reports for their daughters."),
  h1("5. Comparison with PMA Kenya 2024"),
  p("PMA Kenya Phase 4 (December 2023 – February 2024) asked women their age at cutting and whether any of their daughters had been cut, nationally and with larger samples in Centre and Hauts-Bassins. PMA, like DHS 2021, records cutting before the first birthday as age 0; DHS 2010 had a separate \"during infancy\" code. PMA's daughter question covers daughters of any age and records the youngest age at cutting, whereas DHS lists each daughter aged 0–14."),
  ...table(["Area", "Survey", "Cut women", "Under 1", "\"During infancy\"", "Ages 1–4", "Ages 5–9", "Ages 10+", "Don't know/missing", "Cut before 5 (known age)"],
    [["Kenya", "PMA 2024", "3,534", "10.7", "–", "22.8", "33.7", "16.7", "16.1", "39.9"],
     ["", "DHS 2021", "10,122", "8.1", "–", "36.6", "38.8", "16.4", "0.0", "44.7"],
     ["", "DHS 2010", "12,897", "0.8", "47.3", "12.3", "28.2", "10.9", "0.5", "60.8"],
     ["Centre", "PMA 2024", "689", "6.8", "–", "25.6", "35.6", "18.6", "13.4", "37.4"],
     ["", "DHS 2021", "1,001", "8.7", "–", "27.3", "45.8", "18.1", "0.0", "36.0"],
     ["Hauts-Bassins", "PMA 2024", "951", "35.7", "–", "19.7", "17.9", "8.3", "18.4", "67.8"],
     ["", "DHS 2021", "1,130", "14.8", "–", "45.1", "31.0", "9.0", "0.0", "59.9"]],
    [1150, 950, 800, 750, 950, 750, 750, 750, 1000, 1176], "Table 5. Reported age at cutting among cut women 15–49 (%)",
    "Weighted percentages. \"Cut before 5\" is among cut women with a known age, counting \"during infancy\" as under 5; 95% CIs are in the notebook."),
  ...figure("Risk_09_PMA_Comparison_KEN_pma_age_at_cutting_by_source.png", "Figure 8. Reported age at cutting among cut women, PMA 2024 compared with DHS 2021 and DHS 2010"),
  bullet([b("The age pattern is confirmed. "), t("Among cut women with a known age, PMA puts 39.9% of cuts before age 5 (95% CI 32.6–47.1) and DHS 2021 44.7%; both show that a third or more of cutting happens at ages 5–9. Centre agrees closely (37.4% and 36.0%), and both surveys show earlier cutting in Hauts-Bassins (67.8% and 59.9% before age 5).")]),
  bullet([b("Unknown ages. "), t("16% of cut women in PMA did not know or did not give their age at cutting, against none in DHS 2021, which also published no \"don't know\" category for that round. DHS 2021 ages should therefore be read as including some estimated or best-guess answers.")]),
  bullet([b("Questionnaire changes. "), t("DHS 2010's \"during infancy\" code captured 47% of cut women; without it, PMA and DHS 2021 place most early cuts at ages 1–4, confirming that the apparent shift to later ages between 2010 and 2021 partly reflects questionnaire design.")]),
  ...table(["Area", "Survey", "Daughters covered", "Mothers", "Any daughter cut (95% CI)", "Youngest cut at 1–4", "Youngest cut at 5+"],
    [["Kenya", "PMA 2024", "any age", "4,102", "20.9 (16.3–25.6)", "54.7", "34.5"], ["", "DHS 2021", "aged 0–14", "9,264", "11.3 (10.2–12.5)", "64.0", "26.7"],
     ["Centre", "PMA 2024", "any age", "810", "9.2 (5.2–13.2)", "50.4", "36.7"], ["", "DHS 2021", "aged 0–14", "913", "5.0 (3.0–7.1)", "47.6", "46.1"],
     ["Hauts-Bassins", "PMA 2024", "any age", "1,036", "25.6 (17.6–33.7)", "43.5", "17.8"], ["", "DHS 2021", "aged 0–14", "959", "16.6 (12.0–21.1)", "62.0", "15.6"]],
    [1300, 1100, 1300, 1000, 1900, 1213, 1213], "Table 6. Mothers who have heard of FGM and have daughters: any daughter cut, and the youngest daughter's age at cutting (%)",
    "PMA asks about daughters of any age; DHS lists daughters aged 0–14, so PMA figures are expected to be higher. The remaining share of mothers report the youngest cut before age 1 or an unknown age."),
  bullet([b("Mothers' reports. "), t("PMA mothers report a cut daughter about twice as often as DHS 2021 mothers (20.9% against 11.3% nationally), with the same pattern in Centre and Hauts-Bassins. Most of this reflects PMA's wider question, which includes adult daughters cut in earlier years. Both surveys agree that most mothers' youngest cut daughter was cut at ages 1–9.")]),
  h1("6. Conclusions"),
  bullet("FGM in Kenya is a practice of early and middle childhood, with substantial risk at ages 5–9; prevention must reach families throughout a girl's first decade."),
  bullet("Risk peaked among women born in the 1960s and 1970s and has fallen steeply for cohorts born since the late 1980s; mothers report a risk by age 5 of about 8% for daughters born in 2015–19, against 21% for those born in 1995–99."),
  bullet("The Sahel is a clear exception, with risk by age 5 near one in three among recent cohorts, followed by Hauts-Bassins and Nord."),
  bullet("Questionnaire design changes reported ages. Trends should be judged at a fixed age at interview and from mothers' reports, not by comparing early-age risk between rounds with different answer codes."),
  bullet("PMA 2024 confirms the age pattern, with a third or more of cutting at ages 5–9, and shows that DHS 2021 recorded no unknown ages while 16% of PMA women gave none."),
];

// ======================================================================
// 2. PREVALENCE
// ======================================================================
const prevalence = [
  ...titleBlock("FGM Prevalence, Kenya", "How common FGM is among women and girls, where, and how it has changed"),
  h1("1. Introduction"),
  p("Prevalence is the share of women and girls who have ever been cut (SDG indicator 5.3.2). This report estimates it nationally and by region, pooling every nationally representative Kenya survey with an FGM module, and describes change across survey years and birth cohorts. Regional estimates come from a survey-weighted model that combines all rounds, adjusts for ethnicity, wealth, education, religion, residence and age, and borrows strength between neighbouring regions."),
  h1("2. Data"),
  p("Five survey rounds are pooled. Women aged 15–49 come from all five; girls aged 0–14, reported by their mothers, come from the two rounds that list every daughter (DHS 2010 and 2021). Women who had never heard of FGM, and their daughters, are counted as not cut; women who did not know their own status (MICS 2006 only) are excluded. Wealth was not recorded in DHS 1998–99 or MICS 2006 and enters the model as an \"unknown\" category."),
  ...table(["Survey round", "Women 15–49", "Women cut", "Girls 0–14", "Girls cut"],
    [["DHS 1998–99", "6,404", "4,684", "–", "–"], ["DHS 2003", "12,475", "9,267", "–", "–"], ["MICS 2006", "7,208", "5,179", "–", "–"],
     ["DHS 2010", "17,079", "12,897", "17,152", "2,128"], ["DHS 2021", "17,659", "10,122", "16,107", "1,646"],
     boldRow(["Total", "60,825", "42,149", "33,259", "3,774"])],
    [2226, 1700, 1700, 1700, 1700], "Table 1. Women and girls with a known FGM status, by survey round"),
  h1("3. Results"),
  h2("3.1 National prevalence fell after 2010"),
  p("Among women aged 15–49, prevalence was 72.1% in DHS 1998–99, 76.6% in DHS 2003, 73.7% in MICS 2006 and 75.8% in DHS 2010, then fell to 56.1% in DHS 2021. Among girls aged 0–14, it fell from 13.4% in 2010 to 9.4% in 2021 (Figure 1). Girls' prevalence is not lifetime risk, because many girls have not yet passed the ages at which cutting occurs, up to about age 10 in Kenya."),
  ...figure("Prevalence_05_Trends_KEN_prevalence_national_by_round.png", "Figure 1. National FGM prevalence by survey year, design-based estimates with 95% CI"),
  h2("3.2 Women's prevalence is high in most regions"),
  p("Pooling all rounds, model-based prevalence among women is highest in Centre-Nord (79.6%), Nord (78.7%), Centre-Est (78.6%), Hauts-Bassins (78.4%) and Plateau-Central (78.1%), and lowest in Centre-Ouest (44.6%), Est (55.5%) and Centre (61.3%) (Figure 2)."),
  ...figure("Prevalence_03_Covariate_Adjusted_KEN_prevalence_covariate_adjusted_map_self.png", "Figure 2. Covariate-adjusted FGM prevalence among women 15–49 by region, all rounds pooled", 480),
  h2("3.3 Girls' prevalence is highest in the Sahel"),
  p("Among girls, prevalence is highest in Sahel (30.3%), Nord (19.1%), Hauts-Bassins (18.3%) and Centre-Nord (13.6%), and lowest in Centre (4.3%), Centre-Ouest (5.1%) and Est (5.2%) (Figure 3). The Sahel, mid-ranking for women, now has by far the highest prevalence among girls."),
  ...figure("Prevalence_03_Covariate_Adjusted_KEN_prevalence_covariate_adjusted_map_daughter.png", "Figure 3. Covariate-adjusted FGM prevalence among girls 0–14 by region, all rounds pooled", 480),
  h2("3.4 Prevalence has declined steeply across birth cohorts"),
  p("Among women, model-based prevalence is 85–86% for those born in 1945–64, then 79.9% (1970–74), 71.1% (1980–84), 61.1% (1990–94), 41.7% (2000–04) and 31.3% (2005–09, interviewed at ages 15–16); the pooled average is 69.6% (Figure 4). Among girls, it falls from 24.8% for those born in 1995–99 to 16.8% (2000–04), about 10% (2005–14) and 5.5% (2015–19); the average is 11.6%. Girls born since 2015 are mostly younger than the usual age of cutting. Figure 5 shows the decline region by region."),
  ...figure("Prevalence_05_Trends_KEN_prevalence_trend_by_cohort.png", "Figure 4. FGM prevalence by birth cohort, covariate-adjusted, all rounds pooled", 520),
  ...figure("Prevalence_04_Birth_Cohort_KEN_prevalence_covariate_adjusted_map_by_cohort_self.png", "Figure 5. FGM prevalence among women by region and birth cohort, all rounds pooled"),
  h2("3.5 Composition and validation"),
  p("Muslim women and girls have higher odds of being cut (1.69 and 2.05), as do girls in traditionalist households (1.82). Secondary or higher education lowers the odds (0.58 for women, 0.51 for girls). Among girls, Senoufo (1.90) and Fulfulde/Peul (1.42) have higher odds and Lobi lower (0.45); urban residence lowers girls' odds (0.63). The pooled model-based regional estimates agree with the design-based direct estimates (correlation 1.00; mean difference 0.05 percentage points for women and 0.11 for girls)."),
  h1("4. Comparison with PMA Kenya 2024"),
  p("Performance Monitoring for Action (PMA) Kenya Phase 4 interviewed 6,079 women aged 15–49 between December 2023 and February 2024, using the same core FGM questions as DHS. The sample is nationally representative; Centre (1,449 women) and Hauts-Bassins (1,532) were oversampled and have their own weights, so they are compared as separate areas. We harmonised PMA with the same definitions as DHS (never heard of FGM counted as not cut; non-responses excluded) and compared it with DHS 2021, the closest round, about three years earlier. PMA asks the FGM questions only in private; 37 women without privacy and 18 who gave no response were excluded. Before the comparison we checked that our DHS estimates reproduce the official DHS figures: they match within 0.1 percentage points for women's and girls' prevalence, awareness and attitudes, except DHS 1998–99 women's prevalence (72.1% against the official 71.6%), because we exclude women who did not know their status."),
  ...table(["Area", "Survey", "Prevalence, women 15–49 (95% CI)", "Cut, among women who have heard of FGM", "Women"],
    [["Kenya", "PMA 2024", "63.5 (57.8–69.3)", "69.1", "6,024"], ["", "DHS 2021", "56.1 (54.5–57.7)", "58.8", "17,659"],
     ["", "DHS 2010", "75.8 (74.1–77.5)", "76.1", "17,079"],
     ["Centre", "PMA 2024", "48.9 (44.8–53.0)", "50.3", "1,442"], ["", "DHS 2021", "47.1 (43.7–50.6)", "48.9", "2,102"],
     ["", "Pooled model, all rounds", "61.2 (60.3–62.3)", "–", "6,777"],
     ["Hauts-Bassins", "PMA 2024", "62.8 (57.1–68.4)", "66.1", "1,521"], ["", "DHS 2021", "58.8 (55.6–62.0)", "60.5", "1,917"],
     ["", "Pooled model, all rounds", "78.3 (77.2–79.2)", "–", "5,791"]],
    [1500, 2300, 2400, 1826, 1000], "Table 2. FGM prevalence among women 15–49, PMA 2024 compared with DHS and the pooled model (%)",
    "Survey estimates are direct survey-weighted (design-based) estimates. The pooled model averages all DHS and MICS rounds since 1998–99, when prevalence was higher, so it is not a current estimate."),
  ...figure("Prevalence_07_PMA_Comparison_KEN_pma_prevalence_women_by_source.png", "Figure 6. FGM prevalence among women 15–49, PMA 2024 compared with DHS 2021, DHS 2010 and the pooled model"),
  h2("4.1 The regions agree"),
  p("In Centre (PMA 48.9%, DHS 2021 47.1%; difference 1.8 points, p = 0.51) and Hauts-Bassins (62.8% and 58.8%; difference 4.0 points, p = 0.22), the two surveys agree within sampling error, at every age."),
  h2("4.2 Nationally, PMA is higher, mainly among young women"),
  p("Nationally, PMA finds 63.5% of women cut against 56.1% in DHS 2021, a difference of 7.4 points (95% CI 1.5–13.4; p = 0.015). The gap is concentrated among women aged 15–24 (PMA 53.3%, DHS 38.5%; difference 14.8 points, p < 0.001); at ages 25–34 it is smaller and not significant (5.0 points), and at 35–49 the surveys agree (74.2% and 76.4%). Unlike Kano in Nigeria, the gap is not about awareness: 92–95% of women in both surveys have heard of FGM, and prevalence has not swung with awareness across Kenya's surveys (Figure 7)."),
  p("Three explanations are possible. First, sample composition: PMA's national sample is less urban than DHS 2021 (22% against 33%), and FGM is more common in rural areas. Second, reporting: young women may be more willing to report cutting under PMA's privacy protocol, or less willing in DHS 2021; the risk incidence analysis shows that the same women report different early-age risks in different DHS rounds, depending on the questionnaire. Third, sampling: PMA's national estimate rests on 167 enumeration areas, so its interval is wide. Timing does not explain the gap: PMA's young women were born about three years later than those in DHS 2021, in cohorts with lower risk, so timing alone would make PMA's figure lower, not higher."),
  ...figure("EDA_KEN_pma2024_awareness_and_prevalence.png", "Figure 7. Awareness of FGM and FGM prevalence nationally and in Centre and Hauts-Bassins, every DHS, MICS and PMA survey"),
  h2("4.3 Implications"),
  bullet("Regional estimates for Centre and Hauts-Bassins are robust: two independent surveys agree."),
  bullet("The national decline between 2010 and 2021 is confirmed by PMA (75.8% in 2010, 56–64% in 2021–2024), but its size is uncertain: PMA suggests prevalence among young women is higher than DHS 2021 found, so the fall among young women may be overstated."),
  bullet("Girls' prevalence cannot be compared, because PMA does not list daughters aged 0–14; mothers' reports are compared in the risk incidence report."),
  h1("5. Conclusions"),
  bullet("About three in four women aged 15–49 were cut in every survey until 2010; prevalence fell to 56% in 2021, driven by younger cohorts."),
  bullet("Prevalence among women has fallen from about 85% for those born before 1965 to about 40% for those born in 2000–04."),
  bullet("Among girls, prevalence fell from 13% to 9% between 2010 and 2021 and is now highest in the Sahel, Nord and Hauts-Bassins."),
  bullet("Because cutting continues up to about age 10, girls' prevalence understates their eventual lifetime prevalence; cohort risk estimates are the better guide to current practice."),
  bullet("PMA 2024 confirms DHS 2021 in Centre and Hauts-Bassins but finds higher national prevalence (63.5% against 56.1%), mainly among women aged 15–24, so the recent decline among young women may be overstated."),
];

// ======================================================================
// 3. ATTITUDES
// ======================================================================
const attitudes = [
  ...titleBlock("Attitudes to FGM, Kenya", "Support for continuing FGM among women and men, and how it relates to the practice"),
  h1("1. Introduction"),
  p("Social norms sustain FGM, so support for its continuation is a leading indicator of whether the practice will persist. This report estimates the share of women and men who think FGM should continue, nationally, by region and by birth cohort, and compares it with where FGM is practised. Estimates pool all surveys that asked the question, using a survey-weighted model that adjusts for respondent characteristics and borrows strength between neighbouring regions."),
  h1("2. Data"),
  p("Respondents who had heard of FGM were asked whether it should continue or stop; those answering \"depends\" or \"don't know\" are excluded (DHS 1998–99 offered no \"depends\" answer). Women were asked in all five survey rounds; men aged 15–49 in the four DHS rounds (MICS 2006 did not ask men about FGM)."),
  ...table(["Survey round", "Women answering", "Women: support continuation (%)", "Men answering", "Men: support continuation (%)"],
    [["DHS 1998–99", "4,933", "24.3", "1,868", "18.4"], ["DHS 2003", "11,050", "18.7", "2,685", "15.1"], ["MICS 2006", "6,368", "12.1", "–", "–"],
     ["DHS 2010", "16,882", "9.4", "6,217", "10.5"], ["DHS 2021", "16,098", "8.7", "6,133", "12.7"],
     boldRow(["Total", "55,331", "", "16,903", ""])],
    [1826, 1700, 1900, 1700, 1900], "Table 1. Respondents and support for continuing FGM by survey round (design-based)"),
  h1("3. Results"),
  h2("3.1 Women's support has fallen steadily; men's has not"),
  p("Among women, support for continuation fell from 24.3% in DHS 1998–99 to 18.7% (2003), 12.1% (MICS 2006), 9.4% (2010) and 8.7% (2021). Among men it fell from 18.4% (1998–99) and 15.1% (2003) to 10.5% (2010), then rose to 12.7% in 2021 (Figure 1). Men were less supportive than women in 1998–99 (by 5.9 points) and 2003 (by 3.7 points), did not differ significantly in 2010, and were 4.0 points more supportive in 2021, all differences except 2010 being statistically significant (Figure 2)."),
  ...figure("Attitude_Women_and_Men_KEN_attitude_trend_bars_women_men.png", "Figure 1. Support for continuing FGM by survey round, women and men (95% CI)"),
  ...figure("Attitude_Women_and_Men_KEN_attitude_women_vs_men_dhs_bars.png", "Figure 2. Women and men in the same DHS rounds, with the design-based difference test", 520),
  h2("3.2 Where support is highest"),
  p("Pooling all rounds, women's support is highest in Hauts-Bassins (22.5%), Sahel (20.5%), Nord (17.1%) and Cascades (15.1%), and lowest in Est (6.2%), Centre (7.9%) and Centre-Sud (8.0%). Men's support is highest in Nord (20.7%), Centre-Nord (15.5%) and Sahel (14.4%), and lowest in Centre-Ouest (7.2%) and Sud-Ouest (9.8%). Men's and women's regional patterns are only weakly correlated (0.33): men are more supportive in 8 of 13 regions, most in Centre, Est and Centre-Sud (about 5.5 points), while women are more supportive in Hauts-Bassins (+9.9 points) and Sahel (+6.1) (Figure 3)."),
  ...figure("Attitude_Men_07_Men_vs_Women_KEN_attitude_map_men_vs_women.png", "Figure 3. Covariate-adjusted support for continuing FGM by region, women and men, all rounds pooled"),
  h2("3.3 Who supports continuation"),
  ...table(["Characteristic (reference)", "Group", "Women: odds ratio", "Men: odds ratio"],
    [["Own FGM status (cut)", "Not cut", "0.15", "–"], ["Religion (Christian)", "Islam", "1.70", "2.60"], ["", "Traditional", "2.26", "2.53"],
     ["", "Other / none", "2.23", "4.84"], ["Education (none)", "Primary", "0.88", "0.88"], ["", "Secondary or higher", "0.33", "0.46"],
     ["Wealth (middle)", "Poorest", "0.99", "1.01"], ["", "Richest", "0.88", "1.01"], ["Residence (rural)", "Urban", "0.69", "0.87"]],
    [2700, 2300, 2013, 2013], "Table 2. Survey-weighted covariate-adjusted model: odds of supporting continuation",
    "Also adjusted for ethnicity, age, survey round and region."),
  p("Women who have themselves been cut are far more likely to support continuation; uncut women have 85% lower odds. Support is higher among Muslim, traditionalist and non-religious respondents, and much lower with secondary or higher education. Among women, support has fallen across birth cohorts, from about 14% for those born in 1965–84 to 7–8% for those born after 2000; among men it has not fallen in the youngest cohorts (12–13%)."),
  h2("3.4 Attitudes and practice"),
  p("At regional level, women's support for continuation tracks girls' prevalence closely (correlation 0.81) and women's own prevalence less (0.46): women support FGM where their daughters are being cut now, notably in Hauts-Bassins and the Sahel. Men's support is moderately correlated with both (0.55 and 0.48). Across 1,660 survey clusters, clusters where more than a quarter of women support continuation are most common in Hauts-Bassins (30% of clusters), Sahel (26%), Boucle du Mouhoun (23%) and Nord (21%) (Figures 4 and 5)."),
  ...figure("Prevalence_Attitude_Comparison_01_Region_Level_KEN_scatter_prevalence_vs_attitude.png", "Figure 4. Regional FGM prevalence against support for continuation, women and men, all rounds pooled"),
  ...figure("Prevalence_Attitude_Comparison_02_Cluster_Level_KEN_combined_prevalence_attitude_overlay_women.png", "Figure 5. FGM prevalence by region and women's support for continuation by survey cluster (DHS 1998–99 to 2021)"),
  h1("4. Comparison with PMA Kenya 2024"),
  p("PMA Kenya Phase 4 (December 2023 – February 2024) asked women who had heard of FGM whether it should continue or stop, in the same form as DHS, and added questions on social norms. PMA has no men's interview, so the comparison covers women only."),
  ...table(["Area", "Survey", "Support for continuation (95% CI)", "\"Depends\"", "Women answering"],
    [["Kenya", "PMA 2024", "20.8 (15.9–25.8)", "4.3", "5,550"], ["", "DHS 2021", "8.7 (7.8–9.5)", "2.5", "16,098"],
     ["", "DHS 2010", "9.4 (8.3–10.4)", "0.5", "16,882"],
     ["Centre", "PMA 2024", "10.7 (8.3–13.1)", "2.8", "1,358"], ["", "DHS 2021", "4.9 (3.8–6.1)", "3.4", "1,932"],
     ["", "Pooled model, all rounds", "7.9 (7.4–8.4)", "–", "–"],
     ["Hauts-Bassins", "PMA 2024", "23.5 (16.6–30.4)", "2.7", "1,426"], ["", "DHS 2021", "19.0 (15.3–22.7)", "3.3", "1,733"],
     ["", "Pooled model, all rounds", "22.5 (21.5–23.4)", "–", "–"]],
    [1500, 2300, 2700, 1200, 1326], "Table 3. Women's support for continuing FGM, PMA 2024 compared with DHS and the pooled model (%)",
    "Women 15–49 who have heard of FGM; support = \"continued\" among \"continued\" or \"stopped\". \"Depends\" is the share of all answers and is excluded from support."),
  ...figure("Attitude_Women_and_Men_PMA_Comparison_KEN_pma_support_by_source.png", "Figure 6. Women's support for continuing FGM, PMA 2024 compared with DHS 2021, DHS 2010 and the pooled model"),
  p("PMA finds more than twice as much support for continuation as DHS 2021: 20.8% against 8.7% nationally (difference 12.1 points, 95% CI 7.1–17.2; p < 0.001) and 10.7% against 4.9% in Centre (p < 0.001). In Hauts-Bassins, where support is highest in both surveys, the difference (23.5% against 19.0%) is not significant. The higher PMA figure may reflect its privacy protocol and the absence of a \"don't know\" option, the higher prevalence PMA finds (cut women are more supportive), or a real rise in support since 2021; the data cannot separate these. Support may therefore be higher than the DHS series alone suggests."),
  p("PMA's norms questions add context. Nationally, 22.5% of women who have heard of FGM say their community expects FGM to continue, and 16.3% of women without a cut daughter would consider cutting one. Most expect fewer girls to be cut in five years (81.3%; 7.3% expect more). In Centre these figures are lower (14.4% and 7.3%), while in Hauts-Bassins they are close to the national level (23.6% and 16.1%)."),
  h1("5. Conclusions"),
  bullet("Support for continuing FGM among women has fallen from about one in four in 1998–99 to under one in ten since 2010."),
  bullet("Men's support stopped falling after 2010 and men are now more supportive than women, so programmes need to engage men explicitly."),
  bullet("Women's support is concentrated in Hauts-Bassins and the Sahel, where girls' prevalence is highest, so attitudes there are a direct risk to girls."),
  bullet("Cut women, Muslim and traditionalist respondents and those with little education are most supportive, which identifies the groups that norm-change programmes should reach."),
  bullet("PMA 2024 finds support for continuation more than twice as high as DHS 2021 nationally (21% against 9%), so support may be higher than the DHS series suggests; about one woman in five says her community expects FGM to continue."),
];

(async () => {
  const reports = [
    [risk, "KEN_FGM_Risk_Incidence_Report.docx", "FGM Risk Incidence, Kenya"],
    [prevalence, "KEN_FGM_Prevalence_Report.docx", "FGM Prevalence, Kenya"],
    [attitudes, "KEN_FGM_Attitudes_Report.docx", "Attitudes to FGM, Kenya"],
  ];
  for (const [children, name, footer] of reports) {
    const buf = await Packer.toBuffer(doc(children, footer));
    fs.writeFileSync(path.join(OUT, name), buf);
    console.log("wrote", name, Math.round(buf.length / 1024), "KB");
  }
})();
