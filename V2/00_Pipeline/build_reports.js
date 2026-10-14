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
      children: [new TextRun({ text: "Kenya · DHS 1998, 2003, 2008–09, 2014 and 2022 · all rounds pooled", font: FONT, size: 20, color: "555555" })] }),
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
  ...titleBlock("FGM Risk Incidence, Kenya", "At what age girls are cut, how risk has changed across generations, and how robust the estimates are to reporting"),
  h1("1. Introduction"),
  p("Female genital mutilation (FGM) is performed once and cannot be reversed, so a woman's report of whether and at what age she was cut is a record of what happened to her birth cohort, often decades before the survey. Risk incidence uses these reports to estimate the probability of being cut at each age, by birth cohort and by area."),
  p("This report pools every Kenya Demographic and Health Survey (DHS) that recorded age at cutting. Pooling gives longer cohort coverage than any single survey and observes each cohort several times, which allows the sensitivity of the results to reporting to be tested (section 4). Unlike Nigeria and Burkina Faso, most cutting in Kenya happens at ages 5–15, so the key indicator is the risk of being cut by age 15."),
  h1("2. Data"),
  p("Four DHS rounds record women's age at cutting: 1998, 2008–09, 2014 and 2022. DHS 2003 asked only whether a woman had been cut, not when, and is used for prevalence only. Kenya has no national MICS; its five subnational MICS are compared in section 5. Two kinds of history are analysed:"),
  bullet([b("Women's own histories: "), t("women aged 15–49 in the four rounds, covering birth cohorts 1945–49 to 2005–09.")]),
  bullet([b("Daughters' histories: "), t("girls aged 0–14 reported by their mothers in DHS 2014 and 2022, the two rounds that list every daughter, covering birth cohorts 1995–99 to 2020–24.")]),
  p("Women who have never heard of FGM, and their daughters, are counted as not cut (DHS convention). In DHS 2014 and 2022 the FGM questions were asked only in the long-questionnaire subsample (about half of women). No cut case was dropped for lacking an exact age: \"during infancy\" answers were spread over ages 0–4 and \"don't know\", missing or inconsistent ages over ages 0 to the current age, using women of the same survey and birth cohort who gave an exact age. Our estimates reproduce the official DHS national figures for every round."),
  ...table(["Survey round", "Women's histories", "Women cut", "Exact age", "\"During infancy\"", "Age unknown", "Daughters' histories", "Daughters cut"],
    [["DHS 1998", "7,873", "2,748", "2,650", "–", "98", "–", "–"],
     ["DHS 2008–09", "8,433", "2,541", "2,391", "49", "101", "–", "–"],
     ["DHS 2014", "14,734", "4,377", "4,249", "66", "62", "13,189", "739"],
     ["DHS 2022", "16,902", "4,036", "3,924", "30", "82", "13,525", "689"],
     boldRow(["Total", "47,942", "13,702", "13,214", "145", "343", "26,714", "1,428"])],
    [1306, 1100, 1000, 1000, 1200, 1000, 1320, 1100],
    "Table 1. Records used for risk incidence, by survey round",
    "In total, 74,656 histories (15,130 cut). DHS 1998 had no \"during infancy\" code. Daughters include those of mothers who never heard of FGM (counted as not cut). DHS 2003 (8,174 women, 2,724 cut) did not ask age at cutting."),
  h1("3. Results"),
  h2("3.1 Most girls are cut at ages 5–15"),
  p("Only 2–4% of cut women report being cut before age 5; 19–30% report ages 5–9, 42–45% ages 10–14 and 22–32% age 15 or older. In the discrete-time hazard model the annual odds of being cut, relative to ages 0–4, are 13 times higher at ages 5–9, 52 times at 10–14 and 19 times at 15 and older. Figures 1 and 2 show the probability of remaining uncut by birth cohort; the fall is concentrated between ages 5 and 16."),
  ...figure("Risk_01_Kaplan_Meier_KEN_kaplan_meier_self.png", "Figure 1. Probability of remaining FGM-free by age and birth cohort, women's own reports (all rounds pooled)"),
  ...figure("Risk_01_Kaplan_Meier_KEN_kaplan_meier_daughter.png", "Figure 2. Probability of remaining FGM-free by age and birth cohort, mothers' reports for daughters aged 0–14"),
  h2("3.2 Risk has fallen steadily for four decades"),
  p("Women's risk of being cut by age 15 is 37–42% for women born in 1945–64, then falls to 26.4% (1970–74), 19.7% (1980–84), 15.9% (1985–89), 11.3% (1995–99) and 8.5% (2005–09) (Figure 3). In the hazard model, the odds of being cut fall from 0.68 (1965–69, relative to 1945–49) to 0.30 (1985–89), 0.16 (2000–04) and 0.09 (2010–14); no single change point was detected. Mothers report that 3–7% of daughters born since 1995 were cut by age 10, and 3.1% of those born in 2010–14."),
  ...figure("Risk_06_Trends_KEN_national_risk_trend_all_bands.png", "Figure 3. Risk of first FGM within each age band, by birth cohort and reporting source (pooled Kaplan–Meier)"),
  h2("3.3 Who is at risk"),
  ...table(["Factor (reference)", "Group", "Odds ratio", "95% CI"],
    [["Age (0–4)", "5–9", "13.49", "11.49–15.84"], ["", "10–14", "51.68", "43.05–62.04"], ["", "15+", "18.97", "15.47–23.27"],
     ["Residence (rural)", "Urban", "0.57", "0.52–0.63"],
     ["Ethnicity (Kikuyu)", "Somali", "20.33", "13.89–29.76"], ["", "Kisii", "13.88", "10.97–17.57"], ["", "Maasai", "7.98", "6.74–9.46"],
     ["", "Kalenjin", "2.18", "1.85–2.57"], ["", "Meru/Embu", "1.04", "0.83–1.30"], ["", "Kamba", "0.50", "0.39–0.64"],
     ["", "Mijikenda/Swahili", "0.22", "0.13–0.37"], ["", "Luhya", "0.06", "0.04–0.09"], ["", "Luo", "0.01", "0.01–0.03"],
     ["Reporting source (daughter)", "Woman's own report", "1.06", "0.90–1.24"]],
    [2900, 2400, 1800, 1926], "Table 2. Survey-weighted discrete-time hazard model: annual odds of being cut",
    "All odds ratios are adjusted for age group, birth cohort, residence, ethnicity, province and reporting source. Province effects, relative to Central, are highest in North Eastern (4.12), Eastern (2.00) and Nyanza (1.54) and lowest in Western (0.17). The 2020–24 cohort has no reported cases and is not interpreted."),
  p("Ethnicity dominates: risk is highest among Somali, Kisii and Maasai girls and very low among Luo, Luhya and Mijikenda/Swahili girls. Urban residence lowers the annual odds by 43%. Women's own reports and mothers' reports give the same odds once other factors are held constant."),
  h2("3.4 North Eastern stands apart"),
  p("Among women born in 2005–09, smoothed risk is 72% at ages 5–10 and a further 22% at ages 10–15 in North Eastern; in every other province the risk at ages 5–15 is under 15% (Figures 4 and 5). Among daughters born in 2010–14, risk at ages 5–10 is 65% in North Eastern and under 5% elsewhere."),
  ...figure("Risk_04_Risk_Maps_KEN_map_ages_5-10_self.png", "Figure 4. Smoothed risk of FGM at ages 5–10 by province and birth cohort, women's own reports"),
  ...figure("Risk_04_Risk_Maps_KEN_map_ages_10-15_self.png", "Figure 5. Smoothed risk of FGM at ages 10–15 by province and birth cohort, women's own reports"),
  h2("3.5 Validation"),
  p("The pooled national estimates reproduce the survey-weighted Kaplan–Meier estimator exactly. The covariate-adjusted spatial model agrees with province direct estimates (correlation 0.87 for women and 0.96 for daughters)."),
  h1("4. Sensitivity analysis"),
  p("Risk incidence assumes that women recall, and are willing to report, whether and when they were cut. Because pooling observes each birth cohort several times, we can test that assumption. Checks on women's own reports use the risk of being cut by age 15; the comparison with mothers' reports uses risk by age 10, because daughters are observed only to age 14."),
  h2("4.1 The same cohort gives similar answers in different surveys"),
  p("Women born in 1975–79 report a risk by age 15 of 23.3% in DHS 1998, 23.4% in DHS 2008–09, 22.1% in DHS 2014 and 18.4% in DHS 2022; women born in 1965–69 report 30.8%, 31.6% and 31.5% (Table 3, Figure 6). Unlike Burkina Faso, reporting is stable up to DHS 2014. DHS 2022 gives lower figures for cohorts born before 1985 (17.8% against 24.6–30.5% for women born in 1970–74), consistent with growing reluctance to report since FGM was criminalised in 2011."),
  ...table(["Birth cohort", "DHS 1998", "DHS 2008–09", "DHS 2014", "DHS 2022"],
    [["1955–59", "40.2", "36.9", "–", "–"], ["1960–64", "38.0", "35.8", "36.1", "–"], ["1965–69", "30.8", "31.6", "31.5", "–"],
     ["1970–74", "30.5", "28.0", "24.6", "17.8"], ["1975–79", "23.3", "23.4", "22.1", "18.4"], ["1980–84", "24.0", "21.5", "18.5", "16.7"],
     ["1985–89", "–", "16.1", "15.7", "16.1"], ["1990–94", "–", "14.1", "12.5", "11.2"], ["1995–99", "–", "–", "11.4", "11.1"],
     ["2000–04", "–", "–", "–", "8.9"], ["2005–09", "–", "–", "–", "8.5"]],
    [2226, 1700, 1700, 1700, 1700], "Table 3. Risk of FGM by age 15 (%), same birth cohort reported in successive surveys (women's own reports)"),
  ...figure("Risk_06_Trends_KEN_risk_by_round_and_cohort.png", "Figure 6. Cumulative risk of FGM by birth cohort, estimated separately in each survey round (women by age 15, girls by age 10)"),
  h2("4.2 Holding age at interview fixed, the decline is unchanged"),
  p("Using only women interviewed at ages 20–29, risk by age 15 falls from 30.5% (born 1970–74) to 20.6% (1980–84), 11.8% (1990–94) and 9.5% (2000–04), the same path as the all-ages series (Table 4, Figure 7). The decline is robust to age at interview."),
  h2("4.3 Women's own reports and mothers' reports agree"),
  p("For girls born in 2000–04, risk by age 10 is 5.9% from women's own reports and 6.6% from mothers' reports; for 2005–09 it is 7.2% and 3.9%. Both sources show low and falling risk among recent cohorts (Table 4). Because Kenyan girls are cut older, most remember and report their own cutting."),
  ...table(["Birth cohort", "Women, all ages: by 15", "Women aged 20–29: by 15 (95% CI)", "Own reports: by 10 (95% CI)", "Mothers' reports: by 10 (95% CI)"],
    [["1965–69", "31.2", "31.3 (26.0–36.2)", "11.0 (9.6–12.3)", "–"], ["1970–74", "26.4", "30.5 (27.4–33.4)", "10.9 (9.8–11.9)", "–"],
     ["1975–79", "21.7", "22.6 (19.4–25.8)", "10.8 (9.6–12.0)", "–"], ["1980–84", "19.7", "20.6 (16.4–24.6)", "11.0 (9.9–12.1)", "–"],
     ["1985–89", "15.9", "16.2 (14.2–18.1)", "9.2 (8.0–10.4)", "–"], ["1990–94", "12.3", "11.8 (10.5–13.2)", "7.2 (6.3–8.2)", "–"],
     ["1995–99", "11.3", "11.1 (9.6–12.7)", "7.8 (6.9–8.7)", "4.2 (1.5–6.9)"], ["2000–04", "8.9", "9.5 (7.8–11.2)", "5.9 (5.0–6.9)", "6.6 (5.4–7.8)"],
     ["2005–09", "8.5", "–", "7.2 (5.7–8.7)", "3.9 (3.3–4.5)"], ["2010–14", "–", "–", "–", "3.1 (2.7–3.6)"]],
    [1300, 1700, 2200, 1913, 1913], "Table 4. Risk of FGM (%) under alternative reporting assumptions"),
  ...figure("Risk_08_Sensitivity_Analysis_KEN_sensitivity_risk.png", "Figure 7. Risk of FGM by age 15 (all ages vs fixed age at interview) and by age 10 (own vs mothers' reports)"),
  h2("4.4 Further checks"),
  bullet([b("Age at interview within cohort. "), t("No consistent pattern: women born in 1985–89 report 15.5% at ages 15–24, 15.7% at 25–34 and 17.2% at 35–49.")]),
  bullet([b("DHS and MICS. "), t("Not possible nationally, as Kenya has no national MICS; the subnational MICS agree with DHS for the same areas (section 5 and the prevalence report).")]),
  bullet([b("Estimating vague and unknown ages. "), t("Using only exact ages changes risk by age 15 by under 3 points for every cohort, because almost all Kenyan women give an exact age.")]),
  h2("4.5 What the sensitivity analysis adds"),
  p("In Kenya the results are robust to how and when women report: the same cohorts give similar answers across rounds, the decline holds at a fixed age at interview, own and mothers' reports agree, and the handling of vague ages makes almost no difference. The one sign of under-reporting is DHS 2022, where older cohorts report less cutting than in earlier rounds, which may reflect the 2011 criminalisation of FGM."),
  h1("5. Comparison with Kenya's subnational MICS"),
  p("MICS 2013–14 asked women their age at cutting in Bungoma, Kakamega and Turkana counties. FGM is rare there (1–4% of women), so only 38, 11 and 31 women were cut in the MICS samples, and 6, 3 and 13 in DHS 2014. In Bungoma most MICS respondents report cutting at 15 or older (60%), in line with initiation-age cutting in the area, while Turkana reports mostly ages 5–14; the DHS samples are too small for a meaningful comparison. Nationally, DHS 2014 and 2022 agree closely on the age pattern (2% before age 5, 27–30% at 5–9, 43–45% at 10–14)."),
  h1("6. Conclusions"),
  bullet("FGM in Kenya is mostly performed at ages 5–15, so prevention must reach girls through primary-school age and early adolescence."),
  bullet("Risk of being cut by age 15 has fallen from about 40% for women born before 1965 to under 10% for those born since 2000, and mothers report around 3% of girls born in 2010–14 cut by age 10."),
  bullet("Risk remains extremely high in North Eastern province, where most girls born in 2005–09 were cut by age 15, and among Somali, Kisii and Maasai communities."),
  bullet("The estimates are robust to reporting, but DHS 2022 suggests some under-reporting by older women since the 2011 law."),
];

// ======================================================================
// 2. PREVALENCE
// ======================================================================
const prevalence = [
  ...titleBlock("FGM Prevalence, Kenya", "How common FGM is among women and girls, where, and how it has changed"),
  h1("1. Introduction"),
  p("Prevalence is the share of women and girls who have ever been cut (SDG indicator 5.3.2). This report estimates it nationally, for the 8 former provinces (every round) and the 47 counties (DHS 2014 and 2022), pooling every Kenya DHS round with an FGM module, and describes change across survey years and birth cohorts. Province estimates come from a survey-weighted model that combines all rounds, adjusts for ethnicity, wealth, education, religion, residence and age, and borrows strength between neighbouring provinces."),
  h1("2. Data"),
  p("Five DHS rounds are pooled. Women aged 15–49 come from all five; girls aged 0–14, reported by their mothers, come from DHS 2014 and 2022. Women who had never heard of FGM, and their daughters, are counted as not cut. In DHS 2014 and 2022 the FGM questions were asked only in the long-questionnaire subsample. Kenya has no national MICS; its five subnational MICS are compared in section 4. Our estimates reproduce the official DHS national figures for every round (women's prevalence, awareness, girls' prevalence and attitudes, all within 0.1 points)."),
  ...table(["Survey round", "Women 15–49", "Women cut", "Girls 0–14", "Girls cut"],
    [["DHS 1998", "7,873", "2,748", "–", "–"], ["DHS 2003", "8,174", "2,724", "–", "–"], ["DHS 2008–09", "8,433", "2,541", "–", "–"],
     ["DHS 2014", "14,734", "4,377", "13,189", "739"], ["DHS 2022", "16,902", "4,036", "13,525", "689"],
     boldRow(["Total", "56,116", "16,426", "26,714", "1,428"])],
    [2226, 1700, 1700, 1700, 1700], "Table 1. Women and girls with a known FGM status, by survey round",
    "Girls include the daughters of mothers who had never heard of FGM, counted as not cut. DHS 1998 did not sample North Eastern province."),
  h1("3. Results"),
  h2("3.1 National prevalence has more than halved"),
  p("Among women aged 15–49, prevalence fell from 37.7% in DHS 1998 to 32.2% (2003), 27.1% (2008–09), 21.0% (2014) and 14.8% (2022). Among girls aged 0–14 it was 2.9% in 2014 and 1.6% in 2022 (Figure 1). Girls' prevalence is far below lifetime risk, because most Kenyan girls are cut at ages 5–15 and many have not yet reached those ages."),
  ...figure("Prevalence_05_Trends_KEN_prevalence_national_by_round.png", "Figure 1. National FGM prevalence by survey year, design-based estimates with 95% CI"),
  h2("3.2 North Eastern stands apart"),
  p("Pooling all rounds, model-based prevalence among women is 95.0% in North Eastern, then 34.7% in Nyanza, 30.5% in Rift Valley, 29.3% in Eastern and 21.7% in Central, and lowest in Coast (11.8%), Nairobi (10.9%) and Western (1.7%) (Figure 2). Among girls it is 35.4% in North Eastern and 4.0% in Nyanza, and under 2% in every other province (Figure 3)."),
  ...figure("Prevalence_03_Covariate_Adjusted_KEN_prevalence_covariate_adjusted_map_self.png", "Figure 2. Covariate-adjusted FGM prevalence among women 15–49 by province, all rounds pooled", 480),
  ...figure("Prevalence_03_Covariate_Adjusted_KEN_prevalence_covariate_adjusted_map_daughter.png", "Figure 3. Covariate-adjusted FGM prevalence among girls 0–14 by province, all rounds pooled", 480),
  h2("3.3 Counties"),
  p("In DHS 2022, prevalence among women is highest in Wajir (96.3%), Mandera (95.9%), Marsabit (82.9%), Garissa (80.4%), Kisii (76.6%), Nyamira (74.1%), Samburu (73.3%) and Isiolo (65.6%); ten counties exceed 50% and two are below 1% (Figure 4). Among girls it is highest in Wajir (42.4%), Mandera (27.4%), Garissa (23.9%), Isiolo (12.8%) and Tana River (11.3%), and under 1% in 14 counties (Figure 5). Between 2014 and 2022, girls' prevalence fell sharply in Kisii (18.5% to 4.2%), Nyamira (19.8% to 5.3%) and Mandera (50.3% to 27.4%)."),
  ...figure("Prevalence_08_County_Level_KEN_county_prevalence_women.png", "Figure 4. FGM prevalence among women 15–49 by county, DHS 2014 and 2022 (BYM2-smoothed)"),
  ...figure("Prevalence_08_County_Level_KEN_county_prevalence_girls.png", "Figure 5. FGM prevalence among girls 0–14 by county, DHS 2014 and 2022 (BYM2-smoothed)"),
  h2("3.4 Prevalence has declined across birth cohorts"),
  p("Among women, model-based prevalence falls from 56.5% for those born in 1945–49 to 34.1% (1970–74), 19.1% (1985–89), 11.2% (1995–99) and 8.9% (2005–09); the pooled average over all rounds is 24.1% (Figure 6). Among girls it falls from 5.3–6.9% for those born in 1995–2004 to 1.4% (2010–14), largely because younger girls have not yet reached the ages of cutting."),
  ...figure("Prevalence_05_Trends_KEN_prevalence_trend_by_cohort.png", "Figure 6. FGM prevalence by birth cohort, covariate-adjusted, all rounds pooled", 520),
  h2("3.5 Composition and consistency"),
  p("Relative to Kalenjin women, the odds of being cut are much higher among Kisii (20.5), Maasai (10.0) and Somali (5.9) women and close to zero among Luo, Luhya and Mijikenda/Swahili women. Muslim women (4.8) and the poorest (1.8) have higher odds, and secondary education (0.24) and urban residence (0.77) lower odds. As a consistency check, the pooled model reproduces the province direct estimates within 0.25 percentage points. Because almost no girls are cut in some groups (e.g. Kikuyu), the model uses a weakly informative prior on its coefficients, without which estimates for low-prevalence provinces were unstable."),
  h1("4. Comparison with Kenya's subnational MICS"),
  p("Kenya has no national MICS. Five MICS cover single areas and are compared with DHS for the same area, closest in time: Bungoma, Kakamega and Turkana MICS 2013–14 with DHS 2014 for the same county; Nyanza MICS 2011 with DHS 2008–09 and 2014 for Nyanza province; and Mombasa informal settlements MICS 2009 with DHS 2014 for the whole of Mombasa county."),
  ...table(["MICS survey", "MICS prevalence (95% CI)", "DHS comparison", "DHS prevalence (95% CI)", "Difference (p)"],
    [["Nyanza 2011", "36.5 (30.9–42.0)", "DHS 2014, Nyanza", "32.4 (27.7–37.2)", "+4.1 (0.27)"],
     ["Bungoma 2013–14", "2.1 (1.0–3.2)", "DHS 2014, Bungoma", "1.2 (0.0–2.3)", "+0.9 (0.27)"],
     ["Kakamega 2013–14", "1.2 (0.5–1.8)", "DHS 2014, Kakamega", "0.8 (0.0–1.7)", "+0.3 (0.55)"],
     ["Turkana 2013–14", "3.2 (0.0–7.9)", "DHS 2014, Turkana", "3.7 (0.0–8.4)", "−0.5 (0.89)"],
     ["Mombasa informal settlements 2009", "12.4 (10.0–14.8)", "DHS 2014, Mombasa county", "7.4 (2.8–12.1)", "+5.0 (0.07)"]],
    [2300, 1700, 1900, 1700, 1426], "Table 2. FGM prevalence among women 15–49 (%), subnational MICS compared with DHS for the same area",
    "Nyanza also: DHS 2008–09 33.9% (19.4–48.4). Mombasa is indicative only: informal settlements are a subset of the county."),
  ...figure("Prevalence_07_MICS_Comparison_KEN_mics_prevalence_by_source.png", "Figure 7. FGM prevalence among women, subnational MICS compared with DHS for the same area"),
  p("MICS and DHS agree for every area; none of the differences is statistically significant. This independent check supports the DHS-based provincial and county estimates."),
  h1("5. Conclusions"),
  bullet("FGM prevalence among women in Kenya fell from 37.7% in 1998 to 14.8% in 2022, and among girls from 2.9% (2014) to 1.6% (2022)."),
  bullet("Prevalence remains near-universal in North Eastern (Wajir, Mandera, Garissa) and high in Kisii, Nyamira, Marsabit, Samburu and Isiolo."),
  bullet("Girls' prevalence fell sharply between 2014 and 2022 in Kisii and Nyamira, but remains high among girls in Wajir, Mandera and Garissa."),
  bullet("Subnational MICS confirm the DHS estimates for the areas they cover."),
];

// ======================================================================
// 3. ATTITUDES
// ======================================================================
const attitudes = [
  ...titleBlock("Attitudes to FGM, Kenya", "Support for continuing FGM among women and men, and how it relates to the practice"),
  h1("1. Introduction"),
  p("Social norms sustain FGM, so support for its continuation is a leading indicator of whether the practice will persist. This report estimates the share of women and men who think FGM should continue, nationally, by province and county, and by birth cohort, and compares it with where FGM is practised."),
  h1("2. Data"),
  p("Respondents who had heard of FGM were asked whether it should continue or stop; \"depends\" and \"don't know\" answers are excluded. Women were asked in DHS 1998, 2008–09, 2014 and 2022 (DHS 2003 did not ask); men only in DHS 2014 and 2022. Our estimates reproduce the official DHS figures."),
  ...table(["Survey round", "Women answering", "Women: support (%)", "Men answering", "Men: support (%)"],
    [["DHS 1998", "7,314", "21.3", "–", "–"], ["DHS 2008–09", "7,609", "10.3", "–", "–"],
     ["DHS 2014", "14,073", "6.3", "11,507", "9.5"], ["DHS 2022", "15,890", "4.4", "12,878", "4.9"],
     boldRow(["Total", "44,886", "", "24,385", ""])],
    [1826, 1700, 1900, 1700, 1900], "Table 1. Respondents and support for continuing FGM by survey round (direct survey-weighted estimates)"),
  h1("3. Results"),
  h2("3.1 Support has fallen sharply"),
  p("Among women, support for continuation fell from 21.3% in 1998 to 10.3% (2008–09), 6.3% (2014) and 4.4% (2022). Among men it fell from 9.5% (2014) to 4.9% (2022). Men were more supportive than women in 2014 (+3.2 points, p < 0.001) but not in 2022 (+0.5 points, not significant) (Figures 1 and 2)."),
  ...figure("Attitude_Women_and_Men_KEN_attitude_trend_bars_women_men.png", "Figure 1. Support for continuing FGM by survey round, women and men (95% CI)"),
  ...figure("Attitude_Women_and_Men_KEN_attitude_women_vs_men_dhs_bars.png", "Figure 2. Women and men in the same DHS rounds, with the design-based difference test", 520),
  h2("3.2 Where support is highest"),
  p("Pooling all rounds, support is 85.6% among women and 66.9% among men in North Eastern, against 3–18% among women and 2–8% among men elsewhere; among women it is next highest in Nyanza (18.0%) (Figure 3). By county in 2022, women's support is highest in Mandera (90.5%), Wajir (81.2%) and Garissa (67.5%), and men's (2014 and 2022 pooled) in Mandera (87.0%), Wajir (66.1%) and Garissa (50.0%) (Figures 4 and 5)."),
  ...figure("Attitude_Men_07_Men_vs_Women_KEN_attitude_map_men_vs_women.png", "Figure 3. Covariate-adjusted support for continuing FGM by province, women and men, all rounds pooled"),
  ...figure("Attitude_Women_and_Men_MICS_and_County_KEN_county_support_women.png", "Figure 4. Women's support for continuing FGM by county, DHS 2014 and 2022"),
  ...figure("Attitude_Women_and_Men_MICS_and_County_KEN_county_support_men.png", "Figure 5. Men's support for continuing FGM by county, DHS 2014 and 2022 pooled", 380),
  h2("3.3 Who supports continuation"),
  ...table(["Characteristic (reference)", "Group", "Women: odds ratio", "Men: odds ratio"],
    [["Own FGM status (cut)", "Not cut", "0.12", "–"], ["Ethnicity (Kalenjin)", "Somali", "7.10", "6.71"], ["", "Kisii", "4.16", "2.75"], ["", "Maasai", "3.29", "2.34"],
     ["Religion (Christian)", "Islam", "2.05", "2.28"], ["", "Traditional religion", "6.03", "7.27"],
     ["Education (none)", "Primary", "0.49", "0.64"], ["", "Secondary or higher", "0.26", "0.33"],
     ["Wealth (middle)", "Poorest", "1.35", "1.46"], ["", "Richest", "1.10", "0.58"], ["Residence (rural)", "Urban", "1.00", "1.08"]],
    [2700, 2300, 2013, 2013], "Table 2. Survey-weighted covariate-adjusted model: odds of supporting continuation",
    "All odds ratios are adjusted for every factor shown, age, survey round and province."),
  p("Women who have not been cut have 88% lower odds of supporting continuation. Support is highest among Somali respondents and followers of traditional religion, and falls with education."),
  h1("4. How attitudes relate to prevalence"),
  p("This section compares support for continuation with FGM prevalence nationally across survey rounds and across counties, and asks whose attitudes, women's or men's, are more closely associated with prevalence. Kenya's 8 provinces are too few for this analysis, so it uses the 47 counties (DHS 2014 and 2022 pooled). All results are associations, not causes."),
  h2("4.1 National level: attitudes and prevalence fell together"),
  p("Women's support and women's prevalence fell together across the four rounds with both measures (correlation 0.98) (Table 3, Figure 6). Men's attitudes and girls' prevalence were measured in only two rounds, too few to correlate."),
  ...table(["Survey round", "Prevalence, women 15–49", "Prevalence, girls 0–14", "Support, women", "Support, men"],
    [["DHS 1998", "37.7", "–", "21.3", "–"], ["DHS 2003", "32.2", "–", "–", "–"], ["DHS 2008–09", "27.1", "–", "10.3", "–"],
     ["DHS 2014", "21.0", "2.9", "6.3", "9.5"], ["DHS 2022", "14.8", "1.6", "4.4", "4.9"]],
    [1826, 1800, 1800, 1800, 1800], "Table 3. National FGM prevalence and support for continuation by survey round (%)"),
  ...figure("Prevalence_Attitude_Comparison_03_National_and_County_Association_KEN_national_prevalence_and_attitude_trend.png", "Figure 6. National FGM prevalence and support for continuation by survey round"),
  h2("4.2 County level: attitudes track prevalence closely"),
  p("Across the 47 counties, women's support correlates 0.93 with girls' prevalence and 0.79 with women's prevalence; men's support correlates 0.91 and 0.79 (Table 4, Figure 7). Rank correlations are lower (0.69–0.81), because the relationship is driven by the North Eastern counties, where both prevalence and support are very high."),
  ...table(["Prevalence measure", "Attitudes of", "Correlation (95% CI)", "p-value", "Variation explained"],
    [["Girls 0–14 (current practice)", "Women", "0.93 (0.87–0.96)", "<0.001", "86%"], ["", "Men", "0.91 (0.85–0.95)", "<0.001", "83%"],
     ["Women 15–49 (past practice)", "Women", "0.79 (0.65–0.88)", "<0.001", "62%"], ["", "Men", "0.79 (0.65–0.88)", "<0.001", "63%"]],
    [2726, 1400, 2000, 1300, 1600], "Table 4. County-level correlation between FGM prevalence and support for continuation (47 counties, DHS 2014 and 2022 pooled)",
    "Pearson correlations between BYM2-smoothed county estimates. Variation explained = R² of each attitude measure alone."),
  ...figure("Prevalence_Attitude_Comparison_03_National_and_County_Association_KEN_county_scatter_prevalence_vs_attitude.png", "Figure 7. County FGM prevalence against support for continuation, women and men"),
  h2("4.3 Whose attitudes matter more?"),
  p("Women's and men's support are almost identical across counties (correlation 0.98), so their separate associations can only be estimated weakly. With both in the same county model, women's support remains associated with girls' prevalence (+7.3 percentage points per standard deviation, 95% CI 2.5–12.0; p = 0.003) while men's adds nothing (+1.4 points, p = 0.57). For women's prevalence neither can be separated from the other. In Kenya, therefore, women's and men's attitudes move together; where they differ, women's attitudes are the closer correlate of whether girls are being cut, as in Nigeria."),
  h2("4.4 Local pattern"),
  p("In the cluster data (DHS 2008–09, 2014 and 2022), clusters where most women support continuation are almost all in North Eastern (97% of its clusters, against 0–10% elsewhere); for men, 84% of North Eastern clusters (Figure 8)."),
  ...figure("Prevalence_Attitude_Comparison_02_Cluster_Level_KEN_combined_prevalence_attitude_overlay_women.png", "Figure 8. FGM prevalence by province and women's support for continuation by survey cluster"),
  h1("5. Comparison with Kenya's subnational MICS"),
  ...table(["MICS survey", "MICS support (%)", "DHS comparison", "DHS support (%)"],
    [["Nyanza 2011", "22.3", "DHS 2008–09 / 2014, Nyanza", "20.1 / 8.9"], ["Bungoma 2013–14", "1.8", "DHS 2014, Bungoma", "2.1"],
     ["Kakamega 2013–14", "3.0", "DHS 2014, Kakamega", "1.3"], ["Turkana 2013–14", "6.4", "DHS 2014, Turkana", "2.4"],
     ["Mombasa informal settlements 2009", "4.0", "DHS 2014, Mombasa county", "2.0"]],
    [2700, 1600, 2900, 1826], "Table 5. Women's support for continuing FGM, subnational MICS compared with DHS for the same area"),
  p("Support agrees between MICS and DHS where they were collected at the same time (Bungoma, Kakamega, Turkana, all 1–6%). In Nyanza, MICS 2011 (22.3%) sits between DHS 2008–09 (20.1%) and DHS 2014 (8.9%), consistent with a sharp fall in support after 2011."),
  h1("6. Conclusions"),
  bullet("Support for continuing FGM in Kenya has fallen from about one woman in five in 1998 to under one in twenty in 2022, and among men from 9.5% to 4.9% since 2014."),
  bullet("Support remains a majority view in the North Eastern counties of Mandera, Wajir and Garissa, among both women and men."),
  bullet("Women who are cut, Somali and traditional-religion respondents, and those with little education are most supportive."),
  bullet("Women's and men's attitudes move together across counties; women's support is the closer correlate of girls' prevalence, so norm-change work in the North East should reach both, with mothers as a priority."),
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
