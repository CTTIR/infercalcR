# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.ig_cls_pd_probability <- function(x, m, panel, basis, lang) {
  if (basis != "observed") .ig_abort("unsupported",
    if (lang == "de") "Kalibrierung und Nettonutzen beziehen sich auf beobachtete Daten, nicht auf ein Praevalenzszenario." else
      "Calibration and net benefit describe observed data, not a prevalence scenario.")
  view <- m$views[[panel]]
  if (is.null(view) || !isTRUE(view$available)) .ig_abort("unsupported",
    .ig_cls_reason_text(view$reason_code %||% "not_probability_scale", lang))
  table <- m$probability[[panel]]
  if (is.null(table)) .ig_abort("unsupported", "This result does not retain the probability diagnostic table.")
  table <- .ig_cls_frame(table, panel)
  if (!nrow(table)) .ig_abort("unsupported", "The probability diagnostic table is empty.")
  summaries <- .ig_cls_frame(m$summaries, "summaries")
  summaries <- summaries[summaries$metric_id %in% c("brier_score","calibration_in_the_large","calibration_intercept","calibration_slope"),,drop=FALSE]
  notes <- if (panel == "calibration") {
    if (lang == "de") "Gruppierte beobachtete Ereignisraten; Gruppierung nur zur Darstellung. Keine Konfidenzintervalle. Diskriminierung belegt keine Kalibrierung." else
      "Grouped observed event rates; grouping is for display only. No confidence intervals. Discrimination does not establish calibration."
  } else if (lang == "de") "Deklarierte Entscheidungsschwellen p; relatives Schadensgewicht p/(1-p). Werte nur an berechneten Schwellen, keine Interpolation. Gueltigkeit haengt von Kalibrierung und Bewertungspopulation ab." else
    "Declared decision probabilities p; relative harm weight p/(1-p). Values only at evaluated thresholds, without interpolation. Validity depends on calibration and the evaluation population."
  list(data=list(points=table,summaries=summaries),notes=notes,
    estimate_kind=m$count_kind,evaluation=x$evaluation)
}
