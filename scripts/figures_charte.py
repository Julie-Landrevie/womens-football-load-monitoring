"""Figures du README et du portfolio, dans la charte visuelle (format carré 1 200 px).

Lit docs/data.js (écrit par le pipeline R, R/05_dashboard.R) et écrit docs/figures/*.png.
Usage : python3 scripts/figures_charte.py
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from matplotlib.colors import LinearSegmentedColormap

sys.path.insert(0, str(Path(__file__).parent))
from charte_jl import C, F, finish, fr, label, new_card, style_axes  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "docs" / "figures"
OUT.mkdir(parents=True, exist_ok=True)

txt = (ROOT / "docs" / "data.js").read_text(encoding="utf-8")
D = json.loads(txt[txt.index("{"): txt.rindex("}") + 1])
T = D["meta"]["thresholds"]
SRC = "Données : SoccerMon (Midoglu et al., Scientific Data, 2024),\ndeux équipes féminines de première division norvégienne, 2020-2021."
CMAP = LinearSegmentedColormap.from_list("jl", [C["card"], "#a4d0c8", C["teal"], "#075a53"])
MOIS = ["janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc."]


def serie(pid):
    s = D["series"][pid]
    n = len(s["load"])
    idx = pd.date_range(s["start"], periods=n, freq="D")
    return pd.DataFrame({k: v for k, v in s.items() if isinstance(v, list) and len(v) == n}, index=idx)


def axe_dates(ax, d0, d1):
    ticks = pd.date_range(d0, d1, freq="6MS")
    ax.set_xticks(ticks)
    ax.set_xticklabels([f"{MOIS[t.month - 1]} {t.year}" for t in ticks])


def out(name):
    return str(OUT / f"{name}.png")


# 1. Suivi d'une joueuse ---------------------------------------------------------------
pid = "A-12"
s = serie(pid)
fig = new_card(f"Joueuse {pid} : la charge du jour,\nla tendance et le ratio",
               "En haut, la charge perçue du jour (sRPE) et ses moyennes sur 7 et 28 jours.\n"
               "En bas, leur rapport (ACWR) ; en rouille, les sorties de zone signalées.")
ax = fig.add_axes([0.10, 0.46, 0.82, 0.24]); style_axes(ax)
ax.bar(s.index, s["load"], width=1.0, color=C["compare"], zorder=1)
ax.plot(s.index, s["acute"], color=C["teal"], lw=2.2, zorder=3)
ax.plot(s.index, s["chronic"], color=C["ink"], lw=2.2, zorder=4)
ax.set_ylim(0, s["load"].quantile(0.995) * 1.05)
ax.set_xticklabels([])
style_axes(ax)
ax.set_ylabel("UA", fontproperties=F["body"], fontsize=13, color=C["ink2"])
label(ax, s.index[-1], s["acute"].iloc[-60:].max() * 1.25, "aiguë, 7 j", ha="right", fontsize=13)
label(ax, s.index[-1], s["chronic"].iloc[-1] * 0.55, "chronique, 28 j", ha="right", fontsize=13)
ax2 = fig.add_axes([0.10, 0.17, 0.82, 0.25]); style_axes(ax2)
ax2.axhspan(T["acwr_low"], T["acwr_high"], color=C["grid"], alpha=0.7, zorder=0, lw=0)
ax2.plot(s.index, s["acwr"], color=C["teal"], lw=2, zorder=3)
al = s[(s["flags"] & 3) > 0]
ax2.plot(al.index, al["acwr_raw"].fillna(al["acwr"]), "o", ms=7, color=C["rust"], mec=C["bg"], mew=1.2, zorder=4)
ax2.set_ylim(0, 2.0)
ax2.set_yticks([0, 0.8, 1.5, 2]); ax2.set_yticklabels(["0", "0,8", "1,5", "2"])
axe_dates(ax2, s.index[0], s.index[-1]); ax.set_xlim(ax2.get_xlim()) if False else None
ax.set_xlim(s.index[0], s.index[-1]); ax2.set_xlim(s.index[0], s.index[-1])
style_axes(ax2)
ax2.set_ylabel("ACWR", fontproperties=F["body"], fontsize=13, color=C["ink2"])
finish(fig, SRC + "\nL’ACWR est un repère de progressivité, pas un prédicteur de blessure.", out("suivi_joueuse"))

# 2. Charge hebdomadaire de l'équipe A -------------------------------------------------
ids = [p for p, t in zip(D["players"]["id"], D["players"]["team"]) if t == "Équipe A"]
weeks = {}
for p in ids:
    s = serie(p)
    w = s["load"].resample("W-MON", label="left", closed="left").sum()
    docs = s["doc"].resample("W-MON", label="left", closed="left").sum()
    weeks[p] = w.where(docs > 0)
M = pd.DataFrame(weeks).T
M = M.loc[sorted(M.index, key=lambda p: -M.loc[p].notna().sum())]
fig = new_card("Deux saisons de charge,\nsemaine par semaine",
               f"Équipe A, {len(M)} joueuses. Charge perçue cumulée sur la semaine (sRPE, UA).\n"
               "Case vide : aucun jour documenté cette semaine-là.")
ax = fig.add_axes([0.10, 0.17, 0.74, 0.56]); style_axes(ax)
im = ax.imshow(M.values, aspect="auto", cmap=CMAP, vmin=0, vmax=np.nanpercentile(M.values, 98),
               interpolation="nearest")
ax.set_yticks(range(len(M))); ax.set_yticklabels(M.index)
cols = M.columns
tk = [i for i, c in enumerate(cols) if c.day <= 7 and c.month in (1, 7)]
ax.set_xticks(tk); ax.set_xticklabels([f"{MOIS[cols[i].month - 1]} {cols[i].year}" for i in tk])
ax.grid(False)
style_axes(ax); ax.grid(False)
for t in ax.get_yticklabels(): t.set_fontsize(10)
cax = fig.add_axes([0.87, 0.30, 0.02, 0.30])
cb = fig.colorbar(im, cax=cax); cb.outline.set_visible(False); cax.tick_params(length=0)
for t in cax.get_yticklabels(): t.set_fontproperties(F["body"]); t.set_fontsize(11); t.set_color(C["ink2"])
cax.set_yticks(cax.get_yticks()); cax.set_yticklabels([fr(v, 0) for v in cax.get_yticks()])
fig.text(0.87, 0.62, "UA", fontproperties=F["bold"], fontsize=12, color=C["ink2"])
finish(fig, SRC, out("charge_hebdo_equipe"))

# 3. Profil avant blessure -------------------------------------------------------------
ip = D["injury_profile"]
fig = new_card("Avant une blessure déclarée,\npas de signal net",
               f"Médiane et intervalle interquartile sur les 21 jours qui précèdent\n{ip['n']} déclarations de blessure.")
for i, (k, tit, yl) in enumerate([("acwr", "ACWR (EWMA)", None), ("well", "Wellness (z-score)", None)]):
    ax = fig.add_axes([0.10 + i * 0.44, 0.20, 0.36, 0.48]); style_axes(ax)
    ax.fill_between(ip["d"], ip[f"{k}_q1"], ip[f"{k}_q3"], color=C["teal"], alpha=0.18, lw=0)
    ax.plot(ip["d"], ip[f"{k}_med"], color=C["teal"], lw=3.5)
    ax.axvline(0, color=C["ink2"], lw=1, ls=":")
    ax.set_title(tit, fontproperties=F["bold"], fontsize=15, color=C["ink"], loc="left", pad=12)
    ax.set_xticks([-20, -15, -10, -5, 0]); ax.set_xticklabels(["−20", "−15", "−10", "−5", "0"])
    style_axes(ax)
    ax.set_yticklabels([fr(v, 1).replace("-", "−") for v in ax.get_yticks()])
    ax.set_xlabel("jours avant la déclaration", fontproperties=F["body"], fontsize=12, color=C["ink2"])
    v = ip[f"{k}_med"]
    label(ax, -21, v[0] + (0.08 if k == "acwr" else 0.12), fr(v[0], 2).replace("-", "−"), fontsize=13)
    label(ax, 0.3, v[-1], fr(v[-1], 2).replace("-", "−"), fontsize=13, va="center")
finish(fig, SRC + "\nBlessures auto-déclarées, sans groupe témoin : analyse descriptive.", out("profil_pre_blessure"))

# 4. Charge interne et externe ---------------------------------------------------------
ie = D["internal_external"]
fig = new_card("Charge ressentie et distance\nne racontent pas la même chose",
               "Un point par joueuse et par jour : distance GPS (km) et charge perçue (sRPE).\n"
               "Les écarts à la droite : séances ressenties plus ou moins dures que leur volume.")
for i, (team, v) in enumerate(ie.items()):
    ax = fig.add_axes([0.10 + i * 0.44, 0.19, 0.37, 0.50]); style_axes(ax)
    km, ld = np.array(v["km"]), np.array(v["load"])
    ax.scatter(km, ld, s=5, color=C["teal"], alpha=0.25, lw=0, zorder=2)
    a, b = np.polyfit(km, ld, 1); xx = np.array([0, np.percentile(km, 99.5)])
    ax.plot(xx, a * xx + b, color=C["rust"], lw=3, zorder=3)
    ax.set_xlim(0, 16); ax.set_ylim(0, 2000)
    ax.set_title(team, fontproperties=F["bold"], fontsize=15, color=C["ink"], loc="left", pad=12)
    style_axes(ax)
    ax.set_yticklabels([fr(t, 0) for t in ax.get_yticks()])
    label(ax, 0.4, 1880, f"r = {fr(v['r'], 2)}", fontsize=14, va="top")
    label(ax, 0.4, 1730, f"{fr(v['n'], 0)} jours", fontsize=12, va="top", color=C["ink2"])
    ax.set_xlabel("distance (km)", fontproperties=F["body"], fontsize=12, color=C["ink2"])
    if i == 0:
        ax.set_ylabel("sRPE (UA)", fontproperties=F["body"], fontsize=12, color=C["ink2"])
finish(fig, SRC + "\nr : corrélation de Pearson.", out("interne_externe"))

# 5. Ratio et niveau de la base --------------------------------------------------------
rows = []
for p in D["players"]["id"]:
    s = serie(p)
    rows.append(s[["acwr", "base", "flags"]].dropna(subset=["acwr", "base"]))
R = pd.concat(rows)
R["x"] = R["base"].clip(upper=2.0); R["y"] = R["acwr"].clip(upper=2.3)
hi = R[(R["flags"] & 1) > 0]; lo = R[(R["flags"] & 2) > 0]; ok = R[(R["flags"] & 3) == 0]
fig = new_card("Même ACWR, exposition différente",
               "Un point par joueuse et par jour : ACWR et charge chronique rapportée\n"
               "à la normale de la joueuse (médiane de l’année précédente).")
ax = fig.add_axes([0.10, 0.17, 0.82, 0.56]); style_axes(ax)
ax.axhspan(T["acwr_low"], T["acwr_high"], color=C["grid"], alpha=0.6, lw=0, zorder=0)
for xv in (T["base_low"], T["base_high"]):
    ax.axvline(xv, color=C["ink2"], lw=1, ls=(0, (4, 4)), zorder=1)
ax.scatter(ok["x"], ok["y"], s=4, color=C["compare"], alpha=0.35, lw=0, zorder=2)
ax.scatter(lo["x"], lo["y"], s=18, color=C["teal"], lw=0, zorder=3)
ax.scatter(hi["x"], hi["y"], s=26, color=C["rust"], lw=0, zorder=4)
ax.set_xlim(0, 2.05); ax.set_ylim(0, 2.35)
ax.set_xticks([0, 0.5, 1, 1.5, 2]); ax.set_xticklabels(["0 %", "50 %", "100 %", "150 %", "200 %"])
ax.set_yticks([0, 0.8, 1.5, 2]); ax.set_yticklabels(["0", "0,8", "1,5", "2"])
style_axes(ax)
ax.set_xlabel("charge chronique / normale de la joueuse", fontproperties=F["body"], fontsize=13, color=C["ink2"])
ax.set_ylabel("ACWR (sRPE)", fontproperties=F["body"], fontsize=13, color=C["ink2"])
for yy, col, t in [(2.27, C["rust"], "Alerte hausse rapide"), (2.14, C["teal"], "Alerte sous-charge")]:
    ax.plot([1.42], [yy], "o", ms=8, color=col, zorder=5)
    ax.text(1.46, yy, t, fontproperties=F["bold"], fontsize=13, color=C["ink"], va="center")
for x, y, t, ha in [(0.04, 1.62, "Reprise depuis\nune base basse", "left"), (2.0, 1.62, "Hausse sur une\nbase déjà haute", "right"),
                    (2.0, 0.12, "Baisse depuis une base haute", "right")]:
    ax.text(x, y, t, fontproperties=F["body"], fontsize=12, color=C["ink2"], ha=ha, va="bottom")
finish(fig, SRC + "\nBande : zone 0,8-1,5. Pointillés : base basse (< 75 %) et haute (> 125 %).", out("ratio_et_base"))

# 6. Retour au jeu ---------------------------------------------------------------------
S = pd.DataFrame(D["returns"]["summary"])
fig = new_card("Reprise individuelle : progressive.\nReprise collective : d’un coup",
               "Charge perçue (sRPE) chaque semaine après une absence d’au moins 14 jours,\n"
               "en % des 28 jours qui précèdent l’absence. Médiane et intervalle interquartile.")
ax = fig.add_axes([0.10, 0.17, 0.82, 0.55]); style_axes(ax)
ax.axhline(1, color=C["ink2"], lw=1); ax.axhline(0.9, color=C["ink2"], lw=1, ls=":")
for typ, col, nom in [("individuelle", C["teal"], "Absence individuelle"), ("collective", C["rust"], "Coupure collective")]:
    d = S[(S["type"] == typ) & (S["metric"] == "srpe")]
    ax.fill_between(d["week"], d["q1"], d["q3"], color=col, alpha=0.13, lw=0)
    ax.plot(d["week"], d["med"], "-o", color=col, lw=3.5, ms=10, mec=C["bg"], mew=2)
    r0 = d.iloc[0]
    label(ax, 1.12, r0["med"] + (0.06 if typ == "collective" else -0.06), f"{nom} : {fr(r0['med'] * 100, 0, pct=True)}",
          fontsize=14, va="bottom" if typ == "collective" else "top")
ax.set_xticks(range(1, 9)); ax.set_xticklabels([f"S{i}" for i in range(1, 9)])
ax.set_ylim(0, 2.5); ax.set_yticks([0, 0.5, 0.9, 1, 1.5, 2, 2.5])
ax.set_yticklabels(["0 %", "50 %", "90 %", "100 %", "150 %", "200 %", "250 %"])
style_axes(ax)
ax.set_xlabel("semaine après la reprise", fontproperties=F["body"], fontsize=13, color=C["ink2"])
st = D["returns"]["stats"]
finish(fig, SRC + f"\n{st['curves_individual']} reprises individuelles, {st['curves_collective']} collectives.", out("retour_au_jeu"))

# 7. Jalons de reprise -----------------------------------------------------------------
m = pd.DataFrame(D["returns"]["milestones"]["summary"]); m = m[m["level"] == 0.9]
NOM = {"srpe": "Charge perçue (sRPE)", "hi": "Haute intensité > 19 km/h", "sprint": "Sprint > 23 km/h", "vmax": "Vitesse max (1 s)"}
rows = [("individuelle", "srpe")] + [("collective", k) for k in ["srpe", "hi", "sprint", "vmax"]]
fig = new_card("Après une absence individuelle,\n15 jours pour retrouver 90 %",
               "Jour où chaque qualité retrouve 90 % de son niveau d’avant l’absence\n"
               "(médiane de Kaplan-Meier, fenêtre glissante de 7 jours : J7 au plus tôt).")
ax = fig.add_axes([0.40, 0.20, 0.50, 0.50]); style_axes(ax, xgrid=True)
for y, (typ, k) in zip(range(len(rows))[::-1], rows):
    r = m[(m["type"] == typ) & (m["metric"] == k)].iloc[0]
    col = C["rust"] if typ == "individuelle" else C["teal"]
    ax.barh(y, r["km_median_day"], height=0.58, color=col, zorder=2)
    label(ax, r["km_median_day"] + 0.4, y, f"J{r['km_median_day']:g}", va="center", fontsize=15)
    ax.text(-0.02, y + 0.12, NOM[k], transform=ax.get_yaxis_transform(), fontproperties=F["bold"], fontsize=14, color=C["ink"], ha="right", va="center")
    ax.text(-0.02, y - 0.22, f"{'absence individuelle' if typ == 'individuelle' else 'coupure collective'} · {int(r['reached'])} sur {int(r['n'])}",
            transform=ax.get_yaxis_transform(), fontproperties=F["body"], fontsize=11, color=C["ink2"], ha="right", va="center")
ax.set_yticks([]); ax.set_xlim(0, 21); ax.set_xticks([0, 7, 14, 21]); ax.set_xticklabels(["J0", "J7", "J14", "J21"])
style_axes(ax, xgrid=True)
finish(fig, SRC + "\nLes données GPS des reprises individuelles sont trop rares (3 reprises).", out("jalons_reprise"))

# 8. Complétude des déclarations --------------------------------------------------------
q = pd.DataFrame(D["qc"]["completeness"])
VAR = {"daily_load (> 0)": "charge", "fatigue": "fatigue", "mood": "humeur", "readiness": "forme", "sleep_quality": "sommeil",
       "soreness": "courbatures", "stress": "stress"}
fig = new_card("Avant toute alerte, vérifier\nce qui est vraiment déclaré",
               "Part des jours renseignés sur la période active de chaque joueuse, variable\npar variable. Clair : peu de jours renseignés ; foncé : tous les jours.")
for i, team in enumerate(D["teams"]):
    P = q[q["team"] == team].pivot(index="id", columns="var", values="v")[list(VAR)]
    P = P.loc[P.mean(axis=1).sort_values(ascending=False).index]
    ax = fig.add_axes([0.12 + i * 0.44, 0.17, 0.34, 0.52]); style_axes(ax)
    ax.imshow(P.values, aspect="auto", cmap=CMAP, vmin=0, vmax=1, interpolation="nearest")
    ax.set_yticks(range(len(P))); ax.set_yticklabels(P.index)
    ax.set_xticks(range(len(VAR))); ax.set_xticklabels(VAR.values(), rotation=55, ha="right")
    ax.set_title(team, fontproperties=F["bold"], fontsize=15, color=C["ink"], loc="left", pad=10)
    style_axes(ax); ax.grid(False)
    for t in ax.get_yticklabels(): t.set_fontsize(9)
    for t in ax.get_xticklabels(): t.set_fontsize(11)
finish(fig, SRC, out("qc_completude"))

# 9. Seuils : couverture ----------------------------------------------------------------
cv = pd.DataFrame(D["thresholds"]["curves"])
fig = new_card("Avec les seuils féminins,\nl’activité intense redevient visible",
               "Part des jours-joueuses (GPS, entraînements et matchs) qui dépassent\n"
               "chaque valeur, avec le seuil masculin puis le seuil féminin.")
for i, (k, tit, unit) in enumerate([("sprint_dist", "Distance de sprint", "m"), ("acc_high", "Accélérations hautes", "")]):
    ax = fig.add_axes([0.10 + i * 0.44, 0.19, 0.37, 0.50]); style_axes(ax)
    d = cv[cv["key"] == k]
    for st_, col in [("masculin", C["compare"]), ("féminin", C["teal"])]:
        e = d[d["set"].str.contains(st_)]
        ax.step(e["x"], e["share"] * 100, where="post", color=col, lw=3.5)
        lab = e["set"].iloc[0].replace("Seuil ", "seuil ")
        y0 = e["share"].iloc[0] * 100
        label(ax, e["x"].max() * 0.04, y0 + (3 if st_ == "féminin" else -4), f"{lab} : {fr(y0, 0, pct=True)}", fontsize=12, va="bottom" if st_ == "féminin" else "top")
    ax.set_ylim(0, 100); ax.set_xlim(0, d["x"].max())
    ax.set_yticks([0, 25, 50, 75, 100]); ax.set_yticklabels(["0 %", "25 %", "50 %", "75 %", "100 %"])
    ax.set_title(tit, fontproperties=F["bold"], fontsize=15, color=C["ink"], loc="left", pad=12)
    style_axes(ax)
    ax.set_xlabel("mètres par jour" if unit else "nombre par jour", fontproperties=F["body"], fontsize=12, color=C["ink2"])
finish(fig, "Seuils féminins et masculins : SkillCorner (webinaire public du 12/10/2026).\n"
            "Calculs sur SoccerMon (Midoglu et al., 2024), deux équipes féminines.", out("seuils_couverture"))

# 10. Seuils : rangs ---------------------------------------------------------------------
rk = pd.DataFrame(D["thresholds"]["ranks"]); rk = rk[rk["key"] == "acc_high"]
fig = new_card("Changer de seuil change le volume,\npresque pas le classement",
               f"Accélérations hautes : rang de chaque joueuse (percentile) avec le seuil masculin\n"
               f"et le seuil féminin. Corrélation de rang : {fr(D['thresholds']['stats']['spearman_acc'], 2)}.")
ax = fig.add_axes([0.14, 0.17, 0.62, 0.56]); style_axes(ax); ax.grid(axis="both", color=C["grid"])
ax.add_patch(__import__("matplotlib").patches.Rectangle((0, 50), 25, 50, color=C["grid"], alpha=0.8, lw=0, zorder=0))
ax.plot([0, 100], [0, 100], color=C["ink2"], lw=1, ls=(0, (4, 4)))
ax.scatter(rk["pct_men"], rk["pct_women"], s=110, color=C["teal"], edgecolor=C["bg"], lw=1.5, zorder=3)
ax.set_xlim(-3, 103); ax.set_ylim(-3, 103)
style_axes(ax); ax.grid(axis="both", color=C["grid"])
ax.set_xlabel("percentile, seuil masculin (3 m/s²)", fontproperties=F["body"], fontsize=13, color=C["ink2"])
ax.set_ylabel("percentile, seuil féminin (2,7 m/s²)", fontproperties=F["body"], fontsize=13, color=C["ink2"])
ax.text(1, 98, "Zone grisée : joueuse mal\nclassée avec le seuil\nmasculin (aucune ici)", fontproperties=F["body"], fontsize=11, color=C["ink2"], va="top")
finish(fig, "Seuils : SkillCorner (webinaire public du 12/10/2026).\nCalculs sur SoccerMon (Midoglu et al., 2024), "
            f"{len(rk)} joueuses classées.", out("seuils_profils_caches"))
print("ok", sorted(p.name for p in OUT.glob("*.png")))
