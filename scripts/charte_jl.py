import os, urllib.request
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
from matplotlib.patches import FancyBboxPatch

C = dict(bg="#f6f1e7", ink="#1c1917", ink2="#57534e", muted="#8a8580", grid="#e4dccd",
         teal="#0d8f84", rust="#cc4a12", compare="#c9c0b1", card="#fbf8f2")
FONT_DIR = os.path.expanduser("~/.fonts_jl")
_FILES = {"title": "dmserifdisplay/DMSerifDisplay-Regular.ttf", "body": "lato/Lato-Regular.ttf",
          "bold": "lato/Lato-Bold.ttf", "black": "lato/Lato-Black.ttf"}
def _font(key):
    os.makedirs(FONT_DIR, exist_ok=True)
    path = os.path.join(FONT_DIR, os.path.basename(_FILES[key]))
    if not os.path.exists(path):
        urllib.request.urlretrieve("https://raw.githubusercontent.com/google/fonts/main/ofl/" + _FILES[key], path)
    return fm.FontProperties(fname=path)
F = {k: _font(k) for k in _FILES}

def new_card(title, subtitle):
    fig = plt.figure(figsize=(10, 10), dpi=120)
    fig.patch.set_facecolor(C["bg"])
    fig.text(0.07, 0.93, title, fontproperties=F["title"], fontsize=31, color=C["ink"], va="top", linespacing=1.1)
    fig.text(0.07, 0.80, subtitle, fontproperties=F["body"], fontsize=15, color=C["ink2"], va="top", linespacing=1.4)
    return fig

def style_axes(ax, xgrid=False):
    ax.set_facecolor(C["bg"])
    for sp in ax.spines.values(): sp.set_visible(False)
    ax.tick_params(length=0)
    ax.grid(axis="x" if xgrid else "y", color=C["grid"]); ax.set_axisbelow(True)
    for t in ax.get_xticklabels() + ax.get_yticklabels():
        t.set_fontproperties(F["body"]); t.set_fontsize(13); t.set_color(C["ink2"])

def label(ax, x, y, text, **kw):
    kw.setdefault("fontsize", 16); kw.setdefault("color", C["ink"])
    return ax.text(x, y, text, fontproperties=F["black"], **kw)

def finish(fig, source, out):
    fig.text(0.07, 0.035, source, fontproperties=F["body"], fontsize=11, color=C["ink2"], linespacing=1.4)
    fig.patches.append(FancyBboxPatch((0.855, 0.035), 0.06, 0.05, boxstyle="round,pad=0,rounding_size=0.008",
                       transform=fig.transFigure, fc="none", ec=C["teal"], lw=2.2, figure=fig))
    fig.text(0.885, 0.059, "JL", fontproperties=F["bold"], fontsize=18, color=C["teal"], ha="center", va="center")
    fig.text(0.845, 0.068, "Julie Landrevie", fontproperties=F["bold"], fontsize=13, color=C["ink"], ha="right", va="center")
    fig.text(0.845, 0.045, "Data & performance football", fontproperties=F["body"], fontsize=11, color=C["ink2"], ha="right", va="center")
    fig.savefig(out, facecolor=C["bg"]); plt.close(fig)

def fr(x, dec=1, pct=False):
    s = f"{x:,.{dec}f}".replace(",", " ").replace(".", ",")
    return s + (" %" if pct else "")
