#!/usr/bin/env python3
"""抓取 Tesfatsion ACE 索引页 + 直接 PDF + 关键 HTML 子页面（深度1）。
按内容分类保存到本地子目录。"""
import os, sys, time
import requests
from urllib.parse import urljoin

BASE = "https://www2.econ.iastate.edu/tesfatsi/"
OUT = os.path.dirname(os.path.abspath(__file__))
PDF_DIR = os.path.join(OUT, "pdfs")
INTRO_DIR = os.path.join(OUT, "intro")
METHOD_DIR = os.path.join(OUT, "method")
TEACH_DIR = os.path.join(OUT, "teaching")
SOFT_DIR = os.path.join(OUT, "software")

os.makedirs(PDF_DIR, exist_ok=True)
os.makedirs(INTRO_DIR, exist_ok=True)
os.makedirs(METHOD_DIR, exist_ok=True)
os.makedirs(TEACH_DIR, exist_ok=True)
os.makedirs(SOFT_DIR, exist_ok=True)

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                  "(KHTML, like Gecko) Chrome/120.0 Safari/537.36"
}

# (url, 本地路径) 列表
direct_pdfs = [
    ("CompleteABM.SSC2021Keynote.LTesfatsion.pdf", "pdfs"),
    ("EconSystemsAsLCSGames.LTesfatsion.pdf", "pdfs"),
    ("MacroConstructiveRationalityWP.SinitskayaTesfatsion.pdf", "pdfs"),
    ("LearnAlgorithms.LT.pdf", "pdfs"),
    ("DynamicEconomicModelingBasics.WPVersion.pdf", "pdfs"),
    ("ABMSyllabus.GrowingArtificialSocieties.AGIsaac2023.pdf", "pdfs"),
    ("CreativeModelingExerciseOnPiketty.Econ502.pdf", "pdfs"),
]

html_subs = [
    ("abmread.htm", INTRO_DIR),    # On-Line Guide for Newcomers
    ("aintro.htm", INTRO_DIR),     # Annotated pointers to ACE intro
    ("AModGuide.htm", METHOD_DIR), # Presentation/Evaluation Guidelines
    ("empvalid.htm", METHOD_DIR),  # Empirical Validation and Verification
    ("acedemos.htm", SOFT_DIR),    # Computational Labs and Demo Software
    ("acecode.htm", SOFT_DIR),     # Software and Toolkits for ABM
]

def get(url, path, is_pdf=False):
    try:
        r = requests.get(url, headers=HEADERS, timeout=60)
        if r.status_code != 200:
            print(f"  [FAIL {r.status_code}] {url}")
            return False
        if is_pdf:
            ctype = r.headers.get("Content-Type", "")
            if "pdf" not in ctype and len(r.content) < 5000:
                print(f"  [WARN non-pdf?] {url} ctype={ctype} size={len(r.content)}")
        with open(path, "wb") as f:
            f.write(r.content)
        print(f"  [OK {len(r.content)//1024}KB] -> {os.path.relpath(path, OUT)}")
        return True
    except Exception as e:
        print(f"  [ERR] {url}: {e}")
        return False

def main():
    print("== 索引页 (ace.htm) ==")
    get("https://faculty.sites.iastate.edu/tesfatsi/archive/tesfatsi/ace.htm",
        os.path.join(OUT, "ace_index.html"))

    print("\n== 直接 PDF ==")
    for name, _ in direct_pdfs:
        get(urljoin(BASE, name), os.path.join(PDF_DIR, name), is_pdf=True)
        time.sleep(0.5)

    print("\n== 关键 HTML 子页面 ==")
    for name, d in html_subs:
        get(urljoin(BASE, name), os.path.join(d, name))
        time.sleep(0.5)

    print("\n完成。")

if __name__ == "__main__":
    main()
