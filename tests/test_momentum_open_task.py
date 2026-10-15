"""实验一恢复流程图版后的页面与附件检查。"""
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit
import csv
import re
import unittest
from zipfile import ZipFile
from xml.etree import ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
COURSE = ROOT / "courses" / "investment-portfolio"
PAGE = COURSE / "experiment-01-momentum-reversal.html"


class PageParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = []
        self.hrefs = []
        self.text = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.hrefs.append(attrs.get("href", ""))

    def handle_data(self, data):
        self.text.append(data)


class MomentumFlowPageTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.html = PAGE.read_text(encoding="utf-8")
        cls.parser = PageParser()
        cls.parser.feed(cls.html)
        cls.text = re.sub(r"\s+", "", "".join(cls.parser.text))

    def test_fixed_stock_sample_remains(self):
        for text in ("481", "2019-12-31", "2026-09-01", "事后上市状态筛选"):
            self.assertIn(text, self.text)
        with (COURSE / "sample500_teaching.csv").open(encoding="utf-8-sig", newline="") as f:
            rows = list(csv.DictReader(f))
        self.assertEqual(len(rows), 481)
        self.assertEqual(len({row["ts_code"] for row in rows}), 481)

    def test_flow_chart_and_mobile_steps_are_restored(self):
        self.assertIn("flow", self.parser.ids)
        self.assertIn('aria-labelledby="student-flow-title student-flow-desc"', self.html)
        self.assertIn("student-flow-title", self.parser.ids)
        self.assertIn("student-flow-desc", self.parser.ids)
        self.assertIn('class="flow-mobile"', self.html)
        self.assertEqual(self.html.count('class="flow-number"'), 8)
        self.assertIn("不通过 → 修正", self.html)

    def test_original_baseline_tasks_are_restored(self):
        for text in ("REV1", "MOM12", "五组等权", "80个", "两个信号的基准回测（50%）"):
            self.assertIn(text, self.text)
        self.assertIn("prompt", self.parser.ids)
        self.assertIn("appendix", self.parser.ids)
        self.assertNotIn("data-score-id=", self.html)
        self.assertNotIn("16个评价点 · 点击查看分档依据", self.html)

    def test_extension_plan_and_honest_results_remain(self):
        for text in ("150–200字", "至少一次有针对性的比较", "不要求结果显著", "不能直接解释为因果关系"):
            self.assertIn(text, self.text)

    def test_all_local_links_and_anchors_resolve(self):
        self.assertEqual(len(self.parser.ids), len(set(self.parser.ids)))
        for href in self.parser.hrefs:
            url = urlsplit(href)
            if url.scheme or url.netloc:
                continue
            with self.subTest(href=href):
                if url.path:
                    self.assertTrue((COURSE / unquote(url.path)).is_file())
                if url.fragment and not url.path:
                    self.assertIn(unquote(url.fragment), self.parser.ids)

    def test_original_deck_matches_fixed_baseline(self):
        with ZipFile(COURSE / "experiment-01-momentum-reversal.pptx") as archive:
            names = [n for n in archive.namelist() if re.fullmatch(r"ppt/slides/slide\d+\.xml", n)]
            self.assertEqual(len(names), 13)
            text = "".join("".join(ET.fromstring(archive.read(n)).itertext()) for n in names)
        self.assertIn("REV1", text)
        self.assertNotIn("方案由你决定", text)

    def test_course_entry_matches_original_page(self):
        entry = (COURSE / "index.html").read_text(encoding="utf-8")
        self.assertIn("任务一完成 REV1", entry)
        self.assertNotIn("任务一自主设计并检验动量与反转", entry)


if __name__ == "__main__":
    unittest.main()
