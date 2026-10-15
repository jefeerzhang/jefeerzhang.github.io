"""实验一公开页面的开放任务与学生资料发放边界。"""
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
RESOURCE = COURSE / "experiment-01-resources"


class PageParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = []
        self.hrefs = []
        self.points = []
        self.text = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.hrefs.append(attrs.get("href", ""))
        if "data-score-id" in attrs:
            self.points.append(attrs["data-score-id"])

    def handle_data(self, data):
        self.text.append(data)


class MomentumOpenTaskTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.html = PAGE.read_text(encoding="utf-8")
        cls.parser = PageParser()
        cls.parser.feed(cls.html)
        cls.text = re.sub(r"\s+", "", "".join(cls.parser.text))

    def test_fixed_scope_and_both_questions_remain(self):
        for text in ("481", "2019-12-31", "2026-09-01", "动量效应和反转效应", "事后存续筛选"):
            self.assertIn(text, self.text)
        with (COURSE / "sample500_teaching.csv").open(encoding="utf-8-sig", newline="") as f:
            rows = list(csv.DictReader(f))
        self.assertEqual(len(rows), 481)
        self.assertEqual(len({row["ts_code"] for row in rows}), 481)

    def test_short_task_does_not_supply_a_recipe(self):
        match = re.search(r'<section id="task1">(.*?)</section>', self.html, re.S)
        assert match is not None, "missing task-one section"
        section = match.group(1)
        for forbidden in ("REV1", "MOM12", "Newey", "Q5", "五组", "操作步骤", "12个月", "maxlags"):
            self.assertNotIn(forbidden, section)
        self.assertIn("请自行设计研究方案", section)
        self.assertIn("正式计算前提交简短方案", section)
        self.assertNotIn('id="prompt"', self.html)
        self.assertNotIn('id="appendix"', self.html)
        self.assertNotIn("复制使用", self.html)
        self.assertNotIn("80 个形成月", self.html)
        self.assertNotIn("滞后阶数固定为 3", self.html)

    def test_student_decisions_confirmation_and_extension(self):
        for text in ("一个未采用的替代方案", "教师确认只表示具备正式执行条件", "不担保方法或结果正确", "再次确认记录", "150—200字", "不局限于稳健性检查", "两个任务均提交后"):
            self.assertIn(text, self.text)

    def test_scoring_is_public_without_unconfirmed_totals(self):
        self.assertEqual(self.parser.points, [])
        self.assertNotIn("16个评价点 · 点击查看分档依据", self.html)
        self.assertIn('experiment-01-resources/04_实验结果评价标准与记录表.docx', self.parser.hrefs)
        for text in ("未确认前不输出正式总分", "材料不足先补核", "不要求全班使用同一方法", "不另套任务各占50%的旧权重"):
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

    def test_only_allowlisted_resources_are_published(self):
        self.assertEqual({p.name for p in RESOURCE.iterdir()}, {
            "实验A_动量反转任务书.docx", "实验A_动量反转任务书.md",
            "04_实验结果评价标准与记录表.docx", "04_实验结果评价标准与记录表.md",
            "学生证据模板.md", "学生研究助手配置.md", "sample500_teaching.csv",
        })
        self.assertEqual((RESOURCE / "sample500_teaching.csv").read_bytes(), (COURSE / "sample500_teaching.csv").read_bytes())
        task = (RESOURCE / "实验A_动量反转任务书.md").read_text(encoding="utf-8")
        self.assertIn("请自行设计研究方案", task)
        self.assertIn("本实验两个任务均提交后", task)

    def test_deck_has_no_fixed_method_demonstration(self):
        with ZipFile(COURSE / "experiment-01-momentum-reversal.pptx") as archive:
            names = [n for n in archive.namelist() if re.fullmatch(r"ppt/slides/slide\d+\.xml", n)]
            self.assertEqual(len(names), 18)
            text = "".join("".join(ET.fromstring(archive.read(n)).itertext()) for n in names)
        self.assertIn("方案由你决定", text)
        self.assertIn("两个任务均提交后", text)
        for forbidden in ("MOM12", "REV1", "五组等权", "maxlags"):
            self.assertNotIn(forbidden, text)

    def test_course_entry_matches_open_task(self):
        entry = (COURSE / "index.html").read_text(encoding="utf-8")
        self.assertIn("任务一自主设计并检验动量与反转", entry)
        self.assertNotIn("任务一完成 REV1", entry)


if __name__ == "__main__":
    unittest.main()
