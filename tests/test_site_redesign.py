from html.parser import HTMLParser
from pathlib import Path
import struct
import unittest


ROOT = Path(__file__).resolve().parents[1]
HOME = ROOT / "index.html"
CV = ROOT / "cv" / "index.html"
CSS = ROOT / "assets" / "site.css"
PROMPT_ICON = ROOT / "assets" / "prompt-optimizer.png"
SKILLS_BANNER = ROOT / "cv" / "skills_banner.png"


class DocumentParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.tags = []
        self.text_stack = []
        self.heading_text = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.tags.append((tag, attrs))
        if tag in {"h1", "h2", "h3"}:
            self.text_stack.append([tag, []])

    def handle_data(self, data):
        if self.text_stack:
            self.text_stack[-1][1].append(data)

    def handle_endtag(self, tag):
        if self.text_stack and self.text_stack[-1][0] == tag:
            heading_tag, chunks = self.text_stack.pop()
            self.heading_text.append((heading_tag, "".join(chunks).strip()))


def parse(path):
    parser = DocumentParser()
    parser.feed(path.read_text(encoding="utf-8"))
    return parser


def class_tokens(attrs):
    return attrs.get("class", "").split()


def find_tags(parser, tag, class_name=None):
    matches = []
    for current_tag, attrs in parser.tags:
        if current_tag != tag:
            continue
        if class_name and class_name not in class_tokens(attrs):
            continue
        matches.append(attrs)
    return matches


def assert_valid_png(testcase, path, min_width=16, min_height=16):
    testcase.assertTrue(path.exists(), f"missing image: {path}")
    data = path.read_bytes()
    testcase.assertEqual(data[:8], b"\x89PNG\r\n\x1a\n")
    width, height = struct.unpack(">II", data[16:24])
    testcase.assertGreaterEqual(width, min_width)
    testcase.assertGreaterEqual(height, min_height)


class SharedSiteContractTests(unittest.TestCase):
    def test_shared_stylesheet_exists_and_has_core_contract(self):
        self.assertTrue(CSS.exists())
        css = CSS.read_text(encoding="utf-8")
        self.assertIn("--accent:", css)
        self.assertIn(".site-nav", css)
        self.assertIn(":focus-visible", css)
        self.assertIn("@media (prefers-reduced-motion: reduce)", css)
        self.assertIn("@media (max-width: 760px)", css)

    def test_both_pages_have_shared_semantics_and_seo(self):
        for path, stylesheet, canonical in (
            (HOME, "assets/site.css", "https://jefeerzhang.github.io/"),
            (CV, "../assets/site.css", "https://jefeerzhang.github.io/cv/"),
        ):
            with self.subTest(path=path):
                html = path.read_text(encoding="utf-8")
                parser = parse(path)
                self.assertEqual(len(find_tags(parser, "nav", "site-nav")), 1)
                self.assertEqual(len(find_tags(parser, "main")), 1)
                self.assertEqual(len(find_tags(parser, "a", "skip-link")), 1)
                self.assertEqual(len([h for h in parser.heading_text if h[0] == "h1"]), 1)
                self.assertIn(f'href="{stylesheet}"', html)
                self.assertIn(f'<link rel="canonical" href="{canonical}">', html)
                self.assertIn('<meta name="description"', html)
                self.assertIn('<meta property="og:title"', html)
                self.assertIn('<meta property="og:description"', html)
                self.assertIn('<meta property="og:url"', html)
                self.assertNotIn("fonts.googleapis.com", html)


class HomepageRedesignTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.html = HOME.read_text(encoding="utf-8")
        cls.parser = parse(HOME)

    def test_homepage_leads_with_academic_identity(self):
        h1s = [text for tag, text in self.parser.heading_text if tag == "h1"]
        self.assertEqual(h1s, ["张剑"])
        self.assertIn("四川农业大学经济学院", self.html)
        self.assertIn("副教授", self.html)
        self.assertIn("硕士研究生导师", self.html)

    def test_homepage_preserves_primary_destinations(self):
        hrefs = {attrs.get("href") for attrs in find_tags(self.parser, "a")}
        expected = {
            "https://jefeerzhang.github.io/master-course/",
            "cv/",
            "cv/#skills",
            "https://prompt.always200.com/#/basic/system",
            "https://orcid.org/0000-0002-8024-5483",
        }
        self.assertTrue(expected.issubset(hrefs))
        self.assertEqual(len(find_tags(self.parser, "article", "entry-card")), 4)

    def test_prompt_optimizer_image_is_valid(self):
        images = [
            attrs
            for attrs in find_tags(self.parser, "img")
            if attrs.get("src") == "assets/prompt-optimizer.png"
        ]
        self.assertEqual(len(images), 1)
        self.assertEqual(images[0].get("width"), "256")
        self.assertEqual(images[0].get("height"), "256")
        assert_valid_png(self, PROMPT_ICON)


class CvRedesignTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.html = CV.read_text(encoding="utf-8")
        cls.parser = parse(CV)

    def test_cv_has_section_navigation_and_no_placeholder(self):
        hrefs = {attrs.get("href") for attrs in find_tags(self.parser, "a")}
        for anchor in ("#profile", "#research", "#publications", "#skills"):
            self.assertIn(anchor, hrefs)
        self.assertNotIn("待补充", self.html)
        self.assertNotIn("<h2>其他</h2>", self.html)

    def test_cv_preserves_research_metrics_and_publications(self):
        self.assertEqual(len(find_tags(self.parser, "div", "metric")), 5)
        self.assertEqual(len(find_tags(self.parser, "article", "publication")), 18)
        for value in ("5", "6", "20+", "27.3", "Top 1%"):
            self.assertIn(value, self.html)

    def test_cv_preserves_identifiers_and_doi_links(self):
        for value in (
            "0000-0002-8024-5483",
            "10.13762/j.cnki.cjlc.2024.04.006",
            "10.14116/j.nkes.2015.02.002",
            "10.1016/j.jia.2025.12.057",
            "10.1016/j.enpol.2019.111071",
        ):
            self.assertIn(value, self.html)

    def test_skills_section_uses_real_banner_and_repository(self):
        images = [
            attrs
            for attrs in find_tags(self.parser, "img")
            if attrs.get("src") == "skills_banner.png"
        ]
        self.assertEqual(len(images), 1)
        self.assertEqual(images[0].get("width"), "1080")
        self.assertEqual(images[0].get("height"), "1080")
        self.assertIn("https://github.com/jefeerzhang/myskill", self.html)
        assert_valid_png(self, SKILLS_BANNER, 1000, 1000)


if __name__ == "__main__":
    unittest.main()
