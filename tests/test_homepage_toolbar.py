from html.parser import HTMLParser
from pathlib import Path
import struct
import unittest


ROOT = Path(__file__).resolve().parents[1]
INDEX = ROOT / "index.html"
ICON = ROOT / "assets" / "prompt-optimizer.png"
CSS = ROOT / "assets" / "site.css"


class TagCollector(HTMLParser):
    def __init__(self):
        super().__init__()
        self.tags = []

    def handle_starttag(self, tag, attrs):
        self.tags.append((tag, dict(attrs)))


def has_class(attrs, class_name):
    return class_name in attrs.get("class", "").split()


class HomepageToolbarTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.html = INDEX.read_text(encoding="utf-8")
        cls.parser = TagCollector()
        cls.parser.feed(cls.html)

    def test_toolbar_uses_scalable_grid(self):
        grids = [
            attrs
            for tag, attrs in self.parser.tags
            if tag == "article" and has_class(attrs, "entry-tool")
        ]
        cards = [
            attrs
            for tag, attrs in self.parser.tags
            if tag == "a" and has_class(attrs, "tool-card")
        ]
        self.assertEqual(len(grids), 1)
        self.assertEqual(len(cards), 1)

    def test_prompt_optimizer_card_contract(self):
        cards = [
            attrs
            for tag, attrs in self.parser.tags
            if tag == "a" and has_class(attrs, "tool-card")
        ]
        self.assertEqual(len(cards), 1)
        card = cards[0]
        self.assertEqual(
            card.get("href"),
            "https://prompt.always200.com/#/basic/system",
        )
        self.assertEqual(card.get("target"), "_blank")
        self.assertIn("noopener", card.get("rel", "").split())
        self.assertIn("提示词优化器", self.html)
        self.assertIn("优化系统提示词与任务指令", self.html)

    def test_prompt_optimizer_icon_is_local_png(self):
        images = [
            attrs
            for tag, attrs in self.parser.tags
            if tag == "img" and attrs.get("src") == "assets/prompt-optimizer.png"
        ]
        self.assertEqual(len(images), 1)
        self.assertEqual(images[0].get("width"), "256")
        self.assertEqual(images[0].get("height"), "256")
        self.assertTrue(images[0].get("alt"))
        self.assertTrue(ICON.exists())

        data = ICON.read_bytes()
        self.assertEqual(data[:8], b"\x89PNG\r\n\x1a\n")
        width, height = struct.unpack(">II", data[16:24])
        self.assertGreaterEqual(width, 16)
        self.assertGreaterEqual(height, 16)

    def test_toolbar_css_is_responsive_and_accessible(self):
        self.assertTrue(CSS.exists())
        css = CSS.read_text(encoding="utf-8")
        self.assertIn(".entry-grid", css)
        self.assertIn(".tool-card:focus-visible", css)
        self.assertIn("@media (max-width: 760px)", css)
        self.assertIn("@media (prefers-reduced-motion: reduce)", css)

    def test_legacy_tool_block_is_removed(self):
        self.assertNotIn("tool-block", self.html)


if __name__ == "__main__":
    unittest.main()
