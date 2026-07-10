# Toolbar Grid Repair Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the unfinished text-only toolbar block with a responsive, accessible tool-card grid that uses the prompt optimizer's real local icon and remains easy to extend.

**Architecture:** Keep the site as a single static HTML page. Add one local PNG asset, replace the legacy `.tool-block` component with a CSS Grid-based `.tools-grid` and semantic `.tool-card`, and protect the contract with a Python standard-library regression test.

**Tech Stack:** Static HTML/CSS, Python 3 `unittest` + `html.parser`, Pillow for one-time ICO-to-PNG conversion, GitHub Pages, GitHub CLI.

---

## File Map

- Create `.gitignore`: exclude `.superpowers/` visual-companion artifacts from commits.
- Create `tests/test_homepage_toolbar.py`: verify the HTML, CSS, link, local image, responsive contract, and removal of the legacy block.
- Create `assets/prompt-optimizer.png`: local PNG converted from the prompt optimizer site's favicon.
- Modify `index.html:83-121`: replace legacy tool-block CSS with grid/card CSS and reduced-motion handling.
- Modify `index.html:150-153`: replace the text-only link with the semantic tool-grid markup.

### Task 1: Lock the Toolbar Contract With a Failing Test

**Files:**
- Create: `.gitignore`
- Create: `tests/test_homepage_toolbar.py`

- [ ] **Step 1: Ignore visual-companion artifacts**

Create `.gitignore` with exactly:

```gitignore
.superpowers/
```

- [ ] **Step 2: Add the toolbar regression test**

Create `tests/test_homepage_toolbar.py`:

```python
from html.parser import HTMLParser
from pathlib import Path
import struct
import unittest


ROOT = Path(__file__).resolve().parents[1]
INDEX = ROOT / "index.html"
ICON = ROOT / "assets" / "prompt-optimizer.png"


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
            if tag == "div" and has_class(attrs, "tools-grid")
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
        self.assertEqual(images[0].get("width"), "32")
        self.assertEqual(images[0].get("height"), "32")
        self.assertTrue(images[0].get("alt"))
        self.assertTrue(ICON.exists())

        data = ICON.read_bytes()
        self.assertEqual(data[:8], b"\x89PNG\r\n\x1a\n")
        width, height = struct.unpack(">II", data[16:24])
        self.assertGreaterEqual(width, 16)
        self.assertGreaterEqual(height, 16)

    def test_toolbar_css_is_responsive_and_accessible(self):
        self.assertIn("grid-template-columns: repeat(2, minmax(0, 1fr))", self.html)
        self.assertIn(".tool-card:focus-visible", self.html)
        self.assertIn(".tools-grid { grid-template-columns: 1fr; }", self.html)
        self.assertIn("@media (prefers-reduced-motion: reduce)", self.html)

    def test_legacy_tool_block_is_removed(self):
        self.assertNotIn("tool-block", self.html)


if __name__ == "__main__":
    unittest.main()
```

- [ ] **Step 3: Run the test and verify the current page fails the new contract**

Run:

```powershell
& 'C:\Users\jefeer\an\python.exe' -m unittest tests.test_homepage_toolbar -v
```

Expected: failures for missing `.tools-grid`, `.tool-card`, `assets/prompt-optimizer.png`, responsive CSS, and remaining `tool-block` markup.

- [ ] **Step 4: Commit the failing regression contract**

```powershell
git add .gitignore tests/test_homepage_toolbar.py
git commit -m "test: define homepage toolbar contract"
```

Expected: one commit containing only `.gitignore` and the test file.

### Task 2: Add the Real Icon and Implement the Tool Grid

**Files:**
- Create: `assets/prompt-optimizer.png`
- Modify: `index.html:83-121`
- Modify: `index.html:150-153`
- Test: `tests/test_homepage_toolbar.py`

- [ ] **Step 1: Download and convert the real favicon**

Run:

```powershell
New-Item -ItemType Directory -Force 'assets' | Out-Null
Invoke-WebRequest -Uri 'https://prompt.always200.com/favicon.ico' -OutFile 'C:\tmp\prompt-optimizer.ico' -UseBasicParsing
& 'C:\Users\jefeer\an\python.exe' -c "from PIL import Image; Image.open(r'C:\tmp\prompt-optimizer.ico').convert('RGBA').save(r'assets\prompt-optimizer.png')"
```

Expected: `assets/prompt-optimizer.png` exists, is a valid PNG, and preserves the favicon artwork.

- [ ] **Step 2: Replace the legacy toolbar CSS**

Replace `.tool-block`, `.tool-block:hover`, `.tool-block img`, `.tool-block:hover img`, and `.tool-name` with:

```css
  .tools-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 18px;
  }
  .tool-card {
    display: grid;
    grid-template-columns: 56px minmax(0, 1fr) 20px;
    align-items: center;
    gap: 16px;
    min-height: 124px;
    padding: 22px;
    color: inherit;
    text-decoration: none;
    background: var(--card);
    border: 1px solid var(--line);
    border-radius: 16px;
    box-shadow: var(--shadow);
    transition: transform .45s var(--ease), box-shadow .45s var(--ease), border-color .45s var(--ease);
  }
  .tool-card:hover {
    transform: translateY(-4px);
    border-color: #d8d1c6;
    box-shadow: 0 2px 4px rgba(28,27,25,.05), 0 22px 44px -16px rgba(28,27,25,.22);
  }
  .tool-card:focus-visible {
    outline: 3px solid rgba(47,93,80,.32);
    outline-offset: 4px;
  }
  .tool-icon {
    display: grid;
    place-items: center;
    width: 56px;
    height: 56px;
    background: var(--accent-soft);
    border-radius: 10px;
  }
  .tool-icon img {
    display: block;
    width: 32px;
    height: 32px;
    object-fit: contain;
  }
  .tool-copy { min-width: 0; }
  .tool-name {
    display: block;
    color: var(--ink);
    font-family: "Noto Serif SC", serif;
    font-size: 17px;
    font-weight: 600;
    letter-spacing: 0;
  }
  .tool-description {
    display: block;
    margin-top: 5px;
    color: var(--ink-soft);
    font-size: 13px;
    line-height: 1.55;
  }
  .tool-external {
    color: var(--accent);
    font-size: 18px;
    line-height: 1;
    transition: transform .35s var(--ease);
  }
  .tool-card:hover .tool-external { transform: translate(2px, -2px); }
```

Add this responsive rule before the reduced-motion rule:

```css
  @media (max-width: 640px) {
    .tools-grid { grid-template-columns: 1fr; }
    .tool-card { min-height: 112px; padding: 20px; }
  }
```

Extend the reduced-motion rule to include:

```css
    .tool-card, .tool-external { transition: none; }
    .tool-card:hover { transform: none; }
```

- [ ] **Step 3: Replace the text-only toolbar markup**

Replace the existing `.tool-block` anchor with:

```html
  <div class="tools-grid">
    <a class="tool-card" href="https://prompt.always200.com/#/basic/system" target="_blank" rel="noopener" aria-label="打开提示词优化器（新窗口）">
      <span class="tool-icon">
        <img src="assets/prompt-optimizer.png" alt="提示词优化器图标" width="32" height="32">
      </span>
      <span class="tool-copy">
        <span class="tool-name">提示词优化器</span>
        <span class="tool-description">优化系统提示词与任务指令</span>
      </span>
      <span class="tool-external" aria-hidden="true">↗</span>
    </a>
  </div>
```

- [ ] **Step 4: Run the regression test and verify it passes**

```powershell
& 'C:\Users\jefeer\an\python.exe' -m unittest tests.test_homepage_toolbar -v
```

Expected: five tests pass.

- [ ] **Step 5: Run static diff checks**

```powershell
git diff --check
if (Select-String -Path index.html -Pattern 'tool-block','gift-box' -Quiet) { throw 'Legacy toolbar references remain in index.html' }
rg -n -e prompt-optimizer.png -e tools-grid -e tool-card index.html tests assets
```

Expected: no `git diff --check` errors; the PowerShell guard does not throw; new grid, card, test, and PNG references are present.

- [ ] **Step 6: Commit the implementation**

```powershell
git add index.html assets/prompt-optimizer.png
git commit -m "fix: rebuild homepage toolbar as tool grid"
```

Expected: one implementation commit containing the HTML/CSS change and the local PNG asset.

### Task 3: Verify Desktop, Mobile, Interaction, and Failure Resilience

**Files:**
- Verify: `index.html`
- Verify: `assets/prompt-optimizer.png`

- [ ] **Step 1: Start the static site locally**

Run in the worktree:

```powershell
& 'C:\Users\jefeer\an\python.exe' -m http.server 8000 --bind 127.0.0.1
```

Expected: the site is available at `http://127.0.0.1:8000/`.

- [ ] **Step 2: Verify the desktop viewport**

Use the Browser skill at 1440 x 1000 and confirm:

- The tool card occupies the first column of a stable two-column grid.
- The real icon appears inside a 56px soft-green tile.
- The title, description, and external-link indicator are visible without overlap.
- Hover raises the card without resizing the layout.
- The browser console has no errors or warnings.

- [ ] **Step 3: Verify the mobile viewport**

Use the Browser skill at 390 x 844 and confirm:

- The grid becomes one column.
- The card fills the available content width.
- Text wraps naturally and stays within the card.
- The touch target remains at least 44px high and no content overlaps.

- [ ] **Step 4: Verify keyboard focus and link behavior**

Press `Tab` until the tool card receives focus. Confirm the focus outline is visible, then activate the link and verify the destination is:

`https://prompt.always200.com/#/basic/system`

- [ ] **Step 5: Verify the text fallback remains structurally independent**

Inspect the tool card in a fresh browser DOM snapshot and confirm `.tool-name`, `.tool-description`, and `.tool-external` are sibling elements of `.tool-icon`, not children of the `<img>`. This proves the card's meaning and link remain present when the image cannot render. Cross-check the same contract with `test_prompt_optimizer_card_contract`.

- [ ] **Step 6: Re-run automated checks after visual verification**

```powershell
& 'C:\Users\jefeer\an\python.exe' -m unittest tests.test_homepage_toolbar -v
git status --short
```

Expected: five tests pass; the branch contains no uncommitted implementation changes.

### Task 4: Fast-Forward Master, Push, and Verify GitHub Pages

**Files:**
- Merge source: branch `fix/toolbar-grid`
- Deploy target: branch `master`

- [ ] **Step 1: Confirm the feature branch is ready**

```powershell
git log --oneline --decorate -4
git status --short --branch
```

Expected: the design, regression-test, and implementation commits are present; the worktree is clean.

- [ ] **Step 2: Fast-forward the main worktree**

Run from `C:\Users\jefeer\Downloads\testclaw\projects\jefeerzhang.github.io`:

```powershell
git merge --ff-only fix/toolbar-grid
```

Expected: `master` advances without a merge commit.

- [ ] **Step 3: Push the completed fix**

```powershell
git push origin master
```

Expected: GitHub accepts the new `master` commits.

- [ ] **Step 4: Check the Pages deployment**

```powershell
gh api repos/jefeerzhang/jefeerzhang.github.io/pages/builds/latest --jq '{status:.status,commit:.commit,error:.error.message}'
```

Expected: the latest build references the pushed commit and reaches `built` with no error message.

- [ ] **Step 5: Verify the live site**

Open `https://jefeerzhang.github.io/` with the Browser skill and repeat the desktop and mobile checks from Task 3. Also verify:

- `https://jefeerzhang.github.io/assets/prompt-optimizer.png` returns the PNG.
- The old text-only white block is absent.
- The live page has no console errors or warnings.

- [ ] **Step 6: Final completion audit**

Compare the live result against every acceptance criterion in `docs/superpowers/specs/2026-07-10-toolbar-grid-design.md`. Only declare completion when each criterion has direct file, test, browser, or deployment evidence.
