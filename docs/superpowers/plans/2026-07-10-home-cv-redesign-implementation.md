# 主页与个人简历页统一改版实施计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 在不修改硕士课程站点、不改变学术事实与页面路由的前提下，统一改版主页和个人简历页，使其具备清晰的学术身份、稳定的共享视觉系统、良好的移动端体验和完整的语义/SEO 基础。

**Architecture:** 保持纯静态 HTML，不引入框架和运行时 JavaScript。`assets/site.css` 负责两页共享令牌、导航、排版、组件、响应式和减少动态规则；`index.html` 与 `cv/index.html` 只保留页面语义和内容。Python `unittest` 使用标准库 `HTMLParser` 检查结构、链接、SEO、资源路径和关键内容保真。

**Tech Stack:** HTML5、原生 CSS Grid/Flexbox、Python 3 标准库 `unittest`/`html.parser`、本地静态资源、Playwright/Chromium、Lighthouse。

---

## 文件职责

- `assets/site.css`：共享颜色、字体、间距、导航、按钮、入口网格、CV 布局、论文列表、技能区、焦点与响应式规则。
- `index.html`：主页 SEO、统一导航、身份首屏、非对称入口区和页脚。
- `cv/index.html`：CV SEO、统一导航、章节导航、科研指标带、论文列表和技能区。
- `tests/test_site_redesign.py`：两页的改版契约测试与资源有效性检查。
- `tests/test_homepage_toolbar.py`：保留提示词优化器链接与本地图标的专项回归测试，并改为检查共享 CSS，而非旧内联 CSS。

### Task 1: 建立改版契约测试

**Files:**
- Create: `tests/test_site_redesign.py`
- Modify: `tests/test_homepage_toolbar.py`

- [ ] **Step 1: 写入失败测试**

  覆盖以下契约：两页引用 `assets/site.css`；均有 skip link、`nav`、`main`、description、canonical 和 Open Graph；主页 H1 为“张剑”并包含四类入口；CV 包含章节导航、五项指标、18 条论文、技能图片，且不含“待补充”；原始 DOI 和外链仍存在；两张 PNG 具有有效签名和非零尺寸。

- [ ] **Step 2: 验证测试先失败**

  Run: `C:\Users\jefeer\an\python.exe -m unittest discover -s tests -v`

  Expected: FAIL，指出共享 CSS、语义结构或改版内容尚未实现。

- [ ] **Step 3: 提交测试节点**

  ```powershell
  git add tests
  git commit -m "test: define homepage and cv redesign contract"
  ```

### Task 2: 建立共享视觉系统

**Files:**
- Create: `assets/site.css`

- [ ] **Step 1: 写入共享设计令牌和基础规则**

  使用冷灰纸面、炭黑正文、森林绿单一强调色；采用本地中文衬线/无衬线字体栈；统一 8px 圆角、44px 触控目标、`:focus-visible`、skip link、容器宽度和基础排版。

- [ ] **Step 2: 写入共享导航、按钮和页脚规则**

  桌面导航保持单行且不高于 72px；移动端允许横向滚动；当前页通过文字颜色和底边框表达。按钮只有实心主操作和文本式次操作两类。

- [ ] **Step 3: 写入主页和 CV 布局规则**

  主页入口区采用 12 列非对称网格；CV 桌面采用 210px 章节导航加正文列，移动端降为单列；指标使用一个分隔数据带；论文使用稳定年份列；技能图使用固定 `aspect-ratio`。

- [ ] **Step 4: 写入移动端与减少动态规则**

  在 760px 和 520px 断点明确收敛网格、导航、指标和论文排版；所有自动进入动画在 `prefers-reduced-motion: reduce` 下关闭。

- [ ] **Step 5: 提交共享样式节点**

  ```powershell
  git add assets/site.css
  git commit -m "feat: add shared academic site design system"
  ```

### Task 3: 重构主页

**Files:**
- Modify: `index.html`

- [ ] **Step 1: 替换 head 元信息**

  添加准确 title、中文 description、canonical、Open Graph 与 favicon；移除 Google Fonts 和全部内联样式；引用 `assets/site.css`。

- [ ] **Step 2: 建立语义导航与身份首屏**

  添加 skip link、共享 `nav`、`main` 和唯一 H1“张剑”；首屏显示四川农业大学经济学院副教授、硕士生导师、研究方向、CV/课程操作和 ORCID 文本链接。

- [ ] **Step 3: 建立非对称入口区**

  硕士课程作为大入口，CV、技能和提示词优化器作为差异化次入口。继续使用 `assets/prompt-optimizer.png`，不增加空网格单元。

- [ ] **Step 4: 完成页脚与链接属性**

  保留 GitHub Pages 信息，加入 GitHub 和 ORCID；外链使用 `target="_blank" rel="noopener"`，内部链接保持现有路由。

- [ ] **Step 5: 运行主页相关测试并提交**

  Run: `C:\Users\jefeer\an\python.exe -m unittest tests.test_homepage_toolbar tests.test_site_redesign.HomepageRedesignTests -v`

  Expected: PASS。

  ```powershell
  git add index.html
  git commit -m "feat: redesign academic homepage"
  ```

### Task 4: 重构个人简历页

**Files:**
- Modify: `cv/index.html`

- [ ] **Step 1: 替换 head 与共享导航**

  添加 CV 专用 SEO/OG 元信息和 `../assets/site.css`；移除 Google Fonts、内联 CSS、旧返回链接和手写 ORCID SVG；使用与主页一致的导航。

- [ ] **Step 2: 重组身份与章节导航**

  保持姓名、机构、职务、博士身份和 ORCID 编号；建立“个人情况、科研业绩、发表论文、自定义技能”四个锚点，桌面粘性导航、移动端紧凑换行。

- [ ] **Step 3: 重组科研数据和论文列表**

  把五张指标卡替换为一个分隔数据带。完整保留 18 条论文、年份、作者、题目、期刊、DOI 与链接，只调整 HTML 结构以提升扫描效率。

- [ ] **Step 4: 完成技能区并移除占位内容**

  使用 `skills_banner.png` 作为辅助图，保留 GitHub 仓库链接和说明；删除“其他/待补充”区域。

- [ ] **Step 5: 运行 CV 与全量测试并提交**

  Run: `C:\Users\jefeer\an\python.exe -m unittest discover -s tests -v`

  Expected: PASS。

  ```powershell
  git add cv/index.html
  git commit -m "feat: redesign academic cv page"
  ```

### Task 5: 浏览器与性能验证

**Files:**
- Modify if needed: `assets/site.css`, `index.html`, `cv/index.html`

- [ ] **Step 1: 启动本地静态服务器**

  Run: `C:\Users\jefeer\an\python.exe -m http.server 56616 --bind 127.0.0.1`

  Expected: `http://127.0.0.1:56616/` 与 `/cv/` 返回 200。

- [ ] **Step 2: 截取桌面与移动端页面**

  使用 Chromium 检查 `1440x1000` 与 `390x844`，确认无横向溢出、文本遮挡、导航断裂、空网格或图片加载失败。

- [ ] **Step 3: 检查交互与控制台**

  用键盘逐项检查 skip link、导航、入口、DOI、仓库和页脚链接；确认焦点可见、控制台无错误，`prefers-reduced-motion` 可关闭进入动画。

- [ ] **Step 4: 运行 Lighthouse**

  对主页和 CV 检查 Performance、Accessibility、Best Practices、SEO；修正阻断性问题，并重新运行全量测试。

- [ ] **Step 5: 提交验证修正**

  ```powershell
  git add assets/site.css index.html cv/index.html
  git commit -m "fix: polish responsive academic site layout"
  ```

### Task 6: 整合、发布与线上核验

**Files:**
- No new production files expected.

- [ ] **Step 1: 检查分支差异与工作区状态**

  Run: `git status --short` and `git diff master...redesign/home-cv --stat`

  Expected: 工作区干净，差异仅覆盖计划文件、测试、共享 CSS、主页和 CV。

- [ ] **Step 2: 将改版分支整合到 master**

  在主工作区使用非交互式 fast-forward 或 merge，保留完整提交记录，不改写用户其他文件。

- [ ] **Step 3: 推送 GitHub Pages 源分支**

  Run: `git push origin master`

  Expected: 远端接受推送。

- [ ] **Step 4: 核验线上页面**

  检查线上主页、`/cv/` 和 `/master-course/`；前两页显示新设计并加载资源，课程站点仍正常；主要静态资源返回 200。

## 自检结果

- 规范覆盖：主页、CV、共享视觉、SEO、无障碍、性能、移动端和发布检查均有对应任务。
- 内容保真：测试明确约束论文数量、DOI、ORCID、课程与技能链接。
- 范围控制：没有任何任务修改 `master-course/` 文件或路由。
- 占位扫描：计划不包含 TBD、TODO 或未定义实现步骤。
