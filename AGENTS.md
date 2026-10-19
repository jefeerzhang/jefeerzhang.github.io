# 网站仓库约定

本仓库为 `jefeerzhang/jefeerzhang.github.io`（公开，GitHub Pages）。与 `jefeerzhang/class2026` 分别维护Git历史和远程。

- 开工时用 `git remote -v` 核对仓库身份，用 `git status` 记录已有改动。
- 课程页位于 `courses/freshman-digital-economy/`。
- `add` / `commit` / `push` 仅在用户明确要求后执行；提交范围限于本次任务文件。
- 推送后，用 `gh api repos/jefeerzhang/jefeerzhang.github.io/pages/builds/latest` 检查Pages构建状态。构建成功后再验证受影响的线上页面，并在验证URL添加查询参数绕过CDN缓存。
- 核对本地提交号与实际推送目标分支的远程提交号；范围外既有改动保持原样。
