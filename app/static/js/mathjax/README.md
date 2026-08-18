# Vendored MathJax

`tex-svg.js` is MathJax **3.2.2** (TeX input, SVG output — the
combined, self-contained component; no separate config file
needed), downloaded from:

https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js

Used to render the `\(...\)` / `\[...\]` / `$$...$$` LaTeX math
in `docs/help/*/details.md`, loaded via `includeMarkdown()` in
`app/view/shared/help_modal.R`. Vendored locally (rather than
loaded from a CDN) so the app's help panel works fully offline,
matching the existing pattern of `app/static/js/*.js`.

## Security note

MathJax 2.x had two known CVEs (an XSS in the `unicode{}` macro,
fixed in 2.7.4; a ReDoS, fixed after 2.7.9). Neither applies to
MathJax 3.x, which is an unrelated rewrite. This app's usage is
also low-risk regardless: MathJax here only ever renders this
app's own bundled `.md` files, never arbitrary user input.

## Updating

To update, download a newer `tex-svg.js` from
`https://cdn.jsdelivr.net/npm/mathjax@<version>/es5/tex-svg.js`
and replace this file. Check the version string with:

```
grep -o '"3\.[0-9]*\.[0-9]*"' tex-svg.js | head -1
```
