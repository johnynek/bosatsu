# GitHub Pages Demo: Multi-Target Bosatsu Compilation Showcase

**Status: COMPLETE**
**Branch: feat/jsgen-phase3**
**Commits: 05fecfec, 7123b6c2**

## Overview

Deploy a demo site to GitHub Pages showcasing Bosatsu programs compiled to multiple targets:
- **JavaScript** (runs in browser)
- **C** (displayed with syntax highlighting)
- **WASM** (C compiled via emscripten, runs in browser)

Plus the existing web compiler UI.

## Site Structure

```
https://snoble.github.io/bosatsu/
├── index.html              # Landing page with links
├── compiler/               # Existing web compiler UI
│   ├── index.html
│   ├── app.css
│   └── bosatsu_ui.js
└── demo/                   # Multi-target compilation showcase
    ├── index.html          # Demo page with tabs
    ├── demo.css
    ├── demo.js
    └── examples/           # Pre-compiled outputs
        ├── fibonacci.bosatsu
        ├── _bundle.js      # Generated JS
        ├── fibonacci.c     # Generated C
        └── fibonacci_wasm.js # WASM via emscripten
```

## Implementation Checklist

- [x] Create demo Bosatsu program (`demo/examples/fibonacci.bosatsu`)
- [x] Create demo HTML page (`demo/index.html`)
- [x] Create demo CSS (`demo/demo.css`)
- [x] Create demo JS shim (`demo/demo.js`)
- [x] Create landing page (`web/index.html`)
- [x] Create build script (`scripts/build_demo.sh`)
- [x] Update GitHub Actions workflow (`.github/workflows/deploy_web.yml`)
- [x] Fix bug bot issues (exports, escaping, recursion, string comparison, UTF-16)

## Files Created/Modified

| File | Action | Purpose |
|------|--------|---------|
| `web/index.html` | Created | Landing page |
| `demo/examples/fibonacci.bosatsu` | Created | Demo program |
| `demo/index.html` | Created | Demo page UI |
| `demo/demo.css` | Created | Demo styling |
| `demo/demo.js` | Created | Demo logic (UI shim) |
| `scripts/build_demo.sh` | Created | Local build script |
| `.github/workflows/deploy_web.yml` | Modified | Add demo build steps |

## Bug Fixes Applied

From Cursor bug bot review:

1. **Module exports only first binding** - Fixed to export all bindings
2. **Entry point name not escaped** - Now escapes name to match module exports
3. **Lambda ignores recName** - Uses named function expressions for recursion
4. **String comparison unboxed** - `_cmp_String` returns `[0]/[1]/[2]`
5. **String pattern uses codePoints** - Uses `.length` for UTF-16 units

## Next Steps

- [ ] Merge to main to trigger deployment
- [ ] Verify live site at https://snoble.github.io/bosatsu/
- [ ] Test JS execution in browser
- [ ] Test WASM execution in browser
