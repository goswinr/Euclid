# Euclid 0.52.0 API baseline

These tables were generated from release commit `d5d8cacb2f3c51931f978c7dec7a0d992a4fe6c2`
using the current `Scripts/ApiTable.fsx`, copied unchanged into a temporary archive
of that commit. The script successfully type checked all 52 historical source files.

| Baseline | Current |
|:--|:--|
| [Combined API](ApiTable.md) | [Combined API](../ApiTable.md) |
| [2D API](ApiTable2D.md) | [2D API](../ApiTable2D.md) |
| [3D API](ApiTable3D.md) | [3D API](../ApiTable3D.md) |

To compare side by side in VS Code, run from the repository root:

```powershell
code --reuse-window --diff Scripts/0.52/ApiTable.md Scripts/ApiTable.md
code --reuse-window --diff Scripts/0.52/ApiTable2D.md Scripts/ApiTable2D.md
code --reuse-window --diff Scripts/0.52/ApiTable3D.md Scripts/ApiTable3D.md
```

The 0.52 baseline appears on the left and the current API on the right.

To compare in the terminal from the repository root:

```powershell
git diff --no-index -- Scripts/0.52/ApiTable.md Scripts/ApiTable.md
git diff --no-index -- Scripts/0.52/ApiTable2D.md Scripts/ApiTable2D.md
git diff --no-index -- Scripts/0.52/ApiTable3D.md Scripts/ApiTable3D.md
```

`git diff --no-index` returns exit code 1 when the files differ.
