#!/bin/bash
# SessionStart hook: ensure the .NET SDK is available in the Claude cloud container.
#
# Euclid targets net6.0/net472 (library), net10.0 (tests) and the project requires
# .NET SDK 10.0+ to build (see README "Prerequisites" and the GitHub workflows,
# which pin dotnet-version '10.x'). Cloud containers don't ship with the SDK, so we
# install it here and restore the project's tools and dependencies.
#
# This runs in async mode: the session starts immediately while the SDK installs in
# the background. See the cloud-session note in README.md / CLAUDE.md.
set -euo pipefail

# Enable async mode so session startup isn't blocked by the SDK download.
echo '{"async": true, "asyncTimeout": 600000}'

# Only run inside Claude Code's remote (web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

DOTNET_CHANNEL="10.0"
DOTNET_ROOT="${DOTNET_ROOT:-$HOME/.dotnet}"

# Install the SDK only if a matching major version isn't already present (idempotent).
if ! "$DOTNET_ROOT/dotnet" --list-sdks 2>/dev/null | grep -q '^10\.'; then
  echo "Installing .NET SDK channel ${DOTNET_CHANNEL}..."
  curl -fsSL https://dot.net/v1/dotnet-install.sh -o /tmp/dotnet-install.sh
  bash /tmp/dotnet-install.sh --channel "$DOTNET_CHANNEL" --install-dir "$DOTNET_ROOT" --no-path
else
  echo ".NET SDK 10 already installed."
fi

export DOTNET_ROOT
export PATH="$DOTNET_ROOT:$DOTNET_ROOT/tools:$PATH"

# Persist the SDK location on PATH for the rest of the session.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  {
    echo "export DOTNET_ROOT=\"$DOTNET_ROOT\""
    echo "export PATH=\"$DOTNET_ROOT:$DOTNET_ROOT/tools:\$PATH\""
    # Skip first-run noise and opt out of telemetry for a quieter, faster CLI.
    echo "export DOTNET_CLI_TELEMETRY_OPTOUT=1"
    echo "export DOTNET_NOLOGO=1"
  } >> "$CLAUDE_ENV_FILE"
fi

echo "Using $(dotnet --version) at $DOTNET_ROOT"

# Restore local tools (Fable, fsdocs) and NuGet dependencies so builds/tests are ready.
dotnet tool restore
dotnet restore

echo ".NET SDK setup complete."
