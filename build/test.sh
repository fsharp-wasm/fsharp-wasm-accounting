#!/usr/bin/env bash
# test.sh — the suites of the accounting grid under .NET.   bash build/test.sh
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
source "$ROOT/build/env.sh"
dotnet run --project "$ROOT/tests/Accounting.Tests/Accounting.Tests.fsproj" -c Release --nologo -v q
