#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

echo "📡 Fetching {{upstream}}..."
git fetch {{upstream}}

echo "🔄 Resetting {{branch}} to {{upstream}}/{{branch}}..."
git checkout {{branch}}
git reset --hard {{upstream}}/{{branch}}

echo "✅ Done! {{branch}} synced with {{upstream}}/{{branch}}."
