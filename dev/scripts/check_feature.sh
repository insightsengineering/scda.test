#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

echo "🌿 Creating branch '{{branch_name}}'..."
git checkout -b {{branch_name}} || { echo "⚠️ Branch exists, switching..."; git checkout {{branch_name}}; }

echo "✅ Branch '{{branch_name}}' ready!"
