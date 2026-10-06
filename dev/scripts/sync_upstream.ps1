$ErrorActionPreference = "Stop"

Set-Location (git rev-parse --show-toplevel)

Write-Host "📡 Fetching {{upstream}}..."
git fetch {{upstream}}

Write-Host "🔄 Resetting {{branch}} to {{upstream}}/{{branch}}..."
git checkout {{branch}}
git reset --hard {{upstream}}/{{branch}}

Write-Host "✅ Done! {{branch}} synced with {{upstream}}/{{branch}}."
