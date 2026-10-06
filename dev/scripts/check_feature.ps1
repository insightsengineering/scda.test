$ErrorActionPreference = "Stop"

Set-Location (git rev-parse --show-toplevel)

Write-Host "🌿 Creating branch '{{branch_name}}'..."
$result = git checkout -b {{branch_name}} 2>&1
if ($LASTEXITCODE -ne 0) {
  Write-Host "⚠️ Branch exists, switching..."
  git checkout {{branch_name}}
}

Write-Host "✅ Branch '{{branch_name}}' ready!"
