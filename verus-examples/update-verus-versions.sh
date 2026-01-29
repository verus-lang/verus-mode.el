#!/usr/bin/env bash
set -euo pipefail

# Get the latest vstd version from crates.io
latest_version=$(cargo search vstd --limit 1 | grep -oP 'vstd = "\K[^"]+')

if [ -z "$latest_version" ]; then
    echo "Error: Could not fetch latest vstd version from crates.io"
    exit 1
fi

echo "Latest vstd version: $latest_version"

# Find all Cargo.toml files and update vstd dependencies
find . -name "Cargo.toml" -type f | while read -r cargo_file; do
    if grep -q '^vstd = "=' "$cargo_file"; then
        echo "Updating $cargo_file"
        sed -i "s/^vstd = \"=[^\"]*\"/vstd = \"=$latest_version\"/" "$cargo_file"
    fi
done

echo "Done. Updated all Cargo.toml files to vstd version $latest_version"
