# Shell functions for the shared manager build (.github/workflows/manager-build.yml)
# and the per-manager steps in data_processing/<manager>/ci/. Source this file; the
# functions use the gh CLI with GH_TOKEN, against the repository named in REPO.

# Download one release asset and check it against the SHA-256 digest GitHub publishes.
download_verified() {  # <tag> <asset> <dir>
  local digest
  digest=$(gh api "repos/$REPO/releases/tags/$1" \
    --jq ".assets[] | select(.name == \"$2\") | .digest" | sed 's/^sha256://')
  [[ "$digest" =~ ^[0-9a-f]{64}$ ]] || { echo "No digest for $2 in $1"; return 1; }
  mkdir -p "$3"
  gh release download "$1" --repo "$REPO" --pattern "$2" --dir "$3" --clobber
  echo "$digest  $3/$2" | sha256sum -c -
}

# The dated release a *-latest alias names on its "**<label>:**" line ("Promoted from"
# for managers, "Snapshot" for sections, "Points to" for raw data and dependencies).
# Any other tag is its own dated release. Prints nothing if the line is missing.
dated_release() {  # <tag> <label>
  if [[ "$1" != *latest* ]]; then echo "$1"; return; fi
  gh release view "$1" --repo "$REPO" --json body -q .body \
    | sed -E -n "s/^\*\*$2:\*\* \`([^\`]*)\`.*/\1/p" | head -1
}

# Append to the build's record of its inputs (key=value lines, then file digests).
record_input() {  # <key> <value>
  echo "$1=$2" >> _ci_outputs/input_identity.txt
}
record_digests() {  # <file> ...
  sha256sum "$@" >> _ci_outputs/input_identity.txt
}

# Add a row to the release notes' Build Info table.
build_info_row() {  # <input> <release> <exact version>
  echo "| **$1** | $2 | $3 |" >> _ci_outputs/build_info_rows.md
}
