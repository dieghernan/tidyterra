#!/usr/bin/env bash
set -euo pipefail

branch=refs/heads/ai-search-manifest
remote="$GITHUB_SERVER_URL/$GITHUB_REPOSITORY.git"
state="$RUNNER_TEMP/ai-search-manifest-$GITHUB_RUN_ID-$GITHUB_RUN_ATTEMPT"

state_git() {
  git -C "$state" -c credential.helper= -c 'credential.helper=!gh auth git-credential' "$@"
}

case "${1:-}" in
  restore)
    git init --quiet --initial-branch=ai-search-manifest "$state"
    if state_git ls-remote --exit-code "$remote" "$branch" > /dev/null; then
      state_git fetch --quiet --depth=1 "$remote" "$branch"
      state_git reset --quiet --mixed FETCH_HEAD
      manifest_entry=$(state_git ls-tree HEAD -- manifest.json)
      if [[ "$manifest_entry" ]]; then
        state_git show HEAD:manifest.json > manifest.json
      fi
    else
      status=$?
      if [[ "$status" != 2 ]]; then
        exit "$status"
      fi
      echo "Manifest branch does not exist yet."
    fi
    ;;
  save)
    cp manifest.json "$state/manifest.json"
    state_git add -- manifest.json
    if state_git diff --cached --quiet; then
      echo "Manifest is unchanged."
      exit 0
    fi
    state_git -c user.name='github-actions[bot]' \
      -c user.email='41898282+github-actions[bot]@users.noreply.github.com' \
      commit --quiet -m "Update AI Search manifest"
    state_git push "$remote" "HEAD:$branch"
    ;;
  *)
    echo "Usage: bash manifest-branch.sh {restore|save}" >&2
    exit 2
    ;;
esac
