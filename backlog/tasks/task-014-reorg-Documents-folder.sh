#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=1
VERBOSE=1
MOVE_GIT_REPOS_TO_CODE_ARCHIVE=0

usage() {
  cat <<'EOF'
Reorganise ~/Documents into ~/Code, ~/Research, ~/Teaching.

Safety:
  - Defaults to dry-run (prints actions, does nothing).
  - Refuses to overwrite/merge when a name collision is detected.
  - Moves "delete" targets into a trash folder (no rm) unless you later delete manually.

Usage:
  task-014-reorg-Documents-folder.sh [--apply] [--quiet] [--move-git-repos-to-code-archive]

Options:
  --apply   Execute changes (default is dry-run).
  --quiet   Less logging.
  --move-git-repos-to-code-archive
            Move any remaining git repos under ~/Documents into ~/Code/archive/from-documents/.
EOF
}

log() {
  if [[ "${VERBOSE}" -eq 1 ]]; then
    printf '%s\n' "$*" >&2
  fi
}

die() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

run() {
  if [[ "${DRY_RUN}" -eq 1 ]]; then
    printf '[dry-run] %s\n' "$*" >&2
  else
    log "+ $*"
    "$@"
  fi
}

exists() { [[ -e "$1" ]]; }
is_dir() { [[ -d "$1" ]]; }

is_git_repo_dir() {
  local dir="$1"
  [[ -d "${dir}/.git" ]] || [[ -f "${dir}/.git" ]]
}

ensure_dir() {
  local dir="$1"
  if ! exists "${dir}"; then
    run mkdir -p "${dir}"
  elif ! is_dir "${dir}"; then
    die "Expected directory but found file: ${dir}"
  fi
}

short_hash() {
  local s="$1"
  printf '%s' "${s}" | shasum -a 256 | awk '{print substr($1,1,8)}'
}

safe_move_item() {
  local src="$1"
  local dest="$2"

  if ! exists "${src}"; then
    log "skip (missing): ${src}"
    return 0
  fi

  if exists "${dest}"; then
    die "Destination exists (refusing to overwrite): ${dest} (from ${src})"
  fi

  ensure_dir "$(dirname "${dest}")"
  run mv "${src}" "${dest}"
}

merge_dir_contents_strict() {
  local src_dir="$1"
  local dest_dir="$2"

  if ! is_dir "${src_dir}"; then
    log "skip (missing dir): ${src_dir}"
    return 0
  fi

  ensure_dir "${dest_dir}"

  local had_entries=0
  while IFS= read -r -d '' entry; do
    had_entries=1
    local name
    name="$(basename "${entry}")"
    if exists "${dest_dir}/${name}"; then
      die "Name collision: ${dest_dir}/${name} (from ${src_dir})"
    fi
    run mv "${entry}" "${dest_dir}/"
  done < <(find "${src_dir}" -mindepth 1 -maxdepth 1 -print0)

  if [[ "${had_entries}" -eq 1 ]]; then
    run rmdir "${src_dir}" 2>/dev/null || true
  fi
}

relocate_dir() {
  local src_dir="$1"
  local dest_dir="$2"

  if ! exists "${src_dir}"; then
    log "skip (missing): ${src_dir}"
    return 0
  fi

  if exists "${dest_dir}"; then
    merge_dir_contents_strict "${src_dir}" "${dest_dir}"
    run rmdir "${src_dir}" 2>/dev/null || true
    return 0
  fi

  ensure_dir "$(dirname "${dest_dir}")"
  run mv "${src_dir}" "${dest_dir}"
}

relocate_to_code_archive_if_repo() {
  local repo_dir="$1"
  local code_archive="$2"

  if ! is_dir "${repo_dir}"; then
    return 0
  fi
  if ! is_git_repo_dir "${repo_dir}"; then
    return 0
  fi

  local name hash dest
  name="$(basename "${repo_dir}")"
  hash="$(short_hash "${repo_dir}")"
  dest="${code_archive}/${name}-${hash}"

  if exists "${dest}"; then
    log "skip (already moved?): ${repo_dir} -> ${dest}"
    return 0
  fi

  safe_move_item "${repo_dir}" "${dest}"
}

relocate_project_prefer_dest_else_code_archive() {
  local src_dir="$1"
  local dest_dir="$2"
  local code_archive="$3"

  if ! exists "${src_dir}"; then
    log "skip (missing): ${src_dir}"
    return 0
  fi

  if ! exists "${dest_dir}"; then
    ensure_dir "$(dirname "${dest_dir}")"
    run mv "${src_dir}" "${dest_dir}"
    return 0
  fi

  # Dest exists already; avoid merging/overwriting and move the source repo
  # into the Code archive so we don't leave projects under Documents.
  local name hash dest
  name="$(basename "${src_dir}")"
  hash="$(short_hash "${src_dir}")"
  dest="${code_archive}/${name}-${hash}"
  safe_move_item "${src_dir}" "${dest}"
}

main() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --apply) DRY_RUN=0 ;;
      --quiet) VERBOSE=0 ;;
      --move-git-repos-to-code-archive) MOVE_GIT_REPOS_TO_CODE_ARCHIVE=1 ;;
      -h|--help) usage; exit 0 ;;
      *) die "Unknown argument: $1" ;;
    esac
    shift
  done

  local HOME_DIR DOCS CODE RESEARCH TEACHING
  HOME_DIR="${HOME}"
  DOCS="${HOME_DIR}/Documents"
  CODE="${HOME_DIR}/Code"
  RESEARCH="${HOME_DIR}/Research"
  TEACHING="${HOME_DIR}/Teaching"

  local TRASH
  TRASH="${DOCS}/_reorg-trash/$(date +%Y%m%d-%H%M%S)"

  log "dry-run: ${DRY_RUN} (use --apply to execute)"
  log "trash: ${TRASH}"

  # Create target directories (minimal; leaf dirs are created on-demand).
  ensure_dir "${RESEARCH}"
  ensure_dir "${TEACHING}"
  ensure_dir "${RESEARCH}/students/confirmations"
  ensure_dir "${RESEARCH}/students/theses"
  ensure_dir "${RESEARCH}/service/reviews"
  ensure_dir "${RESEARCH}/studio/admin"
  ensure_dir "${RESEARCH}/admin/anu-internal"
  ensure_dir "${RESEARCH}/archive"
  ensure_dir "${TEACHING}/admin"
  ensure_dir "${TEACHING}/tools"
  ensure_dir "${TEACHING}/archive"

  # Ensure Code archive exists for the many repos currently under Documents.
  local CODE_ARCHIVE
  CODE_ARCHIVE="${CODE}/archive/from-documents"
  ensure_dir "${CODE_ARCHIVE}"

  # Phase 2: known software repos from edex into existing language buckets.
  ensure_dir "${CODE}/elixir"
  ensure_dir "${CODE}/js"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/cozzieloops" "${CODE}/elixir/cozzieloops" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/fair_dinkum" "${CODE}/elixir/fair_dinkum" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/langchain" "${CODE}/elixir/langchain" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/presserbot" "${CODE}/elixir/presserbot" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/presserbot-crawler" "${CODE}/elixir/presserbot-crawler" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/swarm_grid" "${CODE}/elixir/swarm_grid" "${CODE_ARCHIVE}"
  relocate_project_prefer_dest_else_code_archive "${DOCS}/edex/ascii-experiments" "${CODE}/js/ascii-experiments" "${CODE_ARCHIVE}"

  # Phase 3: non-code edex projects into Research/studio.
  relocate_dir "${DOCS}/edex/ai-art" "${RESEARCH}/studio/ai-art"
  relocate_dir "${DOCS}/edex/australian-cybernetic-scripts" "${RESEARCH}/studio/australian-cybernetic-scripts"
  relocate_dir "${DOCS}/edex/cybernetic-futures" "${RESEARCH}/studio/cybernetic-futures"
  relocate_dir "${DOCS}/edex/human-scale-ai" "${RESEARCH}/studio/human-scale-ai"
  relocate_dir "${DOCS}/edex/llms-unplugged" "${RESEARCH}/studio/llms-unplugged"
  relocate_dir "${DOCS}/edex/metrics" "${RESEARCH}/studio/metrics"
  relocate_dir "${DOCS}/edex/nga" "${RESEARCH}/studio/nga"
  relocate_dir "${DOCS}/edex/panic" "${RESEARCH}/studio/panic"
  relocate_dir "${DOCS}/edex/recruitment" "${RESEARCH}/studio/recruitment"
  relocate_dir "${DOCS}/edex/reg" "${RESEARCH}/studio/reg"
  relocate_dir "${DOCS}/edex/shitposting" "${RESEARCH}/studio/shitposting"
  relocate_dir "${DOCS}/edex/studio-bites" "${RESEARCH}/studio/studio-bites"
  relocate_dir "${DOCS}/edex/systems-modelling" "${RESEARCH}/studio/systems-modelling"
  relocate_dir "${DOCS}/edex/working_with_name_for_dummies" "${RESEARCH}/studio/working_with_name_for_dummies"

  merge_dir_contents_strict "${DOCS}/studio-documents" "${RESEARCH}/studio/admin"

  # Phase 4: research content into Research/.
  relocate_dir "${DOCS}/research/papers" "${RESEARCH}/papers"
  relocate_dir "${DOCS}/research/grants" "${RESEARCH}/grants"
  relocate_dir "${DOCS}/research/conferences" "${RESEARCH}/conferences"
  relocate_dir "${DOCS}/research/student-theses" "${RESEARCH}/students/theses"
  relocate_dir "${DOCS}/research/blurbs" "${RESEARCH}/talks/blurbs"
  relocate_dir "${DOCS}/research/rscs-researcher-slides" "${RESEARCH}/talks/rscs-researcher-slides"
  relocate_dir "${DOCS}/research/benswift.me" "${RESEARCH}/website"
  relocate_dir "${DOCS}/research/tools" "${RESEARCH}/tools"
  relocate_dir "${DOCS}/research/media" "${RESEARCH}/media"
  relocate_dir "${DOCS}/research/anu-internal-documents" "${RESEARCH}/admin/anu-internal"

  relocate_dir "${DOCS}/research/ccc-studio" "${RESEARCH}/studio/ccc-studio"
  relocate_dir "${DOCS}/research/ccc-studio-website-eleventy" "${RESEARCH}/studio/ccc-studio-website"

  # Service
  merge_dir_contents_strict "${DOCS}/service" "${RESEARCH}/service"
  relocate_dir "${DOCS}/ace-26-reviews" "${RESEARCH}/service/reviews/ace-26"

  # Archive (old/inactive)
  merge_dir_contents_strict "${DOCS}/research/archive" "${RESEARCH}/archive"
  relocate_dir "${DOCS}/research/R" "${RESEARCH}/archive/R"
  relocate_dir "${DOCS}/research/SuperCollider" "${RESEARCH}/archive/SuperCollider"
  relocate_dir "${DOCS}/research/altair-sizing-examples" "${RESEARCH}/archive/altair-sizing-examples"
  relocate_dir "${DOCS}/research/anu-cs-blog" "${RESEARCH}/archive/anu-cs-blog"
  relocate_dir "${DOCS}/research/code2k18" "${RESEARCH}/archive/code2k18"
  relocate_dir "${DOCS}/research/codeisbeautiful" "${RESEARCH}/archive/codeisbeautiful"
  relocate_dir "${DOCS}/research/covid19-taskforce" "${RESEARCH}/archive/covid19-taskforce"
  relocate_dir "${DOCS}/research/extemporelang" "${RESEARCH}/archive/extemporelang"
  relocate_dir "${DOCS}/research/francophone-pronunciation-app" "${RESEARCH}/archive/francophone-app"
  relocate_dir "${DOCS}/research/ggerganov" "${RESEARCH}/archive/ggerganov"
  relocate_dir "${DOCS}/research/godot" "${RESEARCH}/archive/godot"
  relocate_dir "${DOCS}/research/jekyll-fontawesome-svg" "${RESEARCH}/archive/jekyll-fontawesome-svg"
  relocate_dir "${DOCS}/research/microbit" "${RESEARCH}/archive/microbit"
  relocate_dir "${DOCS}/research/ned" "${RESEARCH}/archive/ned"
  relocate_dir "${DOCS}/research/skeleton-pic" "${RESEARCH}/archive/skeleton-pic"
  relocate_dir "${DOCS}/research/tidalcycles" "${RESEARCH}/archive/tidalcycles"

  # Loose PDFs in Documents root.
  if exists "${DOCS}"; then
    local pdf_found=0
    while IFS= read -r -d '' pdf; do
      pdf_found=1
      safe_move_item "${pdf}" "${RESEARCH}/students/confirmations/$(basename "${pdf}")"
    done < <(find "${DOCS}" -maxdepth 1 -type f -name '*.pdf' -print0)
    if [[ "${pdf_found}" -eq 0 ]]; then
      log "skip (no PDFs in ${DOCS} root)"
    fi
  fi

  # Phase 5: teaching content into Teaching/.
  relocate_dir "${DOCS}/teaching/awards" "${TEACHING}/admin/awards"
  relocate_dir "${DOCS}/teaching/curriculum-design" "${TEACHING}/admin/curriculum-design"
  relocate_dir "${DOCS}/teaching/course-peer-reviews" "${TEACHING}/admin/course-peer-reviews"
  relocate_dir "${DOCS}/teaching/higher-education-academy" "${TEACHING}/admin/higher-education-academy"
  relocate_dir "${DOCS}/teaching/letters" "${TEACHING}/admin/letters"

  relocate_dir "${DOCS}/teaching/comp-course-homepage" "${TEACHING}/tools/comp-course-homepage"
  relocate_dir "${DOCS}/teaching/marking" "${TEACHING}/tools/marking"
  relocate_dir "${DOCS}/teaching/enrolment-datavis" "${TEACHING}/tools/enrolment-datavis"
  relocate_dir "${DOCS}/teaching/cs-projects-visualisation" "${TEACHING}/tools/cs-projects-visualisation"
  relocate_dir "${DOCS}/teaching/tools" "${TEACHING}/tools/misc"

  merge_dir_contents_strict "${DOCS}/teaching/archive" "${TEACHING}/archive"
  relocate_dir "${DOCS}/teaching/comp1100-2020-s2-website" "${TEACHING}/archive/comp1100-2020-s2"
  relocate_dir "${DOCS}/teaching/comp2710-lens-2021" "${TEACHING}/archive/comp2710-lens-2021"
  relocate_dir "${DOCS}/teaching/comp4610-2021" "${TEACHING}/archive/comp4610-2021"
  relocate_dir "${DOCS}/teaching/extn1019" "${TEACHING}/archive/extn1019"
  relocate_dir "${DOCS}/teaching/socy-adire" "${TEACHING}/archive/socy-adire"
  relocate_dir "${DOCS}/teaching/ccc-studio" "${TEACHING}/archive/ccc-studio"
  relocate_dir "${DOCS}/teaching/cs-outreach-hub" "${TEACHING}/archive/cs-outreach-hub"
  relocate_dir "${DOCS}/teaching/comp-course-grade-bimodality" "${TEACHING}/archive/comp-course-grade-bimodality"

  # Phase 6: rename and cleanup in Documents (no deletes; trash instead).
  if exists "${DOCS}/business"; then
    if ! exists "${DOCS}/admin"; then
      run mv "${DOCS}/business" "${DOCS}/admin"
    else
      merge_dir_contents_strict "${DOCS}/business" "${DOCS}/admin"
      run rmdir "${DOCS}/business" 2>/dev/null || true
    fi
  fi

  # Move "cruft" into trash folder (instead of rm -rf).
  ensure_dir "${TRASH}"
  if exists "${DOCS}/edx"; then safe_move_item "${DOCS}/edx" "${TRASH}/edx"; fi
  if exists "${DOCS}/paperless-ngx"; then safe_move_item "${DOCS}/paperless-ngx" "${TRASH}/paperless-ngx"; fi
  if exists "${DOCS}/personal/__MACOSX"; then safe_move_item "${DOCS}/personal/__MACOSX" "${TRASH}/__MACOSX"; fi
  if exists "${DOCS}/REORGANISATION-PLAN.md"; then safe_move_item "${DOCS}/REORGANISATION-PLAN.md" "${TRASH}/REORGANISATION-PLAN.md"; fi

  if [[ "${MOVE_GIT_REPOS_TO_CODE_ARCHIVE}" -eq 1 ]]; then
    # Move any remaining git repos under Documents into Code archive.
    # Note: this is optional because many non-code document collections are also git repos.
    if exists "${DOCS}"; then
      while IFS= read -r -d '' git_path; do
        local repo_dir
        repo_dir="$(dirname "${git_path}")"
        relocate_to_code_archive_if_repo "${repo_dir}" "${CODE_ARCHIVE}"
      done < <(
        find "${DOCS}" \
          -path "${DOCS}/_reorg-trash" -prune -o \
          \( -type d -name .git -o -type f -name .git \) -print0
      )
    fi
  fi

  # Attempt to remove now-empty staging folders.
  run rmdir "${DOCS}/edex" 2>/dev/null || true
  run rmdir "${DOCS}/research" 2>/dev/null || true
  run rmdir "${DOCS}/teaching" 2>/dev/null || true

  log "done"
  if [[ "${DRY_RUN}" -eq 1 ]]; then
    log "re-run with: --apply"
  fi
}

main "$@"
