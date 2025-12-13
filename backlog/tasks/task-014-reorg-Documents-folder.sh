#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=1
VERBOSE=1

usage() {
  cat <<'EOF'
Reorganise ~/Documents into ~/Code, ~/Documents/research, ~/Documents/teaching.

Safety:
  - Defaults to dry-run (prints actions, does nothing).
  - Refuses to overwrite/merge when a name collision is detected.

Usage:
  task-014-reorg-Documents-folder.sh [--apply] [--quiet]

Options:
  --apply   Execute changes (default is dry-run).
  --quiet   Less logging.
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

ensure_dir() {
  local dir="$1"
  if ! exists "${dir}"; then
    run mkdir -p "${dir}"
  elif ! is_dir "${dir}"; then
    die "Expected directory but found file: ${dir}"
  fi
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

main() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --apply) DRY_RUN=0 ;;
      --quiet) VERBOSE=0 ;;
      -h|--help) usage; exit 0 ;;
      *) die "Unknown argument: $1" ;;
    esac
    shift
  done

  local HOME_DIR DOCS CODE RESEARCH TEACHING
  HOME_DIR="${HOME}"
  DOCS="${HOME_DIR}/Documents"
  CODE="${HOME_DIR}/Code"
  RESEARCH="${DOCS}/research"
  TEACHING="${DOCS}/teaching"

  log "dry-run: ${DRY_RUN} (use --apply to execute)"

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

  # Phase 2: known software repos from edex into existing language buckets.
  ensure_dir "${CODE}/elixir"
  ensure_dir "${CODE}/js"
  safe_move_item "${DOCS}/edex/cozzieloops" "${CODE}/elixir/cozzieloops"
  # fair_dinkum renamed to fair_dinkum_game (collision with different project in Code/elixir)
  safe_move_item "${DOCS}/edex/fair_dinkum" "${CODE}/elixir/fair_dinkum_game"
  safe_move_item "${DOCS}/edex/langchain" "${CODE}/elixir/langchain"
  safe_move_item "${DOCS}/edex/presserbot" "${CODE}/elixir/presserbot"
  safe_move_item "${DOCS}/edex/presserbot-crawler" "${CODE}/elixir/presserbot-crawler"
  safe_move_item "${DOCS}/edex/swarm_grid" "${CODE}/elixir/swarm_grid"
  safe_move_item "${DOCS}/edex/ascii-experiments" "${CODE}/js/ascii-experiments"

  # Phase 3: non-code edex projects into research/studio.
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

  # Phase 4: reorganise research content within ~/Documents/research/.
  # Note: papers, grants, conferences, tools, media already in correct location - no move needed.
  relocate_dir "${RESEARCH}/student-theses" "${RESEARCH}/students/theses"
  relocate_dir "${RESEARCH}/blurbs" "${RESEARCH}/talks/blurbs"
  relocate_dir "${RESEARCH}/rscs-researcher-slides" "${RESEARCH}/talks/rscs-researcher-slides"
  relocate_dir "${RESEARCH}/benswift.me" "${RESEARCH}/website"
  relocate_dir "${RESEARCH}/anu-internal-documents" "${RESEARCH}/admin/anu-internal"

  relocate_dir "${RESEARCH}/ccc-studio" "${RESEARCH}/studio/ccc-studio"
  relocate_dir "${RESEARCH}/ccc-studio-website-eleventy" "${RESEARCH}/studio/ccc-studio-website"

  # Service
  merge_dir_contents_strict "${DOCS}/service" "${RESEARCH}/service"
  relocate_dir "${DOCS}/ace-26-reviews" "${RESEARCH}/service/reviews/ace-26"

  # Archive (old/inactive) - move misc old projects into research/archive/
  relocate_dir "${RESEARCH}/R" "${RESEARCH}/archive/R"
  relocate_dir "${RESEARCH}/SuperCollider" "${RESEARCH}/archive/SuperCollider"
  relocate_dir "${RESEARCH}/altair-sizing-examples" "${RESEARCH}/archive/altair-sizing-examples"
  relocate_dir "${RESEARCH}/anu-cs-blog" "${RESEARCH}/archive/anu-cs-blog"
  relocate_dir "${RESEARCH}/code2k18" "${RESEARCH}/archive/code2k18"
  relocate_dir "${RESEARCH}/codeisbeautiful" "${RESEARCH}/archive/codeisbeautiful"
  relocate_dir "${RESEARCH}/covid19-taskforce" "${RESEARCH}/archive/covid19-taskforce"
  relocate_dir "${RESEARCH}/extemporelang" "${RESEARCH}/archive/extemporelang"
  relocate_dir "${RESEARCH}/francophone-pronunciation-app" "${RESEARCH}/archive/francophone-app"
  relocate_dir "${RESEARCH}/ggerganov" "${RESEARCH}/archive/ggerganov"
  relocate_dir "${RESEARCH}/godot" "${RESEARCH}/archive/godot"
  relocate_dir "${RESEARCH}/jekyll-fontawesome-svg" "${RESEARCH}/archive/jekyll-fontawesome-svg"
  relocate_dir "${RESEARCH}/microbit" "${RESEARCH}/archive/microbit"
  relocate_dir "${RESEARCH}/ned" "${RESEARCH}/archive/ned"
  relocate_dir "${RESEARCH}/skeleton-pic" "${RESEARCH}/archive/skeleton-pic"
  relocate_dir "${RESEARCH}/tidalcycles" "${RESEARCH}/archive/tidalcycles"

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

  # Phase 5: teaching content (reorganise within ~/Documents/teaching/).
  relocate_dir "${TEACHING}/awards" "${TEACHING}/admin/awards"
  relocate_dir "${TEACHING}/curriculum-design" "${TEACHING}/admin/curriculum-design"
  relocate_dir "${TEACHING}/course-peer-reviews" "${TEACHING}/admin/course-peer-reviews"
  relocate_dir "${TEACHING}/higher-education-academy" "${TEACHING}/admin/higher-education-academy"
  relocate_dir "${TEACHING}/letters" "${TEACHING}/admin/letters"

  relocate_dir "${TEACHING}/comp-course-homepage" "${TEACHING}/tools/comp-course-homepage"
  relocate_dir "${TEACHING}/marking" "${TEACHING}/tools/marking"
  relocate_dir "${TEACHING}/enrolment-datavis" "${TEACHING}/tools/enrolment-datavis"
  relocate_dir "${TEACHING}/cs-projects-visualisation" "${TEACHING}/tools/cs-projects-visualisation"
  # Move contents of teaching/tools into teaching/tools/misc (can't move folder into itself)
  ensure_dir "${TEACHING}/tools/misc"
  while IFS= read -r -d '' entry; do
    local name
    name="$(basename "${entry}")"
    # Skip the new subdirs we're creating
    if [[ "${name}" == "comp-course-homepage" ]] || [[ "${name}" == "marking" ]] || \
       [[ "${name}" == "enrolment-datavis" ]] || [[ "${name}" == "cs-projects-visualisation" ]] || \
       [[ "${name}" == "misc" ]]; then
      continue
    fi
    safe_move_item "${entry}" "${TEACHING}/tools/misc/${name}"
  done < <(find "${TEACHING}/tools" -mindepth 1 -maxdepth 1 -print0 2>/dev/null || true)

  # Archive - teaching/archive already exists with content, move other courses into it
  relocate_dir "${TEACHING}/comp1100-2020-s2-website" "${TEACHING}/archive/comp1100-2020-s2"
  relocate_dir "${TEACHING}/comp2710-lens-2021" "${TEACHING}/archive/comp2710-lens-2021"
  relocate_dir "${TEACHING}/comp4610-2021" "${TEACHING}/archive/comp4610-2021"
  relocate_dir "${TEACHING}/extn1019" "${TEACHING}/archive/extn1019"
  relocate_dir "${TEACHING}/socy-adire" "${TEACHING}/archive/socy-adire"
  relocate_dir "${TEACHING}/ccc-studio" "${TEACHING}/archive/ccc-studio"
  relocate_dir "${TEACHING}/cs-outreach-hub" "${TEACHING}/archive/cs-outreach-hub"
  relocate_dir "${TEACHING}/comp-course-grade-bimodality" "${TEACHING}/archive/comp-course-grade-bimodality"

  # Phase 6: rename and cleanup in Documents.
  if exists "${DOCS}/business"; then
    if ! exists "${DOCS}/admin"; then
      run mv "${DOCS}/business" "${DOCS}/admin"
    else
      merge_dir_contents_strict "${DOCS}/business" "${DOCS}/admin"
      run rmdir "${DOCS}/business" 2>/dev/null || true
    fi
  fi

  # Delete cruft (confirmed safe to remove).
  if exists "${DOCS}/edx"; then run rm -rf "${DOCS}/edx"; fi
  if exists "${DOCS}/paperless-ngx"; then run rm -rf "${DOCS}/paperless-ngx"; fi
  if exists "${DOCS}/personal/__MACOSX"; then run rm -rf "${DOCS}/personal/__MACOSX"; fi
  if exists "${DOCS}/REORGANISATION-PLAN.md"; then run rm "${DOCS}/REORGANISATION-PLAN.md"; fi
  if exists "${DOCS}/notes"; then run rm -rf "${DOCS}/notes"; fi
  if exists "${DOCS}/md-scratch"; then run rm -rf "${DOCS}/md-scratch"; fi

  # Attempt to remove now-empty staging folders.
  run rmdir "${DOCS}/edex" 2>/dev/null || true

  log "done"
  if [[ "${DRY_RUN}" -eq 1 ]]; then
    log "re-run with: --apply"
  fi
}

main "$@"
