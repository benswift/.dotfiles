#!/usr/bin/env bash
#
# Shared bats helpers for nb plugin tests. Each test gets an isolated
# NB_DIR under $BATS_TEST_TMPDIR so tests don't pollute the user's
# real notebook.

# Set up an isolated nb home with the plugin under test installed.
# Creates a notebook called "test" and exports NB_DIR.
nb_test_setup() {
  export NB_DIR="${BATS_TEST_TMPDIR}/nb"
  mkdir -p "${NB_DIR}/.plugins"
  # BATS_TEST_DIRNAME is the directory containing the .bats file.
  # We need to go up to the repo root, then into nb/.
  local plugin_src="$(cd "${BATS_TEST_DIRNAME}/.." && pwd)/nb/lint.nb-plugin"
  ln -s "${plugin_src}" "${NB_DIR}/.plugins/lint.nb-plugin"
  # nb auto-creates "home" notebook on first use. Suppress the welcome
  # banner with NB_AUTO_SYNC=0 + a discarded init call.
  NB_AUTO_SYNC=0 nb notebooks add home >/dev/null 2>&1
  NB_AUTO_SYNC=0 nb notebooks add test >/dev/null 2>&1
}

# Create a note inside the test notebook by writing a file directly to
# the notebook path. Bypasses `nb add` for speed and to control the
# filename precisely. Caller passes <relative-path> and reads content
# from stdin.
nb_test_write() {
  local _rel="$1"
  local _path="${NB_DIR}/test/${_rel}"
  mkdir -p "$(dirname "${_path}")"
  cat > "${_path}"
}

# Run `nb lint` against the test notebook with a clean PATH for nb.
# Uses BATS run so $status and $output are populated.
nb_test_lint() {
  NB_AUTO_SYNC=0 run nb test:lint "$@"
}
