#!/usr/bin/env bats

load helpers/nb-test-setup

setup() {
  nb_test_setup
}

@test "nb lint runs and produces help via subcommands describe" {
  run nb help lint
  [ "$status" -eq 0 ]
  [[ "$output" =~ "Report broken folder-prefixed" ]]
}

@test "reports a broken non-numeric folder-prefixed link" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
---
title: Alice
---

See [[projects/missing-project]] for context.
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "people/alice.md:5:5: [[projects/missing-project]] -- target not found" ]]
}

@test "resolved non-numeric link is not reported" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/projects"
  nb_test_write "people/alice.md" <<'EOF'
---
title: Alice
---

See [[projects/real]] for context.
EOF
  nb_test_write "projects/real.md" <<'EOF'
---
title: Real
---
EOF

  nb_test_lint
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "resolved numeric id link is not reported" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/projects"
  nb_test_write "projects/real.md" <<'EOF'
---
title: Real
---
EOF
  # Index the new file so nb knows its id
  NB_AUTO_SYNC=0 nb test:index reconcile --ancestors >/dev/null 2>&1 || true

  nb_test_write "people/alice.md" <<'EOF'
---
title: Alice
---

See [[projects/1]] for the first project.
EOF

  nb_test_lint
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "broken numeric id link is reported" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
---
title: Alice
---

See [[projects/999]] for the missing project.
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "people/alice.md:5:5: [[projects/999]] -- target not found" ]]
}

@test "anchor is stripped before resolution (resolved case)" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/projects"
  nb_test_write "projects/real.md" <<'EOF'
---
title: Real
---

## Section A
EOF
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/real#section-a]] for details.
EOF

  nb_test_lint
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "anchor is stripped before resolution (broken case reports as-written)" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/gone#section-a]] for details.
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "[[projects/gone#section-a]] -- target not found" ]]
}

@test "multiple links on one line — only broken reported with correct columns" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/projects"
  nb_test_write "projects/real.md" <<'EOF'
title: Real
EOF
  nb_test_write "people/alice.md" <<'EOF'
Mix of [[projects/real]] and [[projects/missing]] and [[projects/also-missing]].
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  # Should NOT report the resolved one
  [[ ! "$output" =~ "[[projects/real]]" ]]
  # Should report both broken ones with the right columns
  # "Mix of " is 7 chars → first link starts at col 8
  # After "[[projects/real]] and " → second link starts at col 30
  # After "[[projects/missing]] and " → third link starts at col 55
  [[ "$output" =~ "people/alice.md:1:30: [[projects/missing]]" ]]
  [[ "$output" =~ "people/alice.md:1:55: [[projects/also-missing]]" ]]
}

@test "case-mismatched link is reported as broken" {
  mkdir -p "${NB_DIR}/test/projects" "${NB_DIR}/test/people"
  nb_test_write "projects/comp4020.md" <<'EOF'
title: COMP4020
EOF
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/COMP4020]] for the course.
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "[[projects/COMP4020]] -- target not found" ]]
}

@test "bare wikilink without folder is ignored" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
See [[mounts]] and [[some-bare-thing]] — these should be ignored.
EOF

  nb_test_lint
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "--summary on a clean notebook prints zero" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/projects"
  nb_test_write "projects/real.md" <<'EOF'
title: Real
EOF
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/real]].
EOF

  nb_test_lint --summary
  [ "$status" -eq 0 ]
  [[ "$output" == "0 broken wikilinks" ]]
}

@test "--summary on a dirty notebook prints counts and exits 1" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/missing]] and [[projects/also-missing]].
EOF
  nb_test_write "people/bob.md" <<'EOF'
See [[topics/gone]].
EOF

  nb_test_lint --summary
  [ "$status" -eq 1 ]
  # --summary suppresses per-link output; only the summary line is emitted.
  [[ "$output" == "3 broken wikilinks across 2 files" ]]
}

@test "broken link inside data/icloud/ is ignored" {
  mkdir -p "${NB_DIR}/test/data/icloud" "${NB_DIR}/test/people"
  nb_test_write "data/icloud/dump.md" <<'EOF'
This file is gitignored — broken links here don't matter:
[[projects/whatever]]
EOF
  nb_test_write "people/alice.md" <<'EOF'
See [[projects/real-broken]].
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "[[projects/real-broken]]" ]]
  [[ ! "$output" =~ "[[projects/whatever]]" ]]
}

@test "wikilink inside fenced code block is not flagged" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
Here is an example link in docs:

```
nb search "[[projects/example]] --list"
```

And a real one: [[projects/missing]].
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  # Only the second link should be reported, not the one in the fence
  [[ "$output" =~ "[[projects/missing]] -- target not found" ]]
  [[ ! "$output" =~ "[[projects/example]]" ]]
}

@test "wikilink with explicit non-md extension resolves as-named" {
  mkdir -p "${NB_DIR}/test/people" "${NB_DIR}/test/scripts"
  printf "print('hi')\n" > "${NB_DIR}/test/scripts/hello.py"
  nb_test_write "people/alice.md" <<'EOF'
See [[scripts/hello.py]] for the script.
EOF

  nb_test_lint
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "wikilink with explicit non-md extension is broken when missing" {
  mkdir -p "${NB_DIR}/test/people"
  nb_test_write "people/alice.md" <<'EOF'
See [[scripts/missing.py]] for the script.
EOF

  nb_test_lint
  [ "$status" -eq 1 ]
  [[ "$output" =~ "[[scripts/missing.py]] -- target not found" ]]
}
