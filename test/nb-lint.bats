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
