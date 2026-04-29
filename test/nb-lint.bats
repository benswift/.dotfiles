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
