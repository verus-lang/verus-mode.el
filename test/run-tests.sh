#!/usr/bin/env bash
# run-tests.sh --- Run verus-mode integration tests

set -e

# Parse arguments
PATTERN="${1:-}"
SKIP_CACHE_PRIMING="${SKIP_CACHE_PRIMING:-}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get the directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo -e "${YELLOW}Running verus-mode tests...${NC}"
if [ -n "$PATTERN" ]; then
    echo -e "Filter pattern: ${GREEN}$PATTERN${NC}"
fi
if [ -n "$VERBOSE" ]; then
    echo -e "Verbose mode: ${GREEN}enabled${NC}"
fi
if [ -n "$INTERACTIVE" ]; then
    echo -e "Interactive mode: ${GREEN}enabled${NC}"
fi
echo ""

# Either one of VERUS_HOME or .emacs-sandbox/verus-home should exist; we initialize the file (later) if the latter is missing
if [ -z "$VERUS_HOME" ] && [ -f "$PROJECT_ROOT/.emacs-sandbox/verus-home" ]; then
    VERUS_HOME="$(cat "$PROJECT_ROOT/.emacs-sandbox/verus-home")"
    export VERUS_HOME
    echo -e "Using VERUS_HOME from sandbox file: ${GREEN}$VERUS_HOME${NC}"
    echo ""
fi

# Check for VERUS_HOME
if [ -z "$VERUS_HOME" ]; then
    echo -e "${RED}ERROR: VERUS_HOME is not set${NC}"
    echo "Please set VERUS_HOME to the path of your Verus installation"
    echo "Example: export VERUS_HOME=/path/to/verus"
    exit 1
fi

if [ ! -d "$VERUS_HOME" ]; then
    echo -e "${RED}ERROR: VERUS_HOME directory does not exist: $VERUS_HOME${NC}"
    exit 1
fi

echo -e "Using VERUS_HOME: ${GREEN}$VERUS_HOME${NC}"
echo ""

# Use the existing .emacs-sandbox directory
SANDBOX_DIR="$PROJECT_ROOT/.emacs-sandbox"

if [ ! -d "$SANDBOX_DIR" ]; then
    echo -e "${YELLOW}Sandbox directory doesn't exist, creating it...${NC}"
    echo "Note: Run 'just emacs-sandbox' first to set up dependencies if tests fail"
    mkdir -p "$SANDBOX_DIR"
fi

if [ ! -f "$SANDBOX_DIR/verus-home" ]; then
    echo -e "${YELLOW}Creating verus-home file in sandbox...${NC}"
    echo "$VERUS_HOME" >"$SANDBOX_DIR/verus-home"
fi

echo "Using sandbox: $SANDBOX_DIR"
echo ""

cd "$PROJECT_ROOT"

# Prime Verus caches before running tests.
#
# On a cold run, the first verification of each example project can take much
# longer than flycheck's/the tests' timeouts (dependency builds, vstd fetching
# and verification, etc.), which causes spurious test failures.  Running the
# same commands the tests would trigger, ahead of time and without a timeout,
# ensures the caches are warm by the time the tests actually run.
prime_caches() {
    local verus_bin=""
    local loc
    for loc in "source/target-verus/release/verus" \
        "source/target-verus/debug/verus" \
        "source/target-verus/release/verus.exe" \
        "source/target-verus/debug/verus.exe" \
        "source/tools/rust-verify.sh"; do
        if [ -x "$VERUS_HOME/$loc" ]; then
            verus_bin="$VERUS_HOME/$loc"
            break
        fi
    done

    if [ -z "$verus_bin" ]; then
        echo -e "${RED}ERROR: Could not find a Verus binary under $VERUS_HOME${NC}"
        exit 1
    fi

    local examples="$PROJECT_ROOT/verus-examples"

    # Standalone files/crates, verified directly with the Verus binary.
    run_prime "$examples" "$verus_bin" syntax.rs
    run_prime "$examples/crate" "$verus_bin" --crate-type=lib src/lib.rs

    # cargo-verus projects, verified through `cargo verus'.  We run both
    # `verify' and `focus' (the latter with the same flags flycheck uses),
    # since they build/cache different artifacts.
    run_prime "$examples/cv-crate" cargo verus verify --
    run_prime "$examples/cv-crate" cargo verus focus --message-format=json -- \
        --verify-module foo::bar --expand-errors
    run_prime "$examples/cv-crate" cargo verus focus --message-format=json -- \
        --verify-root --expand-errors
    run_prime "$examples/cv-workspace" cargo verus verify -p member1 --message-format=json -- --expand-errors
    run_prime "$examples/cv-workspace" cargo verus verify -p member2 --message-format=json -- --expand-errors
}

# Run a cache-priming command in a given directory; failures are non-fatal,
# since some examples intentionally contain verification errors.
run_prime() {
    local dir="$1"
    shift
    echo -e "  ${GREEN}[$(basename "$dir")]${NC} $*"
    if [ -n "$VERBOSE" ]; then
        (cd "$dir" && "$@") || true
    else
        (cd "$dir" && "$@") >/dev/null 2>&1 || true
    fi
}

if [ -z "$SKIP_CACHE_PRIMING" ]; then
    echo -e "${YELLOW}Priming Verus caches (this may take a while on a cold run)...${NC}"
    prime_caches
    echo ""
else
    echo -e "${YELLOW}Skipping cache priming (SKIP_CACHE_PRIMING is set)${NC}"
    echo ""
fi

# Run tests using with-emacs.sh
echo ""
echo -e "${YELLOW}Running integration tests...${NC}"
echo ""

# Build emacs args
EMACS_ARGS=(
    -L .
    -l ert
    -l verus-mode.el
    -l test/integration-test.el
    --eval "(setq ert-batch-backtrace-right-margin 120)"
)

if [ -n "$INTERACTIVE" ]; then
    EMACS_ARGS+=(--eval "(setq debug-on-error t)")
else
    EMACS_ARGS+=(--batch)
fi

if [ -n "$VERBOSE" ]; then
    EMACS_ARGS+=(--eval "(setq ert-batch-print-level 10)")
    EMACS_ARGS+=(--eval "(setq ert-batch-print-length 100)")
fi

if [ -n "$INTERACTIVE" ]; then
    if [ -n "$PATTERN" ]; then
        EMACS_ARGS+=(--eval "(ert \"$PATTERN\")")
    else
        EMACS_ARGS+=(-f ert)
    fi
else
    if [ -n "$PATTERN" ]; then
        EMACS_ARGS+=(--eval "(ert-run-tests-batch-and-exit \"$PATTERN\")")
    else
        EMACS_ARGS+=(-f ert-run-tests-batch-and-exit)
    fi
fi

if [ ! -f "$PROJECT_ROOT/.bin/with-emacs.sh" ]; then
    echo -e "${YELLOW}with-emacs.sh not found, downloading...${NC}"
    mkdir -p "$PROJECT_ROOT/.bin"
    wget https://raw.githubusercontent.com/alphapapa/with-emacs.sh/0bc4f216ed101d86d2e5d52919bad39bc041bdbe/with-emacs.sh -O "$PROJECT_ROOT/.bin/with-emacs.sh"
    chmod +x "$PROJECT_ROOT/.bin/with-emacs.sh"
fi

"$PROJECT_ROOT/.bin/with-emacs.sh" \
    --no-refresh-packages \
    --dir "$SANDBOX_DIR" \
    -- \
    "${EMACS_ARGS[@]}"

TEST_RESULT=$?

echo ""
if [ $TEST_RESULT -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed${NC}"
    echo -e "${YELLOW}Hint: make sure you have a recent version of Verus, and that the vstd"
    echo -e "versions pinned in verus-examples/*/Cargo.toml match it. To update them, run:"
    echo -e "    ./verus-examples/update-verus-versions.sh${NC}"
fi

exit $TEST_RESULT
