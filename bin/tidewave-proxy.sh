#!/bin/bash

# MCP Proxy Development Script
# This script starts mcp-proxy instances for ash_ai and tidewave MCP servers
# and the Phoenix development server with REPL

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
HOST="localhost"
PORT="4000"
ASH_AI_ENDPOINT="http://${HOST}:${PORT}/ash_ai/mcp"
TIDEWAVE_ENDPOINT="http://${HOST}:${PORT}/tidewave/mcp"

# PID files for cleanup
MCP_PROXY_ASH_PID_FILE="/tmp/mcp_proxy_ash.pid"
MCP_PROXY_TIDEWAVE_PID_FILE="/tmp/mcp_proxy_tidewave.pid"

echo -e "${BLUE}🚀 MCP Proxy Development Environment${NC}"
echo "===================================="

# Function to cleanup processes
cleanup() {
    echo -e "\n${YELLOW}🧹 Cleaning up processes...${NC}"

    # Kill mcp-proxy processes
    if [ -f "$MCP_PROXY_ASH_PID_FILE" ]; then
        MCP_ASH_PID=$(cat "$MCP_PROXY_ASH_PID_FILE")
        if kill -0 "$MCP_ASH_PID" 2>/dev/null; then
            echo "Stopping mcp-proxy for ash_ai (PID: $MCP_ASH_PID)"
            kill "$MCP_ASH_PID" 2>/dev/null || true
        fi
        rm -f "$MCP_PROXY_ASH_PID_FILE"
    fi

    if [ -f "$MCP_PROXY_TIDEWAVE_PID_FILE" ]; then
        MCP_TIDEWAVE_PID=$(cat "$MCP_PROXY_TIDEWAVE_PID_FILE")
        if kill -0 "$MCP_TIDEWAVE_PID" 2>/dev/null; then
            echo "Stopping mcp-proxy for tidewave (PID: $MCP_TIDEWAVE_PID)"
            kill "$MCP_TIDEWAVE_PID" 2>/dev/null || true
        fi
        rm -f "$MCP_PROXY_TIDEWAVE_PID_FILE"
    fi

    echo -e "${GREEN}✅ Cleanup complete!${NC}"
}

# Function to start mcp-proxy
start_mcp_proxy() {
    local endpoint=$1
    local name=$2
    local pid_file=$3

    if command -v mcp-proxy > /dev/null 2>&1; then
        echo "Starting mcp-proxy for $name..."
        mcp-proxy "$endpoint" > "/tmp/mcp_${name}.log" 2>&1 &
        echo $! > "$pid_file"
        echo -e "${GREEN}✅ mcp-proxy for $name started (PID: $!)${NC}"
        echo "   Log file: /tmp/mcp_${name}.log"
    else
        echo -e "${YELLOW}⚠️  mcp-proxy not found in PATH - skipping proxy for $name${NC}"
        echo "   Install with: cargo install mcp-proxy"
    fi
}

# Trap cleanup function on script exit
trap cleanup EXIT INT TERM

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --help     Show this help message"
            echo ""
            echo "MCP Endpoints:"
            echo "  Ash AI:    $ASH_AI_ENDPOINT"
            echo "  Tidewave:  $TIDEWAVE_ENDPOINT"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}Configuration:${NC}"
echo "  Host: $HOST"
echo "  Port: $PORT"
echo "  Ash AI MCP: $ASH_AI_ENDPOINT"
echo "  Tidewave MCP: $TIDEWAVE_ENDPOINT"
echo ""

# Start mcp-proxy instances
echo -e "${YELLOW}🔗 Starting mcp-proxy instances...${NC}"
start_mcp_proxy "$ASH_AI_ENDPOINT" "ash_ai" "$MCP_PROXY_ASH_PID_FILE"
start_mcp_proxy "$TIDEWAVE_ENDPOINT" "tidewave" "$MCP_PROXY_TIDEWAVE_PID_FILE"

# Show status and usage information
echo -e "\n${GREEN}🎉 MCP Proxy environment is ready!${NC}"
echo "====================================="
echo ""
echo -e "${BLUE}Available MCP Servers:${NC}"
echo "  📊 Ash AI MCP:    $ASH_AI_ENDPOINT"
echo "  🌊 Tidewave MCP:  $TIDEWAVE_ENDPOINT"
echo ""

echo -e "${BLUE}MCP Proxy Commands:${NC}"
echo "  For Ash AI:    mcp-proxy $ASH_AI_ENDPOINT"
echo "  For Tidewave:  mcp-proxy $TIDEWAVE_ENDPOINT"
echo ""

echo -e "${BLUE}Next Steps:${NC}"
echo "1. Configure your MCP client (Zed, Claude Desktop, etc.)"
echo "2. Use the endpoints above with your MCP client"
echo "3. Test with: curl -H 'Accept: application/json' <endpoint>"
echo ""
echo -e "${BLUE}Log Files:${NC}"
echo "  Ash AI Proxy: /tmp/mcp_ash_ai.log"
echo "  Tidewave Proxy: /tmp/mcp_tidewave.log"
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop all services and clean up${NC}"
echo ""

# Start Phoenix development server with REPL
echo -e "${BLUE}🔥 Starting Phoenix development server with REPL...${NC}"
echo "=================================================="
iex -S mix phx.server
