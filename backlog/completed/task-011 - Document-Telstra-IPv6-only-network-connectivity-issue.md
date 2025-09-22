---
id: task-011
title: Document Telstra IPv6-only network connectivity issue
status: Done
assignee: []
created_date: "2025-09-10 23:56"
labels:
  - networking
  - telstra
  - ipv6
  - documentation
dependencies: []
priority: medium
---

Note: this is done, the info is in a daily note.

## Description

Document the root cause, symptoms, and workarounds for connectivity issues when
tethering via Telstra mobile network's IPv6-only infrastructure with unreliable
NAT64. This is a known infrastructure limitation, not a fixable configuration
issue.

## Acceptance Criteria

- [ ] Root cause analysis is documented with technical details
- [ ] All observed symptoms are listed with examples
- [ ] Investigation findings are recorded for future reference
- [ ] Known workarounds are documented
- [ ] External references and resources are included
- [ ] Clear distinction made between what is/isn't caused by this issue

## Implementation Notes

### Root Cause

Telstra mobile network is IPv6-only with unreliable NAT64 gateway, causing
IPv4-only services to fail intermittently.

### Symptoms

- `ping` to IPv4 addresses fails with "No route to host" error
- Git operations to GitHub fail (IPv4-only git protocol)
- SSH to IPv4-only hosts fails
- HTTP/HTTPS to dual-stack websites works fine via IPv6
- Only occurs when tethering via Bluetooth (or any tethering method) on Telstra
  mobile network

### Investigation Findings

- No IPv4 default route present when tethering
- Only gets 169.254.x.x self-assigned IPv4 address (link-local)
- IPv6 connectivity works perfectly with proper global addresses
- Network diagnostics show missing IPv4 routing table entries
- Issue persists across different devices and tethering methods

### What This Is NOT Caused By

- Tailscale configuration or routing
- zsh shell configuration changes
- Local machine network configuration
- DNS resolution issues
- Firewall or security software

### Workarounds

1. **Primary**: Use regular Wi-Fi networks when available (most support IPv4)
2. **Mobile**: Configure VPN on iPhone to provide IPv4 tunnel through NAT64
3. **Alternative**: Consider switching to carrier with better IPv4 support
4. **Temporary**: Use mobile data directly on device instead of tethering

### External References

- [SIDN article on Telstra's IPv6 switch](https://www.sidn.nl/en/news-and-blogs/telstra-switches-to-ipv6-only-mobile-network)
- [GitHub No_Telstra_IPv6 repository](https://github.com/serioussam/No_Telstra_IPv6) -
  Community workarounds
- [Telstra IPv6 deployment documentation](https://www.telstra.com.au/support/mobiles-devices/network-coverage/ipv6)

### Technical Details

```
# Typical broken IPv4 routing when tethering:
$ route -n get default
route: writing to routing socket: No such process

# Working IPv6 connectivity:
$ ping6 google.com
PING6(56=40+8+8 bytes) 2001:8003:xxxx::1 --> 2404:6800:4006:c1b::71
```

This is a known carrier infrastructure limitation documented for future
troubleshooting reference.
