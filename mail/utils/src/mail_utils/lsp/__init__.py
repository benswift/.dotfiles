"""Language server for mutt/neomutt compose buffers.

Provides completions for:
- Email addresses in To/Cc/Bcc headers (via mu cfind)
- File paths in Attach headers (via fd)
- Greetings in the message body with recipient name lookup
"""

import os
import re
import subprocess

from lsprotocol import types
from pygls.lsp.server import LanguageServer

ADDRESS_HEADER_RE = re.compile(r"^(To|Cc|Bcc|From|Reply-To)\s*:", re.IGNORECASE)
ATTACH_HEADER_RE = re.compile(r"^Attach\s*:", re.IGNORECASE)
MAX_ATTACHMENT_SIZE_MB = 10
GREETING_TRIGGERS = ("hey", "hi", "hello", "g'day", "dear")

NICKNAMES: dict[str, str] = {
    # "john.smith@example.com": "Smithy",
}


def is_address_header(line: str) -> bool:
    return ADDRESS_HEADER_RE.match(line) is not None


def is_attach_header(line: str) -> bool:
    return ATTACH_HEADER_RE.match(line) is not None


def extract_query(line: str, character: int) -> str | None:
    colon_idx = line.find(":")
    if colon_idx < 0 or character <= colon_idx:
        return None
    after_colon = line[colon_idx + 1 : character]
    segments = after_colon.split(",")
    return segments[-1].strip()


def parse_contacts(output: str) -> list[dict[str, str]]:
    if not output:
        return []
    contacts = []
    for line in output.strip().split("\n"):
        if not line or "\t" not in line:
            continue
        parts = line.split("\t")
        if len(parts) >= 2:
            email = parts[0]
            name = parts[1] if parts[1] != "nil" else ""
            contacts.append({"email": email, "name": name})
    return contacts


def format_display_name(name: str | None, email: str) -> str:
    if name and name != email:
        return f"{name} <{email}>"
    return f"<{email}>"


def contacts_to_completion_items(
    contacts: list[dict[str, str]],
) -> list[types.CompletionItem]:
    items = []
    for contact in contacts:
        email = str(contact.get("email", ""))
        name = contact.get("name")
        name_str = str(name) if name else None
        display = format_display_name(name_str, email)
        items.append(
            types.CompletionItem(
                label=display,
                kind=types.CompletionItemKind.Text,
                insert_text=display,
                filter_text=f"{name_str or ''} {email}",
            )
        )
    return items


def run_mu_cfind(query: str, max_results: int = 50) -> str:
    if not query:
        return ""
    try:
        result = subprocess.run(
            ["mu", "cfind", "--format=mutt-ab", f"--maxnum={max_results}", query],
            capture_output=True,
            text=True,
            timeout=5,
        )
        return result.stdout if result.returncode == 0 else ""
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return ""


def run_fd_for_attach(query: str, max_results: int = 50) -> list[str]:
    if not query:
        return []
    if not (query.startswith("/") or query.startswith("~") or query.startswith(".")):
        return []
    try:
        expanded = os.path.expanduser(query)
        if os.path.isdir(expanded):
            search_dir = expanded
            pattern = "."
        else:
            search_dir = os.path.dirname(expanded) or "."
            pattern = os.path.basename(expanded) or "."
        if not os.path.isdir(search_dir):
            return []
        result = subprocess.run(
            [
                "fd",
                "-t",
                "f",
                "-S",
                f"-{MAX_ATTACHMENT_SIZE_MB}M",
                "-a",
                pattern,
                search_dir,
            ],
            capture_output=True,
            text=True,
            timeout=5,
            stderr=subprocess.DEVNULL,
        )
        files = [f for f in result.stdout.strip().split("\n") if f]
        return files[:max_results]
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return []


def format_file_size(size_bytes: int) -> str:
    if size_bytes < 1024:
        return f"{size_bytes} B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f} KB"
    else:
        return f"{size_bytes / (1024 * 1024):.1f} MB"


def files_to_completion_items(files: list[str]) -> list[types.CompletionItem]:
    items = []
    home = os.path.expanduser("~")
    for path in files:
        display = path.replace(home, "~", 1) if path.startswith(home) else path
        try:
            size = os.path.getsize(path)
            detail = format_file_size(size)
        except OSError:
            detail = None
        items.append(
            types.CompletionItem(
                label=display,
                kind=types.CompletionItemKind.File,
                insert_text=path,
                filter_text=display,
                detail=detail,
            )
        )
    return items


def find_body_start(lines: list[str]) -> int | None:
    for i, line in enumerate(lines):
        if line.strip() == "":
            return i
    return None


def in_header_section(lines: list[str], current_line: int) -> bool:
    for i in range(current_line):
        if lines[i].strip() == "":
            return False
    return True


def in_body_section(lines: list[str], current_line: int) -> bool:
    body_start = find_body_start(lines)
    return body_start is not None and current_line > body_start


def parse_to_header(lines: list[str]) -> tuple[str | None, str | None]:
    for line in lines:
        if line.strip() == "":
            break
        if line.lower().startswith("to:"):
            value = line[3:].strip()
            email_match = re.search(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+", value)
            email = email_match.group(0) if email_match else None
            name = re.sub(r"<[^>]*>", "", value).strip().strip('"')
            if name and email and name.lower() == email.lower():
                name = None
            return name if name else None, email
    return None, None


def get_greeting_name(lines: list[str]) -> str | None:
    name, email = parse_to_header(lines)
    if email and email.lower() in NICKNAMES:
        return NICKNAMES[email.lower()]
    if name:
        return name.split()[0]
    return None


def greeting_completions(
    lines: list[str], line_text: str
) -> list[types.CompletionItem]:
    name = get_greeting_name(lines)
    if not name:
        return []
    items = []
    greetings = [
        ("Hey", f"Hey {name},\n\n"),
        ("Hi", f"Hi {name},\n\n"),
        ("Hello", f"Hello {name},\n\n"),
        ("G'day", f"G'day {name},\n\n"),
        ("Dear", f"Dear {name},\n\n"),
    ]
    for label, text in greetings:
        if line_text.lower().startswith(label.lower()[: len(line_text)]):
            items.append(
                types.CompletionItem(
                    label=f"{label} {name},",
                    kind=types.CompletionItemKind.Snippet,
                    insert_text=text,
                    filter_text=label.lower(),
                    detail="greeting",
                )
            )
    return items


server = LanguageServer("mutt-compose-lsp", "v0.2.0")


@server.feature(
    types.TEXT_DOCUMENT_COMPLETION,
    types.CompletionOptions(trigger_characters=[",", " ", "/"]),
)
def completions(params: types.CompletionParams) -> types.CompletionList | None:
    doc = server.workspace.get_text_document(params.text_document.uri)
    lines = doc.source.splitlines()
    line_idx = params.position.line
    character = params.position.character

    if line_idx >= len(lines):
        return None

    line = lines[line_idx]

    if in_header_section(lines, line_idx):
        if is_attach_header(line):
            query = extract_query(line, character)
            if query is None:
                return None
            files = run_fd_for_attach(query)
            items = files_to_completion_items(files)
            return types.CompletionList(is_incomplete=True, items=items)

        if is_address_header(line):
            query = extract_query(line, character)
            if query is None:
                return None
            stdout = run_mu_cfind(query)
            contacts = parse_contacts(stdout)
            items = contacts_to_completion_items(contacts)
            return types.CompletionList(is_incomplete=True, items=items)

    if in_body_section(lines, line_idx):
        line_text = line[:character].strip()
        if line_text and any(
            line_text.lower().startswith(t[: len(line_text)]) for t in GREETING_TRIGGERS
        ):
            items = greeting_completions(lines, line_text)
            if items:
                return types.CompletionList(is_incomplete=False, items=items)

    return None


def main():
    server.start_io()


if __name__ == "__main__":
    main()
