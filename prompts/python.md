# Python

This is a modern python project, built using the following principles:
- use Pydantic for data validation and SQLModel for persistance
- use modern python: we support 3.12 and above
- use type hints
- use uv for dependency management
- don't use try/except for anything outside the top-level functions: it's fine for most functions to not handle any exceptions that occur
- use the logging module for logging (and don't overuse the INFO level - DEBUG is fine in most cases))
