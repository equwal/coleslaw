# Changelog

Spenser Truex

## 2.0
- Test framework in tests.el.
- Skeleton now does error checking on user input, and asks before inserting
  superfluous headers.
- Date auto-inserts the current date in ISO8601 (2000-01-01 style) format.

## 0.1.3 2019-06-16
- One-key interface (bound to `M-;`) now is
  `coleslaw-insert-header-or-dispatch`. Should only be needed when changing a
  files mode in the `format:`, to redispatch the modes.
- Easier control over mode dispatch using `coleslaw-default-format-modes`.
- Changed the lighted to `CSLAW`.
- Dispatchment on old files based on the `format:` field.
- Use different separators with `coleslaw-header-separator`.

## 0.0.1
- Skeleton on open, with mode dispatch (but no mode dispatch on nonfresh files).
