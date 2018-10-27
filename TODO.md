- Decouple parsing and formatting to make possible to parse a string once, and
  use it several times.
- Add a caching `FnOnce` wrapper.
