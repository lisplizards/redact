# redact

Library and Lack middleware to scrub sensitive values from the Clack environment.

Use in conjunction with other middleware to prevent PII and other sensitive data
from leaking to logs, notifications, etc.

Features:

* redact query-string, query-parameters, and body-parameters where the key partially matches a configured parameter name
* redact specific headers
* redact cookies by default, specifying any cookie values for which to skip redaction
* redact backtraces

## Usage

This Git repository includes two ASDF systems, `foo.lisp.lack-middleware-redact` and `foo.lisp.redact`.

Package `lack/middleware/redact` allows for configuring your application's redaction policies.

Package `foo.lisp.redact` contains functions to copy the Clack environment, redacting any
sensitive values, and is intended to be called from any downstream middlewares that display,
store, or transmit the ENV.

### lack/middleware/redact

The Lack middleware dynamically binds special variable `*REDACT*` so that downstream
middlewares can call the `REDACT` or `PRINT-ENV` functions to scrub the Clack environment.

Include the redact middleware at or near the top of your middleware chain.

Wrap app:

```common-lisp
(funcall lack/middleware/redact:*lack-middleware-redact*
         *app*
         :parameters '("token" "code" "password" "full_name" "family_name" "given_name" "username")
         :headers '("x-access-token")
         :preserve-cookies '("lack.session"))
```

Lack Builder

```common-lisp
(lack:builder
 (:redact :parameters '("token" "code" "password" "full_name" "family_name" "given_name" "username")
          :headers '("x-access-token")
          :preserve-cookies '("lack.session"))
 *app*)
```

#### Options

* `PARAMETERS`: list of parameter names for which the values should be redacted; any partial, case insensitive matches will be redacted
* `HEADERS`: list of header names for which the values should be redacted; the "authorization" and "proxy-authorization" headers do not need to be included, as they are treated as special cases and redacted by default
* `PRESERVE-COOKIES`: names of any cookies for which to skip redaction; cookies are redacted by default

### foo.lisp.redact

Function `REDACT` returns a new ENV list with redacted values.

Function `PRINT-ENV` is meant to be used whenever the redacted env needs to be displayed or
transmitted. Typical use-cases handled by downstream middleware include printing to the console,
displaying in a debug page, or inclusion in an error notification.

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Development

Run tests:

```common-lisp
(asdf:test-system :foo.lisp.redact)
```

```common-lisp
(asdf:test-system :foo.lisp.lack-middleware-redact)
```

## Dependencies

* [cl-ppcre](https://github.com/edicl/cl-ppcre/)
* [foo.lisp.print-hash](https://github.com/lisplizards/print-hash)
* [trivial-backtrace](https://github.com/hraban/trivial-backtrace)

### Tests

* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [rove](https://github.com/fukamachi/rove)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
