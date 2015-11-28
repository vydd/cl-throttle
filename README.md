# cl-throttle

This library introduces throttles to Common Lisp.

Throttles solve the problem of maintaining state for some types of branching code and keeping track of function calls when rate limiting is needed.

## Examples (WORK IN PROGRESS)

### `(limit n)`, `(once)`

Making the code runnable for only a `LIMIT`ed number of times.

```lisp
...
(case genie-command
  ...
  (:grant-a-wish (with-throttle (limit 3)
                   (close-eyes)
                   (speak-magic-words)
                   "Wish granted!")))
  ...)
...
```

For convenience, throttle `ONCE` is provided as well.

```lisp
(defun-throttled (once) contract-chickenpox ()
  (cough)
  (sneeze)
  (scratch))
```

You can use `INTERVAL` to limit the rate of code execution.

```lisp
(defun-throttled (interval 1000) login (user password)
  (if (auth user password)
    "Welcome!"
    "Go away."))
```

It's possible to `THROTTLE-AND` throttles.

```lisp
(defun-throttled (throttle-and (interval 1000) (limit 3)) strict-login (user password)
  (if (auth user password)
    "Welcome!"
    "Careful."))
```

And also `THROTTLE-OR`.

```lisp
; can be executed 3 times a day
(defun-throttled (throttle-or (interval 1000) (limit 3)) send-email (recipient)
	(email recipient "Hello"))
```

## Values returned by `WITH-THROTTLE`

If you need to know if throttled code was executed or not, you can use the second value returned by it (the first value is the value returned by your code).

It's possible to reset throttles, by providing the third value returned by `WITH-THROTTLE` to `RESET-THROTTLE-STATE`. This functionality is meant to be used sparingly in user programs, if not at all, but can be useful when extending `CL-THROTTLE`.

## License

MIT
