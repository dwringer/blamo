## Unsecured logging via HTTP requests

### CL dependencies
- Quicklisp
- drakma
- clack
- snooze
- chipz
- salza2
- flexi-streams
- sqlite (requires sqlite3 binary)

### blamo.lisp
This self-contained file implements the HTTP server itself.

### blamo-client.lisp
This file can be loaded into any lisp program to enable logging

### freezip.lisp
Loose collection of compression functions / not implemented

### timestamp.lisp
ISO format [UTC] timestamp functions / redundant

### txt-io.lisp
Some basic functions for reading/writing textfile data / redundant

### ws-path.lisp
Functions adapted from CL-FAD for basic pathname/file manipulation / redundant

