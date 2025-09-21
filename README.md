
# TLE: Timmy's Lisp Environment

A text editor written in Common Lisp, for writing lisp code or doing
everyday computing tasks. Similar to Emacs but with an explicit
disinterest in becoming an IDE for other programming languages.

Core prinicples are simplicity and minimizing dependencies.

All dependencies are under vendor/ and they are imported using asdf
and `CL_SOURCE_REGISTRY`.

# Webview


Build command on guix:
```
(cd vendor/webview && guix shell --network --container nss-certs bash git coreutils webkitgtk gcc-toolchain cmake ninja pkg-config -- sh -c "cd c/ && rm -rf build/ && cmake -G Ninja -B build -S . -D CMAKE_BUILD_TYPE=Release && cmake --build build && cd .. && cp -r c/build/lib/* lib/linux/x64/ && echo success")
```


# Prompts to reuse

```
Look at TODO-EDITOR-FUNCTIONS.md and implement the next editor function that isn't checked.
Make sure you add lisp tests for it in the tests directory, ensure undo/redo works (and try it twice in a row), add a key binding to web-ui.lisp, and then check it off the list.
If you have syntax issues, you can try using util/paren-checker.lisp to debug.
```
