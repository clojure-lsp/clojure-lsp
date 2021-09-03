---
name: Editor Bug report
about: Create a report for an issue with clojure-lsp running in your editor
title: ''
labels: [bug, editor]
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

<!-- Fill the template below with the json request/response logs between the LS client (your editor plugin, like Calva, lsp-mode, nvim) and clojure-lsp. -->

<details>
 <summary><b>Log - client <-> server</b></summary>
<pre>
ADD JSON HERE
</pre>
</details>

<!-- Fill the template below with the content of the clojure-lsp log if any exceptions/relevant logs, check https://clojure-lsp.io/troubleshooting/#getting-server-log how to get it -->

<details>
 <summary><b>Log - clojure-lsp</b></summary>
<pre>
ADD HERE
</pre>
</details>

**User details (please complete the following information):**
 - OS: [e.g. ArchLinux, MacOS, Windows 10]
 - Editor [e.g. emacs, nvim, VSCode (Calva)]
 - Version: (post the result of `clojure-lsp --version`)

**Additional context**
Add any other context about the problem here.
