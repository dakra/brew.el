# Changelog

## 0.1 (unreleased)

Initial release.

- `brew` top-level transient dispatcher.
- `brew-list-packages`: unified formula/cask list with package-menu
  style marks (`i`/`d`/`U`) and batch execution (`x`), pinning,
  filtering, search over all of Homebrew and a package detail buffer.
- `brew-services`: start, stop and restart `brew services` daemons.
- `brew-taps`: list, add and remove taps; `RET` or a mouse click on a
  tap lists its packages in the package list buffer.
- `brew-install` with instant completion over all available packages
  from Homebrew's cached API name lists.
- Maintenance commands: `brew-update`, `brew-upgrade-all`,
  `brew-cleanup`, `brew-autoremove`, `brew-doctor`.
- All brew calls are asynchronous; mutations stream into the `*brew*`
  buffer with progress-bar and color rendering.
