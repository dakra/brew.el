# Homebrew package manager UI for Emacs

Manage [Homebrew](https://brew.sh) from Emacs: list, search, install,
uninstall and upgrade packages (formulae **and** casks),
manage services and taps, and run maintenance commands.

The package list works like the built-in `M-x list-packages`:
mark packages with single keys, then execute all marks at once.
The entry point and flag-heavy commands use [transient](https://github.com/magit/transient) menus.

## Installation

brew.el has no dependencies outside of Emacs (≥ 28.1).

```elisp
(use-package brew
  :vc (:url "https://github.com/dakra/brew.el" :rev :newest)
  :defer t)
```

## Usage

`M-x brew` opens the top-level menu:

```
Buffers                Packages           Maintenance
 l Packages             s Search…          U Update
 o Outdated packages    i Install…         c Cleanup…
 S Services             I Install version… A Autoremove
 t Taps                 u Upgrade all…     D Doctor
 v Vulnerabilities      d Uninstall…
```

### Package list (`M-x brew-list-packages`)

Lists all installed formulae and casks with their installed and latest
versions and a status (`outdated`, `pinned`, `disabled`, `deprecated`,
`installed`, `dependency` or `available`).

| Key         | Action                                                                                                                      |
|-------------|-----------------------------------------------------------------------------------------------------------------------------|
| `i`         | Mark for install (only on `available` rows, e.g. search results)                                                            |
| `d`         | Mark for uninstall                                                                                                          |
| `U`         | Mark all `outdated` packages for upgrade                                                                                    |
| `u` / `DEL` | Unmark (forward / backward)                                                                                                 |
| `x`         | Execute the marks (transient with `--greedy`, `--zap`, `--force`, `--dry-run`); with no marks, acts on the package at point |
| `P`         | Pin/unpin the formula at point                                                                                              |
| `s`         | Search all of Homebrew; results are markable with `i`                                                                       |
| `RET`       | Show package details (homepage, deps, caveats, …)                                                                           |
| `b`         | Browse the package homepage                                                                                                 |
| `/ n`       | Filter by name (regexp)                                                                                                     |
| `/ s`       | Filter by status                                                                                                            |
| `/ t`       | Filter by type (formula/cask)                                                                                               |
| `/ /`       | Clear all filters                                                                                                           |
| `g` / `r`   | Refresh (also returns from search or tap results)                                                                           |
| `?`         | Help menu                                                                                                                   |

The mode line shows the number of installed and outdated packages and
the active filters.

### Services (`M-x brew-services`)

Lists `brew services` with their status.
`s` starts, `o` stops and `r` restarts the service at point.
`RET` visits the launchd plist/systemd unit file.

### Vulnerabilities (`M-x brew-vulns`)

Checks all installed formulae for known vulnerabilities from OSV.dev
(`brew vulns`) and lists them, most severe first.
`RET` opens the vulnerability on OSV.dev, `u` upgrades its formula and
`g` rescans.  "Fixed in" lists released versions with a fix.
The mode line shows how many formulae were checked and how many could
not be scanned, for example because their source is unsupported or
their tap is not trusted.

### Taps (`M-x brew-taps`)

Lists your taps and whether Homebrew trusts them.  Homebrew only loads
formulae, casks and commands from non-official taps that are trusted,
either as a whole or item by item (`brew trust`).  The Trust column
shows `official`, `trusted` or the number of individually trusted
items.  Taps with a custom remote are trusted under their remote URL
and show no trust status.
`a` adds a tap, `d` removes the tap at point and `t` trusts or
untrusts it; untrusting a tap also drops the trust of its items.
`RET` (or a mouse click on the tap name) lists the packages the tap
provides in the package list buffer, where they can be marked and
installed like search results.
At most `brew-tap-max-packages` are fetched in detail.

### Standalone commands

`brew-install` completes over *all* available formulae and casks
(instantly, from Homebrew's cached API name lists).
`brew-version-install` installs a specific version of a formula
(`brew version-install`), extracting it into a personal tap.
Unless Homebrew already has a versioned formula for that version, this
needs a local clone of homebrew/core (`brew tap homebrew/core`).
`brew-uninstall`, `brew-update`, `brew-upgrade-all`, `brew-cleanup`,
`brew-autoremove` and `brew-doctor` do what their names say.
`brew-upgrade-all` can restrict the upgrade to formulae with known
vulnerabilities (`--vulnerable`, optionally with a minimum `--severity`).
Mutating commands stream their output into the `*brew*` buffer.

## Customization

| Variable                   | Purpose                                              |
|----------------------------|------------------------------------------------------|
| `brew-executable`          | Path to the brew executable                          |
| `brew-search-max-results`  | Cap on search results fetched in detail (default 50) |
| `brew-tap-max-packages`    | Cap on tap packages fetched in detail (default 200)  |
| `brew-api-cache-directory` | Where Homebrew caches its API name lists             |

## Development

```
make            # compile + checkdoc + test
make lint       # package-lint (downloads package-lint from MELPA)
```

`test/brew-stub` is a fake brew executable answering with canned JSON.
Point `brew-executable` at it to test mutations without touching your system.

## License

GPL-3.0-or-later, see [LICENSE](LICENSE).
