# Emacs Velocity: search and create text files

> It didn't take long to discover that his main occupation, even when he wasn't keeping records of his behavior, was centered on keeping records of everything else. Astonishing as it must have seemed to any self-respecting scientist like himself, his observations revealed that about 85% of his "thinking" time was actually spent "getting into a position to think, to make a decision, to learn something I needed to know. Much more time went into finding or obtaining information than into digesting it."
> — Howard Rheingold, [Tools for Thought](http://www.rheingold.com/texts/tft/07.html#Chap07)

Inspired by [Notational Velocity](http://notational.net).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
  - [Searching](#searching)
  - [Creating](#creating)
- [Related work](#related-work)

<!-- markdown-toc end -->

## Demo

![](demo.gif)

# Features

- **Multiple formats**: org, markdown, plain, and an API for adding more.
- **File and sub-file search**: search and return entire files or sections within filesj, configurable on a per-file basis.
- **File and sub-file create**: create content as files or as sections within files.

## Requirements

Currently supports [Helm](https://github.com/emacs-helm/helm) as front end.

## Installation

Download this repository and add it to Emacs's `load-path`. MELPA coming soon.

## Configuration

Bind `velocity` to a key combination, e.g.:

```
(global-set-key (kbd "C-c r") velocity)
```

Set or customize the `velocity-searches` variable to specify what files should be searched and how. For example:

```emacs-lisp
(setq velocity-searches
      '(;; Search level-1 sections in Org files
        (:files ("~/notes/bucket.org" "~/journal.org") :backend org-heading-1)

        ;; Search whole Org files
        (:files ("~/notes/roam/*.org") :backend org-file)

        ;; Search level-2 sections in Markdown files
        (:files ("~/projects/foobar/manual.md" :backend markdown-heading-2))

        ;; Search whole Markdown files
        (:files ("~/projects/*/README.md" :backend markdown-file))

        ;; Search plain text files
        (:files ("/usr/share/doc/*/README.txt")))
```

Set or customize the `velocity-targets` variable to specify where new content should go. For example:

```emacs-lisp
(setq velocity-targets
      '(;; Add new content as level-1 section Org file
        (:file "~/bucket.org" :backend org-heading-1)
        ;; Add new content as stand-alone Mardown file
        (:file "~/blog/posts/%s.md" :backend markdown-file))))
```

## Usage

### Searching

Invoke `velocity` and write some words. Matching notes will appear in the helm buffer.

Follow-mode is enabled by default. To disable it, invoke `C-c C-f` during the helm session.

### Creating

When the input is at least 15 characters, Velocity guesses you're writing the title of a new entry instead of a search query, and displays a list of possible targets; if you select any of those, a new note is created and displayed.

## Related work

- [Notational Velocity](http://notational.net/)
- Other Emacs modules with similar purposes: [org-velocity](http://orgmode.org/worg/org-contrib/org-velocity.html), [deft](https://jblevins.org/projects/deft/), [helm-org](https://github.com/emacs-helm/helm/blob/master/helm-org.el), [helm-rifle](https://github.com/alphapapa/helm-org-rifle), [helm-browse](https://github.com/michael-heerdegen/helm-browse).
