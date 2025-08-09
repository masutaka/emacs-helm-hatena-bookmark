# helm-hatena-bookmark.el

[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]
[![Ask DeepWiki][deepwiki-badge]][deepwiki-link]

[melpa-link]: https://melpa.org/#/helm-hatena-bookmark
[melpa-stable-link]: https://stable.melpa.org/#/helm-hatena-bookmark
[deepwiki-link]: https://deepwiki.com/masutaka/emacs-helm-hatena-bookmark
[melpa-badge]: https://melpa.org/packages/helm-hatena-bookmark-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/helm-hatena-bookmark-badge.svg
[deepwiki-badge]: https://deepwiki.com/badge.svg

## Introduction

`helm-hatena-bookmark.el` is Hatena::Bookmark helm interface.

## Screenshot

![helm-hatena-bookmark](image/helm-hatena-bookmark.gif)

## Requirements

* Emacs 24.5 or higher
* helm 2.8.2 or higher
* curl
* GNU sed

## Installation

You can install `helm-hatena-bookmark.el` from [MELPA](https://melpa.org) with package.el (`M-x package-install helm-hatena-bookmark`).

## Setup

```lisp
(setq helm-hatena-bookmark-username "Your Hatena Account")
(helm-hatena-bookmark-initialize)
```

## Usage

### `helm-hatena-bookmark`

* A function to be able to select your Hatena::Bookmark.

### `helm-hatena-bookmark-source`

* A helm source for your Hatena::Bookmark.

## Acknowledgment

Thanks to k1LoW who is Author of anything-hatena-bookmark.el

https://github.com/k1LoW/anything-hatena-bookmark
