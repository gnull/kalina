# Kalina

Kalina is an experimental RSS reader written in Haskell. Its design was strongly
inspired by [Newsboat](https://github.com/newsboat/newsboat/).

Currently, it currently supports the most basic things: fetching RSS feeds,
browsing news items and opening them in browser. In a nutshell, it's a glue
between `wreq` for fetching feeds, `feed` for parsing them, `pandoc` for
rendering HTML as plaintext, and `brick` for displaying data on terminal.

You can install it with `stack install`, and run with `kalina`. If you have
Newsboat urls file in `~/.newsboat/urls`, Kalina will read urls from it.
