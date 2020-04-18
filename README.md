# Kalina

Kalina is an experimental RSS reader written in Haskell. Its design is strongly
inspired by [Newsboat](https://github.com/newsboat/newsboat/).

It currently supports the most basic things: fetching RSS feeds, browsing news
items and opening them in a browser. In a nutshell, it's a glue between `wreq`
for fetching feeds, `feed` for parsing them, `pandoc` for rendering HTML as
plaintext, and `brick` for terminal interaction.

You can install it with `stack install`, and run with `kalina`. If you have
Newsboat urls file in `~/.newsboat/urls` Kalina will read urls from it,
otherwice run `kalina --help` to see what you can do.
