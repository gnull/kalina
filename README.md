# Kalina

| ![2020-04-28-170814_884x382_scrot](https://user-images.githubusercontent.com/7102119/80504194-213b4c00-8973-11ea-94ff-266d1dab7274.png) | ![2020-04-28-170809_884x382_scrot](https://user-images.githubusercontent.com/7102119/80504212-239da600-8973-11ea-82cc-12e0e0376d08.png) |
|:---:|:---:|

Kalina is an experimental RSS reader written in Haskell. Its design is strongly
inspired by [Newsboat](https://github.com/newsboat/newsboat/).

It currently supports the most basic things: fetching RSS feeds, browsing news
items and opening them in a browser. In a nutshell, it's a glue between `wreq`
for fetching feeds, `feed` for parsing them, `pandoc` for rendering HTML as
plaintext, and `brick` for terminal interaction.

You can install it with `stack install`, and run with `kalina`. If you have
Newsboat urls file in `~/.newsboat/urls` Kalina will read urls from it,
otherwice run `kalina --help` to see what you can do.
