# Dragonmark Web

A bunch of [Lift](http://liftweb.net)-like tools for
Clojure web development.

## Basic info

[![Clojars Project](http://clojars.org/dragonmark/web/latest-version.svg)](http://clojars.org/dragonmark/web)

The current build status:
<a href="https://travis-ci.org/dragonmark/web">
![Build Status](https://travis-ci.org/dragonmark/web.svg?branch=develop)</a>

## Lift, **o'really?!?**

Yeah.

Lift's [CSS Selector Transforms](http://simply.liftweb.net/index-7.10.html#toc-Section-7.10) are the best thing in web development, ever.

Granted, Lift's CSS Selector Transforms are built off concepts
in [Enlive](https://github.com/cgrand/enlive), Lift treats the
transforms as composable components... and that means more concise,
reusable code.

If you're doing client-side work, there's [Enfocus](https://github.com/ckirkendall/enfocus) and [kioo](https://github.com/ckirkendall/kioo),
but they are very brittle and don't play well with [Reagent](https://reagent-project.github.io/).

So, I decided to write a bunch of web utilities for ClojureScript,
single page apps.

## CSS Selector Transforms



## Who

Right now, [David Pollak](https://twitter.com/dpp) is the
only one working on the project and there's no runnable code...
but that will change.

I'm not sure about the pull request policy yet. Having
clean IP in [Lift](http://liftweb.net) has been a real plus.
The core Clojure code is only changeable by Clojure committers.
On the other hand, I want to encourage contribution to Dragonmark...
once there's something worth contributing to.

## License

Dragonmark is dual licensed under the Eclipse Public License,
just like Clojure, and the LGPL 2, your choice.

A side note about licenses... my goal with the license is to
make sure the code is usable in a very wide variety of projects.
Both the EPL and the LGPL have contribute-back clauses. This means
if you make a change to Dragonmark, you have to make your changes
public. But you can use Dragonmark in any project, open or closed.
Also, the dual license is meant to allow Dragonmark to be used in
GPL/AGPL projects... and there are some "issues" between the FSF
and the rest of the world about how open the EPL, the Apache 2, etc.
licenses are. I'm not getting caught in that deal.

(c) 2014 WorldWide Conferencing, LLC
