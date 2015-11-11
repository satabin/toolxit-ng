[![Build Status](https://travis-ci.org/satabin/toolxit-ng.svg?branch=master)](https://travis-ci.org/satabin/toolxit-ng) [![Accepted tickets](https://badge.waffle.io/satabin/toolxit-ng.png?label=accepted&title=accepted)](https://waffle.io/satabin/toolxit-ng) [![In progress tickets](https://badge.waffle.io/satabin/toolxit-ng.png?label=in progress&title=in progress)](https://waffle.io/satabin/toolxit-ng)
ToolXiT
=======

ToolXiT is a set of scala library allowing to work with TeX files.
It is decomposed into several sub-projects:
 - `ToolXiT-core` contains the common elements used by all other modules,
 - `ToolXiT-eyes` is the TeX lexer that returns unexpanded TeX tokens out of characters,
 - `ToolXiT-mouth` is the TeX parser that turns tokens into expanded sequences of primitive commands
