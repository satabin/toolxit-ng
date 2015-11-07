[![Stories in Ready](https://badge.waffle.io/satabin/toolxit-ng.png?label=ready&title=Ready)](https://waffle.io/satabin/toolxit-ng)
ToolXiT
=======

ToolXiT is a set of scala library allowing to work with TeX files.
It is decomposed into several sub-projects:
 - `ToolXiT-core` contains the common elements used by all other modules,
 - `ToolXiT-eyes` is the TeX lexer that returns unexpanded TeX tokens out of characters,
 - `ToolXiT-mouth` is the TeX parser that turns tokens into expanded sequences of primitive commands
