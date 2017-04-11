ToolXiT
=======

[![Build Status](https://travis-ci.org/satabin/toolxit-ng.svg?branch=master)](https://travis-ci.org/satabin/toolxit-ng)
[![Join the chat at https://gitter.im/toolxit-ng/Lobby](https://badges.gitter.im/toolxit-ng.svg)](https://gitter.im/toolxit-ng?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


ToolXiT is a set of scala library allowing to work with TeX files.
It is decomposed into several sub-projects:
 - `core` contains the common elements used by all other modules ;
 - `fonts` contains the font implementations to be used in other ToolXiT modules ;
 - `math` contains the mathematical formulas composition logic ;
 - `eyes` is the TeX lexer that returns unexpanded TeX tokens out of characters ;
 - `mouth` is the TeX parser that turns tokens into expanded sequences of primitive commands ;
 - `stomach` is the TeX stomach processor that executes the primitive commands ;
 - `xonsole` is a TeX console.
