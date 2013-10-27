Bitlmacs
========

By Kyle Machulis <kyle@nonpolynomial.com>

About
-----

Bitlmacs is a package for making an IM client-ish interface in emacs
using ERC and bitlbee. The model follows the pidgin/adium model of a
single nicklist and an IM window that can page back and forth between
IM buffers.

Currently, nicklist functionality is provided via a hacked up version
of erc-nicklist from the erc-extras kit, and the IM window
functionality exists via keybinds set buffer local in erc-query
buffers. In the future, we may write our own nicklist implementation,
since nicklist is rather dated.

Bitlmacs is happiest living in its own frame, either by spawning an
outside frame or using a desktop switcher like
escreen/elscreen/workgroups. However, each IM window can access any
other IM window via keybinds, so it's also possible to bring up IMs as
needed if a dedicated setup is not easy or available.
