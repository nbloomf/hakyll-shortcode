# hakyll-shortcode

This module aims to approximate the functionality of WordPress shortcodes. It's intended to be used with the Hakyll static site generator, but this is not required.

Shortcodes are strings of the form

``[tag attr1='val1' attr2='val2' ... attrN='valN']``

which get expanded at compile time, typically into an ``iframe``. Attributes can be in any order. Values must be quoted with ``'``, ``"``, ``‘’``, or ``“”`` and nested quotes can be escaped with ``\``. Constraints on values are encoded as types to guard against XSS.

The module exports a function ``expandBLAHShortcodes`` for each shortcode type, which (surprise!) expands shortcodes with tag ``BLAH``, as well as ``expandAllShortcodes``, which expands all implemented shortcodes.

We do our best to validate input and sanitize the rendered HTML. But this library is not well tested yet, so be very very careful before using this with untrusted input.

# The Shortcodes

## ``youtube``

See the [API docs](https://developers.google.com/youtube/player_parameters).

| Key              | Values (Default)      | Description
| ---------------- | --------------------- | -----------
| ``id``           |                       | (Required.) The id of the video to be embedded.
| ``class``        | ``youtube-container`` | Class of the ``div`` wrapping an ``iframe``; for CSS.
| ``height``       |                       | Height of the ``iframe``.
| ``width``        |                       | Width of the ``iframe``.
| ``autoplay``     | ``yes``, ``no``       | If yes, automatically start after loading.
| ``start``        | A counting number     | Start time, in seconds, from the beginning.
| ``end``          | A counting number     | 
| ``captions``     | ``show``, ``default`` | Show closed captions automatically, or defer to client.
| ``show-related`` | ``yes``, (``no``)     | If yes, show related videos at the end.
