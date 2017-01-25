# hakyll-shortcode

This module aims to approximate the functionality of WordPress shortcodes in sites built with Hakyll. Shortcodes are strings of the form

``[tag attr1='val1' attr2='val2' ... attrN='valN']``

which get expanded at compile time, typically into an ``iframe``. Attributes can be in any order and if an attribute appears twice only the last (rightmost) one takes effect. Values must be quoted with ``'``, ``"``, ``‘’``, or ``“”`` and nested quotes can be escaped with ``\``.

The module exports a function ``expandBLAHShortcodes`` for each shortcode type, which (surprise!) expands shortcodes with tag ``BLAH``, as well as ``expandAllShortcodes``, which expands all implemented shortcodes.

We try our best to validate input and sanitize the rendered HTML. But, at least for now, be very very careful before using this with untrusted input.

# The Shortcodes

## ``youtube``

See the [API docs](https://developers.google.com/youtube/player_parameters).

| Key          | Values (Default)      | Description
| ------------ | --------------------- | -----------
| ``id``       |                       | (Required.) The id of the video to be embedded.
| ``class``    | ``youtube-container`` | Class of the ``div`` wrapping an ``iframe``; for CSS.
| ``height``   |                       | Height of the ``iframe``.
| ``width``    |                       | Width of the ``iframe``.
| ``autoplay`` | ``yes``, ``no``       | If yes, automatically start after loading.
