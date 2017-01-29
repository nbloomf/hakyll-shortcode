# hakyll-shortcode

This module aims to approximate the functionality of WordPress shortcodes for [Hakyll](https://jaspervdj.be/hakyll/) sites.

## short-what?

Shortcodes are strings of the form

``[tag attr1='val1' attr2='val2' ... attrN='valN']``

that you sprinkle throughout your markdown (between two blank lines) and which get expanded at compile time. Shortcodes can expand into anything, but mostly they're used to embed stuff in web pages, like youtube videos.

## Usage

If you're using Hakyll, after installing this package add the line

```haskell
import Hakyll.Shortcode
```

to your list of module imports. Then, in any compiler where you want shortcodes expanded, add a ``>>= applyShortcodes allServices``. For example, here's a compiler for my web page.

```haskell
matchClasses :: Rules ()
matchClasses = match "classes/**" $ do
  route $ setExtension "html"
  compile $ pandocMathCompiler
    >>= applyShortcodes allServices -- woo! shortcodes!
    >>= loadAndApplyTemplate
          "templates/default.html" postCtx
    >>= relativizeUrls
```

That ``allServices`` token has type ``[ShortcodeService]``, and it specifies which shortcodes we want expanded. (In this case, all of them.) But you can define your own list here; see the ``Hakyll.Shortcodes`` module for a list of services.

## Disclaimer

We try *really* hard to validate input and sanitize the rendered HTML. But this library is not well tested yet (working on it!), so be careful before using this with untrusted input.

# The Shortcodes

## ``youtube``

See the [API docs](https://developers.google.com/youtube/player_parameters).

| Key                  | Values (Default)      | Description
| ----------------     | --------------------- | -----------
| ``id``               |                       | The id of the video to be embedded.
| ``class``            | ``youtube-container`` | Class of the ``div`` wrapping an ``iframe``; for CSS.
| ``height``           |                       | Height of the ``iframe``.
| ``width``            |                       | Width of the ``iframe``.
| ``start``            | A counting number     | Start time, in seconds, from the beginning.
| ``end``              | A counting number     | 
| ``origin``           |                       | The domain+scheme where the iframe is embedded
| ``list``             |                       | Comma separated list of video IDs
| ``autoplay``         | ``yes``, (``no``)     | If yes, automatically start after loading.
| ``show-related``     | ``yes``, (``no``)     | If yes, show related videos at the end.
| ``loop``             | ``yes``, (``no``)     | If yes, loop the video or playlist
| ``disable-keyboard`` | ``yes``, (``no``)     | If yes, disable keyboard shortcuts for video controls
| ``show-fullscreen``  | (``yes``), ``no``     | If no, do not enable the fullscreen button
| ``show-info``        | ``yes``, ``no``       |
| ``play-inline``      | ``yes``, ``no``       |
| ``show-logo``        | ``yes``, ``no``       |
| ``show-annotations`` | ``yes``, ``no``       |
| ``enable-js-api``    | ``yes``, ``no``       |
| ``captions``         | ``show``, ``default`` | Show closed captions automatically, or defer to client.
| ``show-controls``    | ``never``, ``onload``, ``onplay`` |
| ``color``            | ``red``, ``white``    |
| ``list-type``        | ``playlist``, ``search``, ``user-uploads`` |


## ``geogebra``


