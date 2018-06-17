# Daily Reporter

It compiles email html (no sending yet) from various sources.

It *should be* easily extensible, relatively robust, and type-safe.
It will also only grab sources from the last 24 hours. You can currently only edit this from the Haskell Utils.hs function.

TODOS:
src/Lib.hs:-- TODO: Allow user config for sources.
src/Lib.hs:-- TODO: Allow easier creation for new sources of standard RSS type.
src/Lib.hs:-- -- |TODO allow user-defined file paths.
src/Utils.hs:-- TODO: Replace with something better than * (mustache templating)
src/Apod.hs:-- TODO: Look for a better way. Salvation.
src/Weather.hs:-- TODO allow user-defined file paths.
src/Smbc.hs:-- TODO make part of a SmbcConfig file.

Fix documentation spacing.

Compile with:

` $ stack build`

If you want to compile the documentation, use

` $ stack build --haddock --no-haddock-deps`

Created By:
Jackson Garner

Contributions from:
Joseph McKinsey
