# Daily Reporter

It compiles email html (no sending yet) from various sources. Then, it sends it
with postfix sendmail (look elsewhere on how to configure this).

It *should be* easily extensible, relatively robust, and type-safe.

It will also only grab sources from the last 24 hours. You can currently only
edit this from the Haskell Utils.hs function.

TODOS:
* src/Lib.hs:-- TODO: Allow user config for sources.
* src/Lib.hs:-- TODO: Allow easier creation for new sources of standard RSS type.
* src/Lib.hs:-- -- |TODO allow user-defined file paths.
* src/Utils.hs:-- TODO: Replace with something better than * (mustache templating)
* src/Apod.hs:-- TODO: Look for a better way to fix `<p>`. Salvation.
* src/Weather.hs:-- TODO allow user-defined file paths.
* src/Smbc.hs:-- TODO make part of a SmbcConfig file.

Fix documentation spacing.

Compile with:

` $ stack build --no-nix --docker && cp $(stack --docker --no-nix path --local-install-root)/bin/DailyReporter-exe bootstrap && zip -r function.zip bootstrap configs templates`

Then upload the zipped file to AWS lambda. The handler should be
"app/Main.handler", it should use a custom runtime, memory should be ~300MB, and
runtime should be ~3 minutes although it usually doesn't reach those limits. To
make it go off every morning, add a trigger from eventbridge and use their cron
syntax. I used ` cron(00 12 * * ? *) ` and edited the input to be a constant
`[]`.

If you want to compile the documentation, use

` $ stack build --haddock --no-haddock-deps`

Created By:
Jackson Garner

Contributions from:
Joseph McKinsey

Based on a Ad-hoc Python Script by Joseph McKinsey
