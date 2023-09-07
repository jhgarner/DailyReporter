# Daily Reporter

A bot for sending out batched Matrix messages whenever a source you're watching
changes. For example, it can collect comics from sources like Xkcd or Smbc and
send them every morning when a new one is detected. Works best if you run it on
some kind of schedule. I use Github actions to run it every morning.

You can build the project using `nix-build` or `cabal build`. If you want a
really fast `nix-build`, visit https://app.cachix.org/cache/jhgarner-projects.

When running the app, you must have an environment variable called `CONFIG` with
the following contents:

```yaml
{
    "apodApikey": "Key from api.nasa.gov",
    "deviceId": "A device id for the bot",
    "roomId": "Your Matrix room",
    "password": "Your bot's Matrix password",
    "weatherApikey": "Key from api.pirateweather.net",
    "long": "10.0",
    "lat": "-10.0"
}
```


Created By:
Jackson Garner

Contributions from:
Joseph McKinsey

Based on a Ad-hoc Python Script by Joseph McKinsey
