# Daily Reporter

Uses Nix and a Github action to send out the email. You can build the project
using `nix-build` or `stack build`.

If you want a really fast `nix-build`, visit
https://app.cachix.org/cache/jhgarner-projects.

When running the app, you must have an environment variable called `CONFIG` with
the following contents:

```yaml
{
    "fromName": "Someone",
    "fromEmail": "someEmail@gmail.com",
    "to": [
      {
        "name": "someone else (or maybe not)",
        "email": "email@gmail.com"
      }
    ],
    "subject": "Daily Reporter",
    "accessKey": "AWS access key here with access to SES and an S3 bucket",
    "secretKey": "Similar to above",
    "apodApikey": "Get this from Nasa's website",
    "weatherApikey": "TODO Darksky doesn't give these out anymore",
    "long": "50.111",
    "lat": "-50.111"
}
```


Created By:
Jackson Garner

Contributions from:
Joseph McKinsey

Based on a Ad-hoc Python Script by Joseph McKinsey
