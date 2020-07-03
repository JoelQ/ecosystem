# Ecosystem

Ecosystem balancing sim for Elm gamejam #4

## Running locally

```
bin/server
```

## Deployment

The game is automatically deployed to Netlify whenever the `master` branch gets
pushed. It is available at [https://elm-ecosystem.netlify.app].

[https://elm-ecosystem.netlify.app]: https://elm-ecosystem.netlify.app

## Release process

The game is deployed to two places. While the latest `master` is auto-deployed
to Netlify, we need to manually make a release to itch.io. We also cut a release
on GitHub.

1. Create a new tag with `git tag -a "vX.X.X"` with the desired version number.
2. Push tag with `git push --tags`
3. Make a production build with `bin/build` and compress the `dist/` directory
4. Find the tag on GitHub and create a new release. Attach the `dist.zip` file.
5. Publish to itch.io with `butler push dist.zip joelq/ecosystem:html5 --userversion X.X.X` (assumes you have [butler](https://itch.io/docs/butler/) installed).
6. Optionally create a new devlog on itch.io to discuss new features in the
   release. I often have very similar content to what I would put in the GitHub
   release description.
