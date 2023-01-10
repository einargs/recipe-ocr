# TODO

Working on:
- [X] allow for updates of recipe images
- [X] Set up recipe updates to re-create the body via ocr. Delay feature for
      editing the body until it's requested.
- [ ] Fields for editing the name and tags
- [ ] Add UI for displaying and re-arranging the recipes

Pi Deployment:
- [ ] build and package up the production distribution of the vue app using nix
      so it can be included in the raspberry pi nixos thing.
- [ ] rewrite the server to take commandline arguments determining production,
      port, sqlite db file, and static file directory.
- [ ] write a nice, configurable, nixos module that takes port arguments.
- [ ] use https.
