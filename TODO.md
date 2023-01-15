# TODO

Working on:
- [X] allow for updates of recipe images
- [X] Set up recipe updates to re-create the body via ocr. Delay feature for
      editing the body until it's requested.
- [ ] Fields for editing the name and tags
- [ ] Add UI for displaying and re-arranging the recipes

Nixos
- [ ] Isolate the dev shell from changes in the repo so it doesn't rebuild the
  dev sell every time.

Pi Deployment:
- [ ] build and package up the production distribution of the vue app using nix
      so it can be included in the raspberry pi nixos thing.
- [X] rewrite the server to take commandline arguments determining production,
      port, sqlite db file, and static file directory.
- [ ] write a nice, configurable, nixos module that takes port arguments.
- [ ] use https.

Feedback
- [ ] OCR takes forever on the pi. Hash the image or send whether they've
  been edited.
- [ ] Improve UI showing recipe saving progress bar and disable the save button
  while uploading.
- [ ] Implement OCR queue. STM?
- [ ] Make text in content field bigger.
- [X] Tags aren't being saved. It looks like there's an SQL error involving
  syntax somehow???
- [X] can't delete recipes either.
- [ ] Tags can't be searched.
- [ ] Tesseract doesn't have a specified dpi?
- [ ] Don't log the whole image and text.
- [ ] Tesseract 5 is out.
- [ ] maybe some UI to confirm the content before saving an image.
