# TODO

Nixos
- [ ] Isolate the dev shell from changes in the repo so it doesn't rebuild the
  dev sell every time.

Pi Deployment
- [X] build and package up the production distribution of the vue app using nix
  so it can be included in the raspberry pi nixos thing.
- [X] write a nice, configurable, nixos module that takes port arguments.
- [ ] use https.
- [ ] Look into what user I should run the systemd service as. DynamicUser? See:
  - https://doc.flyingcircus.io/roles/fc-22.05-production/systemd.html
- [ ] It looks like systemd has some way to deal with port binding problems, so
  I'd rather just use that. If that doesn't work, there's `setcap`. See:
  - https://superuser.com/questions/710253/allow-non-root-process-to-bind-to-port-80-and-443
  - https://github.com/NixOS/nixpkgs/issues/11908

Feedback
- [ ] There's definitiely a problem with the image list where deleting the first
  image deletes the second.
- [ ] I think there's a bug where after changing image order the client doesn't
  properly update even though the server should be right.
- [X] Move the recipe saving indicator to show up in the middle of the screen.
- [X] Disable the save button while uploading a recipe.
- [ ] Make text in content field bigger.
- [ ] Tesseract doesn't have a specified dpi?
- [ ] Don't log the whole image and text.
- [ ] Tesseract 5 is out.
- [ ] maybe some UI to confirm the content before saving an image.
- [ ] Implement OCR queue. STM? Would this even help? I think it's more
  important to just make sure that we don't end up with the same recipe being
  saved twice.
