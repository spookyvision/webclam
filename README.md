# webclam
USB webcam settings utility. I was annoyed there was no free tool under macos, so I wrote one IN RUST!!!11!. Not tested under water.

## Attention: this tool has become useless
I wrote this mainly for OSX because I was too stingy/stubborn to buy an existing settings app.
In the meantime Apple has upped their USB security game, making my app lock up and I can't be arsed to look into it.

## Previously
Usefulness on Linux and Windows is currently limited because taking control over the USB requires admin privileges or at least (under Linux) [write access to the video device](https://stackoverflow.com/a/32022908). A better approach would be to use those platforms' existing high level control interfaces (e.g. under Linux, communicate using `/dev/videoX`) - patches welcome!


## Installation
Cargo packages coming soon; binaries probably not.

### Build from Source
Dependencies:

- CMake
### Linux
Dependencies:

- gtk3-devel

### Windows
Dependencies:

- libusb (probably best installed via [vcpkg](https://github.com/libusb/libusb/wiki/Windows#vcpkg_port))
- libclang (can be installed with VS build tools? I think?)
