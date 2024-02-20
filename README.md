# Image_IO
Output in PPM and BMP formats; input in BMP, GIF, JPG, PNG, PNM, QOI, and TGA formats

Package Image_IO provides the ability to input and output images.

Uncompressed BMP is a universal format and can easily be converted to compressed formats. PPM is intended to be a set of portable formats, but lacks support by default on some platforms.

Write_P3 uses [PragmARC.Text_IO](https://github.com/jrcarter/PragmARC/blob/Ada-12/pragmarc-text_io.ads) to obtain uniform line terminators across platforms. This can be easily changed to Ada.Text_IO if the line terminators are not a concern.

Read is a wrapper around [Generic Image Decoder (GID)](https://github.com/zertovitch/gid) for the most common use: reading an image from a file. GID may be used to decode images from other sources, but that generality makes it complex to use. Read ignores transparency (Alpha channel) and only reads the first frame of an animation.

Pure_RGB is a demo program that reads an image using Read, modifies it to change all color components to the closest of 0 or 255, and outputs the result using Write_BMP. It is invoked with

     pure_rgb <name>

The output name is the input name with ".bmp" appended. Pure_RGB does no error handling; if the input file name is omitted, doesn't exist, or is not a supported image format, the program will terminate with an unhandled exception.

Image_IO and Pure_RGB have been compiled and tested with GNAT 12.3.0 on Linux and ObjectAda 10.5U3 on Windows.
