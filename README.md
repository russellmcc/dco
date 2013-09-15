This is a simple design for an incredibly cost-sensitive DCO module for eurorack format modular synthesizers.

Firmware was written with Arduino, using @brendan0powers's [Xmegaduino fork](https://github.com/brendan0powers/Xmegaduino).  To program, use the following avrdude command from the `firmware` directory, using an [avrismpmkII](http://www.atmel.com/tools/AVRISPMKII.aspx):

    avrdude -patxmega32a4 -cavrispmkII -Pusb -Uflash:w:firmware.hex

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License.](http://creativecommons.org/licenses/by-sa/3.0/deed.en_US)