# Choosing the right package #

In the download-section, there are different mocca packages:

  1. mocca23: This is designed for Ubuntu 8.04 with EMC, version <= 2.3.5
  1. mocca24: For the newer EMC (2.4.x)
  1. mocca25: For the new LinuxCNC (2.5.x)
  1. mocca26: For the upcoming LinuxCNC (2.6.x)

If you installed EMC via the recent LiveCD (oct. 2012), then you will choose mocca25.
If your installation is older, then you go with mocca23 or -24.

Unfortunately Mocca is not available for EMC with Ubuntu 6.06.
You will have to update Ubuntu first, before using mocca.

**Make sure** you download the package designed for your Ubuntu version.

  * Ubuntu 8.04 is Hardy Heron
  * Ubuntu 10.04 is Lucid Lynx

You can find the operatin system in the description of each download-element,
and also the filename starts with the version of the target-ubuntu (e.g. 8.04_)_


# Installation #

The installation itself is very easy.
Just download the correct package, and open it by doubleclicking on its icon.
Then click on install, and wait a few seconds.
You could also install it via terminal.

# Configuration #

Now you need to tell EMC, that you don't like axis any more ;)


---

**Update**
Instead of using the way described below, you can now use the mocca-setup wizard.
To do so, download and install the mocca-setup package, and afterwards type mocca-setup in a terminal.

---


To do so, find the INI-file of your EMC-Configuration.
Normally it should be found here:
/home/**your\_username**/emc2/configs/**your\_config**/**your\_config**.ini

Now look out for a line, beginning with "DISPLAY = "
Replace the name of your former display-program with "mocca"

It should look like this, now:
```
DISPLAY = mocca
```


---


During installation, different designs have been copied to this directory:
/usr/share/**package\_name**/skins

if you installed EMC 2.4.x for example, the the path would look like this:
/usr/share/mocca24/skins

Now, choose a design, with a suiting resolution and language.
Keep the path to this design in mind.


---


Back to your INI-file:
Scroll to the bottom, and add a new section:
[MOCCA](MOCCA.md)

In this new section, add the variable:
CONFIG =

This variable has to contain the path to your design.
The section should then look like this:
```
[MOCCA]
CONFIG = /usr/share/mocca24/skins/default_en
```

Then save your INI-file. Now you're done.
Start EMC with your profile and enjoy the new GUI.

# possible problems #
## missing dependicies ##
Maybe, there are some packages missing for the installation of mocca.
Please ensure these packages are installed, and then try to install mocca again:

### Ubuntu 8.04 Hardy Heron ###
  * libglib1.2dbg
  * libgdkpixbuf2
  * libgtkglext1

### Ubuntu 10.04 Lucid Lynx ###
  * libgtkglext1