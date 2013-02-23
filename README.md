JotFractals
===========

A scheme script for producing fractals using jot as the iterator for values of points.

How To Generate Things
======================

    > (load "jot.scm")
    > (runjot "1000-bsa.ppm" binstrapp 1000 150)
    > (runjot "3000-bsa.ppm" binstrapp 3000 150)
    > (runjotcolor "1200-rbgtest.ppm" rbcolors2 1200 150)
    > (runjotcolor "1200-rbgtest2.ppm" rbcolors3 1200 150)
    > (runjotcolor "3000-colorset.ppm" 
        (color-set binstrapp color-1) 3000 255)

That's pretty much it. To generate the images from the files, you just use netpbm:

    ppmtojpeg [filename] > [output.jpg]



