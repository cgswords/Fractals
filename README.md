JotFractals
===========

A scheme script for producing fractals using jot as the iterator for values of points.

How To Generate Things
======================

    > (load "jot.scm")
    > (runjot "1000-bsa.ppm" binstrapp 1000 150)
    > (runjot "3000-bsa.ppm" binstrapp 3000 150)
    > (runjotcolor "1200-rgbtest.ppm" rbcolors2 1200 150)
    > (runjotcolor "1200-rgbtest2.ppm" rbcolors3 1200 150)
    > (runjotcolor "3000-colorset.ppm" 
        (color-set binstrapp color-1) 3000 255)

That's pretty much it. To generate the images from the files, you just use netpbm:

    ppmtojpeg [filename] > [output.jpg]

Nova Fractals
=============

I added nova fractals as well. You use them as follows:

    > (load "nova.scm")
    > (runnovacolor "testnova-8-20-20"   
        (color-gen (nova 3 2 0 255) gen-rainbow) 
        1200 8 20 20)

They're fun to play with. The scaling doesn't seem to be doing exactly what I'd like it to right now, but I am not in a mood to fix it.


