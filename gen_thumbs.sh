#!/bin/sh
# Generate thumbnails of frontpage of all PDFs

# Then use a GUI Emacs to open them

# Choices:       http://ghostscript.com/doc/current/Devices.htm#PNG
#  -sDEVICE=png256 (256-color PNG, also can be png16m, pngalpha, etc)
#  -r96 (DPI, 144 is slower but higher res. 72 is faster but ugly)

echo "Generating PDF thumbnails in " `pwd`
sleep 1

mkdir .pdf_thumbs
find . -name '*.pdf' -maxdepth 1 | while read i ; do
	OUT=.pdf_thumbs/"${i%%pdf}"png
	if [ ! -s "$OUT" ] ; then
	   echo "***> $i"
	   gs -sDEVICE=png256 -dFirstPage=1 -dLastPage=1 -o "$OUT" -r96 "$i"
	fi
done
