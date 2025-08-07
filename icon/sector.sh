
# A WHOLE FOLDER ??


cd ~/Dropbox/icon/sector

mkdir ~/Dropbox/icon/sector/small
mkdir ~/Dropbox/icon/sector/vsmall

for f in *.png; 
do filename="${f%.*}";
echo "Currently converting: $filename";
convert $filename.png -resize 50 small/$filename.png;
convert $filename.png -resize 25 vsmall/$filename.png;
done

cd ~/Dropbox/icon
sh _to_website.sh
