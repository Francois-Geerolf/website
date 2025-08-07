
# A WHOLE FOLDER ??


mkdir ~/Dropbox/icon/flag/small
mkdir ~/Dropbox/icon/flag/vsmall

cd ~/Dropbox/icon/flag
scp south-korea.png korea.png
scp slovakia.png slovak-republic.png
scp czech-republic.png czechia.png

cd ~/Dropbox/icon/flag/round
scp europe.png european-union.png
scp europe.png euro-area.png
scp hongkong.png hong-kong.png



scp round/south-korea.png round/korea.png
scp square/south-korea.png square/korea.png
scp waving/south-korea.png waving/korea.png
scp small/south-korea.png small/korea.png


scp round/czech-republic.png round/czechia.png
scp square/czech-republic.png square/czechia.png
scp waving/czech-republic.png waving/czechia.png
scp small/czech-republic.png small/czechia.png

cd ~/Dropbox/icon/flag

for f in european-union.png; 
do filename="${f%.*}";
echo "Currently converting: $filename";
convert $filename.png -resize 50 small/$filename.png;
convert $filename.png -resize 25 vsmall/$filename.png;
done


scp united-states-of-america.png united-states.png
scp round/united-states-of-america.png round/united-states.png
scp square/united-states-of-america.png square/united-states.png
scp waving/united-states-of-america.png waving/united-states.png
scp small/united-states-of-america.png small/united-states.png


cd ~/Dropbox/icon
sh _to_website.sh



