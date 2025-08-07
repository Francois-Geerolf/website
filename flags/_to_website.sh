local=~/Dropbox/data/flags
website=~/Dropbox/website/data/flags/
  
cd $local
mkdir $website

for filename in *.RData *.html
do
echo "Moving: $filename"
mv $local/$filename $website
chmod 644 $website$filename
done

rm -rf $website/*.pdf

scp -r $local/*_files $website
rm -r $local/*_files
scp -r $local/libs $website
rm -r $local/libs

# To fgeerolf.com ------ 

~/Dropbox/website.sh


