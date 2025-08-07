local=~/Dropbox/data/flags
website=~/Dropbox/website/data/flags/

mkdir $website

cd $local

for filename in *.html *.RData
do
  echo "Moving: $filename"
	mv $local/$filename $website
done

 
scp -r $local/*_files $website
rm -rf $local/*_files
scp -r $local/libs $website
rm -rf $local/libs
