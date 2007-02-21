new2_name=`echo $1 | sed 's/.cpp/.cxx/'`
echo $new2_name
svn mv $1 $new2_name

