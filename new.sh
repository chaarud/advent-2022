# should be run as source ./new.sh abc to successfully cd in the terminal process

if [ -z "$1" ]; then
  echo "You must provide a directory name to set up"
  exit
fi

mkdir "$1"
cp template.scala "$1"/"$1".scala
touch "$1"/input
cd "$1" || exit