yomi="$1"
bs="$(basename $yomi .zip)"

ext=${yomi##*.}

if [[ "$ext" != "zip" ]];
then
    echo "unrecognized"
    exit
fi

dirn="${bs}yomi"
mkdir "$dirn"
mv "$yomi" "$dirn"
cd "$dirn"
unzip "$yomi"
cd ..

stardir="${bs}_star"
mkdir "$stardir"
python yomi2tab.py "$dirn" -o "${bs}.tab"
mv "${bs}.tab" "$stardir"
cd "$stardir"
pyglossary "${bs}.tab" "${bs}.ifo"
rm "${bs}.tab"
sed -i "s/${bs}.tab/${bs}" "${bs}.ifo"
cd ..
