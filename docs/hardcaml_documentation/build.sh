#!/bin/sh
HARDCAML=../../../hardcaml

# Copy and rename
cp $HARDCAML/docs/*.mdx .
for file in *.mdx
do
    name=$(basename -s .mdx $file)
    echo "---\ntitle: $name\nlayout: default\n---\n$(cat $file)" | \
        sed 's/mdx/md/g' |
        sed 's/ocaml skip//g' > $name.md
done

rm *.mdx
