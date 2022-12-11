#!/bin/zsh

#
# Try to bring across all the standard hardcaml documentation and automatically
# make it ready to publish on the website.
#

HARDCAML=../../../hardcaml

#
# Map each documentation page to it's place in the nav
#

typeset -A category

category[always]="getting-started"
category[circuits]="getting-started"
category[combinational_logic]="getting-started"
category[index]="getting-started"
category[installing_with_opam]="getting-started"
category[instantiation]="getting-started"
category[introduction]="getting-started"
category[naming]="getting-started"
category[rtl_generation]="getting-started"
category[sequential_logic]="getting-started"
category[simulation]="getting-started"
category[waveforms]="getting-started"

category[hardcaml_interfaces]="advanced-concepts"
category[module_hierarchy]="advanced-concepts"
category[state_machine_always_api]="advanced-concepts"
category[enums]="advanced-concepts"
category[waveterm_interactive_viewer]="advanced-concepts"
category[simulating_with_interfaces]="advanced-concepts"

category[counter_example]="examples"
category[fibonacci_example]="examples"
category[serial_multiplier_example]="examples"

category[accelerating_simulations]="libraries"

#
# Copy and rename, add front matter.
#

cp $HARDCAML/docs/*.mdx .
for file in *.mdx
do
    name=$(basename -s .mdx $file)
    cat=${category[$name]}
    header="\
---
title: $name
layout: default
category: $cat
---
"
    echo $name $cat
    echo "$header$(cat $file)" | \
        sed 's/mdx/md/g' |
        sed 's/ocaml skip/ocaml/g' > $name.md
done

rm *.mdx
