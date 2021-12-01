#!/usr/bin/fish

set -l session_id (cat .session_id)

for arg in $argv
    # download input
    curl -b "session=$session_id" https://adventofcode.com/2019/day/$arg/input > inputs/$arg
    # clone template
    set -l pfile src/Puzzles/P"$arg".hs
    if ! test -e $pfile
        cp src/Puzzles/Template.hs $pfile
        perl -pi -e "s/Puzzles\.Template/Puzzles.P$arg/; s/PUZZLE/$arg/g" $pfile
    end
    # import into main
    perl -pi -e 'if (/^import Puzzles\.P(\d+)$/) {$a=1; $b=1 if ($1 eq "'$arg'")} else {print "import Puzzles.P'$arg'\n" if ($a && !$b); $a=$b=0}' app/Main.hs
    # add to puzzle list
    perl -pi -e 's/\]/,p'$arg']/ if (/^puzzles = \[(p\d+,?)+\]$/ and not grep {$_ eq "p'$arg'"} /p\d+/g)' app/Main.hs
end
