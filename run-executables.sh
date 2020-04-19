# Just launch all the executables in the project
stack build && clear && for p in `cat *.cabal | grep ^executable | sed s'/.* //'g` ; do
    echo "--------"; echo $p; echo; stack exec $p ;
done
