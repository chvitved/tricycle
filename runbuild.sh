echo "Building $revision"
let "rnd=$RANDOM % 2"
if let "rnd == 0" ; then
    echo "Succeeding" ; true
else
    echo "Failing" ; false
fi

