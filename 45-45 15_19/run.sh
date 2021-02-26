for ((i = 1 ; i <= 29 ; i++)) ; do
    echo Step $i:
    cd "45-45 15_19/step $i"
    julia add.jl #|| break
done