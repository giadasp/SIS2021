for ((i = 1 ; i <= 29 ; i++)) ; do
echo Step $i:
cd "C:/Users/giada.spaccapanico2/Desktop/repos/SIS2021/TIMSS/2015/45-45 15_19/step $i"
julia add.jl #|| break
done