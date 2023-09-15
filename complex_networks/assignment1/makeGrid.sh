# makeGrid.sh

#!/usr/bin/env bash

folderName="networkViz2/"
fileName="chart"
fileType="png"
outputFile="chartGrid2.png"

# Given you want to combine images in nrow*ncol pattern, then update those
# Files in $folderName should be numbered sequentially
ncol=4
nrow=4

rowFiles=()
for i in $(seq 0 $(($nrow-1)))
do
    files=()
    for j in $(seq 0 $(($ncol-1)))
    do
        k=$(($i*$ncol+$j+17))
        files+=" $folderName$fileName$k.$fileType "
    done
    rowFileName=" row"$i".png "
    rowFiles+=$rowFileName

    convert $files +append $rowFileName
done

convert $rowFiles -append $outputFile

rm $rowFiles 
