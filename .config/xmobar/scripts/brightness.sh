#!bin/bash

brightness=$(brightnessctl g)
max=$(brightnessctl max)

brightnessInPercentege=$((($brightness * 100)/$max))

echo $brightnessInPercentege
