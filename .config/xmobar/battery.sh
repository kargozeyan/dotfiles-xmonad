#!bin/bash

# while test $# -gt 0; do
# 	case "$1" in 
# 		--normal)
# 			shift 
# 			normal=$1
# 			shift 
# 			;;
# 		--low)
# 			shift 
# 			low=$1
# 			shift
# 			;;
# 		--charging)
# 			shift 
# 			charging=$1
# 			shift
# 			;;
# 		*) 
# 			echo "$1 is not recognized"
# 			return 1;
# 			;;
# 	esac
# done
			
capacity=$(cat /sys/class/power_supply/BAT0/capacity)
status=$(cat /sys/class/power_supply/BAT0/status)
CHARGING_STATUS="Charging"


lvl=$(( $capacity / 20 ))

if [ "$status" == "$CHARGING_STATUS" ]; then
	echo "<fc=#a3be8c> $capacity</fc>"
elif [ $capacity - 0 < 20 ]; then
 	if [ $capacity - 0 > 10 ]; then
 		icon=""
 	else
 		icon=""
 	fi

	echo "<fc=#bf616a>$icon $capacity</fc>"
else
	case $lvl in
		1) ic="" ;;
		2) ic="" ;;
		3) ic="" ;;
		4) ic="" ;;
		5) ic="" ;;
	esac
	echo "<fc=#88c0d0>$ic $capacity</fc>"
fi