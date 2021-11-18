#!bin/bash
			
capacity=$(cat /sys/class/power_supply/BAT0/capacity)
status=$(cat /sys/class/power_supply/BAT0/status)
CHARGING_STATUS="Charging"
icons=("" "" "" "" "" "")

lvl=$(( ($capacity - 0)/ 25 ))

function getIconIndex {
	len=${#icons[@]}
	interval=$((100/($len-1)))
	return $(($1/interval))
}

if [ "$status" == "$CHARGING_STATUS" ]; then
	echo "${icons[0]} $capacity"
else
	getIconIndex $capacity
	echo "${icons[$?]} $capacity"
fi
