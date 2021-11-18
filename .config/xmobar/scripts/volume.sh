#!bin/bash
volume=($(awk -F "[][]" '/Left:/ { print $2" "$4 }' <(amixer sget Master)))
levelWithP=${volume[0]} # level with percentage at the end
level=${levelWithP::-1} # level without percentage symbol
muted=${volume[1]}
icons=("奄" "奔" "墳")
 
if [ "$muted" == "off" ]; then
	echo "婢"
else
	if [ "$level" -eq 0 ]; then 
		icon=${icons[0]}
	elif [ "$level" -le 50 ]; then
		icon=${icons[1]}
	else
		icon=${icons[2]}
	fi
	echo "$icon$level"
fi
