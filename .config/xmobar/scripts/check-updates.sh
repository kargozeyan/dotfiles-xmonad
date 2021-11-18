#!/bin/bash

res=$(checkupdates | wc -l)

if [ "$res" -gt "0" ]; then
	echo " $res"
else
	echo " No updates"
fi
