#!/bin/bash
# Program:
#	Separate train.csv as 20 parts since there is 10,000,000 rows.
#	For calculating click_bool conveniently.
# History:
# 2017/06/01	LeongKaOn	First release

len=$(wc -l train.csv | cut -d " " -f 1)

echo 'START\n'
i=2	#start without head(row1)
name=0
row=500000
while [ $i -le $len ]
do
	name=$(($name+1))
	sed -n "${i},$(($i+$row-1))p" train.csv > "part${name}.csv"
	echo '\n'
	i=$(($i+$row))
done

