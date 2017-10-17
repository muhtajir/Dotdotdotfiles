#!/bin/bash
source $(dirname $(realpath $0))/colors

calc() {
    awk "BEGIN { print "$*" }";
}

read -r -a df_line <<< $(df -BM -l --output=source,size,used | grep ${BLOCK_INSTANCE})
used=${df_line[2]::-1}
size=${df_line[1]::-1}
percentage=$(calc $used/$size*100)
left=$(calc $size-$used)

if (( $left > 9999 )); then
    left="$(( $left / 1000 ))G"
else
    left="${left}M"
fi

echo "${percentage%.*}% <span size='x-small'>($left)</span>"
echo ${percentage%.*}%

if (( ${percentage%.*} > 92 )); then
    echo $color04
fi
