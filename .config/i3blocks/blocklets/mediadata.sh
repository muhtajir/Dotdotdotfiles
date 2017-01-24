sleep .5

artist=$(playerctl metadata artist)
title=$(playerctl metadata title)

if [[ $? != 0 ]]; then
    # echo " <i>Silence</i>"
    echo ""
else
    echo " <b>${artist}</b> - ${title}"
fi
