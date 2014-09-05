CONV=$1
CORPUS="$HOME/data/ami/Data/AMI/NXT-format/AMI-metadata.xml"

VARS='($d dact)($dt da-type): ($d >"da-aspect" $dt)'

java FunctionQuery -o $CONV -c $CORPUS -q "$VARS" -atts '$d@nite:id' '$d@starttime' '$d@endtime' '$d@who' '$dt@gloss' '$d'

