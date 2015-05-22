whattocount="lines"
showoutline=0
levelout="info"
rootdir=""
files=""
entities=:""


function getparamvals() {
    _getparamvals_result=($OPTARG)
    OPTARG=${!OPTIND}
    while [ ${OPTARG:0:1} != "-" -a $OPTIND -le $# ]; do
	_getparamvals_result+=" "
        _getparamvals_result+=$OPTARG
	OPTIND=$((OPTIND+1))
	OPTARG=${!OPTIND}
    done
}

while getopts "sef:c:r:l:" opt; do
  case $opt in
    f) getparamvals $@
       files=$_getparamvals_result
       ;;
    r) getparamvals $@
       rootdir=$_getparamvals_result
       ;;
    e) getparamvals $@
       entities=$_getparamvals_result
       ;;
    c) getparamvals $@
       whattocount=$_getparamvals_result
       ;;
    s) showoutline=1
       ;;
    l) getparamvals $@
       levelout=$_getparamvals_result
       ;;
    \?) exit
  esac
done
