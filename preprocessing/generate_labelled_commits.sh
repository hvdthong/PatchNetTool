#!/bin/bash

BEGIN_DATE=""
END_DATE=""
DEVEL_REPO=""
STABLE_REPO=""
STABLE_BRANCH=""
OUTPUT=""

TMPFILE=/tmp/commits.tmp

usage()
{
	echo "Usage:${0} -b BEGIN_DATE (format: yyyy-mm-dd)"\
		" -e END_DATE (format: yyyy-mm-dd) "\
		" -d DEVEL_REPO (A directory containing Linux's devel "\
		"version of GIT repository.)"\
		" -s STABLE_REPO (A directory containing Linux's stable "\
		"version of GIT repository.) "\
		"-r STABLE_BRANCH (eg: \"\" stands for linux-stable main line)"\
		" -o OUTPUT" 1>&2;
	exit 1;
}

get_commits()
{
	rm -f ${TMPFILE}
	git -C ${DEVEL_REPO} log --since=${BEGIN_DATE} --until=${END_DATE} \
		--pretty=format:%H  > ${TMPFILE}
}

label_commits()
{
	rm -f ${OUTPUT}
	cat $TMPFILE | while read LINE
	do
	if [ -z "$STABLE_BRANCH" ]; then
		git -C ${STABLE_REPO} log --oneline -1 ${LINE}
		if [ $? -eq 0 ] ; then
			echo "${LINE}: true" >> ${OUTPUT}
		else
			echo "${LINE}: true" >> ${OUTPUT}
		fi

	else
		key="commit ${LINE} upstream"
		found=$(git -C ${STABLE_REPO} log --branches=${STABLE_BRANCH} --grep "$key")
		echo $found
		echo $found | grep "${key}"
		if [ $? -eq 0 ] ; then
			echo "${LINE}: true" >> ${OUTPUT}
		else
			echo "${LINE}: false" >> ${OUTPUT}
		fi
	fi
	done
}


while getopts "b:e:d:s:r:o:" arg; do
	case ${arg} in
		b)
			BEGIN_DATE=${OPTARG}
			;;
		e)
			END_DATE=${OPTARG}
			;;
		d)
			DEVEL_REPO=${OPTARG}
			;;
		s)
			STABLE_REPO=${OPTARG}
			;;
		r)
			STABLE_BRANCH=${OPTARG}
			;;
		o)
			OUTPUT=${OPTARG}
			;;
		*)
			usage
			exit 1
			;;
	esac
done

get_commits
label_commits

exit 0

