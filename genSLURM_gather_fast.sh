#!/bin/bash
# submit_wrapper.sh

PROJECT="$1"
WORKDIR=$(pwd)
LOGFILE="$WORKDIR/slurm-${PROJECT}-%j.out"

TMPFILE=$(mktemp)
cat << EOF > $TMPFILE
#!/bin/bash
#SBATCH -D $WORKDIR
#SBATCH -o $LOGFILE
#SBATCH -J ${PROJECT}_job

# Your job commands here
echo "Running project $PROJECT in $WORKDIR"
EOF

sbatch $TMPFILE
rm $TMPFILE