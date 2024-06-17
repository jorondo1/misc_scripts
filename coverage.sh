#!/bin/bash -l

#SBATCH --job-name=assembly_coverage
#SBATCH -D /nfs3_ib/nfs-ip34/home/def-ilafores/analysis/boreal_moss
#SBATCH -o /nfs3_ib/nfs-ip34/home/def-ilafores/analysis/boreal_moss/logs/coverage-%A_%a.slurm.out
#SBATCH --time=48:00:00
#SBATCH --mem=30G
#SBATCH -N 1
#SBATCH -n 2
#SBATCH -A def-ilafores
#SBATCH --time=12:00:00

# Load necessary modules
module load bwa-mem2/2.2.1
module load samtools/1.16.1
module load bedtools/2.30.0

# Define variables
SAM_NUM=$(awk "NR==$SLURM_ARRAY_TASK_ID" ${ANCHOR}${COVERAGE})
IFS=$'\t' read -r SAM_ID FQ_P1 FQ_P2 ASSEMBLY <<< "$SAM_NUM" # array it up
export SAM_ID FQ_P1 FQ_P2 ASSEMBLY

ASSEMBLY="assembly.fasta"
FQ_P1="reads_R1.fastq"
FQ_P2="reads_R2.fastq"
OUT=$(dirname $ASSEMBLY)/coverage
THREADS=32

mkdir $OUT

# Step 1: Index the assembly
bwa-mem2 index $ASSEMBLY

# Step 2: Align reads to the assembly
bwa-mem2 mem -t $THREADS $ASSEMBLY $FQ_P1 $FQ_P2 > $OUT/aligned_reads.sam

# Step 3: Convert and sort the SAM file
samtools view -@ $THREADS -S -b $OUT/aligned_reads.sam > $OUT/aligned_reads.bam
samtools sort -@ $THREADS $OUT/aligned_reads.bam -o $OUT/sorted_reads.bam
samtools index $OUT/sorted_reads.bam

# Step 4: Calculate coverage
bedtools genomecov -ibam $OUT/sorted_reads.bam -g $ASSEMBLY > $OUT/assembly_coverage.txt

# Optional: Calculate average coverage
samtools depth $OUT/sorted_reads.bam > $OUT/coverage_per_base.txt
awk '{sum+=$3} END { print "Average coverage: ",sum/NR}' $OUT/coverage_per_base.txt > $OUT/average_coverage.txt

echo "Coverage calculation completed."
