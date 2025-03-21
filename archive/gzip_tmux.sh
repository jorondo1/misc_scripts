# Create a new tmux session named 'gzip_session'
tmux new-session -d -s gzip_session

# Generate the list of files to be gzipped
files=($(find cat_reads -type f \( -name "*_1.fastq" -o -name "*_2.fastq" \)))

# Get the total number of files
total_files=${#files[@]}

# Set the number of windows (maximum 48)
num_windows=24

# Calculate the number of files per window
files_per_window=$(( (total_files + num_windows - 1) / num_windows ))

# Distribute the gzip commands across tmux windows
for ((i=0; i<num_windows; i++)); do
    # Create a new window if not the first one
    if [ $i -gt 0 ]; then
        tmux new-window -t gzip_session
    fi
    
    # Calculate the start and end index for this window
    start_index=$(( i * files_per_window ))
    end_index=$(( start_index + files_per_window ))
    
    # Generate the gzip commands for this window
    commands=""
    for ((j=start_index; j<end_index && j<total_files; j++)); do
        file=${files[j]}
        commands+="gzip \"$file\"; "
    done
    
    # Send the commands to the window
    tmux send-keys -t gzip_session:$i "$commands" C-m
done

# Attach to the tmux session
tmux attach-session -t gzip_session


#
# ### cat paired unpaired
# for file in $(find cat_reads/ -type f -name '*paired_1.fastq'); do
# 	path="cat_reads/$sample/${sample}"
# 	sample=$(basename $file)
# 	sample=${sample%_paired_1.fastq}
# 	echo "concatenating sample $sample ..."
# 	cat ${path}_paired_1.fastq ${path}_unmatched_1.fastq > ${path}_1.fastq
# 	cat ${path}_paired_2.fastq ${path}_unmatched_2.fastq > ${path}_2.fastq
# 	gzip ${path}_1.fastq
# 	gzip ${path}_2.fastq
# done
