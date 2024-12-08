# This script will remove any silence in any song inside of $dir
# and recode everything at the same bitrate at $outputDir

# Define the path to the directory
$dir = "X:\Música" # "C:\Users\RMRub\Desktop\t" #

# Define the output directory
$outputDir = "X:\trimsong\"

# Define the output files
$tobeRepairedFile = Join-Path -Path $outputDir -ChildPath "toberepaired.txt"
$normalSongsFile = Join-Path -Path $outputDir -ChildPath "normalsongs.txt"

# If a song is not detected, it will be skipped
$noiseLevel_st = "-50dB" # Noise level threshold at start of song
$noiseLevel_fn = "-46dB" # Noise level threshold at end of song
$duration = "0.046"      # Silence duration to be removed
$duration_detect = "3"   # Silence duration to be detected

# Clear the output files if they exist
if (Test-Path $tobeRepairedFile) { Clear-Content $tobeRepairedFile }
if (Test-Path $normalSongsFile) { Clear-Content $normalSongsFile }

# Get all mp3 files in the directory and its subdirectories
$files = Get-ChildItem -Path $dir -Recurse -Include *.mp3

# Loop through each file
foreach ($file in $files) {
    # Use ffmpeg to get the bitrate of the input file
    $bitrate = & ffmpeg -i $file.FullName 2>&1 | Select-String -Pattern "Audio:" | ForEach-Object { ($_ -split ",")[4].Trim() }
    # Remove the "kb/s" unit from the bitrate
    $bitrate = $bitrate -replace " kb/s","k"

    # Use ffmpeg to check for silence
    $output = & ffmpeg -i $file.FullName -af "silencedetect=noise=$($noiseLevel_fn):d=$($duration_detect)" -f null - 2>&1

    # Join the output into a single string
    $outputString = $output -join " "

    # Check if silence was detected that meets the criteria
    if ($outputString.Contains("silence_end:") -and $outputString.Contains("silence_duration:")) {
        # Write the file path to the to be repaired file
        Add-Content -Path $tobeRepairedFile -Value $file.FullName

        # Trim the silence from the start and end of the audio file - Comment below to not repair
        $relativePath = $file.FullName.Substring($dir.Length)
        $trimmedFile = Join-Path -Path $outputDir -ChildPath $relativePath
        $trimmedFileDir = Split-Path -Path $trimmedFile -Parent
        if (!(Test-Path $trimmedFileDir)) { New-Item -ItemType Directory -Force -Path $trimmedFileDir }
        & ffmpeg -i $file.FullName -af "silenceremove=start_periods=1:start_duration=$($duration):start_threshold=$($noiseLevel_st),silenceremove=stop_periods=-1:stop_duration=$($duration):stop_threshold=$($noiseLevel_fn)" -b:a $bitrate -write_xing 0 -y $trimmedFile
    } else {
        # Write the file path to the normal songs file
        Add-Content -Path $normalSongsFile -Value $file.FullName
    }
}
