# README
# creates a basic folder structure for the repo

$foldernms = "data", "data_raw", "figures", "notebooks", "R", "sql"
$dirpath = (get-location).path
foreach ($folder in $foldernms)
{
New-Item -Path $dirpath -Name $folder -ItemType Directory
}
