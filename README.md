# Investigating the Impact of the Growth in Diagnostic Procedures on Urgent and Emergency Care Services

Over the last decade, the growth in diagnostic testing in EDs has been rapid. This growth has implications for the timeliness of patient care; for decisions on whether to admit a patient; and for crowding in EDs. We aim to better understand the impact of this growth on urgent and emergency care services.

This repository contains the code for a project carried out by the Strategy Unit on behalf of NHS England.

## Running the code

### First time setup

We have setup a powershell script to create a set of empty directories that are used by the code within this project. Open a terminal window. You can do this by clicking the terminal tab within RStudio, cmd prompt, or opening powershell directly. Navigate to the project root directory and run: powershell .\create_directory_structure.ps1 If you get an error about running scripts being disabled you can temporarily enable local scripts by running:
`powershell .\create_directory_structure.ps1` 
If you get an error about running scripts being disabled you can temporarily enable local scripts by running:  
 - `powershell` to run powershell
 - `Set-ExecutionPolicy RemoteSigned -Scope Process` to temporarily enable local scripts  
 - `.\create_directory_structure.ps1` to run the script
