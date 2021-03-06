folderName="_gen/$1"
projectName="aoc$1"

dataStringMarker="<Compile Include=\"Program.fs\" \\/>"
dataExport="    <Content Include=\"data/*.*\">\\n      <CopyToOutputDirectory>Always</CopyToOutputDirectory>\\n    </Content>"
dataExport="$dataExport\n    <ProjectReference Include=\"..\\\\..\\\\lib\\\\lib.fsproj\">\n      <Name>lib.fsproj</Name>\n    </ProjectReference>"

if [ -d "$folderName" ]; then
    echo "folderName '$folderName' already exists. No project was created"
    exit 1
fi

echo "Creating new project '$projectName' in folder '$folderName'"

dotnet new console -lang "F#" -n "$projectName" -o "$folderName"
dotnet sln add "$folderName"

mkdir "./$folderName/data"
touch "./$folderName/data/source.txt"

# add data export to project file
awk -v replacement="$dataExport" "/$dataStringMarker/ { print; print replacement; next }1" "./$folderName/$projectName.fsproj" > temp
cat temp > "./$folderName/$projectName.fsproj"
rm temp
