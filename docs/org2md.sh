#!bin/bash
echo "begin to convert"
pandoc -f org -s README.org -t markdown_github -o README.md
echo "convert has been completed"
