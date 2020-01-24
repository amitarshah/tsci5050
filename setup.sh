#!/bin/bash

git init;
./initgitmods.sh;
git add .gitignore .Rprofile data *.R *.sh README.md LICENSE
git ci -a -m "First commit";
git push -u origin master;
