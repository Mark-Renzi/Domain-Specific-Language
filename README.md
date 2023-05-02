# CS-536-Group-Project

Code defining a domain specific language, parsed in Scala and transpiled to C.<br>
For WPI CS 536 - Programming Language Design.

## How to use
Run 'CustomLanguageParser.scala' and ensure the fastparse library from the sbt file is installed. This by default parses 'CustomLanguage.txt' into an AST, runs static type-checking analysis on it, and transpiles it to 'outputC.c'.

