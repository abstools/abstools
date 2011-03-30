#!/usr/bin/python

#  Copyright 2003-2011
#  Software Technology Group, University of Kaiserlautern, Germany
#  all rights reserved
#  written by Jan Schaefer
#
# This script replaces all copyright notes from the head
# of all Kava files of a directory
# A copyright note is a Non-Javadoc comment at the head
# of a java file.
# It is placed before the package keyword.
#
# pseudo code :
#
# go to the specified directory
# get a list of all java files
# loop over the list :
#      if file is a directory make recursive call
#      read current java file
#      read until the keyword package
#      read the rest of the file including
#           the line with the package keyword to a variable
#      delete old file
#      write copyright note to new file
#      write rest of the file to the new file
# finished
import sys
import os
import string

def printUsage():

    print
    print "Usage: "+sys.argv[0] + " <filename>"
    print
    print "replaces the copyright note of the given filename"
    print "if the filename is a directory replaces recursivly"
    print "all Java files in this directory and all subdirectories"
    print
    print "(C) Copyright 2003-2011"
    print "Software Technology Group, University of Kaiserlautern, Germany"
    print "All rights reserverd"
    print "written by Jan Schaefer"
    print

def replaceJavaFile(filename):
    print "Replacing copyright note of file "+filename
    file = open(filename, 'r')

    lines = file.readlines()
    i = 0
    for line in lines:
        if -1 < string.find(line,"package"):
            break
        i = i+1

    copyrightText = """/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
"""

    newlines = []
    newlines.extend(lines[i:])

    file.close()

    file = open(filename, 'w')
    file.write(copyrightText);
    for line in newlines:
        file.write(line)



def replaceFile(file):
    """checks if file is a Java file and if so calls replaceJavaFile"""
    ext = os.path.splitext(file)
    if (ext[1] == ".java"):
        replaceJavaFile(file)


def replace(path):
    if (os.path.isfile(path)):
        replaceFile(path)
    else:
        dirlist = os.listdir(path)
        for file in dirlist:
            replace(path+"/"+file)


##################################################
# MAIN
##################################################

if len(sys.argv) < 2:
    printUsage()
else:
    replace(sys.argv[1])







