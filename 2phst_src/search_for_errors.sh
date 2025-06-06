#!/bin/bash
grep -ilr "." -e "Execution halted" > files_with_errors.txt
