#
# Makefile for efpp sample code
#
#       * Akira Kageyama,
#         Kobe University, Japan
#         kage@port.kobe-u.ac.jp
#         2023.08.11
#

.SUFFIXES:

eflist := $(shell ls *.ef)   # e.g., example.ef
filebase := $(basename $(notdir $(eflist))) # => example
f90list := $(addsuffix .F90, $(filebase)) # => example.F90
modlist := $(addsuffix .mod, $(filebase)) # => example.mod
objlist := $(addsuffix .o, $(filebase)) # => example.o

.SECONDARY: $(f90list)  # to avoid deleting F90 files.
       # .SECONDARY: obj/%.F90 does not work (GNU Make 3.81).


.PHONY: clean

FC = gfortran

runme: test
	./test


%.F90: %.ef
	../../efpp.py $< > $@

%.o: %.F90
	$(FC) $(FFLAGS) -o $@ -c $<

-include depend_list.mk

depend_list.mk: *.ef
	../../gendep.sh > $@ 

# time.o: constants.o
# vecfield.o: constants.o
# main.o: constants.o time.o vecfield.o

test: main.o
	$(FC) -o test *.o

clean:
	rm -rf *.o *.lst *.F90 *.mod test
	rm -f depend_list.mk

