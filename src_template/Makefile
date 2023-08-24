#

.SUFFIXES:

eflist := $(shell ls *.ef)   # e.g., example.ef
filebase := $(basename $(notdir $(eflist))) # => example
f90list := $(addsuffix .F90, $(filebase)) # => example.F90
modlist := $(addsuffix .mod, $(filebase)) # => example.mod
objlist := $(addsuffix .o, $(filebase)) # => example.o

mpi_nprocs := $(shell ../bin/grep_process_num_from_src.sh const_sim.ef)

.SECONDARY: $(f90list)  # to avoid deleting F90 files.
       # .SECONDARY: obj/%.F90 does not work (GNU Make 3.81).

.PHONY: clean line list

%.F90: %.ef
	$(EFPP) $< > $@

%.o: %.F90
	$(FC) $(FFLAGS) -o $@ -c $< $(FLIB)

EFPP = ../bin/efpp.py

# MYLIB_NAME =  
# MYLIB_DIR  = 
# FLIB = -I$(MYLIB_DIR) -L$(MYLIB_DIR) -l$(MYLIB_NAME)
FLIB = 


this_host := $(shell hostname | cut -c1-3)

ifeq ($(this_host),alf)  # Alfven
  FC = mpinfort
  FFLAGS = 
else ifeq ($(this_host),pif)  # Kobe
  FC = mpifrtpx
  FFLAGS = -X03 -Free -Kopenmp -NRtrap
else ifeq ($(this_host),fes)  # NIFS
  FC = mpifrtpx
  FFLAGS := -Kopenmp
  # FFLAGS += -X03 -Free -NRtrap -Qt -Koptmsg=2
  # FFLAGS += -Haefosux
  # FFLAGS += -g
  # FFLAGS += -Nquickdbg=argchk
  # FFLAGS += -Nquickdbg=subchk
else
  FC = mpifort
  FFLAGS = 
endif


runme: program 
	mpiexec -n $(mpi_nprocs) ./program

program: $(objlist)
	$(FC) $(FFLAGS) -o program $(objlist) $(FLIB)

-include depend_list.mk

depend_list.mk: *.ef
	../bin/gendep.sh > $@ 


line:
	@echo "="{1..100} | sed 's/[ 0-9]//g' # bash one-liner for a line

clean:
	rm -rf depend_list.mk
	rm -rf *.o *.lst *.F90 *.mod *.L
	rm -rf program
