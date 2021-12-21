#Makefile for L3 software
#Developed for the sunstudio F95 compiler.
#2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
#2012/08/24  MJ creates adds "includes" for better configurability
#2015/02/27 OS: now sources dependencies.inc, objects now located in
  #separate folder, libfile currently not used

archfile=../orac/config/arch.gfortran.inc
libfile=../orac/config/lib.inc
include $(archfile)
include $(libfile)

OBJS=.
AUXFLAGS=-Q $(OBJS) -J $(OBJS)


L2toL3_OBJ =  $(OBJS)/l2tol3_script.o \
              $(OBJS)/vartypes.o \
              $(OBJS)/calender.o \
              $(OBJS)/common_constants.o \
              $(OBJS)/preproc_constants.o \
              $(OBJS)/structures.o \
              $(OBJS)/set_struct.o \
              $(OBJS)/nc_open.o \
              $(OBJS)/nc_dim_info.o \
              $(OBJS)/nc_read_file.o \
              $(OBJS)/nc_create_global.o \
              $(OBJS)/nc_close.o \
              $(OBJS)/nc_global_att_L3.o \
              $(OBJS)/nc_defdata.o \
              $(OBJS)/nc_write_L3.o \
              $(OBJS)/make_histograms.o \
              $(OBJS)/nc_defdata_hist.o \
              $(OBJS)/locate.o \
              $(OBJS)/nc_write_L3_histograms.o \
              $(OBJS)/nc_create_globall2b.o \
              $(OBJS)/compute_stats.o

l2tol3_script.x: $(L2toL3_OBJ)
	$(F90) $(LFLAGS) -fno-range-check -o l2tol3_script.x $(L2toL3_OBJ) $(LIBS)

%.o:	%.f90 
	$(F90) $(FFLAGS) -fno-range-check $(INC) -c $<

depend:
	@../orac/tools/make_depend.pl $(L2toL3_OBJ) > dependencies.inc 

clean:
	rm -f *.o
	rm -f *.mod
	rm -f *.lst
	rm -f l2tol3_script.x

include dependencies.inc
