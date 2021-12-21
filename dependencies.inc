$(OBJS)/l2tol3_script.o: $(OBJS)/calender.o $(OBJS)/make_histograms.o \
        $(OBJS)/nc_write_L3.o $(OBJS)/nc_write_L3_histograms.o \
        $(OBJS)/structures.o $(OBJS)/vartypes.o L2b_sampling.inc \
        L3abc_averaging.inc L3d_averaging.inc do_L2b_sampling.inc \
        prepare_L3abc_averaging.inc read_L2B_SUM_ORAC.inc \
        read_L2_input_FAME-C.inc read_L2_input_L3d.inc read_L2_input_ORAC.inc \
        read_L2_input_ORAC_L2B.inc write_L2b_data.inc write_L3abc_data.inc
$(OBJS)/calender.o: $(OBJS)/preproc_constants.o
$(OBJS)/preproc_constants.o: $(OBJS)/common_constants.o
$(OBJS)/structures.o: $(OBJS)/vartypes.o
$(OBJS)/set_struct.o: $(OBJS)/structures.o $(OBJS)/vartypes.o
$(OBJS)/nc_dim_info.o: $(OBJS)/vartypes.o
$(OBJS)/nc_read_file.o: $(OBJS)/vartypes.o
$(OBJS)/nc_create_global.o: $(OBJS)/vartypes.o
$(OBJS)/nc_defdata.o: $(OBJS)/vartypes.o
$(OBJS)/nc_write_L3.o: $(OBJS)/vartypes.o
$(OBJS)/make_histograms.o: $(OBJS)/structures.o $(OBJS)/vartypes.o
$(OBJS)/nc_defdata_hist.o: $(OBJS)/vartypes.o
$(OBJS)/locate.o: $(OBJS)/vartypes.o
$(OBJS)/nc_write_L3_histograms.o: $(OBJS)/vartypes.o
$(OBJS)/nc_create_globall2b.o: $(OBJS)/vartypes.o
$(OBJS)/compute_stats.o: $(OBJS)/structures.o $(OBJS)/vartypes.o
$(OBJS)/common_constants.o:
$(OBJS)/vartypes.o:
$(OBJS)/nc_open.o:
$(OBJS)/nc_close.o:
$(OBJS)/nc_global_att_L3.o:
