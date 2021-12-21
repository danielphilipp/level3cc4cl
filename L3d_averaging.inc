  do idim=1,ncols
     do jdim=1,nrows

        if(l3d_macro%points(idim,jdim) .gt. 0 ) then

           l3d_macro%ctt_mean(idim,jdim)=l3d_macro%ctt_mean(idim,jdim)/l3d_macro%points(idim,jdim)
           l3d_macro%stemp_mean(idim,jdim)=l3d_macro%stemp_mean(idim,jdim)/l3d_macro%points(idim,jdim)	
           l3d_macro%cth_mean(idim,jdim)=l3d_macro%cth_mean(idim,jdim)/l3d_macro%points(idim,jdim)
           l3d_macro%ctp_mean(idim,jdim)=l3d_macro%ctp_mean(idim,jdim)/l3d_macro%points(idim,jdim)
           l3d_macro%cct_mean(idim,jdim)=l3d_macro%cct_mean(idim,jdim)/l3d_macro%points(idim,jdim)

        else

           l3d_macro%ctt_mean(idim,jdim)=real_fill_value
	   l3d_macro%stemp_mean(idim,jdim)=real_fill_value
           l3d_macro%cth_mean(idim,jdim)=real_fill_value
           l3d_macro%ctp_mean(idim,jdim)=real_fill_value
           l3d_macro%cct_mean(idim,jdim)=real_fill_value

        endif

        if(l3d_micro%points(idim,jdim) .gt. 0 ) then

           l3d_micro%cot_mean(idim,jdim)=l3d_micro%cot_mean(idim,jdim)/l3d_micro%points(idim,jdim)
           l3d_micro%ref_mean(idim,jdim)=l3d_micro%ref_mean(idim,jdim)/l3d_micro%points(idim,jdim)
           l3d_micro%cwp_mean(idim,jdim)=l3d_micro%cwp_mean(idim,jdim)/l3d_micro%points(idim,jdim)
           l3d_micro%cty_mean(idim,jdim)=l3d_micro%cty_mean(idim,jdim)/l3d_micro%points(idim,jdim)

        else

           l3d_micro%cot_mean(idim,jdim)=real_fill_value
           l3d_micro%ref_mean(idim,jdim)=real_fill_value
           l3d_micro%cwp_mean(idim,jdim)=real_fill_value
           l3d_micro%cty_mean(idim,jdim)=real_fill_value

        endif

        do ind=1,lastfile

           if(l3d_macro_input%ctp_mean(idim,jdim,ind) .ge. filter_micro .and. &
                l3d_macro%points(idim,jdim) .gt. 0) then

              l3d_macro%ctp_std(idim,jdim)=l3d_macro%ctp_std(idim,jdim)+&
                   & (l3d_macro_input%ctp_mean(idim,jdim,ind)-l3d_macro%ctp_mean(idim,jdim))**2.0
              l3d_macro_input%ctp_mean_cleared(idim,jdim,ind)=l3d_macro_input%ctp_mean(idim,jdim,ind)-&
                   & l3d_macro%ctp_mean(idim,jdim)

           endif

           if(l3d_micro_input%cot_mean(idim,jdim,ind) .ge. filter_micro .and. &
                l3d_micro%points(idim,jdim) .gt. 0) then

              l3d_micro%cot_std(idim,jdim)=l3d_micro%cot_std(idim,jdim)+&
                   & (l3d_micro_input%cot_mean(idim,jdim,ind)-l3d_micro%cot_mean(idim,jdim))**2.0
              l3d_micro_input%cot_mean_cleared(idim,jdim,ind)=l3d_micro_input%cot_mean(idim,jdim,ind)-&
                   & l3d_micro%cot_mean(idim,jdim)                    

           endif

        enddo

        if(l3d_macro%points(idim,jdim) .gt. 0 ) then

           l3d_macro_input%ctp_std(idim,jdim)=sqrt(1.0/real((l3d_macro%points(idim,jdim)),kind=sreal)*&
                & l3d_macro%ctp_std(idim,jdim))

        endif

        if(l3d_micro%points(idim,jdim) .gt. 0 ) then

           l3d_micro_input%cot_std(idim,jdim)=sqrt(1.0/real((l3d_micro%points(idim,jdim)),kind=sreal)*&
                & l3d_micro%cot_std(idim,jdim))

        endif

        do ind=2,lastfile

           if(l3d_macro_input%ctp_mean(idim,jdim,ind) .ge. filter_micro .and. &
                l3d_macro_input%ctp_mean(idim,jdim,ind-1) .ge. filter_micro .and. &
                l3d_macro%points(idim,jdim) .gt. 0 .and. l3d_macro_input%ctp_std(idim,jdim) .gt. 0.00 ) then

              l3d_macro_input%ctp_phi(idim,jdim,ind)=l3d_macro_input%ctp_mean_cleared(idim,jdim,ind)*&
                   l3d_macro_input%ctp_mean_cleared(idim,jdim,ind-1)/l3d_macro_input%ctp_std(idim,jdim)**2.0
              l3d_macro%pairs(idim,jdim)=l3d_macro%pairs(idim,jdim)+1
              l3d_macro%ctp_phi_total(idim,jdim)=l3d_macro%ctp_phi_total(idim,jdim)+l3d_macro_input%ctp_phi(idim,jdim,ind)

           endif

           if(l3d_micro_input%cot_mean(idim,jdim,ind) .ge. filter_micro .and. &
                l3d_micro_input%cot_mean(idim,jdim,ind-1) .ge. filter_micro .and. &
                l3d_micro%points(idim,jdim) .gt. 0 .and. l3d_micro_input%cot_std(idim,jdim) .gt. 0.00) then

              l3d_micro_input%cot_phi(idim,jdim,ind)=l3d_micro_input%cot_mean_cleared(idim,jdim,ind)*&
                   l3d_micro_input%cot_mean_cleared(idim,jdim,ind-1)/l3d_micro_input%cot_std(idim,jdim)**2.0
              l3d_micro%pairs(idim,jdim)=l3d_micro%pairs(idim,jdim)+1
              l3d_micro%cot_phi_total(idim,jdim)=l3d_micro%cot_phi_total(idim,jdim)+l3d_micro_input%cot_phi(idim,jdim,ind)

           endif

        enddo

        if(l3d_macro%pairs(idim,jdim) .gt. 0 ) then

           l3d_macro%ctp_phi_total(idim,jdim)=l3d_macro%ctp_phi_total(idim,jdim)/l3d_macro%pairs(idim,jdim)

        endif

        if(l3d_micro%pairs(idim,jdim) .gt. 0 ) then

           l3d_micro%cot_phi_total(idim,jdim)=l3d_micro%cot_phi_total(idim,jdim)/l3d_micro%pairs(idim,jdim)

        endif

        if(l3d_macro%pairs(idim,jdim) .eq. 0 .or. l3d_macro%points(idim,jdim) .eq. 0 .or. &
             & l3d_macro%ctp_phi_total(idim,jdim) .gt. 1.0 .or. &
             & l3d_macro%ctp_phi_total(idim,jdim) .lt. -1.0 ) then

           l3d_macro%ctp_std(idim,jdim)=real_fill_value
           l3d_macro%ctp_std_corr(idim,jdim)=real_fill_value
           l3d_macro%ctp_phi_total(idim,jdim)=real_fill_value

        else

           l3d_macro%ctp_std(idim,jdim)=l3d_macro_input%ctp_std(idim,jdim)/&
                &sqrt(real(l3d_macro%points(idim,jdim),kind=sreal))
           l3d_macro%ctp_std_corr(idim,jdim)=l3d_macro_input%ctp_std(idim,jdim)*&
                & sqrt((1.0+l3d_macro%ctp_phi_total(idim,jdim))/(1.0-l3d_macro%ctp_phi_total(idim,jdim)))

        endif

        if(l3d_micro%pairs(idim,jdim) .eq. 0 .or. l3d_micro%points(idim,jdim) .eq. 0 .or. &
             & l3d_micro%cot_phi_total(idim,jdim) .gt. 1.0 .or. &
             & l3d_micro%cot_phi_total(idim,jdim) .lt. -1.0 ) then

           l3d_micro%cot_std(idim,jdim)=real_fill_value
           l3d_micro%cot_std_corr(idim,jdim)=real_fill_value
           l3d_micro%cot_phi_total(idim,jdim)=real_fill_value

        else

           l3d_micro%cot_std(idim,jdim)=l3d_micro_input%cot_std(idim,jdim)/&
                &sqrt(real(l3d_micro%points(idim,jdim),kind=sreal))
           l3d_micro%cot_std_corr(idim,jdim)=l3d_micro_input%cot_std(idim,jdim)*&
                & sqrt((1.0+l3d_micro%cot_phi_total(idim,jdim))/(1.0-l3d_micro%cot_phi_total(idim,jdim)))

        endif

     enddo

  enddo

  !commented out as this mode is only experimental and neither  used nor necessary
  !if (wo .eq. 1) write(*,*) "write L3d output"
  !include "include_outputl3d.f90"
