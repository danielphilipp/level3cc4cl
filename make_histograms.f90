! Name: make_histograms.f90
!
!
! Purpose: 
! 
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload
! 2015/03/11 CP adds cloud albedo
! 2016/02/18 OS now using CTP_corrected as basis for joint histogram
! $Id$
!
! Bugs:
!
!none known
module make_histograms

contains

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_2d_histogram_cot_ctp(hist_cot, hist_ctp, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_cot(n_hist_cot), hist_ctp(n_hist_ctp)

    real(kind=sreal) :: current_ctp, current_cot

    integer(kind=lint) v(:,:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jcot, jctp, jphase

    current_ctp=l2_input_2d%ctp_corrected(idim,jdim,inode)
    current_cot=l2_input_2d%cot(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_cot,n_hist_cot,current_cot,jcot)
    call locate(hist_ctp,n_hist_ctp,current_ctp,jctp)

    if(jctp .lt. 1) jctp=1
    if(jctp .eq.n_hist_ctp) jctp=n_hist_ctp-1

    if(jcot .lt. 1) jcot=1
    if(jcot .eq.n_hist_cot) jcot=n_hist_cot-1

    if(jphase .ne. 0 ) then

       v(lon_i,lat_j,jcot,jctp,jphase)=&
            & v(lon_i,lat_j,jcot,jctp,jphase)+1

    endif

  end subroutine make_2d_histogram_cot_ctp


  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_1d_histogram_ctp(hist_ctp_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim, jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_ctp_1d_axis(n_ctp_bins)

    real(kind=sreal) :: current_ctp

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jctp, jphase

    current_ctp=l2_input_2d%ctp(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_ctp_1d_axis,n_ctp_bins+1,current_ctp,jctp)

    if(jctp .lt. 1 ) jctp=1
    if(jctp .eq. n_ctp_bins+1  ) jctp=n_ctp_bins

    if(jphase .ne. 0 ) then

       v(lon_i,lat_j,jctp,jphase)=&
            & v(lon_i,lat_j,jctp,jphase)+1

    endif

  end subroutine make_1d_histogram_ctp

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_1d_histogram_ctt( hist_ctt_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_ctt_1d_axis(n_ctt_bins)

    real(kind=sreal) :: current_ctt

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jctt, jphase


    current_ctt=l2_input_2d%ctt(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_ctt_1d_axis,n_ctt_bins+1,current_ctt,jctt)

    if(jctt .lt. 1 ) jctt=1
    if(jctt .eq. n_ctt_bins+1  ) jctt=n_ctt_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jctt,jphase) = v(lon_i,lat_j,jctt,jphase)+1

  end subroutine make_1d_histogram_ctt

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_1d_histogram_cot(hist_cot_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_cot_1d_axis(n_cot_bins)

    real(kind=sreal) :: current_cot

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jcot, jphase

    current_cot=l2_input_2d%cot(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_cot_1d_axis,n_cot_bins+1,current_cot,jcot)

    if(jcot .lt. 1 ) jcot=1
    if(jcot .eq. n_cot_bins+1  ) jcot=n_cot_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jcot,jphase) = v(lon_i,lat_j,jcot,jphase)+1

  end subroutine make_1d_histogram_cot

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_1d_histogram_ref(hist_ref_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_ref_1d_axis(n_ref_bins)

    real(kind=sreal) :: current_ref

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jref, jphase

    current_ref=l2_input_2d%ref(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_ref_1d_axis,n_ref_bins+1,current_ref,jref)

    if(jref .lt. 1 ) jref=1
    if(jref .eq. n_ref_bins+1  ) jref=n_ref_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jref,jphase) = v(lon_i,lat_j,jref,jphase)+1

  end subroutine make_1d_histogram_ref





  subroutine make_1d_histogram_cloud_albedo1(hist_cloud_albedo1_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_cloud_albedo1_1d_axis(n_cloud_albedo1_bins)

    real(kind=sreal) :: current_cloud_albedo1

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jcloud_albedo1, jphase

    current_cloud_albedo1=l2_input_2d%cloud_albedo1(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_cloud_albedo1_1d_axis,n_cloud_albedo1_bins+1,current_cloud_albedo1,jcloud_albedo1)

    if(jcloud_albedo1 .lt. 1 ) jcloud_albedo1=1
    if(jcloud_albedo1 .eq. n_cloud_albedo1_bins+1  ) jcloud_albedo1=n_cloud_albedo1_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jcloud_albedo1,jphase) = v(lon_i,lat_j,jcloud_albedo1,jphase)+1

  end subroutine make_1d_histogram_cloud_albedo1




  subroutine make_1d_histogram_cloud_albedo2(hist_cloud_albedo2_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_cloud_albedo2_1d_axis(n_cloud_albedo2_bins)

    real(kind=sreal) :: current_cloud_albedo2

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jcloud_albedo2, jphase

    current_cloud_albedo2=l2_input_2d%cloud_albedo2(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_cloud_albedo2_1d_axis,n_cloud_albedo2_bins+1,current_cloud_albedo2,jcloud_albedo2)

    if(jcloud_albedo2 .lt. 1 ) jcloud_albedo2=1
    if(jcloud_albedo2 .eq. n_cloud_albedo2_bins+1  ) jcloud_albedo2=n_cloud_albedo2_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jcloud_albedo2,jphase) = v(lon_i,lat_j,jcloud_albedo2,jphase)+1

  end subroutine make_1d_histogram_cloud_albedo2











  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  subroutine make_1d_histogram_cwp( hist_cwp_1d_axis, &
       & lon_i,lat_j,idim,jdim,inode,l2_input_2d,v)

    use structures

    use vartypes

    implicit none

    integer(kind=lint) :: idim,jdim, lon_i,lat_j,inode

    real(kind=sreal) :: hist_cwp_1d_axis(n_cwp_bins)

    real(kind=sreal) :: current_cwp

    integer(kind=lint) v(:,:,:,:)

    type(l2_input_struct_2d) :: l2_input_2d

    integer(kind=stint) :: jcwp, jphase

    current_cwp=l2_input_2d%cwp(idim,jdim,inode)

    jphase=0

    if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0

       jphase=1

    elseif(l2_input_2d%cty(idim,jdim,inode) .ge. 1.5 ) then !ice: 2.0

       jphase=2

    endif

    call locate(hist_cwp_1d_axis,n_cwp_bins+1,current_cwp,jcwp)

    if(jcwp .lt. 1 ) jcwp=1
    if(jcwp .eq. n_cwp_bins+1  ) jcwp=n_cwp_bins

    if(jphase .ne. 0 ) v(lon_i,lat_j,jcwp,jphase) = v(lon_i,lat_j,jcwp,jphase)+1

  end subroutine make_1d_histogram_cwp

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

end module make_histograms
