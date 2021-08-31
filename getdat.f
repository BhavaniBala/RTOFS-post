      subroutine getartype(flnm,artype)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      integer          artype
c
c --- read artype from archive file.
c ---     artype==1 for normal archive files
c ---     artype==2 for   mean archive files
c ---     artype==3 for stddev archive files
c
      character cline*80
      integer   l,ni
c
      data ni/14/
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
        open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &        status='old',action='read')
        read( ni,'(a80/a80/a80/a80)') ctitle
        read( ni,'(a)') cline  ! iversn
        read( ni,'(a)') cline  ! iexpt
        read( ni,'(a)') cline  ! yrflag
        read( ni,'(a)') cline  ! idm
        read( ni,'(a)') cline  ! jdm
c
        read( ni,'(a)') cline
        if     (cline(25:28).eq.'mean') then
          artype = 2
        elseif (cline(25:28).eq.'std.') then
          artype = 3
        else
          artype = 1
        endif
        close(ni)
      else
        artype  = 1
      endif
      return
      end
      subroutine getdat(flnm,time,iweight,mntype,
     &                  lsteric,icegln,trcout, iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
c
      character        flnm*240
      double precision time(3)
      logical          lsteric,icegln,trcout
      integer          iweight,iexpt,mntype,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 array I/O archive or mean/mnsq archive file.
c
c --- this version only works for trcout .false.
c
      character*240 :: cfile
c
      cfile = ' '
      call getenv('ARCHVS',cfile)
      if     (cfile.eq.' ') then
c ----  standard archive
        call getdata(flnm,time,iweight,mntype,
     &               lsteric,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin)
      else
c ----  partial surface archive
        call getdat1(flnm,time,
     &               lsteric,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin)
        iweight = 1  ! standard archive
        mntype  = 1
      endif
      end
      subroutine getdata(flnm,time,iweight,mntype,
     &                   lsteric,icegln,trcout,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
c
      character        flnm*240
      double precision time(3)
      logical          lsteric,icegln,trcout
      integer          iweight,iexpt,mntype,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 array I/O archive or mean/mnsq archive file.
c
c --- this version only works for trcout .false.
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      real      thbase,thet
      integer   i,j,k,iversn,l,layer
      integer   ios,ktr,ntr
      logical   nodens
c
c
      data ni/14/
c
c
      if (trcout) then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - no tracer processing allowed'
        write(lp,*)
        call flush(lp)
      endif
      call xcstop('error in getdat - no tracer processing allowed')
         stop
      endif
c
      l = len_trim(flnm)
      open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     .      status='old',action='read')
      call zaiopf(flnm(1:l-2)//'.a','old', ni)
c
      read( ni,'(a80/a80/a80/a80)') ctitle
      if(mnproc.eq.1)then
        write(lp,'(a80/a80/a80/a80)') ctitle
      endif
      read( ni,*) iversn,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',iversn
      endif
      if (cvarin.ne.'iversn') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                          ' but should be iversn'
        write(lp,*)
        call flush(lp)
      endif
        call xcstop( 'error in getdat - iversn')
        stop
      endif
      read( ni,*) iexpt,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',iexpt
      endif
      if (cvarin.ne.'iexpt ') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - iexpt')
        stop
      endif
      read( ni,*) yrflag,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',yrflag
      endif
      if (cvarin.ne.'yrflag') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - yrflag')
        stop
      endif
      read( ni,*) idmtst,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',idmtst
      endif
      if (cvarin.ne.'idm  ') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - idm')
        stop
      endif
      read( ni,*) jdmtst,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',jdmtst
      endif
      if (cvarin.ne.'jdm  ') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - idm')
        stop
      endif
c
      if (idmtst.ne.itdm .or. jdmtst.ne.jtdm) then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input itdm,jtdm',
     .                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',itdm,   jtdm,   '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - idm,jdm inconsistent')
        stop
      endif
c
      read( ni,'(a)') cline
      if(mnproc.eq.1)then
        write(lp,'(a)') trim(cline)
      endif
      if     (cline(24:28).eq.'model') then
        iweight = 1  ! standard archive
        mntype  = 0
      elseif (cline(24:28).eq.' mean') then
        iweight = 0  ! mean archive, see below
        mntype  = 1       
      elseif (cline(24:28).eq.' mnsq') then
        iweight = 0  ! mean archive, see below
        mntype  = 2
      else
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - wrong archive type.'
        write(lp,*) 'only "model", " mean", or " mnsq" allowed.'
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - wrong archive type')
        stop
      endif
      if(mnproc.eq.1)then
            write(lp,'(a,i2)') 'artype =',mntype+1
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(1),layer,thet,hminb,hmaxb
      call getfld(montg, ni, hminb,hmaxb, .false.)
c        call xctilr(montg,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'montg   '
* --- discard montg
      endif
c
c --- detect version 2.2 normal archive files
      nodens = layer.ne.0
      if     (nodens) then
        sigver = layer
        thbase = thet
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(2),layer,thet,hminb,hmaxb
      call getfld(srfht, ni, hminb,hmaxb, .false.)
c        call xctilr(srfht,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'srfht   '
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      lsteric = cline(1:8).eq.'steric  '
      if     (lsteric) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(steric, ni, hminb,hmaxb, .false.)
c          call xctilr(steric,1,1,nbdy,nbdy, halo_ps)
        if(mnproc.eq.1)then
          write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
        endif
        read (ni,'(a)',end=6) cline
        if(mnproc.eq.1)then
          write(lp,'(a)')       trim(cline)
        endif
      else
        steric(:,:) = 0.0
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(surflx, ni, hminb,hmaxb, .false.)
c        call xctilr(surflx,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
      endif
c
      call time_hour(time)  !reset, assuming time is on the hour
      if(mnproc.eq.1)then
        write(lp,*) 'time3 = ',time
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(salflx, ni, hminb,hmaxb, .false.)
c        call xctilr(salflx,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(dpbl, ni, hminb,hmaxb, .false.)
c        call xctilr(dpbl,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(dpmixl, ni, hminb,hmaxb, .false.)
c        call xctilr(dpmixl,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
      endif
c
      if     (.not. nodens) then
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(tmix, ni, hminb,hmaxb, .false.)
c        call xctilr(tmix,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'tmix    '
      endif
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(smix, ni, hminb,hmaxb, .false.)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'smix    '
      endif
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(thmix, ni, hminb,hmaxb, .false.)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'thmix   '
      endif
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(umix, ni, hminb,hmaxb, .true.)
        call xctilr(umix,1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'umix    '
      endif
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(vmix, ni, hminb,hmaxb, .true.)
        call xctilr(vmix,1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'vmix    '
c      WRITE(6,*)'GETDAT:326 vmix(1:2,191),vmix(1:192)'
c      WRITE(6,*)'GETDAT:327',vmix(1,191),vmix(1,192),
c    +           vmix(2,191),vmix(2,192)
      endif
      endif !.not. nodens
c
      if     (iweight.eq.0) then  ! mean archive
        if     (.not. allocated(kemix)) then
           allocate( kemix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        endif
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(kemix, ni, hminb,hmaxb, .true.)
c        call xctilr(kemix,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kemix   '
      endif
      endif
c
c --- is there ice?
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      icegln = cline(1:8).eq.'covice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(covice, ni, hminb,hmaxb, .false.)
c        call xctilr(covice,1,1,nbdy,nbdy, halo_ps)
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(thkice, ni, hminb,hmaxb, .false.)
c        call xctilr(thkice,1,1,nbdy,nbdy, halo_ps)
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(temice, ni, hminb,hmaxb, .false.)
c        call xctilr(temice,1,1,nbdy,nbdy, halo_ps)  
c
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      else
        covice = 0.0
        thkice = 0.0
        temice = 0.0
      endif
c
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(ubaro, ni, hminb,hmaxb, .true.)
      call xctilr(ubaro,1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(vbaro, ni, hminb,hmaxb, .true.)
      call xctilr(vbaro,1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
      endif
c
      if     (iweight.eq.0) then  ! mean archive
        if     (.not. allocated(kebaro)) then
          allocate( kebaro(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        endif
        read (ni,'(a)',end=6) cline
        if(mnproc.eq.1)then
          write(lp,'(a)')       trim(cline)
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(kebaro, ni, hminb,hmaxb, .true.)
c        call xctilr(kebaro,1,1,nbdy,nbdy, halo_ps)
        if(mnproc.eq.1)then
          write(lp,'("input  ",a," into ",a)') cline(1:8),'kebaro  '
        endif
      endif
c
      kkin=1
      do 14 k=1,kk
      if     (k.eq.2) then
c ---   already input at end of k=1 loop.
      else
        read (ni,'(a)',end=6)   cline
      if(mnproc.eq.1)then
        write(lp,'(a)')         trim(cline)
      endif
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      if     (cline(1:8).ne.'u-vel.  ') then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - layer ',k,
     .             ' does not exist (kk= ',kk,')'
        write(lp,*)
        call flush(lp)
      endif
        call xcstop('error in getdat - layer does not exist')
        stop
      endif
      call getfld(u(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
        call xctilr(u(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(v(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
        call xctilr(v(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
      endif
c
      if     (iweight.eq.0) then  ! mean archive
        if     (.not. allocated(ke)) then
          allocate( ke(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkmax) )
        endif
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(ke(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
        call xctilr(ke(1-nbdy,1-nbdy,k),1,1,ndby,ndby,halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'k.e.    ',k
      endif
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(dp(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
        call xctilr(dp(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
      endif   
c
      if (mntype.eq.2) then  ! mnsq
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        if     (cline(1:8).ne.'mnthknss') then
      if(mnproc.eq.1)then
          write(lp,*)
          write(lp,*)
     &      'error in getdat - must have mnthknss in mnsq archive'
          write(lp,*)
          call flush(lp)
        call xcstop('error - must have mnthknss in mnsq archive')
      endif
          stop
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(dw(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
        call xctilr(dw(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
        if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dw      ',k
        endif
      else
        dw(:,:,k) = dp(:,:,k)
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(temp(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
        call xctilr(temp(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
      endif
c
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(saln(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
        call xctilr(saln(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
      endif
c
      if     (.not. nodens) then
        read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
        write(lp,'(a)')       trim(cline)
      endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(th3d(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)   
        call xctilr(th3d(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'th3d    ',k
      endif
      else
        call th3d_p(temp(1-nbdy,1-nbdy,k),saln(1-nbdy,1-nbdy,k),
     &              th3d(1-nbdy,1-nbdy,k),ii,jj, sigver,thbase)
      if(mnproc.eq.1)then
        write(lp,'(16x,"calc ",a,i3,a,i3,a,f7.2)') 'th3d    ',k,
     &     '   sigver =',sigver,'   thbase =',thbase
      endif
        if     (k.eq.1) then
           tmix(:,:) = temp(:,:,1)
           smix(:,:) = saln(:,:,1)
          thmix(:,:) = th3d(:,:,1)
           umix(:,:) =    u(:,:,1)
           vmix(:,:) =    v(:,:,1)
      if(mnproc.eq.1)then
          write(lp,'("copy   ",a," into ",a)') 'temp.1  ','tmix    '
          write(lp,'("copy   ",a," into ",a)') 'saln.1  ','smix    '
          write(lp,'("copy   ",a," into ",a)') 'th3d.1  ','thmix   '
          write(lp,'("copy   ",a," into ",a)') '   u.1  ','umix    '
          write(lp,'("copy   ",a," into ",a)') '   v.1  ','vmix    '
      endif
        endif !k==1
      endif !.not.nodens:else
c
c --- skip tracers and visc/diff.
c
      if     (k.eq.1) then
        do ktr= 1,999
          read (ni,'(a)',iostat=ios) cline
      if(mnproc.eq.1)then
          write(lp,'(a)')            trim(cline)
      endif
          if (ios.ne.0) then
      if(mnproc.eq.1)then
            write(lp,'(a,f9.5)') 'finished reading data for layer',thet
            call flush(lp)
      endif
            theta(k)=thet
            goto 114  ! archive containing only 1 layer
          elseif (cline(1:8).ne.'tracer  ' .and.
     &            cline(1:8).ne.'viscty  ' .and.
     &            cline(1:8).ne.'t-diff  ' .and.
     &            cline(1:8).ne.'s-diff  '      ) then
            exit !end of tracers and visc/diff
          else
            call zaiosk(ni)
          endif
        enddo !ktr
        ntr=ktr-1
      else !k.gt.1
        do ktr= 1,ntr
          read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
          write(lp,'(a)')       trim(cline)
      endif
          call zaiosk(ni)
        enddo !ktr
      endif !tracers+visc/diff
c
      if(mnproc.eq.1)then
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      endif
      call flush(lp)
      theta(k)=thet
 14   continue
      kkin=kk
114   continue

      close( unit=ni)
      call zaiocl(ni)
      if(mnproc.eq.1)then
          write(lp,'(a)') 'closed archive file'
          call flush(lp)
      endif    
c  
      return
c
c --- unexpected end of file
 6    continue
      if(mnproc.eq.1)then
       write (lp,*) '***** unexpected end of archive file *****'
        call flush(lp)
        call xcstop('**** unexpected end of archive file *****')
        stop '(e-o-f)'
      endif
      end
      subroutine getdat1(flnm,time,
     &                   lsteric,icegln,trcout,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lsteric,icegln,trcout
      integer          iexpt,iversn,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 and 2.2 array I/O archive file.
c --- (no time-averaged fluxes in this version)
c --- version for partial surface archives
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      integer   i,j,ios,l,k,ktr,ntr
c
      integer, parameter :: nfields=19  !no. fields in surface archive
      logical            :: l_arch(nfields) !field output flags
      character*6        :: c_arch(nfields) !field names
      character*240      :: cfile
c
      data ni/14/
c
c ---   list of field names, 6 character versions of 8 character names
c
      c_arch( 1) = 'montg1'
      c_arch( 2) = 'srfhgt'
      c_arch( 3) = 'steric'
      c_arch( 4) = 'surflx'
      c_arch( 5) = 'salflx'
      c_arch( 6) = 'bldpth'
      c_arch( 7) = 'mldpth'
      c_arch( 8) = 'covice'
      c_arch( 9) = 'thkice'
      c_arch(10) = 'temice'
      c_arch(11) = 'ubtrop'
      c_arch(12) = 'vbtrop'
      c_arch(13) = 'u-vel.'
      c_arch(14) = 'v-vel.'
      c_arch(15) = 'thknss'
      c_arch(16) = 'temp  '
      c_arch(17) = 'salin '
      c_arch(18) = 'surtx '  !after salflx
      c_arch(19) = 'surty '
c
c ---   read in archvs.input.
c
      call getenv('ARCHVS',cfile)
      open(unit=uoff+98,file=trim(cfile))
      do k= 1,nfields
        call blkinl_98(l_arch(k),c_arch(k))
      enddo
      close (unit=uoff+98)
c
      if (trcout) then
      if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - no tracer processing allowed'
        write(lp,*)
        call flush(lp)
      endif
      call xcstop('error in getdat - no tracer processing allowed')
         stop
      endif
c
      l = len_trim(flnm)
      open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &      status='old',action='read')
      call zaiopf(flnm(1:l-2)//'.a','old', ni)
c
      read( ni,'(a80/a80/a80/a80)') ctitle
      if(mnproc.eq.1)then
        write(lp,'(a80/a80/a80/a80)') ctitle
      endif
      read( ni,*) iversn,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',iversn
      endif
      if (cvarin.ne.'iversn') then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iversn'
        write(lp,*)
        call flush(lp)
        endif
        call xcstop( 'error in getdat - iversn')
        stop
      endif
      read( ni,*) iexpt,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',iexpt
      endif
      if (cvarin.ne.'iexpt ') then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - iexpt')
        stop
      endif
      read( ni,*) yrflag,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',yrflag
      endif
      if (cvarin.ne.'yrflag') then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - yrflag')
        stop
      endif
      read( ni,*) idmtst,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',idmtst
      endif
      if (cvarin.ne.'idm  ') then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - idm')
        stop
      endif
      read( ni,*) jdmtst,cvarin
      if(mnproc.eq.1)then
        write(lp,*) cvarin,' = ',jdmtst
      endif
      if (cvarin.ne.'jdm  ') then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     .                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - jdm')
        stop
      endif
c
      if (idmtst.ne.itdm .or. jdmtst.ne.jtdm) then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdat - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',itdm,   jtdm, '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - idm,jdm inconsistent')
        stop
      endif
c
c --- artype==1 for normal archive files
c --- artype==2 for   mean archive files
c --- artype==3 for stddev archive files
c
      read( ni,'(a)') cline
      if(mnproc.eq.1)then
      write(lp,'(a)') trim(cline)
      endif
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      else
        artype = 1
      endif
      if(mnproc.eq.1)then
      write(lp,'(a,i2)') 'artype =',artype
      endif
c
      if (artype.ne.1) then
        if(mnproc.eq.1)then
        write(lp,*)
        write(lp,*) 'error in getdata1 - artype must be 1'
        write(lp,*)
        call flush(lp)
        endif
        call xcstop('error in getdat - wrong archive type')
        stop
      endif
c
      if     (.not.l_arch(1)) then
      nodens = .true.
      sigver = 4     !a guess
      thbase = 34.0  !a guess
      montg(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      nodens = layer.ne.0  !detect version 2.2 normal archive files
      if     (nodens) then
        sigver = layer
        thbase = thet
      else
        sigver = 0
      endif
      call getfld(montg, ni, hminb,hmaxb, .false.)
c     call xctilr(montg,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'montg   '
      endif
      endif !l_arch
c
      if     (.not.l_arch(2)) then
      srfht(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(srfht, ni, hminb,hmaxb, .false.)
c     call xctilr(srfht,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'srfht   '
      endif
      endif !l_arch
c
      if     (.not.l_arch(3)) then
      steric(:,:) = 0.0
      lsteric = .false.
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  steric, ni, hminb,hmaxb, .false.)
c     call xctilr(surflx,1,1,nbdy,nbdy, halo_ps)
      lsteric = .true.
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(4)) then
      surflx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(surflx, ni, hminb,hmaxb, .false.)
c     call xctilr(surflx,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(5)) then
      salflx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  salflx, ni, hminb,hmaxb, .false.)
c     call xctilr(salflx,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
      endif
      endif !l_arch
      if     (.not.l_arch(18)) then
      surtx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       cline(1:len_trim(cline))
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(surtx, ni, hminb,hmaxb, .false.)
      call xctilr(surtx,1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surtx  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(19)) then
      surty(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       cline(1:len_trim(cline))
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(surty, ni, hminb,hmaxb, .false.)
      call xctilr(surty,1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surty  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(6)) then
      dpbl(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  dpbl, ni, hminb,hmaxb, .false.)
c     call xctilr(dpbl,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
      endif
      endif !l_arch
c
      if     (.not.l_arch(7)) then
      dpmixl(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(dpmixl, ni, hminb,hmaxb, .false.)
c     call xctilr(dpmixl,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
      endif
      endif !l_arch
c
c --- is there ice?
      icegln = l_arch(8) .or. l_arch(9) .or. l_arch(10)
c
      if     (.not.l_arch(8)) then
      covice(:,:) = 0.0
      else
      read (ni,'(a)',iostat=ios) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')            trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(covice, ni, hminb,hmaxb, .false.)
c     call xctilr(covice,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'covice  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(9)) then
      thkice(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(thkice, ni, hminb,hmaxb, .false.)
c     call xctilr(thkice,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'thkice  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(10)) then
      temice(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(temice, ni, hminb,hmaxb, .false.)
c     call xctilr(temice,1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'temice  '
      endif
      endif !l_arch
c
      if     (.not.l_arch(11)) then
      ubaro(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(ubaro, ni, hminb,hmaxb, .true. )
      call xctilr(ubaro,1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
      endif
      endif !l_arch
c
      if     (.not.l_arch(12)) then
      vbaro(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  vbaro, ni, hminb,hmaxb, .true. )
      call xctilr(vbaro,1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
      endif
      endif !l_arch
c
      kkin=1
      do 14 k=1,kkin
      if     (.not.l_arch(13)) then
      u(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6)   cline
      if(mnproc.eq.1)then
      write(lp,'(a)')         trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(u(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
      call xctilr(u(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_uv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
      endif
      endif !l_arch
c
      if     (.not.l_arch(14)) then
      v(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(v(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
      call xctilr(v(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_vv)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
      endif
      endif !l_arch
c
      if     (.not.l_arch(15)) then
      dp(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(dp(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .true.)
      call xctilr(dp(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
      endif
      endif !l_arch
c
      if     (.not.l_arch(16)) then
      temp(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(temp(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
      call xctilr(temp(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
      endif
      endif !l_arch
c
      if     (.not.l_arch(17)) then
      saln(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      if(mnproc.eq.1)then
      write(lp,'(a)')       trim(cline)
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(saln(1-nbdy,1-nbdy,k), ni, hminb,hmaxb, .false.)
      call xctilr(saln(1-nbdy,1-nbdy,k),1,1,nbdy,nbdy, halo_ps)
      if(mnproc.eq.1)then
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
      endif
c
      endif !l_arch
      if     (l_arch(16) .and. l_arch(17)) then
        call th3d_p(temp(1,1,k),saln(1,1,k),
     &              th3d(1,1,k),ii,jj, sigver,thbase)
        if(mnproc.eq.1)then
        write(lp,'("    ",a8,"calculate ",a,i3)') " ",'th3d    ',k
        endif
      else
        th3d(:,:,k) = 0.0
      endif
c
      if     (k.eq.1) then
         tmix(:,:) = temp(:,:,1)
         smix(:,:) = saln(:,:,1)
        thmix(:,:) = th3d(:,:,1)
         umix(:,:) =    u(:,:,1)
         vmix(:,:) =    v(:,:,1)
        if(mnproc.eq.1)then
        write(lp,'("copy   ",a," into ",a)') 'temp.1  ','tmix    '
        write(lp,'("copy   ",a," into ",a)') 'saln.1  ','smix    '
        write(lp,'("copy   ",a," into ",a)') 'th3d.1  ','thmix   '
        write(lp,'("copy   ",a," into ",a)') '   u.1  ','umix    '
        write(lp,'("copy   ",a," into ",a)') '   v.1  ','vmix    '
        endif
      endif !k==1
c
      if(mnproc.eq.1)then
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      endif
      theta(k)=thet
 14   continue
c
      close( unit=ni)
      call zaiocl(ni)
      if(mnproc.eq.1)then
      write(lp,'(a)') 'closed archive file'
      call flush(lp)
      endif
c
      time(1) = timedum
      time(2) = timedum
      time(3) = timedum
      call time_hour(time)  !reset, assuming time is on the hour
      if(mnproc.eq.1)then
      write(lp,*) 'time3 = ',time
      endif
c
*     if(mnproc.eq.1)then
*     write(lp,'(a)') 'shown below: sea surface height'
*     endif
*     call zebra(srfht,ii,ii1,jj1)
*     call flush(lp)
c
      return
c
c --- unexpected end of file
 6    continue
      if(mnproc.eq.1)then
      write (lp,*) '***** unexpected end of archive file *****'
      call flush(lp)
      endif
      call xcstop('error in getdat - e-o-f')
      stop
      end
      subroutine getfld(work, iunit, hminb,hmaxb, lzero)
      use mod_za ! HYCOM array I/O interface
c
c --- read a single array
c
      logical lzero
      integer iunit
      real    work(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy), hminb,hmaxb
c
      integer mask(1,1)
      real    hmina,hmaxa
c
      call zaiord(work,mask,.false., hmina,hmaxa, iunit)
c
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
      if(mnproc.eq.1)then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        call flush(lp)
        end if
        call xcstop('error - .a and .b files not consistent: ')
        stop
      endif
c
      if     (lzero) then
        do j= 1-nbdy,jdm+nbdy
          do i= 1-nbdy,idm+nbdy
            if     (work(i,j).gt.2.0**99) then
              work(i,j) = 0.0
            endif
          enddo
        enddo
      endif
c============================================================================
      return
      end
      subroutine th3d_p(temp,saln,th3d,no,mo,sigver,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo,sigver
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the appropriate equation of state.
c
      if     (sigver.eq.1) then
        call th3d_p1(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.2) then
        call th3d_p2(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.3) then
        call th3d_p3(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.4) then
        call th3d_p4(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.5) then
        call th3d_p5(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.6) then
        call th3d_p6(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.7) then
        call th3d_p7(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.8) then
        call th3d_p8(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.46) then
        call th3d_p46(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.48) then
        call th3d_p48(temp,saln,th3d,no,mo,thbase)
      else  !unknown
        th3d(:,:) = 0.0
      endif
      return
      end
      subroutine th3d_p1(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_7term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p2(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_7term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p3(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_9term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p4(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_9term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p5(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p6(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p7(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p8(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p46(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA4_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p48(temp,saln,th3d,no,mo,thbase)
      use mod_xc, only : nbdy
      implicit none
c
      integer no,mo
      real    temp(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        saln(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),
     &        th3d(1-nbdy:no+nbdy,1-nbdy:mo+nbdy),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA4_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine time_hour(time)
      implicit none
c
      double precision time(3)
c
c --- reset time to an exact hour if very close to an hour.
c
      integer k
      double precision day,hour,ihr
c
      do k= 1,3
        day  = int(time(k))
        hour = (time(k)-day)*24.d0
        ihr  = nint(hour)
        if     (abs(hour-ihr).le.0.15d0) then
          time(k) = day + ihr/24.d0
        endif
      enddo
      return
      end

