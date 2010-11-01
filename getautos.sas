/* GetAutos.sas - parse SASAUTOS option into pathnames
   assumptions - no path contains a space or single
                 or double quotes
               - a path may be contained within single
                 or double quotes
   note: Windows allows space and single quotes in pathnames
   
   - Added tilda expansion
*/

%macro getautos(out=paths);
   data &out ( keep = path ) ;
      length q $ 32000 temp $ 300 var path $ 255 ;
      q = getoption ( "sasautos" ) ;

      *-- remove surrounding (), and internal quotes;
      if index ( q , "(" ) then
         q = left(translate ( q , " " , "()'""" )) ;

      do while ( 1 ) ;

         temp = scan ( q || "***" , 1 , "() " ) ;

         if temp = "***" then leave ;

         q = left(substr ( q , length ( temp ) + 1)) ;

         if temp =: "!" then
         do ;
            var = scan ( temp , 1 , "!/\ " ) ;
            temp = tranwrd ( temp , '!' || trim(var)
                           , sysget(trim(var)) ) ;
         end ;

		*-- do tilda expansion.  Works for ~/ and ~user/ where user is
		    the current user;
         if temp =: "~" & symget('sysscp') ^= 'WIN' then 
         do ;
            var = scan ( temp , 1 , "~/\" ) ;  put var=;
			home = sysget('HOME');   put home=;
			if var = ' '
				then temp = tranwrd( temp, '~', sysget('HOME'));
				else do;
					userid = sysget('USER'); put userid=;
					temp = tranwrd( temp, '~' || trim(userid), sysget('HOME'));
					put temp=;
					end;
         end ;

		*-- Check for a fileref;
         path = pathname ( temp ) ;
         if path = " " then path = temp ;

         if index ( path , "(" ) then
         do ;
            q = trim(left(translate(path, " ", "()'""")))
                 || " " || trim ( q ) ;
         end ;
         else
         if path ^= "***" then
            output ;
      end ;
   run ;
%mend;

/*
proc contents data=work._all_;

proc contents data=work.sasmacr memtype=catalog;
proc sql;
  Select objname from Dictionary.Catalogs where libname='WORK' and
memname='SASMACR' and objtype='MACRO';

*/
*proc contents data=sashelp._all_;
	


   
%getautos();
proc print data = paths ; run ;

data _null_;
	file print;
	length sasautos $32000 sasroot $200;
	sasroot = sysget('SASROOT');
	put 'SASROOT:' sasroot=;
	sasautos = getoption('sasautos');
	put sasautos=;
	sasautos = compbl(sasautos);
	put sasautos=;
	sasautos = compress(sasautos, '("'')');
	put 'SASAUTOS:' sasautos=;
	run;
