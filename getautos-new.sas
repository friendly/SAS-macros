%macro getautos(out=paths);

   data &out ( keep = path ) ;
      length q $ 32000 temp $ 300 var path $ 255 ;
      q = getoption ( "sasautos" ) ;
