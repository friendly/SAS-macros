%Macro removewrd( List = , WordsToRM = , DLM = %Str( ) ) ;
%Local I Stop NewList ;

%Let Stop = %Eval( %SysFunc( CountC( &List , &DLM ) ) + ( %Length( &List ) > 0 ) ) ;

%Do I = 1 %To &Stop ;
 %If ( %Index( %Scan( %Upcase(&List) , &I , &DLM ) , %Upcase( &WordsToRM ) ) = 0 )
 %Then %Do ;
   %Let NewList = &NewList %Scan( &List , &I , &DLM ) ;
 %End ;
%End ;

&NewList

%Mend  ;

%Let List =  Anno AAA b mean Cc ;
%Put NewList = %removewrd( List = &List , WordsToRM = Mean ) ;
%Put NewList = %removewrd( List = &List , WordsToRM = Anno ) ;

