%macro rainbow(
	n=,                    /* number of hues         */
	start=,                /* starting hue, in [0,1] */
	end=max(1, &n-1)/&n,   /* ending hue, in [0,1]   */
	light=0.5,             /* lightness value(s)     */
	sat=1,                 /* saturation value(s)    */
	out=rainbow
	);

data &out;
	drop step start end;
	step = (&start - &end)/ (&n+1);
	start = &start;
	end = &end;
		if start > end
			then end = mod(1+end,1);
	step = (end - start)/ &n;
			
	put start= end= step=;
	do h = start to end by step;
		do l=&light;
			do s=&sat;
			  hue = 360 * h;
			  light = 255 * l;
			  sat = 255 * s;
			  hls = 'H' || put(hue,z3.) || put(light,hex2.) || put(sat,hex2.);
			  link hls_to_rgb;
			  output;
			  end;
			end;
		end;

hls_to_rgb:  * (h, l, s);
  drop m1 m2;
  if ( l <= 0.5 ) then
    m2 = l + l * s;
  else
    m2 = l + s - l * s;
 
  m1 = 2.0 * l - m2;

  if ( s = 0.0 ) then do;
    r = l;
    g = l;
    b = l;
	end;
  else do;
/*
    r = %hls_value ( m1, m2, h + 120.0 );
    g = %hls_value ( m1, m2, h );
    b = %hls_value ( m1, m2, h - 120.0 );
*/
	hu = h+120;  link hls_value; r=result;
	hu = h;      link hls_value; g=result;
	hu = h-120;  link hls_value; b=result;
  end;

  return;

hls_value: * (m1, m2, hu);
  drop hu result;
  if(hu>=360) then hu = hu-360;
  if(hu<0)    then hu = hu+360;

  if(hu>=240) then result = m1;
  if(hu<240)  then result = m1+(m2-m1)*(240-hu)/60;
  if(hu<180)  then result = m2;
  if(hu<60)   then result = m1+(m2-m1)*hu/60;

  return;

%macro hls_value( n1, n2, hue );
  %if(&hue>=360) %then %let hue = %eval(&hue-360);
  %if(&hue<0)    %then %let hue = %eval(&hue+360);

  %if(&hue>=240) %then %let result = %eval(&n1);
  %if(&hue<240)  %then %let result = %sysevalf(&n1+(&n2-&n1)*(240-&hue)/60);
  %if(&hue<180)  %then %let result = %eval(&n2);
  %if(&hue<60)   %then %let result = %sysevalf(&n1+(&n2-&n1)*&hue/60);
	&result
*  return($result);
%mend;

/*
function valore($n1,$n2,$hue){
  if($hue>=360) $hue = $hue-360;
  if($hue<0) $hue = $hue+360;

  if($hue>=240) $result = $n1;
  if($hue<240) $result = $n1+($n2-$n1)*(240-$hue)/60;
  if($hue<180) $result = $n2;
  if($hue<60) $result = $n1+($n2-$n1)*$hue/60;

  return($result);
}
*/

%mend;

/*
%rainbow(n=8, start=.7, end=.1);
proc print;
*/


/*
proc hls2rgb {h l s} {
    # Posted by frederic.bonnet@ciril.fr
    # h, l and s are floats between 0.0 and 1.0, ditto for r, g and b
    # h = 0   => red
    # h = 1/3 => green
    # h = 2/3 => blue

    set h6 [expr {($h-floor($h))*6}]
    set r [expr {  $h6 <= 3 ? 2-$h6
                            : $h6-4}]
    set g [expr {  $h6 <= 2 ? $h6
                            : $h6 <= 5 ? 4-$h6
                            : $h6-6}]
    set b [expr {  $h6 <= 1 ? -$h6
                            : $h6 <= 4 ? $h6-2
                            : 6-$h6}]
    set r [expr {$r < 0.0 ? 0.0 : $r > 1.0 ? 1.0 : double($r)}]
    set g [expr {$g < 0.0 ? 0.0 : $g > 1.0 ? 1.0 : double($g)}]
    set b [expr {$b < 0.0 ? 0.0 : $b > 1.0 ? 1.0 : double($b)}]

    set r [expr {(($r-1)*$s+1)*$l}]
    set g [expr {(($g-1)*$s+1)*$l}]
    set b [expr {(($b-1)*$s+1)*$l}]
    return [list $r $g $b]
}


<?php

function valore($n1,$n2,$hue){
  if($hue>=360) $hue = $hue-360;
  if($hue<0) $hue = $hue+360;

  if($hue>=240) $result = $n1;
  if($hue<240) $result = $n1+($n2-$n1)*(240-$hue)/60;
  if($hue<180) $result = $n2;
  if($hue<60) $result = $n1+($n2-$n1)*$hue/60;

  return($result);
}


function rgb2hls ($r,$g,$b){

  $c1 = $r/255;
  $c2 = $g/255;
  $c3 = $b/255;

  $kmin = min($c1,$c2,$c3);
  $kmax = max($c1,$c2,$c3);

  $l = ($kmax+$kmin)/2;

  if($kmax == $kmin){
    $s=0;
    $h=0;
  } else {

    if($l<=0.5) $s = ($kmax-$kmin)/($kmax+$kmin);
    else $s = ($kmax-$kmin)/(2-$kmax-$kmin);

    $delta = $kmax-$kmin;
    if($kmax==$c1) $h = ($c2-$c3)/$delta;
    if($kmax==$c2) $h = 2+($c3-$c1)/$delta;
    if($kmax==$c3) $h = 4+($c1-$c2)/$delta;

    $h = $h*60;
    if($h<0) $h = $h+360;
  }

  $out->h = $h;
  $out->s = $s;
  $out->l = $l;

  return($out);
}


function hls2rgb($h,$l,$s){

  if($l<=0.5) $m2 = $l*(1+$s);
  else $m2 = $l+$s*(1-$l);

  $m1 = 2*$l-$m2;

  $c1 = valore($m1,$m2,$h+120);
  $c2 = valore($m1,$m2,$h);
  $c3 = valore($m1,$m2,$h-120);

  if ($s==0 && $h==0){
    $c1 = $l;
    $c2 = $l;
    $c3 = $l;
  }
  $r = round($c1*255);
  $g = round($c2*255);
  $b = round($c3*255);

  $out->r = $r;
  $out->g = $g;
  $out->b = $b;

  return($out);
}

subroutine hls_to_rgb ( h, l, s, r, g, b )
!
!*******************************************************************************
!
!! HLS_TO_RGB converts HLS to RGB color coordinates.
!
!
!  Definition:
!
!    The HLS color system describes a color based on the qualities of
!    hue, lightness, and saturation.  A particular color has three 
!    coordinates, (H,L,S).  The L and S coordinates must be between
!    0 and 1, while the H coordinate must be between 0 and 360, and
!    is interpreted as an angle.
!
!    The RGB color system describes a color based on the amounts of the 
!    base colors red, green, and blue.  Thus, a particular color
!    has three coordinates, (R,G,B).  Each coordinate must be between
!    0 and 1.  
!
!  Reference:
!
!    Foley, van Dam, Feiner, and Hughes,
!    Computer Graphics, Principles and Practice,
!    Addison Wesley, Second Edition, 1990.
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real H, L, S, the HLS color coordinates to be converted.
!
!    Output, real R, G, B, the corresponding RGB color coordinates.
!
  real b
  real g
  real h
  real hls_value
  real l
  real m1
  real m2
  real r
  real s
!
  if ( l <= 0.5 ) then
    m2 = l + l * s
  else
    m2 = l + s - l * s
  end if

  m1 = 2.0 * l - m2

  if ( s == 0.0 ) then
    r = l
    g = l
    b = l
  else
    r = hls_value ( m1, m2, h + 120.0 )
    g = hls_value ( m1, m2, h )
    b = hls_value ( m1, m2, h - 120.0 )
  end if

  return
end
function hls_value ( n1, n2, h )
!
!*******************************************************************************
!
!! HLS_VALUE is a utility function used by HLS_TO_RGB.
!
!
!  Reference:
!
!    Foley, van Dam, Feiner, and Hughes,
!    Computer Graphics, Principles and Practice,
!    Addison Wesley, Second Edition, 1990.
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real N1, N2, H.
!
!    Output, real HLS_VALUE.
!
  real h
  real hls_value
  real hue
  real n1
  real n2
  real r_modp
!
!  Make sure HUE lies between 0 and 360.
!
  hue = r_modp ( h, 360.0 )

  if ( hue < 60.0 ) then
    hls_value = n1 + ( n2 - n1 ) * hue / 60.0
  else if ( hue < 180.0 ) then
    hls_value = n2
  else if ( hue < 240.0 ) then
    hls_value = n1 + ( n2 - n1 ) * ( 240.0 - hue ) / 60.0
  else
    hls_value = n1
  end if

  return
end

*/

	
