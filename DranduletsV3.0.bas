#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB
#EndIf
#define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )
Screen 20, 32, 2', GFX_FULLSCREEN
Cls

Const xls = -500
Const xrs = 1500
Const yus = -500
Const yds = 1000
Const xsl = xrs - xls
Const ysl = yds - yus

Const NOTHING = -1
Const SOLID = 1
Const LIQUID = 2

Type tpoint
  x As Double
  y As Double
End Type

Type Tpix
  leftpos As Integer
  rightpos As Integer
  earth As Integer
  x As Integer
  y As Integer
End Type

Type TNewPix
  Col As UInteger
  VX As Double
  VY As Double
  Moved As Integer = 0
  PType As Integer = SOLID
End Type

Type ttankpixels
  x As Double
  y As Double
End Type

Type tbullet
  x As Double
  y As Double
  r As Integer
  expr As Double
  dam As Double
  exdam As Double
  bulcol As UInteger
  expcol As UInteger
  penitration As Integer
  maxpenitr As Integer
  timesleep As Integer
  velx As Double
  vely As Double
  vel As Double
  ungle As Double
  exist As Integer
  mass As Double
  waterresistance As Integer
  dec As Double
  expclear As Integer
  particles As Integer
  spoil As Integer
  ammonum As Integer
  targetnum As Integer
End Type

Type tstar
  x As Integer
  y As Integer
End Type

Type tfallpix
  x As Integer
  y As Integer
  fall As Byte
End Type

Type tfpix
  x As Double
  y As Double
  moved As Integer = 0
End Type

Dim Shared As Integer players = 2

Dim Shared As Integer ear1, ear2 = 0

Dim Shared As Integer i, j, k, l ,e, ec = 0

Dim Shared As Integer skycolor, skycolor2, skycolor3, maxstar, maxdrops, maxbul, maxfallpix = 0

Dim Shared As Double g = .01

Dim Shared As Double xtank( 100 ), ytank( 100 ), ung1( 100 ), ung2( 100 )
Dim Shared As Integer tankrightr( 100 ), tanktrightr( 100 ), tankleftr( 100 ), tankupr( 100 ), tankdownr( 100 ), tanklength( 100 )
Dim Shared As UInteger rtankcolor( 100 )
Dim Shared As Integer earth( 100 ), direct( 100 ), fall( 100 ), move( 100 ), waitmore( 100, 100 ), curammo( 100 ), maxammo( 100 ), life( 100 ), yet( 100 )
Dim Shared As Tpix pix( 100 )
Dim Shared As Double hp( 100 ), hpmax( 100 ), vx( 100 ), vy( 100 ), tankvel( 100 ), accel( 100 ), break( 100 ), maxvel( 100 )
Dim Shared bul( 1000 ) As tbullet
Dim Shared star( 1000 ) As tstar
Dim Shared tankpix( 100, 3500 ) As ttankpixels
Dim Shared yupperpix( 1024 ) As Integer
Dim Shared pcol( xls To xrs, yus To yds ) As UInteger
Dim Shared fallpix( 1024 ) As tfallpix
Dim Shared fallingwater( 1024 ) As Byte
Dim Shared NewPix( xls To xrs, yus To yds ) As TNewPix
Dim Shared As tfpix fpix( 1 To ( xrs - xls ) * ( yds - yus ) )
Dim Shared As Any Ptr ScIm

Declare Function CheckPix( X As Integer, Y As Integer ) As Integer
Declare Function OneFall( X As Double, Y As Double ) As tpoint
Declare Sub FallingGround()

Randomize Timer

Function checking( x As Integer, y As Integer ) As Byte
  If ( pcol( x, y ) <> RGB( 255, 255, 255 ) ) And ( pcol( x, y ) <> skycolor ) And ( pcol( x, y ) <> skycolor2 ) And ( pcol( x, y ) <> skycolor3 ) Then
    If ( RGBA_R( pcol( x, y ) ) = 0 ) And ( RGBA_G( pcol( x, y ) ) = 0 ) And ( RGBA_B( pcol( x, y ) ) >= 150 ) Then Return 2 Else Return 1
  Else Return 0
  EndIf
End Function

Sub restoration( num As Integer )
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  For i = xtank( num ) - tanklength( num ) - 1 To xtank( num ) + tanklength( num ) + 1
    For j = ytank( num ) - tanklength( num ) - 1 To ytank( num ) + tanklength( num ) + 1
      If ( Point( i, j ) <> pcol( i, j ) ) And checking( i, j ) Then
        PSet( i, j ), pcol( i, j )
      EndIf
    Next j
  Next i
End Sub

Sub groundgeneration( groundcolor As UInteger, beauty As Integer = ( 0 ) )
  Var rand = Cast( Integer, 0 )
  Var xpix = Cast( Integer, 0 )
  Var ypix = Cast( Integer, 500 )
  Var nextsom = Cast( Integer, 0 )
  Var xnext = Cast( Integer, 0 )
  Var i = Cast( Integer, 0 )
  Var no = Cast( Integer, 0 )
  Var ww = Cast( Integer, 0 )
  For i = 1 To 268
    PSet( xpix, ypix + i ), RGB( Int( Rnd * 60 + 62 ), Int( Rnd * 30 + 56 ), Int( Rnd * 20 + 33 ) )
  Next i
  nextsom = Int( Rnd * 6 )
  xnext = xpix + Int( Rnd * 200 ) + 50
  PSet( xpix, ypix ), groundcolor
  While ( xpix < 1024 )
    rand = Int( Rnd * 1000 )
    If ( rand < 500 ) And ( nextsom = 0 ) Or ( rand < 350 ) And ( nextsom = 1 ) Or ( rand < 200 ) And ( nextsom = 2 ) Or ( rand < 150 ) And ( nextsom > 2 ) Then
      xpix = xpix + 1
      If ( ypix >= 200 ) Then ypix = ypix - 1
      yupperpix( xpix ) = ypix
      no = 0
    EndIf
    If ( rand >= 500 ) And ( rand < 700 ) And ( nextsom = 0 ) Or ( rand >= 350 ) And ( rand < 700 ) And ( nextsom = 1 ) Or ( rand >= 200 ) And ( rand < 700 ) And ( nextsom = 2 ) Or ( rand >= 150 ) And ( rand < 300 ) And ( nextsom > 2 ) Then
      If ( ypix >= 200 ) Then
        ypix = ypix - 1
        PSet( xpix, ypix ), groundcolor
        ypix = ypix - 1
        yupperpix( xpix ) = ypix
        no = 1
      EndIf
    EndIf
    If ( rand >= 700 ) And ( rand < 850 ) And ( nextsom < 3 ) Or ( rand >= 300 ) And ( rand < 800 ) And ( nextsom = 3 ) Or ( rand >= 300 ) And ( rand < 650 ) And ( nextsom = 4 ) Or ( rand >= 300 ) And ( rand < 500 ) And ( nextsom = 5 ) Then
      xpix = xpix + 1
      If ( ypix <= 760 ) Then ypix = ypix + 1
      yupperpix( xpix ) = ypix
      no = 0
    EndIf
    If ( rand >= 850 ) And ( rand < 1000 ) And ( nextsom < 3 ) Or ( rand >= 800 ) And ( rand < 1000 ) And ( nextsom = 3 ) Or ( rand >= 650 ) And ( rand < 1000 ) And ( nextsom = 4 ) Or ( rand >= 500 ) And ( rand < 1000 ) And ( nextsom = 5 ) Then
      If ( ypix <= 760 ) Then
        ypix = ypix + 1
        PSet( xpix, ypix ), groundcolor
        If ( no = 0 ) Then
          For i = 1 To 768 - ypix
            PSet( xpix, ypix + i ), RGB( Int( Rnd * 20 + 82 ), Int( Rnd * 20 + 56 ), Int( Rnd * 10 + 33 ) )
          Next i
          ypix = ypix + 1
        EndIf
      EndIf
    EndIf
    If ( xpix >= xnext ) Then
      If ( nextsom < 3 ) Then nextsom = Int( Rnd * 3 ) + 3 Else nextsom = Int( Rnd * 3 )
      xnext = xpix + Int( Rnd * 200 ) + 50
      If ( ypix > 650 ) Then nextsom = Int( Rnd * 3 )
      If ( ypix < 200 ) Then nextsom = Int( Rnd * 3 ) + 3
    EndIf
    PSet( xpix, ypix ), groundcolor
    If ( no = 0 ) Then
      For i = 1 To 768 - ypix
        PSet( xpix, ypix + i ), RGB( Int( Rnd * 20 + 82 ), Int( Rnd * 20 + 56 ), Int( Rnd * 10 + 33 ) )
      Next i
    EndIf
  Wend
  If ( beauty = 0 ) Then Paint( 1, 520 ), RGB( 82, 56, 33 ), groundcolor
End Sub

Sub water
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  For i = 0 To 1024
    For j = 600 To 768
      If ( Point( i, j ) = skycolor ) Then PSet( i, j ), RGB( 0, 0, Int( 150 + Rnd * 105 ) )
    Next j
  Next i
End Sub

Function round( num As Double ) As Integer
  If ( Frac( num ) < 0.5 ) Then
    round = Int( num )
  Else
    round = Int( num ) + 1
  EndIf
End Function

Sub barobject( xcenter As Integer, ycenter As Integer, leftr As Integer, rightr As Integer, upr As Integer, downr As Integer, ung As Double = ( 3.14 ), barcolor As UInteger,  bartempcolor As UInteger, needpaint As Byte = ( 0 ), addung As Double =( 0 ), obnum As Byte = ( 0 )  )
Var i = Cast( Integer, 0 )
Var j = Cast( Integer, 0 )
Var k = Cast( Integer, 0 )
  ung = ung + addung
  'If ( needpaint = 1 ) And ( barcolor = skycolor ) Then Paint( round( xcenter + ( rightr - leftr ) / 2 * Cos ( ung )  + ( downr - upr ) / 2 * Sin( ung ) ), round( ycenter + ( downr - upr ) / 2 * Cos( ung ) + ( leftr - rightr ) / 2 * Sin( ung ) ) ), barcolor, RGB( 0, 255, 0 ) 'заливка
  Line ( round( xcenter + rightr * Cos( ung ) ), round( ycenter - rightr * Sin( ung ) ) ) - ( round( xcenter + rightr * Cos( ung ) + downr * Sin( ung ) ), round( ycenter - rightr * Sin( ung ) + downr * Cos( ung ) ) ), barcolor 'задн€€ планка
  Line ( round( xcenter - leftr * Cos( ung ) + downr * Sin( ung ) ), round( ycenter + leftr * Sin( ung ) + downr * Cos( ung ) ) ) - ( round( xcenter + rightr * Cos( ung ) + downr * Sin( ung ) ), round( ycenter - rightr * Sin( ung ) + downr * Cos( ung ) ) ), barcolor 'верхн€€ планка
  Line ( round( xcenter - leftr * Cos( ung ) + downr * Sin( ung ) ), round( ycenter + leftr * Sin( ung ) + downr * Cos( ung ) ) ) - ( round( xcenter - leftr * Cos( ung ) ), round( ycenter + leftr * Sin( ung ) ) ), barcolor 'передн€€ планка
  Line ( round( xcenter + rightr * Cos( ung ) ), round( ycenter - rightr * Sin( ung ) ) ) - ( round( xcenter - leftr * Cos( ung ) ), round( ycenter + leftr * Sin( ung ) ) ), barcolor 'нижн€€ планка
  If ( needpaint = 1 ) Then Paint( round( xcenter + ( rightr - leftr ) / 2 * Cos ( ung )  + ( downr - upr ) / 2 * Sin( ung ) ), round( ycenter + ( downr - upr ) / 2 * Cos( ung ) + ( leftr - rightr ) / 2 * Sin( ung ) ) ), barcolor 'заливка
  'Circle( round( xcenter + ( rightr - leftr ) / 2 * Cos ( ung )  + ( downr - upr ) / 2 * Sin( ung ) ), round( ycenter + ( downr - upr ) / 2 * Cos( ung ) + ( leftr - rightr ) / 2 * Sin( ung ) ) ), 1, RGB( 0, 0, 255 )
  'If ( e = 1 ) Then Circle( xcenter, ycenter ), 1, RGB( 0, 255, 0 )
  k = 1
  While ( tankpix( obnum, k ).x <> 0 ) Or ( tankpix( obnum, k ).y <> 0 )
    tankpix( obnum, k ).x = 0
    tankpix( obnum, k ).y = 0
    k += 1
  Wend
  k = 0
  For i = xcenter - ( leftr + rightr ) To xcenter + ( leftr + rightr )
    For j = ycenter - ( leftr + rightr ) To ycenter + ( leftr + rightr )
      If ( Point( i, j ) = barcolor ) And ( barcolor <> skycolor ) Then
        k += 1
        tankpix( obnum, k ).x = i
        tankpix( obnum, k ).y = j
      EndIf
    Next j
  Next i
End Sub

Sub tank( num As Integer, tankcolor As UInteger )
  Var tcol = Cast( UInteger, 0 )
  If ( tankcolor = skycolor ) Then tcol = rtankcolor( num ) Else tcol = skycolor
  barobject( xtank( num ), ytank( num ), tankleftr( num ), tankrightr( num ), tankupr( num ), tankdownr( num ), ung1( num ), tankcolor, tcol, 1, , num ) 'танк
  If ( direct( num ) = 1 ) Then barobject( round( xtank( num ) - tankrightr( num ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) ), round( ytank( num ) + tankleftr( num ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) ) , 0, tankrightr( num ), tankupr( num ), tankdownr( num ) / 3, ung1( num ), tankcolor, tcol, 1, ung2( num ) ) 'пушка
  If ( direct( num ) = 2 ) Then barobject( round( xtank( num ) - tankrightr( num ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) ), round( ytank( num ) + tankleftr( num ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) ) , tankleftr( num ), 0, tankupr( num ), tankdownr( num ) / 3, ung1( num ), tankcolor, tcol, 1, ung2( num ) ) 'пушка
  Circle( round( xtank( num ) - tankrightr( num ) / 10 * 7 * Cos( ung1( num ) + 90 * 3.14 / 180 ) ), round( ytank( num ) + tankrightr( num ) / 10 * 7 * Sin( ung1( num ) + 90 * 3.14 / 180 ) ) ), tanklength( num ) / 3, tankcolor, , , ,F 'башн€
End Sub

Sub falling( num As Integer )
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  Var k = Cast( Integer, 0 )
  Var ear1 = Cast( Integer, 0 )
  Var ear2 = Cast( Integer, 0 )
  Var wtf = Cast( Integer, 0 )
  tank( num, skycolor )
  restoration( num )
  For i = 0 To 20
  	 k = 0
  	 earth( num ) = 0
  	 ear1 = 0
  	 ear2 = 0
    For j = round( -tanklength( num ) / 2 ) To round( tanklength( num ) / 2 )
      pix( k ).x = round( xtank( num ) + j * Cos( ung1( num ) ) - 1 * Sin( ung1( num ) ) )
      pix( k ).y = round( ytank( num ) - j * Sin( ung1( num ) ) - 1 * Cos( ung1( num ) ) )
      If  checking(  pix( k ).x, pix( k ).y ) = 1 Then
        If ( j <= 0 ) Then
          pix( k ).earth = 1
          pix( k ).leftpos = 1
          ear1 = 1
          earth( num ) = 1
          tankleftr( num ) = k
          tankrightr( num ) = tanklength( num ) - tankleftr( num )
        EndIf
        If ( j >= 0 ) And ( ear2 = 0 ) Then
          pix( k ).earth = 1
          pix( k ).rightpos = 1
        	 ear2 = 1
        	 earth( num ) = 1
        	 tankleftr( num ) = k
          tankrightr( num ) = tanklength( num ) - tankleftr( num )
        EndIf
      EndIf
      k = k + 1
    Next j
    xtank( num ) = xtank( num ) + ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ytank( num ) = ytank( num ) - ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    if ( ear1 = 0 ) and ( ear2 > 0 ) Then
      ung1( num ) = ung1( num ) - 0.01
    EndIf
    if ( ear1 > 0 ) and ( ear2 = 0 ) Then
      ung1( num ) = ung1( num ) + 0.01
    EndIf
    If ( earth( num ) = 0 ) Then ytank( num ) = ytank( num ) + 1 * tankvel( num ): tankvel( num ) += g / 10: fall( num ) = 1
    If ( fall( num ) = 1 ) And ( earth( num ) <> 0 ) Then
      fall( num ) = 0:
    EndIf
    xtank( num ) = xtank( num ) - ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ytank( num ) = ytank( num ) + ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    tankleftr( num ) = round( tanklength( num ) / 2 )
    tankrightr( num ) = tankleftr( num )
  Next i
  tank( num, rtankcolor( num ) )
End Sub

Sub movement( num As Integer, direct As Integer )
  Var i = Cast( Integer, 0 )
  Var k = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  Var no = Cast( Integer, 0 )
  Var yeah = Cast( Integer, 0 )
  Var tempung = Cast( Double, 0 )
  Var tx = Cast( Double, 0 )
  Var ty = Cast( Double, 0 )
  If ( earth( num ) <> 0 ) Then
  tank( num, skycolor )
  restoration( num )
  yeah = 0
  tempung = ung1( num )
  tx = xtank( num )
  ty = ytank( num )
  If ( direct = 1 ) Then
  	 If ( ung1( num ) > 3.14 ) Then tankvel( num ) = tankvel( num ) + g / 5
  	 If ( ung1( num ) < 3.14 ) Then tankvel( num ) = tankvel( num ) - g / 1.1
    vx( num ) = tankvel( num ) * Cos( ung1( num ) )
    vy( num ) = tankvel( num ) * Sin( ung1( num ) )
  EndIf
  If ( direct = 2 ) Then
  	 If ( ung1( num ) > 3.14 ) Then tankvel( num ) = tankvel( num ) - g / 1.1
  	 If ( ung1( num ) < 3.14 ) Then tankvel( num ) = tankvel( num ) + g / 5
    vx( num ) = tankvel( num ) * Cos( ung1( num ) )
    vy( num ) = tankvel( num ) * Sin( ung1( num ) )
  EndIf
  For j = 0 To 100
  	 k = 0
    For i = -tankleftr( num ) To tankrightr( num )
      pix( k ).x = round( xtank( num ) + i * Cos( ung1( num ) ) - 1 * Sin( ung1( num ) ) )
      pix( k ).y = round( ytank( num ) - i * Sin( ung1( num ) ) - 1 * Cos( ung1( num ) ) )
      If checking( pix( k ).x, pix( k ).y ) = 1 Then
        tankleftr( num ) = k
        tankrightr( num ) = tanklength( num ) - tankleftr( num )
        If ( direct = 1 ) Then Exit For
      EndIf
      k += 1
    Next i
    xtank( num ) = xtank( num ) + ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ytank( num ) = ytank( num ) - ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    tx = tx + ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ty = ty - ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    For i = 0 To tankdownr( num )
      If ( direct = 1 ) Then
        If checking( round( xtank( num ) + ( tankrightr( num ) + 1 ) * Cos( ung1( num ) ) + i * Sin( ung1( num ) ) ), round( ytank( num ) - ( tankrightr( num ) + 1 ) * Sin( ung1( num ) ) + i * Cos( ung1( num ) ) ) ) = 1 Then
          ung1( num ) -= .01
          vx( num ) = tankvel( num ) * Cos( ung1( num ) )
          vy( num ) = tankvel( num ) * Sin( ung1( num ) )
          Exit For
        EndIf
      EndIf
    	If ( direct = 2 ) Then
        If checking( round( xtank( num ) - ( tankleftr( num ) + 1 ) * Cos( ung1( num ) ) + i * Sin( ung1( num ) ) ), round( ytank( num ) + ( tankleftr( num ) + 1 ) * Sin( ung1( num ) ) + i * Cos( ung1( num ) ) ) ) = 1 Then
          ung1( num ) += .01
          vx( num ) = tankvel( num ) * Cos( ung1( num ) )
          vy( num ) = tankvel( num ) * Sin( ung1( num ) )
          Exit For
        EndIf
    	EndIf
    Next i
    no = 0
    For i = 0 To tankdownr( num )
      If ( checking( round( xtank( num ) + ( tankrightr( num ) + 1 ) * Cos( ung1( num ) ) + i * Sin( ung1( num ) ) ), round( ytank( num ) - ( tankrightr( num ) + 1 ) * Sin( ung1( num ) ) + i * Cos( ung1( num ) ) ) ) = 1 ) And ( direct = 1 ) Then no = 1
      If ( checking( round( xtank( num ) - ( tankleftr( num ) + 1 ) * Cos( ung1( num ) ) + i * Sin( ung1( num ) ) ), round( ytank( num ) + ( tankleftr( num ) + 1 ) * Sin( ung1( num ) ) + i * Cos( ung1( num ) ) ) ) = 1 )  And ( direct = 2 ) Then no = 1
    Next i
    If ( no = 0 ) Then
    	tx = xtank( num )
    	ty = ytank( num )
      If ( direct = 1 ) Then
    	  xtank( num ) += .05 * vx( num )
        ytank( num ) -= .05 * vy( num )
      EndIf
    	If ( direct = 2 ) Then
        xtank( num ) -= .05 * vx( num )
        ytank( num ) += .05 * vy( num )
    	EndIf
    EndIf
    xtank( num ) = xtank( num ) - ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ytank( num ) = ytank( num ) + ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    tx = tx - ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
    ty = ty + ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
    tankleftr( num ) = round( tanklength( num ) / 2 )
    tankrightr( num ) = tankleftr( num )
  Next j
  xtank( num ) = xtank( num ) - ( tanktrightr( num ) - tankrightr( num ) ) * Cos( ung1( num ) )
  ytank( num ) = ytank( num ) + ( tanktrightr( num ) - tankrightr( num ) ) * Sin( ung1( num ) )
  tankleftr( num ) = round( tanklength( num ) / 2 )
  tankrightr( num ) = tankleftr( num )
  tank( num, rtankcolor( num ) )
  EndIf
End Sub

Sub fire( num As Integer, ammo As Integer )
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  Var already = Cast( Integer, 0 )
  already = 0
  For i = 0 To 1000
    If ( bul( i ).exist = 0 ) Then
    If ( ammo <> 5 ) Or ( already >= 30 ) Then Exit For Else bul( i ).exist = 1: bul( i ).expclear = 0: bul( i ).spoil = 0
    If ( ammo = 5 ) Then already += 1
    EndIf
  Next i
  If ( i > maxbul ) Then maxbul = i
  bul( i ).exist = 1
  bul( i ).expclear = 0
  bul( i ).spoil = 0
  If ( ammo = 1 ) Then
    waitmore( num, ammo ) = 20
    bul( i ).r = tankdownr( num ) / 8 * 2: bul( i ).expr = bul( i ).r * 10: bul( i ).dam = Rnd * 10 + 10: bul( i ).exdam = Rnd * 10
    bul( i ).bulcol = RGB( 100, 100, 0 ): bul( i ).expcol = RGB( 100, 0, 0 ): bul( i ).penitration = 0: bul( i ).timesleep = 0
    bul( i ).ungle = ung1( num ) + ung2( num )
    If ( direct( num ) = 2 ) Then bul( i ).ungle -= 3.14
    bul( i ).velx = .6 * Cos( bul( i ).ungle ): bul( i ).vely = .6 * Sin( bul( i ).ungle )
    bul( i ).mass = 10: bul( i ).waterresistance = 0
    bul( i ).dec = .05: bul( i ).particles = tanklength( num ) * 10
    bul( i ).ammonum = 2
  EndIf
  If ( ammo = 2 ) Then
  	 bul( i ).expclear = 1
    waitmore( num, ammo ) = 30
    bul( i ).r = tankdownr( num ) / 8 * 2: bul( i ).expr = bul( i ).r * 15: bul( i ).dam = Rnd * 5 + 5: bul( i ).exdam = Rnd * 0
    bul( i ).bulcol = RGB( 82, 56, 33 ): bul( i ).expcol = RGB( 82, 56, 33 ): bul( i ).penitration = 0: bul( i ).timesleep = 0
    bul( i ).ungle = ung1( num ) + ung2( num )
    If ( direct( num ) = 2 ) Then bul( i ).ungle -= 3.14
    bul( i ).velx = .45 * Cos( bul( i ).ungle ): bul( i ).vely = .45 * Sin( bul( i ).ungle )
    bul( i ).mass = 0: bul( i ).waterresistance = 100
    bul( i ).dec = .05: bul( i ).particles = 0
    bul( i ).ammonum = 2
  EndIf
  If ( ammo = 3 ) Then
    waitmore( num, ammo ) = 0
    bul( i ).r = 1: bul( i ).expr = bul( i ).r * 1: bul( i ).dam = Rnd * 1: bul( i ).exdam = Rnd * 1
    bul( i ).bulcol = RGB( 0, 255, 255 ): bul( i ).expcol = RGB( 255, 0, 0 ): bul( i ).penitration = 0: bul( i ).timesleep = 0
    bul( i ).ungle = ung1( num ) + ung2( num ) + Rnd * .1 - .05
    If ( direct( num ) = 2 ) Then bul( i ).ungle -= 3.14
    bul( i ).velx = .7 * Cos( bul( i ).ungle ): bul( i ).vely = .7 * Sin( bul( i ).ungle )
    bul( i ).mass = 1: bul( i ).waterresistance = 100
    bul( i ).dec = .05: bul( i ).particles = 0
    bul( i ).ammonum = 3
  EndIf
  If ( ammo = 4 ) Then
    waitmore( num, ammo ) = 50
    bul( i ).r = tankdownr( num ) / 8 * 2: bul( i ).expr = bul( i ).r * 10: bul( i ).dam = Rnd * 5 + 5: bul( i ).exdam = Rnd * 5
    bul( i ).bulcol = RGB( 100, 100, 0 ): bul( i ).expcol = RGB( 100, 0, 0 ): bul( i ).penitration = 0: bul( i ).timesleep = 5000
    bul( i ).ungle = ung1( num ) + ung2( num )
    If ( direct( num ) = 2 ) Then bul( i ).ungle -= 3.14
    bul( i ).velx = .1 * Cos( bul( i ).ungle ): bul( i ).vely = .1 * Sin( bul( i ).ungle )
    bul( i ).vel = .02
    bul( i ).mass = 10: bul( i ).waterresistance = 0
    bul( i ).dec = .01: bul( i ).particles = tanklength( num ) * 10
    bul( i ).ammonum = 4
    If ( num <= players / 2 ) Then bul( i ).targetnum = Int( players / 2 + Rnd * ( players / 2 )  + 1 ) Else bul( i ).targetnum = Int( Rnd * players / 2 + 1 )
    'Locate( 2, 1 )
    'Print( bul( i ).targetnum )
    'Locate( 3, 1 )
    'Print( bul( i ).velx )
    'Locate( 4, 1 )
    'Print( bul( i ).vely )
    'bul( i ).targetnum = 1
  EndIf
  If ( ammo = 5 ) Then
  	 For j = 0 To 30
      waitmore( num, ammo ) = 0
      bul( i - j ).r = tankdownr( num ) / 8 * 1: bul( i - j ).expr = bul( i - j ).r * 0: bul( i - j ).dam = Rnd * 1: bul( i - j ).exdam = Rnd * 0
      bul( i - j ).bulcol = RGB( 255, 102, 0 ): bul( i - j ).expcol = RGB( 255, 102, 0 ): bul( i - j ).penitration = 0: bul( i - j ).timesleep = 1000
      bul( i - j ).ungle = ung1( num ) + ung2( num ) + Rnd * .6 - .3
      If ( direct( num ) = 2 ) Then bul( i - j ).ungle -= 3.14
      bul( i - j ).velx = .1 * Cos( bul( i - j ).ungle ): bul( i - j ).vely = .1 * Sin( bul( i - j ).ungle )
      bul( i - j ).mass = 0: bul( i - j ).waterresistance = -100
      bul( i - j ).dec = 0: bul( i - j ).particles = 0
      bul( i - j ).ammonum = 5

      If ( direct( num ) = 1 ) Then
        bul( i - j ).x = round( ( xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) + ( tankrightr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Sin( ung1( num ) + ung2( num ) )  + xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) + ( tankrightr( num ) + bul( i - j ).r ) * Cos( ung1( num ) + ung2( num ) ) ) / 2 )
        bul( i - j ).y = round( ( ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) - ( tankrightr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Cos( ung1( num ) + ung2( num ) ) + ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) - ( tankrightr( num ) + bul( i - j ).r ) * Sin( ung1( num ) + ung2( num ) ) ) / 2 )
      EndIf
      If ( direct( num ) = 2 ) Then
        bul( i - j ).x = round( ( xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) - ( tankleftr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Sin( ung1( num ) + ung2( num ) )  + xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) - ( tankleftr( num ) + bul( i - j ).r ) * Cos( ung1( num ) + ung2( num ) ) ) / 2 )
        bul( i - j ).y = round( ( ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) + ( tankleftr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Cos( ung1( num ) + ung2( num ) ) + ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) + ( tankleftr( num ) + bul( i - j ).r ) * Sin( ung1( num ) + ung2( num ) ) ) / 2 )
      EndIf
      bul( i - j ).maxpenitr = bul( i ).penitration
  	 Next j
  EndIf
  If ( direct( num ) = 1 ) Then
    bul( i ).x = round( ( xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) + ( tankrightr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Sin( ung1( num ) + ung2( num ) )  + xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) + ( tankrightr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) ) / 2 )
    bul( i ).y = round( ( ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) - ( tankrightr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Cos( ung1( num ) + ung2( num ) ) + ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) - ( tankrightr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) ) / 2 )
  EndIf
  If ( direct( num ) = 2 ) Then
    bul( i ).x = round( ( xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) - ( tankleftr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Sin( ung1( num ) + ung2( num ) )  + xtank( num ) - ( tankrightr( num ) + bul( i ).r ) / 10 * 11 * Cos( ung1( num ) + 90 * 3.14 / 180 ) - ( tankleftr( num ) + bul( i ).r ) * Cos( ung1( num ) + ung2( num ) ) ) / 2 )
    bul( i ).y = round( ( ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) + ( tankleftr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) + tankdownr( num ) / 3 * Cos( ung1( num ) + ung2( num ) ) + ytank( num ) + ( tankleftr( num ) + bul( i ).r ) / 10 * 11 * Sin( ung1( num ) + 90 * 3.14 / 180 ) + ( tankleftr( num ) + bul( i ).r ) * Sin( ung1( num ) + ung2( num ) ) ) / 2 )
  EndIf
  bul( i ).maxpenitr = bul( i ).penitration
End Sub

Sub circ( num As Integer )
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  For i = bul( num ).x - bul( num ).r To bul( num ).x + bul( num ).r
    For j = bul( num ).y - bul( num ).r To bul( num ).y + bul( num ).r
      'If ( ( bul( num ).x - i ) * ( bul( num ).x - i ) + ( bul( num ).y - j ) * ( bul( num ).y - j ) <= bul( num ).r * bul( num ).r ) Then PSet( i, j ), pcol( i, j )
      PSet( i, j ), pcol( i, j )
    Next j
  Next i
End Sub

Function CheckPix( X As Integer, Y As Integer ) As Integer
Dim As Integer Need, No = 0
Dim As Integer i = 0
  If ( ( NewPix( X, Y ).VX = 0 ) And ( NewPix( X, Y ).VY = 0 ) And ( NewPix( X, Y ).PType <> NOTHING ) ) Then
    If ( NewPix( X, Y + 1 ).Col = RGB( 255, 0, 255 ) ) Then Need = 1: NewPix( X, Y ).VY += g
    If ( ( NewPix( X + 1, Y ).Col = RGB( 255, 0, 255 ) ) And ( NewPix( X, Y ).PType = LIQUID ) ) Then Need = 1: NewPix( X, Y ).VX += g / 2
    If ( ( NewPix( X - 1, Y ).Col = RGB( 255, 0, 255 ) ) And ( NewPix( X, Y ).PType = LIQUID ) ) Then Need = 1: NewPix( X, Y ).VX -= g / 2
  EndIf
  If ( Need = 1 ) Then
    For i = 1 To maxfallpix
      If ( ( round( fpix( i ).x ) = X ) And ( round( fpix( i ).y = Y ) ) ) Then No = 1
    Next i
    If ( No = 0 ) Then
      'Stop
      maxfallpix += 1
      fpix( maxfallpix ).x = X
      fpix( maxfallpix ).y = Y
      fpix( maxfallpix ).moved = 0
    EndIf
  EndIf
  Return 0
End Function

Function OneFall( X As Double, Y As Double ) As tpoint
Dim As Integer i, PX, PY, Found = 0
Dim As tpoint p1, p2
  PX = round( X + NewPix( X, Y ).VX )
  PY = round( Y + NewPix( X, Y ).VY )
  If ( PX = X ) And ( PY = Y ) Then
    p1.x = X
    p1.y = Y
    NewPix( X, Y ).Moved = 1
    'Print( "OH NO" )
    'Sleep
    'Stop
    Return p1
    Exit Function
  EndIf
  If ( NewPix( PX, PY ).Moved = 0 ) Then
    If ( NewPix( PX, PY ).Col <> RGB( 255, 0, 255 ) ) Then
      Found = 0
      For i = 1 To maxfallpix
        If ( ( round( fpix( i ).x ) = PX ) And ( round( fpix( i ).y = PY ) ) ) Then
          Found = 1
          p2 = OneFall( PX, PY )
          fpix( i ).x = p2.x
          fpix( i ).y = p2.y
        EndIf
      Next i
      If ( Found = 0 ) Then
        'Stop
        maxfallpix += 1
        fpix( maxfallpix ).x = PX
        fpix( maxfallpix ).y = PY
        fpix( maxfallpix ).moved = 0
      EndIf
    EndIf
    If ( ( NewPix( PX, PY ).Col <> RGB( 255, 0, 255 ) ) And ( NewPix( PX, PY ).Moved = 0 ) ) Then
      While ( NewPix( PX, PY ).Col <> RGB( 255, 0, 255 ) )
        NewPix( PX, PY ).VX = NewPix( X, Y ).VX / 2
        NewPix( PX, PY ).VY = NewPix( X, Y ).VY / 2
        NewPix( X, Y ).VX /= 2
        NewPix( X, Y ).VY /= 2
        If ( NewPix( X, Y ).VX < .0001 ) And ( NewPix( X, Y ).VY < .0001 ) Then
          NewPix( X, Y ).VY = 0
          NewPix( X, Y ).VX = 0
          NewPix( X, Y ).Moved = 1
          Exit While
        EndIf
        p2 = OneFall( PX, PY )
      Wend
    EndIf
  EndIf
  If ( NewPix( PX, PY ).Col = RGB( 255, 0, 255 ) ) Then
    NewPix( PX, PY ).Col = NewPix( X, Y ).Col
    NewPix( PX, PY ).VX = NewPix( X, Y ).VX
    NewPix( PX, PY ).VY = NewPix( X, Y ).VY
    NewPix( PX, PY ).VY += g
    NewPix( PX, PY ).Moved = 1
    NewPix( PX, PY ).PType = NewPix( X, Y ).PType
    NewPix( X, Y ).Col = RGB( 255, 0, 255 )
    NewPix( X, Y ).VX = 0
    NewPix( X, Y ).VY = 0
    NewPix( X, Y ).Moved = 0
    p1.x = PX
    p1.y = PY
    NewPix( X, Y ).PType = NOTHING
    If ( ( X - 1 <> PX ) Or ( Y <> PY ) ) Then CheckPix( round( X ) - 1, round( Y ) )
    If ( ( X + 1 <> PX ) Or ( Y <> PY ) ) Then CheckPix( round( X ) + 1, round( Y ) )
    If ( ( X <> PX ) Or ( Y - 1 <> PY ) ) Then CheckPix( round( X ), round( Y ) - 1 )
  EndIf
    NewPix( PX, PY ).Moved = 1
  Return p1
End Function

Sub FallingGround()
Dim As Integer i
Dim As tpoint p1
  /'While ( ( NewPix( fpix( maxfallpix ).x, fpix( maxfallpix ).y ).VX = 0 ) And ( NewPix( fpix( maxfallpix ).x, fpix( maxfallpix ).y ).VY = 0 ) And ( maxfallpix > 0 ) )
    maxfallpix -= 1
  Wend'/
  For i = 1 To maxfallpix
    PSet ScIm, ( fpix( i ).x, fpix( i ).y ), RGB( 255, 0, 255 )
  	 p1 = OneFall( fpix( i ).x, fpix( i ).y )
  	 fpix( i ).x = p1.x
  	 fpix( i ).y = p1.y
  	 PSet ScIm, ( fpix( i ).x, fpix( i ).y ), NewPix( fpix( i ).x, fpix( i ).y ).Col
  Next i
End Sub

Sub flyingbullet
Dim As Integer i, j, j2, k, l, i2 = 0
Dim As Double u, bx, by = 0

  While bul(maxbul).exist = 0
    maxbul -= 1
  Wend
  For i = 0 To maxbul
    If ( bul( i ).exist = 1 )	Then
    	If ( bul( i ).ammonum = 4 ) Then
        If ( bul( i ).x < xtank( bul( i ).targetnum ) ) And ( bul( i ).velx < 1 * bul( i ).vel ) Then bul( i ).velx += bul( i ).dec
        If ( bul( i ).x > xtank( bul( i ).targetnum ) ) And ( bul( i ).velx > -1 * bul( i ).vel ) Then bul( i ).velx -= bul( i ).dec
        If ( bul( i ).y < ytank( bul( i ).targetnum ) ) And ( bul( i ).vely > -2 * bul( i ).vel ) Then bul( i ).vely -= bul( i ).dec / 3
        If ( bul( i ).y > ytank( bul( i ).targetnum ) ) And ( bul( i ).vely < 2 * bul( i ).vel ) Then bul( i ).vely += bul( i ).dec / 3
        barobject( bul( i ).x, bul( i ).y, bul( i ).r, bul( i ).r, 0, bul( i ).r, bul( i ).ungle, skycolor, bul( i ).bulcol, 1, 0, 0 )
        bul( i ).ungle = Atn( bul( i ).vely / bul( i ).velx )
    	EndIf
      For j = 1 To 100
        If ( bul( i ).ammonum <> 4 ) Then
          circ( i )
        Else
          barobject( bul( i ).x, bul( i ).y, bul( i ).r, bul( i ).r, 0, bul( i ).r, bul( i ).ungle, skycolor, bul( i ).bulcol, 1, 0, 0 )
        EndIf
        bul( i ).x = bul( i ).x + bul( i ).velx
        bul( i ).y = bul( i ).y - bul( i ).vely
        If ( bul( i ).x + bul( i ).r > xrs ) Or ( bul( i ).x - bul( i ).r < xls ) Or ( bul( i ).y + bul( i ).r > yds ) Or ( bul( i ).y - bul( i ).r < yus ) Then bul( i ).exist = 0: Exit For
        If ( bul( i ).timesleep > 1 ) Then bul( i ).timesleep -= 1
        For k = 1 To players
          l = 1
          If ( round( Abs( bul( i ).x - xtank( k ) ) ) <= bul( i ).r + tanklength( k ) ) And ( round( Abs( bul( i ).y - ytank( k ) ) ) <= bul( i ).r + tanklength( k ) ) Then
            While ( tankpix( k, l ).x <> 0 ) And ( tankpix( k, l ).y <> 0 )
              If ( round( bul( i ).x + bul( i ).r * Cos( bul( i ).ungle ) ) = tankpix( k, l ).x ) And ( round( bul( i ).y - bul( i ).r * Sin( bul( i ).ungle ) ) = tankpix( k, l ).y ) Or ( round( bul( i ).x + bul( i ).r * Cos( bul( i ).ungle ) ) = tankpix( k, l ).x ) And ( round( bul( i ).y + bul( i ).r * Sin( bul( i ).ungle ) ) = tankpix( k, l ).y ) Then
                tank( k, skycolor )
                hp( k ) -= bul( i ).dam
                bul( i ).exist = 2
                If ( hp( k ) < 0 ) Then
                  hp( k ) = 0
                  life( k ) = 0
                  Locate( 1, 1 )
                  Print( "Player " +Str( k ) + " looser" )
                EndIf
                Exit While
              EndIf
              l += 1
            Wend
          EndIf
        Next k
        If ( checking( bul( i ).x, bul( i ).y ) = 2 ) Or ( checking( bul( i ).x - Cos( bul( i ).ungle ), bul( i ).y ) = 2 ) Or ( checking( bul( i ).x + Cos( bul( i ).ungle ), bul( i ).y ) = 2 ) Or ( checking( bul( i ).x, bul( i ).y - Sin( bul( i ).ungle ) ) = 2 ) Or ( checking( bul( i ).x, bul( i ).y + Sin( bul( i ).ungle ) ) = 2 ) Then
          bul( i ).vely /= 1.01
          bul( i ).velx /= 1.01
          bul( i ).spoil -= 1
        Else
          If ( bul( i ).spoil > -2 ) Then bul( i ).spoil = 0
        EndIf
        If ( checking( bul( i ).x, bul( i ).y ) = 1 ) Or ( bul( i ).timesleep = 1 ) Or ( bul( i ).exist = 2 ) Then
          If ( bul( i ).spoil < -1 ) And ( Rnd * 100 > bul( i ).waterresistance ) Then bul( i ).spoil = 1
        	 If bul( i ).spoil = 1 Then bul( i ).expr = bul( i ).r
          bul( i ).penitration -= 1
          bul( i ).exist = 1
          If ( bul( i ).penitration <= 0 ) Then bul( i ).exist = 2
          If ( bul( i ).exist = 2 ) Then
            u = 6.28 / bul( i ).particles
            Exit For
          EndIf
        EndIf
        If ( bul( i ).ammonum <> 4 ) And ( bul( i ).ammonum <> 4 ) Then
          Circle( bul( i ).x, bul( i ).y ), bul( i ).r, bul( i ).bulcol, , , , F
        Else
          barobject( bul( i ).x, bul( i ).y, bul( i ).r, bul( i ).r, 0, bul( i ).r, bul( i ).ungle, bul( i ).bulcol, skycolor, 1, 0, 0 )
        EndIf
      Next j
      If ( bul( i ).exist = 1 ) Then
        If ( bul( i ).ammonum <> 4 ) And ( bul( i ).ammonum <> 4 ) Then
          circ( i )
        Else
          barobject( bul( i ).x, bul( i ).y, bul( i ).r, bul( i ).r, 0, bul( i ).r, bul( i ).ungle, skycolor, bul( i ).bulcol, 1, 0, 0 )
        EndIf
        If ( bul( i ).ammonum <> 4 ) And ( bul( i ).ammonum <> 4 ) Then bul( i ).vely -= bul( i ).dec
        If ( bul( i ).ammonum <> 4 ) And ( bul( i ).ammonum <> 4 ) Then
          Circle( bul( i ).x, bul( i ).y ), bul( i ).r, bul( i ).bulcol, , , , F
        Else
          barobject( bul( i ).x, bul( i ).y, bul( i ).r, bul( i ).r, 0, bul( i ).r, bul( i ).ungle, bul( i ).bulcol, skycolor, 1, 0, 0 )
        EndIf
      EndIf
      If ( bul( i ).y > 800 ) Then bul( i ).exist = 0
    EndIf
  Next
End Sub

Sub recharge
  Var i = Cast( Integer, 0 )
  Var j = Cast( Integer, 0 )
  For i = 0 To 100
    For j = 0 To 100
      If waitmore( i, j ) > 0 Then waitmore( i, j ) -= 1
    Next j
  Next i
  For i = 1 To players
    If ( yet( i ) > 0 ) Then yet( i ) -= 1
  Next i
End Sub

Sub hpdraw( num As Integer, black As Byte = ( 0 ) )
  Var hpcolor = Cast( UInteger, 0 )
  Var hpcolor2 = Cast( UInteger, 0 )
  If ( black = 0 ) Then hpcolor = RGB( 0, 200, 0 ) Else hpcolor = skycolor
  If ( black = 0 ) Then hpcolor2 = RGB( 200, 0, 0 ) Else hpcolor2 = skycolor
  Var x = Cast( Integer, 0 )
  Var y = Cast( Integer, 0 )
  y = 100
  If ( num = 1 )	Then x = 900 Else x = 100
  Line( x - hpmax( num ) / 4, y - hpmax( num ) / 4 ) - ( x + hpmax( num ) / 4, y + hpmax( num ) / 4 ), hpcolor, BF
  If hp( num ) < hpmax( num ) Then
    If ( hp( num ) >= hpmax( num ) / 2 ) Then
      Line( x, y ) - ( x - ( hpmax( num ) - hp( num ) ) / 2, y - hpmax( num ) / 4 ), hpcolor2
      Line( x, y ) - ( x + ( hpmax( num ) - hp( num ) ) / 2, y - hpmax( num ) / 4 ), hpcolor2
      Line( x - ( hpmax( num ) - hp( num ) ) / 2, y - hpmax( num ) / 4 ) - ( x + ( hpmax( num ) - hp( num ) ) / 2, y - hpmax( num ) / 4 ), hpcolor2
      Paint( x, y - ( hpmax( num ) / 4 - 1 ) ), hpcolor2, hpcolor2

      Line( x, y ) - ( x - ( hpmax( num ) - hp( num ) ) / 2, y + hpmax( num ) / 4 ), hpcolor2
      Line( x, y ) - ( x + ( hpmax( num ) - hp( num ) ) / 2, y + hpmax( num ) / 4 ), hpcolor2
      Line( x - ( hpmax( num ) - hp( num ) ) / 2, y + hpmax( num ) / 4 ) - ( x + ( hpmax( num ) - hp( num ) ) / 2, y + hpmax( num ) / 4 ), hpcolor2
      Paint( x, y + ( hpmax( num ) / 4 - 1 ) ), hpcolor2, hpcolor2
    EndIf
    If ( hp( num ) < hpmax( num ) / 2 ) Then
      Line( x, y ) - ( x - hpmax( num ) / 4, y - ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x, y ) - ( x - hpmax( num ) / 4, y + ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x - hpmax( num ) / 4, y - hpmax( num ) / 4 ) - ( x + hpmax( num ) / 4, y - hpmax( num ) / 4 ), hpcolor2
      Line( x - hpmax( num ) / 4, y - hpmax( num ) / 4 ) - ( x - hpmax( num ) / 4, y - ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x + hpmax( num ) / 4, y - hpmax( num ) / 4 ) - ( x + hpmax( num ) / 4, y - ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x, y ) - ( x + hpmax( num ) / 4, y - ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x, y ) - ( x + hpmax( num ) / 4, y + ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x - hpmax( num ) / 4, y + hpmax( num ) / 4 ) - ( x + hpmax( num ) / 4, y + hpmax( num ) / 4 ), hpcolor2
      Line( x - hpmax( num ) / 4, y + hpmax( num ) / 4 ) - ( x - hpmax( num ) / 4, y + ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Line( x + hpmax( num ) / 4, y + hpmax( num ) / 4 ) - ( x + hpmax( num ) / 4, y + ( hpmax( num ) / 2 + ( hp( num ) - hpmax( num ) / 2 ) ) / 2 ), hpcolor2
      Paint( x, y + ( hpmax( num ) / 4 - 1 ) ), hpcolor2, hpcolor2
      Paint( x, y - ( hpmax( num ) / 4 - 1 ) ), hpcolor2, hpcolor2
    EndIf
  EndIf
End Sub

Sub begins
Var i = Cast( Integer, 0 )
Var j = Cast( Integer, 0 )
  ScIm = ImageCreate( xrs - xls, yds - yus )
  xtank( 1 ) = 900
  ytank( 1 ) = 150
  ung1( 1 ) = 3.14
  ung2( 1 ) = 0
  tankleftr( 1 ) = 10
  tankrightr( 1 ) = 10
  tanktrightr( 1 ) = 10
  tanklength( 1 ) = 20
  tankupr( 1 ) = 0
  tankdownr( 1 ) = 10
  rtankcolor( 1 ) = RGB( 200, 200, 200 )
  direct( 1 ) = 1
  hp( 1 ) = 200
  hpmax( 1 ) = 200
  accel( 1 ) = .01
  break( 1 ) = .005
  maxvel( 1 ) = .50
  curammo( 1 ) = 1
  maxammo( 1 ) = 5
  life( 1 ) = 1

  xtank( 2 ) = 100
  ytank( 2 ) = 150
  ung1( 2 ) = 3.14
  ung2( 2 ) = 0
  tankleftr( 2 ) = 10
  tankrightr( 2 ) = 10
  tanktrightr( 2 ) = 10
  tanklength( 2 ) = 20
  tankupr( 2 ) = 0
  tankdownr( 2 ) = 10
  rtankcolor( 2 ) = RGB( 255, 100, 0 )
  direct( 2 ) = 2
  hp( 2 ) = 200
  hpmax( 2 ) = 200
  accel( 2 ) = .01
  break( 2 ) = .005
  maxvel( 2 ) = .50
  curammo( 2 ) = 1
  maxammo( 2 ) = 5
  life( 2 ) = 1
  For i = xls To xrs
    For j = yus To yds
      pcol( i, j ) = skycolor
    Next j
  Next i
  For i = 0 To 1024
    For j = 0 To 768
      pcol( i, j ) = Point( i, j )
    Next j
  Next i
  For i = xls To xrs
    For j = yus To yds
      NewPix( i, j ).Col = pcol( i, j )
      If ( NewPix( i, j ).Col = RGB( 0, 0, 0 ) ) Then NewPix( i, j ).Col = RGB( 255, 0, 255 )
      NewPix( i, j ).VX = 0
      NewPix( i, j ).VY = 0
      PSet ScIm, ( i, j ), NewPix( i, j ).Col
    Next j
  Next i
End Sub

Sub stars
  Var i = Cast( Integer, 0 )
  For i = 1 To maxstar
    star( i ).x = -1
    star( i ).y = -1
  Next i
  maxstar = round( 500 + Rnd * 500 )
  For i = 1 To maxstar
  	 'Do
      star( i ).x = round( Rnd * 1025 )
      star( i ).y = round( Rnd * 600 )
    'Loop While ( Point( star( i ). x, star( i ). y ) <> skycolor )
    If checking( star( i ).x, star( i ).y ) = 0 Then
      If ( Rnd * 100 < 80 ) Then PSet( star( i ).x, star( i ).y ), RGB( 255, 255, 255 ) Else PSet( star( i ).x, star( i ).y ), skycolor
    EndIf
  Next i
End Sub

Sub blink
  Var i = Cast( Integer, 0 )
    For i = 1 To maxstar
      'If ( Point( star( i ).x, star( i ).y ) = skycolor ) Or ( Point( star( i ).x, star( i ).y ) = RGB( 255, 255, 255 ) ) Then
      If checking( star( i ).x, star( i ).y ) = 0 Then
        If ( Rnd * 100 < 99 ) Then PSet( star( i ).x, star( i ).y ), RGB( 255, 255, 255 ) Else PSet( star( i ).x, star( i ).y ), skycolor
      EndIf
    Next i
End Sub

  'skycolor = RGB( 100 , 100, 255 )
  skycolor = RGB( 0, 0, 0 )
  skycolor2 = skycolor
  skycolor3 = skycolor
  Paint( 0, 0 ), skycolor

  players = 2

  groundgeneration( RGB( 0, 255, 0 ), 1 )
  water

  stars

  begins

  'Paint( 0, 0 ), skycolor, RGB( 255, 255, 255 )

  tank( 1, rtankcolor( 1 ) )
  tank( 2, rtankcolor( 2 ) )
  maxfallpix = 0
  For i = 1 To 10
    For j = 1 To 1
      NewPix( 500 + i, 100 + j ).Col = RGB( 255, 0, 0 )
      NewPix( 500 + i, 100 + j ).Moved = 0
      NewPix( 500 + i, 100 + j ).PType = SOLID
      NewPix( 500 + i, 100 + j ).VX = 0
      NewPix( 500 + i, 100 + j ).VY = 0.5
      PSet ScIm, ( 500 + i, 100 + j ), NewPix( 500 + i, 100 + j ).Col
      maxfallpix += 1
      fpix( maxfallpix ).x = 500 + i
      fpix( maxfallpix ).y = 100 + j
      fpix( maxfallpix ).moved = 0
    Next j
  Next i

  While ( Not MultiKey( SC_ESCAPE ) ) And ( life( 1 ) = 1 ) And ( life( 2 ) = 1 )
    ScreenLock
    Cls
    Put ( 0, 0 ), ScIm, Trans
    Print( maxfallpix )
  	 'blink
  	 'rain
    For i = 1 To players
      hpdraw( i, 1 )
    Next i
    For i = 1 To players
    	tank( i, skycolor )
      falling( i )
      FallingGround()
      If ( tankvel( i ) > 0 ) And ( move( i ) = 0 ) Then vx( i ) = tankvel( i ) * Cos( ung1( i ) ): vy( i ) = tankvel( i ) * Sin( ung1( i ) ): movement( i, direct( i ) ): tankvel( i ) -= break( i )
      If tankvel( i ) < 0 Then tankvel( i ) = 0
      '  tank( i, skycolor )
      '  If direct( i ) = 1 Then direct( i ) = 2
      '  If direct( i ) = 2 Then direct( i ) = 1
      '  tank( i, rtankcolor( i ) )
      'EndIf
      move( i ) = 0
    Next i
    'ai( 2 )
    If MultiKey( SC_DOWN ) Then
    	If ( ung2( 1 ) < 0 ) And ( direct( 1 ) = 1 ) Then
    	  tank( 1, skycolor )
    	  ung2( 1 ) += .05
    	  If ( ung2( 1 ) > 0 ) Then ung2( 1 ) = 0
    	  tank( 1, rtankcolor( 1 ) )
    	EndIf
    	If ( ung2( 1 ) > 0 ) And ( direct( 1 ) = 2 ) Then
    	  tank( 1, skycolor )
    	  ung2( 1 ) -= .05
    	  If ( ung2( 1 ) < 0 ) Then ung2( 1 ) = 0
    	  tank( 1, rtankcolor( 1 ) )
    	EndIf
    EndIf
    If MultiKey( SC_UP ) Then
    	If ( ung2( 1 ) > - 3.14 / 2 ) And ( direct( 1 ) = 1 ) Then
    	  tank( 1, skycolor )
    	  ung2( 1 ) -= .05
    	  If ( ung2( 1 ) < -3.14 / 2 ) Then ung2( 1 ) = -3.14 / 2
    	  tank( 1, rtankcolor( 1 ) )
    	EndIf
    	If ( ung2( 1 ) < 3.14 / 2 ) And ( direct( 1 ) = 2 ) Then
    	  tank( 1, skycolor )
    	  ung2( 1 ) += .05
    	  If ( ung2( 1 ) > 3.14 / 2 ) Then ung2( 1 ) = 3.14 / 2
    	  tank( 1, rtankcolor( 1 ) )
    	EndIf
    EndIf
    If MultiKey( SC_LEFT ) Then
    	tank( 1, skycolor )
    	If ( direct( 1 ) = 2 ) Then
    	  If ( tankvel( 1 ) <= 0 ) Then ung2( 1 ) = 0: tankvel( 1 ) = 0: direct( 1 ) = 1 Else tankvel( 1 ) -= break( 1 ) * 2
    	EndIf
    	If ( direct( 1 ) = 1 ) Then
    	  direct( 1 ) = 1
    	  vx( 1 ) = tankvel( 1 ) * Cos( ung1( 1 ) )
    	  vy( 1 ) = tankvel( 1 ) * Sin( ung1( 1 ) )
    	  movement( 1, 1 )
    	  If ( tankvel( 1 ) < maxvel( 1 ) ) Then tankvel( 1 ) += accel( 1 )
    	  move( 1 ) = 1
    	EndIf
    	tank( 1, rtankcolor( 1 ) )
    EndIf
    If MultiKey( SC_RIGHT ) Then
    	tank( 1, skycolor )
    	If ( direct( 1 ) = 1 ) Then
    	  If ( tankvel( 1 ) <= 0 ) Then ung2( 1 ) = 0: tankvel( 1 ) = 0: direct( 1 ) = 2 Else tankvel( 1 ) -= break( 1 ) * 2
    	EndIf
    	If ( direct( 1 ) = 2 ) Then
    	  direct( 1 ) = 2
    	  vx( 1 ) = tankvel( 1 ) * Cos( ung1( 1 ) )
    	  vy( 1 ) = tankvel( 1 ) * Sin( ung1( 1 ) )
    	  movement( 1, 2 )
    	  If ( tankvel( 1 ) < maxvel( 1 ) ) Then tankvel( 1 ) += accel( 1 )
    	  move( 1 ) = 1
    	EndIf
    	tank( 1, rtankcolor( 1 ) )
    EndIf
    If MultiKey( SC_CONTROL ) Then
      If ( waitmore( 1, curammo( 1 ) ) = 0 ) Then fire( 1, curammo( 1 ) )
    EndIf
    If MultiKey( SC_RSHIFT ) Then
    	If ( yet( 1 ) = 0 ) Then
        yet( 1 ) = 10
        curammo( 1 ) += 1
      If ( curammo( 1 ) > maxammo( 1 ) ) Then curammo( 1 ) = 1
    	EndIf
    EndIf
    If MultiKey( SC_D ) Then
    	tank( 2, skycolor )
    	If ( direct( 2 ) = 2 ) Then
    	  If ( tankvel( 2 ) <= 0 ) Then ung2( 2 ) = 0: tankvel( 2 ) = 0: direct( 2 ) = 1 Else tankvel( 2 ) -= break( 2 ) * 2
    	EndIf
    	If ( direct( 2 ) = 1 ) Then
    	  direct( 2 ) = 1
    	  vx( 2 ) = tankvel( 2 ) * Cos( ung1( 2 ) )
    	  vy( 2 ) = tankvel( 2 ) * Sin( ung1( 2 ) )
    	  movement( 2, 1 )
    	  If ( tankvel( 2 ) < maxvel( 2 ) ) Then tankvel( 2 ) += accel( 2 )
    	  move( 2 ) = 1
    	EndIf
    	tank( 2, rtankcolor( 2 ) )
    EndIf
    If MultiKey( SC_G ) Then
    	tank( 2, skycolor )
    	If ( direct( 2 ) = 1 ) Then
        If ( tankvel( 2 ) <= 0 ) Then ung2( 2 ) = 0: tankvel( 2 ) = 0: direct( 2 ) = 2 Else tankvel( 2 ) -= break( 2 ) * 2
    	EndIf
    	If ( direct( 2 ) = 2 ) Then
    	  direct( 2 ) = 2
    	  vx( 2 ) = tankvel( 2 ) * Cos( ung1( 2 ) )
    	  vy( 2 ) = tankvel( 2 ) * Sin( ung1( 2 ) )
    	  movement( 2, 2 )
    	  If ( tankvel( 2 ) < maxvel( 2 ) ) Then tankvel( 2 ) += accel( 2 )
    	  move( 2 ) = 1
    	EndIf
    	tank( 2, rtankcolor( 2 ) )
    EndIf
    If MultiKey( SC_F ) Then
    	If ( ung2( 2 ) < 0 ) And ( direct( 2 ) = 1 ) Then
    	  tank( 2, skycolor )
    	  ung2( 2 ) += .05
    	  If ( ung2( 2 ) > 0 ) Then ung2( 2 ) = 0
    	  tank( 2, rtankcolor( 2 ) )
    	EndIf
    	If ( ung2( 2 ) > 0 ) And ( direct( 2 ) = 2 ) Then
    	  tank( 2, skycolor )
    	  ung2( 2 ) -= .05
    	  If ( ung2( 2 ) < 0 ) Then ung2( 2 ) = 0
    	  tank( 2, rtankcolor( 2 ) )
    	EndIf
    EndIf
    If MultiKey( SC_R ) Then
    	If ( ung2( 2 ) > - 3.14 / 2 ) And ( direct( 2 ) = 1 ) Then
    	  tank( 2, skycolor )
    	  ung2( 2 ) -= .05
    	  If ( ung2( 2 ) < -3.14 / 2 ) Then ung2( 2 ) = -3.14 / 2
    	  tank( 2, rtankcolor( 2 ) )
    	EndIf
    	If ( ung2( 2 ) < 3.14 / 2 ) And ( direct( 2 ) = 2 ) Then
    	  tank( 2 , skycolor )
    	  ung2( 2 ) += .05
    	  If ( ung2( 2 ) > 3.14 / 2 ) Then ung2( 2 ) = 3.14 / 2
    	  tank( 2, rtankcolor( 2 ) )
    	EndIf
    EndIf
    If MultiKey( SC_Q ) Then
      If ( waitmore( 2, curammo( 2 ) ) = 0 ) Then
        fire( 2, curammo( 2 ) )
        If ( curammo( 2 ) = 5 ) Then
          fire( 2, curammo( 2 ) )
          If ( curammo( 2 ) = 5 ) Then
            For e = 1 To 10
              fire( 2, curammo( 2 ) )
            Next e
          EndIf
        EndIf
      EndIf
    EndIf
    If MultiKey( SC_W ) Then
      If ( yet( 2 ) = 0 ) Then
        yet( 2 ) = 10
        curammo( 2 ) += 1
        If ( curammo( 2 ) > maxammo( 2 ) ) Then curammo( 2 ) = 1
      EndIf
    EndIf
    flyingbullet
    recharge
    If MultiKey( SC_C ) Then Cls: Paint( 0, 0 ), skycolor: groundgeneration( RGB( 0, 255, 0 ), 1 ): water: begins: stars: maxdrops = 0
    If MultiKey( SC_N ) Or ( life( 1 ) = 0 ) Or ( life( 2 ) = 0 ) Then
    	tank( 1, skycolor )
    	tank( 2, skycolor )
      xtank( 1 ) = 1000
      ytank( 1 ) = 150
      ung1( 1 ) = 3.14
      tankleftr( 1 ) = 10
      tankrightr( 1 ) = 10
      tanktrightr( 1 ) = 10
      tanklength( 1 ) = 20
      tankupr( 1 ) = 0
      tankdownr( 1 ) = 10

      xtank( 2 ) = 20
  		ytank( 2 ) = 150
  		ung1( 2 ) = 3.14
  		tankleftr( 2 ) = 10
  		tankrightr( 2 ) = 10
  		tanktrightr( 2 ) = 10
  		tanklength( 2 ) = 20
  		tankupr( 2 ) = 0
  		tankdownr( 2 ) = 10
  		begins
    EndIf
    For i = 1 To players
      hpdraw( i, 0 )
    Next i
    ScreenUnLock
    'Sleep( 10 )
    ScreenSync
  Wend

Sleep