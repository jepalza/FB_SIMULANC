



' este evento, refresca la ram grafica creada con screenres y asociada al gadget grafico
Sub evento_grafico()
	    Var hdc=GetDC(wingraf)

	    InvalidateRect( wingraf, null, true )
	    
	    with e_wingraf
		    StretchDIBits( hDC, _
		    0, _
		    0, _
		    .right - .left + 1, _
		    .bottom - .top, _
		    0, _
		    0, _
		    .right - .left + 1, _
		    .bottom - .top, _
		    screenptr, _
		    cptr( BITMAPINFO ptr, @bmi), _
		    DIB_RGB_COLORS, SRCCOPY )
	    End With
	    
	    DeleteDC(hdc)
End Sub


' dibujamos en 3D, segun los giros dados
Sub dibuja_3d()

	otrocolor=0:colorlinea=rojotebis
	
	gcls() ' borramos fondo grafico

	tardanza=Timer

		For f=0 To nlin-1
			X1=coord3D(f,0)
			Y1=coord3D(f,1)
			Z1=coord3D(f,2)
			'
			X2=coord3D(f+1,0)
			Y2=coord3D(f+1,1)
			Z2=coord3D(f+1,2)
					
					' lo del tiempo, solo la primera vez que entramos o cambiamos de fichero, nunca mais!!!
					' cuento tanto G1 como G0, pero podria contar solo G1, sera mas real
					'If coord(f,4)=0 And tiempo_final="" Then 'coord(f,4)=valor de G que debe ser 0 o 1
					If tiempo_final="" Then 'coord(f,4)=valor de G que debe ser 0 o 1
						FF=coord(f,3) : If FF=0 Then FF=FF_en_G01 ' nunca se deja el avance en cero
						fa=Sqr ( (X2-X1)^2 + (Y2-Y1)^2 + (Z2-Z1)^2 )
						lon=lon+fa ' sumo las distancias entre puntos
						If coord(f,4)=0 Then tiempo=tiempo+((60*fa)/FF_en_G00) ' para los saltos en G00 empleo una mayor
						If coord(f,4)=1 Then tiempo=tiempo+((60*fa)/FF) ' para los G01 uso el avance programado
					EndIf
			
			''''''''''saltos''''''''''''''''	
			'deteccion de color de los saltos
			GG=coord(f+1,4)
			' si hay saltos con (P) (que yo he sacado y 'marcado' sumando 10000 al GG), cambio el color (si asi se ha indicado)
			If resaltarzonas Then If GG>9999 Then otrocolor+=1:If otrocolor=14 Then otrocolor=0
			If GG>9999 Then GG=GG-10000 ' una vez usado el indicativo de zona (P), lo quito para el resto de cosas
			If GG=0 Then colorlinea=colorsaltos Else colorlinea=mascolores(otrocolor)

			''''''''''''''''''''''''''''''''
			
			' velocidades de presentacion: 0=sin pausas
			If modoturbo>0 Then
				Dim As double pausa=Timer
				'Locate 1,1:Color textonegro:Print pausa
				siguepausado:
				'modoturbo=.03
				'Locate  2,1: Print Timer,pausa, modoturbo:sleep
				'prt(4,1,1,Str( Timer-pausa))
				If (Timer-pausa)<modoturbo Then GoTo siguepausado
				evento = WindowEvent() ' refresca pantalla tras cada pausa
				
				' varias formas de desactivar el turbo!!! (cuatro)
				'GetMouse(mx,my,m,bm) 
				If mira_ESC() Or bm=2 Then 
					'menus(2)=0:pon_botones() ' apago el boton
					modoturbo=0
					'textoturbo()
					Sleep 200,1
				EndIf
			EndIf
	
			' en basic la Y grafica va de arriba hacia abajo, por lo que debo "invertir" su sentido, restando RESY a cada Y
			a= ((X1-XMIN)*factor)+(BORDE/2)+DIFLIMX :b=resy-( ((Y1-YMIN)*factor)+(BORDE/2)+DIFLIMY ) 
			c= ((X2-XMIN)*factor)+(BORDE/2)+DIFLIMX :d=resy-( ((Y2-YMIN)*factor)+(BORDE/2)+DIFLIMY )

			If GG=0 Then 
				Line (a,b)-(c,d),colorlinea,,&b1100110011001100 ' linea "punteada"
			Else
				linea(a,b,c,d,colorlinea) ' linea "solida" mia con recorte dentro del marco, y respeto al pixel ya dibujado de antes
			EndIf
			
			If verpuntos Then
				Circle (a,b),2,blanco
				Circle (c,d),2,blanco
			End If
	
		Next		
		
		' pone inicio y fin
		X1=coord3D(0,0)
		Y1=coord3D(0,1)
		X2=coord3D(f,0)
		Y2=coord3D(f,1)
		' en basic la Y grafica va de arriba hacia abajo, por lo que debo "invertir" su sentido, restanto RESY a cada Y
		a= ((X1-XMIN)*factor)+(BORDE/2)+DIFLIMX : b=resy-( ((Y1-YMIN)*factor)+(BORDE/2)+DIFLIMY ) 
		c= ((X2-XMIN)*factor)+(BORDE/2)+DIFLIMX : d=resy-( ((Y2-YMIN)*factor)+(BORDE/2)+DIFLIMY )
		Circle (a,b),6,azul,,,,f : Color blanco,azul: Draw String(a-3,b-3),"E"
		Circle (c,d),6,azul,,,,f : Color blanco,azul: Draw String(c-3,d-3),"S"
		tardanza=Timer-tardanza
		
		DIBUJA_EJE()

		' borra la zona de mensajes de advertencia, sea cual sea el que este en ese momento, lo borra tod, por que hemos redibujado y eso reinicia todo
		advertencias(0)

End Sub


Sub haz_zoom()

	otrocolor=0:colorlinea=rojotebis
	
	'' reajusta ZOOM
	'factorzoom=(resx/anchozoom)*factor ' simple regla de tres inversa--> si RESX es a FACTOR, ANCHOZOOM es a "X"
	'If factorzoom<(resy/anchozoom)*factor Then factorzoom=(resy/anchozoom)*factor

	gcls() ' borramos fondo grafico
	
	XMINZOOM=X 'los limites temporales del zoom, son las X e Y encontradas al picar el cuadro de inicio del ZOOM
	YMINZOOM=Y

	For f=0 To nlin-1
		X1=coord3D(f,0)
		Y1=coord3D(f,1)
		'Z1=coord3D(f,2) ' en zoom no hace falta
		'
		X2=coord3D(f+1,0)
		Y2=coord3D(f+1,1)
		'Z2=coord3D(f+1,2) ' en zoom no hace falta
		
		''''''''''saltos''''''''''''''''		
		'deteccion de color de los saltos, mirando punto actual	
		GG=coord(f+1,4)
		' si hay saltos con (P) (que yo he sacado y 'marcado' sumando 10000 al GG), cambio el color (si asi se ha indicado)
		If resaltarzonas Then If GG>9999 Then otrocolor+=1:If otrocolor=14 Then otrocolor=0
		If GG>9999 Then GG=GG-10000 ' una vez usado el indicativo de zona (P), lo quito para el resto de cosas
		If GG=0 Then colorlinea=colorsaltos Else colorlinea=mascolores(otrocolor)
		''''''''''''''''''''''''''''''''

		' nota: compenso las coord. a,b,c,d quitando UN pixel , por que quedaba desplazado en el marco
		a= (X1-XMINZOOM)*factorzoom : b= -( (Y1-YMINZOOM)*factorzoom )
		c= (X2-XMINZOOM)*factorzoom : d= -( (Y2-YMINZOOM)*factorzoom )

		linea (a,b,c,d,colorlinea)
		
		If verpuntos Then
			' solo en los limites, logico
			If (a>4 AND a<resx-2) AND (b>4 And b<resy-2) Then
				Circle (a,b),2,blanco
			End If
   		If (c>4 AND c<resx-2) AND (d>4 And d<resy-2) Then	
				Circle (c,d),2,blanco
   		End If
		End If
			
		If mira_ESC() Then Exit Sub
	Next
	
	' pone inicio y fin
	X1=coord3D(0,0)
	Y1=coord3D(0,1)
	X2=coord3D(f,0)
	Y2=coord3D(f,1)
	a= ( (X1-XMINZOOM)*factorzoom )-1: b= -( ( (Y1-YMINZOOM)*factorzoom ) -1)
	c= ( (X2-XMINZOOM)*factorzoom )-1: d= -( ( (Y2-YMINZOOM)*factorzoom ) -1)  
	' solo dentro de los limites graficos    
   If (a>2 AND a<resx) AND (b>2 And b<resy) Then
		Circle (a,b),6,azul,,,,f : Color blanco,azul: Draw String(a-3,b-3),"E"
   End If
   If (c>2 AND c<resx) AND (d>2 And d<resy) Then	
		Circle (c,d),6,azul,,,,f : Color blanco,azul: Draw String(c-3,d-3),"S"
   End If
	
	'Color rojo, grisfondo
	'Draw String (500,992), "ZOOM ACTIVO"': SALIR CON BOTON DERECHO"
			
End Sub




' calcula giros
' como entrada y salida, emplea ZX,ZY,ZZ, SHARED
SUB CALC3D(ByVal ANGX2 As integer, ByVal ANGY2 As Integer, ByVal ANGZ2 As Integer)

	Dim As Single x0,X1,y0
	Dim As Single Y1,z0,Z1
	Dim As Single X2,Y2,Z2
	Dim As Single rx,ry,rz
	Dim As Single X,Y,Z
	
	' paso de grados a radianes
	rx=(-ANGX2*3.14159)/180 ' compenso la X a negativo, para cuadrar vistas
	ry=( ANGY2*3.14159)/180
	rz=( ANGZ2*3.14159)/180
	
	' cogemos los temporales compartidos comunes
	X=ZX:Y=ZY:Z=ZZ
	
	' giro Z
	X0 = x*cos(Rz) - Y*sin(Rz)
	Y0 = x*Sin(Rz) + Y*cos(Rz)
	Z0 = Z
	
	' giro Y
	X1 = x0*cos(Ry) - z0*Sin(Ry)
	Y1 = y0
	Z1 = x0*sin(Ry) + z0*cos(Ry)
	
	' giro X
	X2 = X1
	Y2 = Y1*cos(Rx) - Z1*sin(Rx)
	Z2 = Y1*Sin(Rx) + Z1*cos(Rx)
	
	ZX=X2:ZY=Y2:ZZ=Z2
End Sub




' dibuja una recta, respetando el pixel de fondo ya colocado, para no pisarlo
Sub linea(x0 As integer, y0 As integer, x1 As integer, y1 As Integer, clr As Integer) 
    Dim As integer paso = FALSE
    Dim As Single t
    Dim As Integer x,Y

 ' si la linea es demasiado inclinada la transponemos
 if (abs(x0-x1)<abs(y0-y1)) Then   
        Swap x0, y0 
        Swap x1, y1 
        paso = TRUE
 End if 

 ' si la linea es de derecha a izquierda intercambiamos
    if (x0>x1) Then  
        Swap x0, x1
        Swap y0, y1 
    End If 
    for x=x0 To x1  
        t = (x-x0)/(x1-x0) 
        y = y0*(1.-t) + y1*t 
  		  ' si esta transpuesta, la re-transponemos
  		  ' solo pinta si NO hay un pixel ya dibujado
  		  ' y solo si esta en los limites del cuadro
        if paso Then  
        		If (Y>0 AND Y<resx-1) AND (x>0 AND X<resy-1) Then
        	  		If Point(Y,X)=negro Then PSet(y, X), clr 
        		End If
        Else  
        	   If (x>0 AND x<resx-1) AND (y>0 And Y<resy-1) Then
            	If Point(X,Y)=negro Then PSet(x, Y), clr
        	   End If
        End If
    Next
End Sub


' calcula un radio segun tres puntos cercanos a la XY del raton. la "L" es la posicion dentro de la variable coord()
' OJO: IMPORTANTE, SOLO VARIABLES DOUBLE. Si usamos SINGLE da un error tremendo en las coordenadas.
Sub calcula_radio(X as Single, Y As Single, L As Integer)
	'Locate 10,10:Print "coordenada actual:";X,Y
	Dim As Double rad(2,1) ' recojo 3 puntos cercanos al raton
	Dim As Double X1,Y1,X2,Y2,X3,Y3 ' los tres puntos del circulo
	Dim As double TOL = 0.0000001 ' si es menor a TOL da error de circulo y sale
	
	' cojo 3 puntos, uno antes, central y uno despues de la XY elegida 
	'Locate 1,1
	'Color blanco,negro
	For f=0 To 2
		rad(f,0)=coord(L+(f-1),0)
		rad(f,1)=coord(L+(f-1),1)
		'rad(f,2)=coord(L+(f-4),2) ' paso de la Z, lo hacemos SIEMPRE plano
		'Print rad(f,0),rad(f,1)',rad(f,2)
	Next

	' cojo central y extremos
	X1=rad(0,0)
	Y1=rad(0,1)
	
	X2=rad(1,0)
	Y2=rad(1,1)
	
	X3=rad(2,0)
	Y3=rad(2,1)
	
	' este ejemplo da radio 13.307 y centro en X-1448.868 Y-81.600
	' pero SOLO si usamos variables DOUBLE. Si empleo SINGLE, da un error tremendo, y sale radio 10.
	'X1=-1461.509
	'Y1=-85.758
	'X2=-1461.267
	'Y2=-86.432
	'X3=-1460.99
	'Y3=-87.09

	' intersecciones
   Dim as Double off= (X2^2) + (Y2^2)
   Dim as Double bc = ( (X1^2) + (Y1^2) - off )/2.0
   Dim as Double cd = (off - (X3^2) - (Y3^2))/2.0
   Dim as Double det= (x1 - x2) * (y2 - y3) - (x2 - x3) * (y1 - y2) 

	' si hay error
   If abs(det) < TOL Then Exit Sub 'Print "error en determinante, resultado '0'":sleep:End

   Dim as Double idet = 1/det

	' centro y radio
   Dim as Double xc =  (bc * (y2 - y3) - cd * (y1 - y2)) * idet
   Dim as Double yc =  (cd * (x1 - x2) - bc * (x2 - x3)) * idet
   Dim As Double rc =  Sqr(((x2 - XC)^2) + ((Y2-yc)^2))
   'Locate 1,1:Color textonegro,negro:Print rc
   
   ' si radio mayor de 100, me salgo, para evitar rollos de calculos.
   If rc>100 Then Color amarillo,negro:Draw String (mx,my-10),"R>100":Exit sub

	' escalado para meter en pantalla las coordenadas (depende de si estamos en ZOOM o no)
	If zoom=0 Then
		a= ((XC-XMIN)*factor)+(BORDE/2)+DIFLIMX
		b= resy-( ((YC-YMIN)*factor)+(BORDE/2)+DIFLIMY ) 
		c= rc*factor ' el radio, idem, lo escalo a pantalla
	Else
		a= (XC-XMINZOOM)*factorzoom
		b= -( (YC-YMINZOOM)*factorzoom )
		c= rc*factorzoom
	End If
	
	' resultado final	
	If (a<0 Or a>resx) Or (b<0 Or b>resy) Then Exit Sub
	Color blanco,negro
	Circle (a,b),c
	sa=LTrim(RTrim(Str(XC)))
	sb=LTrim(RTrim(Str(YC)))
	sc=LTrim(RTrim(Str(RC)))
	If InStr(sa,".") Then sa=Left(sa,InStr(sa,".")+3)
	If InStr(sb,".") Then sb=Left(sb,InStr(sb,".")+3)
	If InStr(sc,".") Then sc=Left(sc,InStr(sc,".")+3)
	
	' para usar el centro del circulo como origen, pero si es un circulo grande, se sale  no se ve
	'sa="X"+sa+" Y"+sb+" R="+sc
	'Color amarillo,negro:Draw String (a,b),sa
	
	' es mejor usar la posicion actual del raton
	sa="CENTRO: X"+sa+" Y"+sb
	Color amarillo,negro:Draw String (mx-80,my-10),sa
	sa="RADIO : "+sc
	Color amarillo,negro:Draw String (mx-50,my+16),sa
		
	'Sleep 100,1
End Sub
