

' "poseso", quita comas y convierte en ".", en los casos X100,123 quedaria como X100.123
function quitacomas(ByVal sa As String) As String
	Dim aa As Integer
	Dim bb As Integer
	If InStr(sa,",")=0 Then Return sa
	For bb=1 To 200
		aa=InStr(sa,",")
		If aa Then Mid(sa,aa,1)="."
		If aa=0 Then Return sa
	Next
End Function




Sub lee_coordenadas(ByVal sa As String)
		
	Dim As Integer a,b,c,d,e,g
	Dim As Integer FF,GG
	Dim As Single X,Y,Z

	
	' las variables XOLD, YOLD, ZOLD, FFOLD, GGOLD deben ser SHARED externas, para que las vea el modulo principal al acabar
	' al igual (de cajon) que las DATA() y la "M", que son donde se guarda el resultado

	If es_hnc Then 
		
			' para los HNC
			e=0
			e=e+InStr(sa,";")
			e=e+InStr(sa,"(")
			e=e+InStr(sa,"CYCL")
			e=e+InStr(sa,"TOOL")
			e=e+InStr(sa,"CALL")
			e=e+InStr(sa,"BEGIN")
			e=e+InStr(sa,"BLK")
			e=e+InStr(sa,"DEF")
			e=e+InStr(sa,"FORM")
			e=e+InStr(sa,"STOP")
			e=e+InStr(sa,"END")
			e=e+InStr(sa,"PGM")
			' si NO es nada de lo mirado antes, cojo coordenadas
			If e=0 Then
				If InStr(sa,"L")=0 Then Exit Sub ' las lineas HEIDEN de coordenadas, empiezan por "L" SIEMPRE, si no hay L, salimos
				sa=quitacomas(sa) ' se mira si es HNC con "," en vez de "."
				e=0
				e=e+InStr(sa,"X")
				e=e+InStr(sa,"Y")
				e=e+InStr(sa,"Z") 
				e=e+InStr(sa,"F") 
				e=e+InStr(sa,"S") 
				'a=a+InStr(sa,"G") 
				If e Then 
					' buscamoX primero FMAX, y si existe, lo usamoX (primero) y borramoX (despues)
					' por que sino, la rutina de buscar "X" confunde "FMAX" con coordenada "X"
					d=InStr(sa,"F")
					If d Then
						If InStr(sa,"FMAX") then 
							' si solo hay FMAX
							Mid(sa,d,4)="    " ' quito el FMAX ya buscado
							FF=9999
							GG=0
						Else
							' si hay F
							GG=1
							FF=Val(Mid(sa,d+1)) 
						EndIf
					Else 
						' si no hay F
						FF=FFOLD
						GG=1
					EndIf
					
					d=InStr(sa,"S")
					If d Then
						SS=Val(Mid(sa,d+1))
					EndIf
					
					' sacamos valores
					a=InStr(sa,"X")
					b=inStr(sa,"Y")
					c=InStr(sa,"Z")
					'e=InStr(sa,"G")
					
					If a Then X=Val(Mid(sa,a+1)) Else X=XOLD
					If b Then Y=Val(Mid(sa,b+1)) Else Y=YOLD
					If c Then Z=Val(Mid(sa,c+1)) Else Z=ZOLD
					
					XOLD=X:YOLD=Y:ZOLD=Z
					If FF=9999 Then FF=2000 ' EN EL CASO DE LOS HNC, NUNCA DEJO EN "0" LA F
					FFOLD=FF
					coord(m,0)=X
					coord(m,1)=Y	
					coord(m,2)=Z	
					coord(m,3)=FF
					coord(m,4)=GG+comentario_zona ' en Heiden no hay G, pero uso FMAX como indicativo --> FMAX? -> GG=0 sino, GG=1
					' SOLO incremento coordenada, SI existen X, o Y o Z. Si SOLO existe F o G (linea solitaria F2000 o G90), NO incremento
					If a+b+c>0 Then m+=1
					comentario_zona=0 ' a cero, que no vuelva a usarse hasta encontrar otro
				EndIf
			Else
				' sino es coordenada, trato SOLO comentarios
				sa=UCase(sa) 'siempre mayusculas, y me evito complicaciones
				a=InStr(sa,"(")
				b=InStr(sa,";")
				If a Or b Then 
					' primero elijo ISO '(' , sino HNC ';', pero no los dos
					If a Then 
						sa=Mid(sa,a) ' caso ISO '('
					Else
					   sa=Mid(sa,b) ' caso HNC ';'
					End If
					If InStr(sa,"FRESA") Then comentario_fresa=sa
					If InStr(sa,"ESPES") Then comentario_espesor=comentario_espesor+sa
					If InStr(sa,"CRECE") Then comentario_espesor=comentario_espesor+sa
					If InStr(sa,"PASO")  Then comentario_espesor=comentario_espesor+sa
					If InStr(sa,"REFER") Then comentario_espesor=comentario_espesor+sa
					comentario_zona=0 ' si NO hay (P)
					If InStr(sa,"(P)") Then comentario_zona=10000 ' indicativo de zona de salto (P) para añadir al GG arriba
				EndIf
			End If
	Else
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''	
			' para los NC del tebis
			e=0
			e=e+InStr(sa,";")
			e=e+InStr(sa,"(")
			' si NO es nada de lo mirado antes, cojo coordenadas
			If e=0 Then
				e=0
				e=e+InStr(sa,"X")
				e=e+InStr(sa,"Y")
				e=e+InStr(sa,"Z") 
				e=e+InStr(sa,"F") 
				e=e+InStr(sa,"G") 
				e=e+InStr(sa,"S") 
				If e Then
					a=InStr(sa,"X")
					b=inStr(sa,"Y")
					c=InStr(sa,"Z")
					d=InStr(sa,"F")
					g=InStr(sa,"G")
					s=InStr(sa,"S")
					If a Then X=Val(Mid(sa,a+1)) Else X=XOLD
					If b Then Y=Val(Mid(sa,b+1)) Else Y=YOLD
					If c Then Z=Val(Mid(sa,c+1)) Else Z=ZOLD
					If d Then FF=Val(Mid(sa,d+1)) Else FF=FFOLD
					If g Then GG=Val(Mid(sa,g+1)) Else GG=GGOLD
					If s Then SS=Val(Mid(sa,s+1)) ' la S "no" necesita un SSOLD!!!
					XOLD=X:YOLD=Y:ZOLD=Z
					FFOLD=FF:GGOLD=GG	
					coord(m,0)=X
					coord(m,1)=Y	
					coord(m,2)=Z	
					coord(m,3)=FF
					coord(m,4)=GG+comentario_zona ' si hay un (P), es una nueva zona del NC, por lo que llevara G00
					'Print X,Y,Z,FF,GG,SS,m:SLEEP
					' SOLO incremento coordenada, SI existen X, o Y o Z. Si SOLO existe F o G (linea solitaria F2000 o G90), NO incremento
					If a+b+c>0 Then m+=1
					comentario_zona=0 ' a cero, que no vuelva a usarse hasta encontrar otro
				EndIf
			Else
				' sino es coordenada, cojo SOLO comentarios
				sa=UCase(sa) 'siempre mayusculas, y me evito complicaciones
				a=InStr(sa,"(")
				b=InStr(sa,";")
				If a Or b Then 
					' primero elijo ISO '(' , sino HNC ';', pero no los dos
					If a Then 
						sa=Mid(sa,a) ' caso ISO '('
					Else
					   sa=Mid(sa,b) ' caso HNC ';'
					End If
					If InStr(sa,"FRESA") Then comentario_fresa=sa
					If InStr(sa,"ESPES") Then comentario_espesor=sa
					comentario_zona=0 ' si NO hay (P)
					If InStr(sa,"(P)") Then comentario_zona=10000 ' indicativo de zona de salto (P) para añadir al GG arriba
				EndIf
			EndIf
	
	
	End If
End Sub




Sub gira_y_saca_limites()
	
	''''''''''''''''''''''''''''''''''''''''''''''''''''
	' factor de escala principal, segun recuadro grafico
	''''''''''''''''''''''''''''''''''''''''''''''''''''
	' actualizamos anchopan y altopan, con las medidas reales "dentro" del marco de guindous, descontando bordes
	anchopan= WindowClientWidth(hwnd)
	altopan = WindowClientHeight(hwnd)
 	' ajustes ventana grafica eliminando las partes de marcos de menus , tanto el de abajo, como el de la derecha
	anchograf  = anchopan-212
	altograf   = altopan-178
	' una simple copia de las variables, pero mas cortas y significativas. se usan para centrajes en muchos sitios (como los ejes, por ejemplo)
	resx=anchograf
	resy=altograf
	''''''''''''''''''''''''''''''''''''''''''''''''''''
	
	XMAX=-99999:YMAX=-99999:ZMAX=-99999
	XMIN=+99999:YMIN=+99999:ZMIN=+99999
	DIFLIMX=0:DIFLIMY=0
	
	For f=0 To nlin
		X1=coord(f,0)
		Y1=coord(f,1)
		Z1=coord(f,2)

		'If X1=99999.999 Then X1=coord(f+1,0) 
		'If Y1=99999.999 Then Y1=coord(f+1,1)
		'If Z1=99999.999 Then Z1=coord(f+1,2)

		' carga las variables globales con las XYZ locales
		ZX=X1:ZY=Y1:ZZ=Z1
		' giramos
		CALC3D(ANGX,ANGY,ANGZ)
		' recogemos la salida
		coord3D(f,0)=ZX
		coord3D(f,1)=ZY
		coord3D(f,2)=ZZ

		If ZX>XMAX Then XMAX=ZX			
		If ZX<XMIN Then XMIN=ZX			
		If ZY>YMAX Then YMAX=ZY			
		If ZY<YMIN Then YMIN=ZY			
		If ZZ>ZMAX Then ZMAX=ZZ		
		If ZZ<ZMIN Then ZMIN=ZZ		
	Next
	
	' para hallar el factor, primero, hacemos los dos factores, tanto X(ancho) como Y(alto)
	' restando el borde primero a la ventana grafica, para dejar un margen, y que el grafico a dbujar no llegue al extremo (50 pixel por lado, 100 al total)
	' para el factr, miramos ancho y alto de pieza (con sus MAX-MIN) y lo usamos de divisor 
	factorx=(resx-BORDE)/Abs(XMAX-XMIN) ' medidas pantalla grafica-borde/medidas pieza
	factory=(resy-BORDE)/Abs(YMAX-YMIN)
	
	' de los dos factores, cogemos el mas restrictivo (el mas grande por valor)
	If factorx>factory Then 
		factor=factory ' y este es nuestro factor
		DIFLIMX=Abs( Abs((XMAX-XMIN)*factor ) - (resx-BORDE) )/2 ' ademas, miramos la suma a añadir al factor "no" usado, para centrarlo (mejor ver que explicar)
	Else 
		factor=factorx ' o este, segun sea uno mas grande que otro
		DIFLIMY=Abs( Abs((YMAX-YMIN)*factor ) - (resy-BORDE) )/2
	EndIf
	
End Sub



' busca una Z aprox. en las cordenadas XY dadas del raton. es muy basto el sistema, pero da una idea de la Z
Function localiza_Z(X As Single, Y As Single) As Integer
	Dim As Double XB, YB
	Dim As double margen	
	Dim As Double fact

	' el factor depende de si estamos en zoom o no
	fact=IIf(zoom, factorzoom, factor)


	' a mas margen, mas encuentra, pero mas error
	margen=6/fact ' es un lio, pero en general, pieza peque, margen peque, factor grande.  pieza grande, margen grande, factor peque
	' ejemplo, un burejo de 10mm. lleva factor 54, margen 0.1, un lateral de 2 metros, factor 0.15, margen 10mm. aprx.
	
	'Locate 1,1:Color textonegro,negro:Print X,Y
	For f=0 To nlin
		XB=coord3D(f,0) ' cojo los datos ya rotados, a pesar de que puede estar en planta, para no hacer dos rutinas, una para planta y otra para 3D
		YB=coord3D(f,1) ' la diferencia es de milesimas de error, entre la matriz COORD() (originales del NC) y COORD3D() (rotados o no)

			If X>(XB-margen) And X<(XB+margen) Then
				If Y>(YB-margen) And Y<(YB+margen) Then
					Return f ' devuelve la posicion en la que localiza la Z
				EndIf
			EndIf

		'If mira_ESC() Then Return -1 ' aqui no tiene sentido. va tan rapido, que no se necesita detener. el solo acaba antes de necesitar pulsar ESC
	Next
	Return -1
End Function
