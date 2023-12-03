

sub salir()
	a=MessageBox(NULL, "     ¿Salir del Simulador?", "SimulaCNC", MB_SERVICE_NOTIFICATION   Or MB_ICONQUESTION   Or MB_YESNO)
	if a=IDYES then Close_Window(hwnd):end ' close window 1:End
	REDIBUJAR=1 ' redibuja si decidimos no salir, por que la ventana pisa el contenido
End Sub

' comprueba el estado de ESC y "X" de ventana
Function mira_ESC() As Integer

	If GetAsyncKeyState(&h1B)<0 Then Return 1

	Return 0
End Function


' borra marco grafico
Sub gcls()
	Line (0,0)-Step(resx-1,resy-1),negro,bf
End Sub


' sustituir por el logo bitmap de tu empresa
logo_miempresa:
Data 0

' logo : aqui podria ir el logo de tu empresa
Sub PON_LOGO(X2 As Integer, Y2 As Integer)
	Restore logo_miempresa
	Dim As Integer f2,g2,a2,A3
	For F2=0 To 156
		For G2=0 To 61
			Read A2
			' como WINDOW9 trabaja con BGR en vez de RGB, tengo que convertirlo (primer byte a tercera pos., segundo queda igual, y tercero, a primera pos.)
			'       primero a tercero             segundo igual          tercero a primero
			A3=((A2 Shl 16) And &h00ff0000) Or (A2 And &h0000ff00) Or ((A2 Shr 16) And &h000000ff)
			PixDraw (x2+F2,Y2+G2,A3)
		Next
	Next
End Sub


menda:
'Data "Joseba Epalza, 2020" ' sin encriptar
Data "o”˜Š‡†Ej•†‘Ÿ†QEWUWU" ' encriptado +37
' marcos principales
Sub marcos()
	WindowStartDraw(hwnd,anchopan-204,altopan-295,199,120,0) 
	Boxdraw (0,0,199,120,&hf0f0f0,&hf0f0f0) ' color fondo guindous (240,240,240)
	PON_LOGO(20,20)
	'
	'"Joseba Epalza, 2020"
	Restore menda
	sa=""
	Read sb
	For f=1 To Len(sb)
		sa=sa+Chr(Asc(Mid(sb,f,1))-37)
	Next
	' para sacar mi menda fuera, una vez encriptado
	'Open "pp.txt" For Output As 1
	' Print #1,sa
	'Close 1
	TextDraw(30, 85, sa,&hf0f0f0,&he0e0e0)	
	
	StopDraw
End Sub


' como entrada emplea ZTEMP() shared
Dim Shared ZTEMP(3,7) As Single
SUB PONEJES3D(xcent As Integer, ycent As integer)

	Dim As Single CX, CY

	'Boxdraw (xcent-45, ycent-48, 90, 90, negro,negro) ' cls, pero no hace falta ya
	LINE (xcent-45, ycent-48 )-step( 90, 90) , grisfondo,b' marco gris

	' DIBUJA LOS TRES EJES
	Line (ZTEMP(1, 1) + xcent,   ZTEMP(2, 1) + ycent )-( ZTEMP(1, 3) + xcent,   ycent - ZTEMP(2, 3) ) , blanco
	Line (ZTEMP(1, 1) + xcent,   ZTEMP(2, 1) + ycent )-( ZTEMP(1, 2) + xcent,   ycent - ZTEMP(2, 2) ) , blanco
	Line (ZTEMP(1, 1) + xcent,   ZTEMP(2, 1) + ycent )-( ZTEMP(1, 4) + xcent,   ycent - ZTEMP(2, 4) ) , blanco

	'  DIBUJA LA X DEL AXONO	
	CX = ZTEMP(1, 5) + xcent+1: CY = ycent - ZTEMP(2, 5) +1
	Line (CX-4,  CY-4 )-( CX+4,  CY+5), blanco
	Line (CX-4,  CY+4 )-( CX+4,  CY-5), blanco
	
	'  DIBUJA LA Y DEL AXONO
	CX = ZTEMP(1, 6) + xcent : CY = ycent - ZTEMP(2, 6)-2
	Line (CX-3,  CY )-( CX,  CY+3), blanco
	Line (CX+3,  CY )-( CX,  CY+3), blanco
	Line (CX,  CY+3 )-( CX,  CY+9), blanco
	
	'  DIBUJA LA Z DEL AXONO
	CX = ZTEMP(1, 7) + xcent: CY = ycent - ZTEMP(2, 7)-2
	Line (CX-3,  CY-5 )-( CX+3,  CY-5), blanco
	Line (CX-3,  CY+4 )-( CX+3,  CY-5), blanco
	Line (CX-3,  CY+4 )-( CX+3,  CY+4), blanco
	
end SUB



Sub DIBUJA_EJE()
	Dim As Integer F,lon, a
	
	' matriz de coordenadas del eje 3d a dibujar
	LON = 29 ' longitud de los ejes
	A = Int(LON/3) ' distancia de las letras sobre el eje
	' EJES TRES LINEAS
	ZTEMP(1, 1) = 0		: ZTEMP(2, 1) = 0			: ZTEMP(3, 1) = 0 ' CENTRO
	ZTEMP(1, 2) = LON		: ZTEMP(2, 2) = 0			: ZTEMP(3, 2) = 0
	ZTEMP(1, 3) = 0		: ZTEMP(2, 3) = LON		: ZTEMP(3, 3) = 0
	ZTEMP(1, 4) = 0		: ZTEMP(2, 4) = 0			: ZTEMP(3, 4) = LON
	' LETRAS EN EJES
	ZTEMP(1, 5) = LON + A: ZTEMP(2, 5) = 0			: ZTEMP(3, 5) = 0
	ZTEMP(1, 6) = 0		: ZTEMP(2, 6) = LON + A	: ZTEMP(3, 6) = 0
	ZTEMP(1, 7) = 0		: ZTEMP(2, 7) = 0			: ZTEMP(3, 7) = LON + A
	
	FOR F = 1 TO 7
	  ZX = ZTEMP(1, F): ZY = ZTEMP(2, F): ZZ = ZTEMP(3, F)
	  CALC3D(ANGX,ANGY,ANGZ) ' hace los calculos al 3D
	  ZTEMP(1, F) = ZX: ZTEMP(2, F) = ZY: ZTEMP(3, F) = ZZ
	Next

	' ejes en 55,55
	PONEJES3D(resx-55,resy-55)
END Sub


' FILEDIALOG
function GetOFN( byval hwndOwner as HWND, _
                 byval pszFile as zstring ptr, _
                 byval nMaxFile as integer) as integer

    dim as OPENFILENAME ofn

    ofn.lStructSize = sizeof(OPENFILENAME)
    ofn.hWndOwner = hwndOwner
    ofn.lpstrFilter = strptr(!"Todos (*.*)\0*.*\0\0")
    ofn.lpstrFile = pszFile
    ofn.nMaxFile = nMaxFile
    ofn.Flags = OFN_FILEMUSTEXIST or OFN_LONGNAMES

    return GetOpenFileName( @ofn )

end Function
