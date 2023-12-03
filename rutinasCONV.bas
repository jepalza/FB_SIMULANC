Function num2cad(num As Single) As String
	Return Right("0000"+LTrim(RTrim(Str(num))),4)
End Function

Function coord2cad(num As Single) As String
	Dim As String SR
	Dim As Integer a1,a2,a3
	a1=Abs(Fix(num)) ' parte entera
	If a1>9999 Then Print "error: coordenadas mayores a 9999.9999. Preguntar":Sleep:End
	a2=(Abs(Frac(num)))*10000 ' parte fraccional
	a3=Sgn(num) ' signo
	SR=IIf(a3=-1,"-","+")+num2cad(a1)+"."+Left(num2cad(a2),3)
	Return SR
End Function


Sub convertir_NC()
	Dim As String nsalida,avance,revol, extension
	Dim As String SX,SY,SZ,SF,SG,SS,SN
	'Dim As Integer numprog=-1 ' para no usarlo, si es -1
	Dim As String XYZ, XYZOLD ' guarda XYZ en formato texto (SX+SY+SZ)
	Dim As Integer num,tipo,NN
	Dim As Integer primeralinea=0
	Dim As String M90 ' para heiden


	' libera la pantalla grafica
	apaga3D()

	'Color blanco,negro
	
	''''''''''''''''''''''''''''''''''''
	Locate 2,2:Print "Formato de salida (ESC salir):"
	Locate 3,2:Print "1 - ISO HEIDENHAIN  (NC)   "
	Locate 4,2:Print "2 - ISO FAGOR       (PIM)  "
	Locate 5,2:Print "3 - ISO FIDIA       (NC)   "
	Locate 6,2:Print "4 - HNC HEIDENHAIN  (H)    "
	'windowevent()
	A3:
	'SS=InKey
	'GetMouse(mx,my,m,bm)
	'If bm=2 Then ss=Chr(27) ' pulsar boton derecho es como pulsar ESC

	'If SS="" Then GoTo A3
	'If SS=Chr(27) Then 
	If GetAsyncKeyState(&h1B)<0 Then Sleep 250,1:enciende3D():Exit sub
	'If SS<>"1" Or SS<>"2" Or SS<>"3" Or SS<>"4" Then GoTo A3
	'tipo=Val(SS)
	if GetAsyncKeyState(Asc("1"))<0 Then tipo=1
	if GetAsyncKeyState(Asc("2"))<0 Then tipo=2
	if GetAsyncKeyState(Asc("3"))<0 Then tipo=3
	if GetAsyncKeyState(Asc("4"))<0 Then tipo=4
	If tipo<1 Or tipo>4 Then GoTo A3

	'limpia_teclado():Sleep 200,1
	GetKey() ' vacia la tecla pulsada arriba, sino, aparece escrita
	
	'''''''''''''''''''''''''''''''''''''''''
	malfichero:
		SS=""
		If tipo=1 Then SS= "(entre 1 y 499 ):"
		If tipo=2 Then SS= "(entre 1 y 9999):"

	A0:
	'Locate 10,2
	Print
	Print "Nombre del fichero ";SS;
	Input "",nsalida ' SI ES TIPO<2 SOLO ACEPTA NUMEROS, Y ADEMAS, OBLIGO A MAYUSCULAS
	Print "Fichero de salida:";nsalida
	'windowevent()
	'If mira_esc_conv() Then Exit sub
	If nsalida="" Then enciende3D():exit Sub'GoTo A0
	
	If tipo=1 Then 
		num=Val(LTrim(RTrim(nsalida)))
		If num<1 Or num>499 Then Print "Nombre incorrecto":GoTo malfichero 'Locate 10,31:Print "            ":GoTo A0
		nsalida=Right(num2cad(num),3)
	EndIf
	
	If tipo=2 Then 
		num=Val(LTrim(RTrim(nsalida)))
		If num<1 Or num>9999 Then Print "Nombre incorrecto":GoTo malfichero 'Locate 10,31:Print "            ":GoTo A0
		nsalida=num2cad(num)
	EndIf
	
	' extension por defecto, sino se incluye en el nombre
	If InStr(nsalida,".")=0 Then 
		If tipo=1 Then extension=".NC" ' heiden ISO
		If tipo=2 Then extension=".PIM" ' ISO Fagor
		If tipo=3 Then extension=".NC" ' ISO fidia 
		If tipo=4 Then extension=".H"  ' Heiden normal
	Else
		' si se ha incluido, lo separo del nombre
		extension=Mid(sa,InStr(sa,"."))
		nsalida =Left(sa,InStr(sa,".")-1)
	EndIf
	
	'''''''''''''''''''''''''''''''''''''''
	A1:
	'Locate 12,2
	Print
	Input "Avance (entre 1 y 9999):",num
	If num<1 Or num>9999 Then Print "Avance entre 1 y 9999 !!!":GoTo A1
	avance=LTrim(RTrim(Str(num)))
	Print "Avance:";avance
	
	
	''''''''''''''''''''''''''''''''''''''
	A2:
	'Locate 14,2
	Print
	Input "Revoluciones (entre 1 y 99999):",num
	If num=0 Then Print "Revoluciones deberian ser mayor a cero":GoTo A2
	revol=LTrim(RTrim(Str(num)))
	Print "Avance:";revol






	'''''''''''''''''''''''''''''''''''''''''
	Open rutanc+nsalida+extension For Output As 2
	
	'''''''''''
	' cabecera
	'''''''''''
	
	' ISO HEIDENHAIN
	If tipo=1 Then
		Print #2,"%";nsalida;" G71" ' G71 prog. en MM, en pulgadas seria G70. Nprog entre 1 y 499 SOLO
		If comentario_fresa<>"" Then Print #2,"N1 ";comentario_fresa
		If comentario_espesor<>"" Then Print #2, "N2 ";comentario_espesor
		Print #2,"N3 G99 T1 L+0 R+0"
		Print #2,"N4 G17 T1 S";LTrim(revol)
		NN=10
	EndIf
	
	' ISO FAGOR
	If tipo=2 Then
		Print #2,"%";nsalida
		If comentario_fresa<>"" Then Print #2,"N1 ";comentario_fresa
		If comentario_espesor<>"" Then Print #2, "N2 ";comentario_espesor
		Print #2,"N3 F";LTrim(RTrim(avance));" S";LTrim(revol)
		Print #2,"N4 G17 G90 M03"
		NN=10
	EndIf
	
	' ISO FIDIA
	If tipo=3 Then
		Print #2,"N1 (PROGRAMA:";nsalida;")"
		If comentario_fresa<>"" Then Print #2,"N2 ";comentario_fresa
		If comentario_espesor<>"" Then Print #2, "N3 ";comentario_espesor
		Print #2,"N4 G17 Q1 G90 M03"	
		Print #2,"N5 F";LTrim(RTrim(avance));" S";LTrim(revol)	
		NN=10
	EndIf
	
	' HEIDENHAIN
	If tipo=4 Then
		Print #2,"0 BEGIN PGM ";nsalida; " MM"
		If comentario_fresa<>"" Then Print #2,"1 ;";comentario_fresa
		If comentario_espesor<>"" Then Print #2, "2 ;";comentario_espesor
		Print #2,"3 TOOL DEF 01 L0, R0"
		Print #2,"4 TOOL CALL 01 Z S";LTrim(revol)	
		NN=10
	EndIf
	
	
	
	'''''''''
	' cuerpo
	'''''''''
	sf="F"+LTrim(RTrim(avance))
	M90="M90" ' para HEIDEN SOLO, tanto ISO como normal, al final de linea
	If tipo=2 Or tipo=3 Then M90="" ' en FAGOR y FIDIA, nada de M90 al final
	' bucle de todas las coordenadas
	For f=0 To nlin
		
		' si existe separador de zona tipo tebis (P), se vuelve a poner
		If coord(f,4)>9999 Then ' con "mi" indicativo de G+10000 inventado al leer coordenadas
			' al poner separador de zona, se incrementa la linea
			If tipo=4 Then Print #2,LTrim(RTrim(Str(NN)));"L ;(P)" ' HEIDEN
			If tipo<4 Then Print #2,"N";LTrim(RTrim(Str(NN)));" (P)" ' ISO
			NN+=1
			' en FAGOR y en ISO-HEIDEN, max. 9999
			If tipo<3 Then If NN>9999 Then NN=1
		EndIf
		
		' numero de linea
		If tipo=4 Then SN=LTrim(RTrim(Str(NN)))+"L" ' HEIDEN
		If tipo<4 Then SN="N"+LTrim(RTrim(Str(NN))) ' ISO
		NN+=1
		
		' en FAGOR y en ISO-HEIDEN, max. 9999
		If tipo<3 Then If NN>9999 Then NN=1
		
		SX="X"+coord2cad(coord(f,0))
		SY="Y"+coord2cad(coord(f,1))
		SZ="Z"+coord2cad(coord(f,2))
		XYZOLD=XYZ ' para no repetir coordenadas
		XYZ=SX+SY+SZ

		'SF=IIf(coord(f,3),"F"  ' las F no las ponemos, solo la que se indica arriba, en modo entrada de usuario
		If tipo<4 Then SG=IIf(coord(f,4),"G01","G00") Else SG="" ' en HEIDEN no hay 'G'

		
		If primeralinea=0 Then 
			primeralinea=1
			If tipo=1 Then Print #2,SN;"G90";SG;XYZ;SF;"M03":SF=""
			If tipo=2 Then Print #2,SN;"G05";SG;XYZ;M90:SF=""
			If tipo=3 Then Print #2,SN;SG;XYZ;M90:SF=""
			If tipo=4 Then Print #2,SN;SG;XYZ;SF;"M03" ' en el caso HEIDEN, el SF lo dejo, para repetir en cada linea
		Else
			' resto de casos
			If XYZ<>XYZOLD Then ' si hay una linea XYZ "IDENTICA" a la anterior ya escrita, no se guarda, se salta
				Print #2,SN;SG;XYZ;SF;M90 ' en HEIDEN, el SF se repite en cada linea al final. , para el resto, SF=""
			End if
		End If
		
		' BARRA DE PROGRESO
		SetGadgetState(barraprogreso,(f/nlin)*100)
		
	Next
	
	
	''''''''''
	' finales
	''''''''''
	
	' ISO HEIDENHAIN
	If tipo=1 Then
		Print #2,"N";LTrim(RTrim(Str(NN)));"M05"
		nn+=1
		Print #2,"N";LTrim(RTrim(Str(NN)));"M02"		
		nn+=1
		Print #2,"N9999%";nsalida;" G71" ' "DEBE" acabar en N9999% . G71= programa en MM. (pulgadas G70)		
	EndIf
	
	' ISO FAGOR
	If tipo=2 Then
		Print #2,"N";LTrim(RTrim(Str(NN)));"M05"
		nn+=1
		Print #2,"N";LTrim(RTrim(Str(NN)));"M30"		
	EndIf
	
	' ISO FIDIA
	If tipo=3 Then
		Print #2,"N";LTrim(RTrim(Str(NN)));"M05"
		nn+=1
		Print #2,"N";LTrim(RTrim(Str(NN)));"M02"
	EndIf
	
	' HEIDENHAIN
	If tipo=4 Then
		Print #2,LTrim(RTrim(Str(NN)));"M30"
		nn+=1
		Print #2,LTrim(RTrim(Str(NN)));"END PGM ";nsalida;" MM"
	EndIf
	
	Close 2,5
	
	'REDIBUJAR=1
	
	' vuelve a encender el 3D
	enciende3D()
End Sub

' M05
' M30
' N9999
' PROCESO CONCLUIDO
' M02
' END PGM
' G71
' N1 G99 T1 L+0 R+0
' N2 G17 T1
' G90
' N1
' N2 G17 G90 M03
' N1 G17 Q1 G90 M03
' N2
' 0 BEGIN PGM
' MM
' 1 TOOL DEF 01 L0, R0
' 2 TOOL CALL 01 Z

