program tt
Implicit none
Integer, dimension(3,3)::tablero=0
Integer,dimension(2):: loc=(/0,0/)
Integer:: i,j,k !auxiliares
Integer:: n=1, theta=0
Logical:: cw=.false.,hw=.false.  !cw= computer wins, hw = Human wins
character:: sel="w"
!Descomentar y cambiar para trucar el juego !Util para hacer debug
!tablero(1,1:3)=(/0,0,0/)
!tablero(2,1:3)=(/0,0,0/)
!tablero(3,1:3)=(/0,0,0/)
Print*, "#######################"
Print*, "# Juego de 3 en linea #"
Print*, "#   Ricardo Montero   #"
Print*, "#######################"
Do
	Do while (.not.(sel == "s" .or. sel == "S" .or. sel == "n" .or. sel=="N"))
		Print*, "Quieres empezar tu? [S/N]"
		Read*, sel
		If (sel == "s" .or. sel == "S") n=2
	enddo 
	!Empieza el juego
	DO while (theta /=9)
		! Selecciona al que le toca jugar, un numero 'n' par indica turno humano, un numero 'n' impar indica turno ordenador
		If (mod(n,2)==0) then
			!Juega humano
			Do !CENTINELA, sin el puede haber problemas
				Print*, "Introduce donde vas a colocar. [F,C]"
				Read*, loc
				If (tablero(loc(1),loc(2))==0) then
					exit 
				else
					Print*, "Selecciona un sitio correcto"
				endif
			enddo
			!La función colocar devuelve el tablero con la nueva dicha del jugador 'n', colocada en la posición loc.
			tablero=colocar(tablero,loc,n) 
		else
			!juega maquina.
			!Se llama a la subrutina pensamiento crítico 'pc' y esta decide entre que escoger.
			!La prioridad de la maquina es ganar, su segunda prioridad es no perder bajo ningun concepto
			!Su ultima prioridad es colocar la ficha en el lugar donde hay mayores combinaciones en
			!el caso de que pudiera poner tantas fichas como quisiera. (Es un poco liosa está ultima)
			Print*, "Turno del ordenador"
			call pc(tablero,loc)
			tablero=colocar(tablero,loc,n)
		endif
		theta=theta+1 !Si se hacen 9 movimientos, que son los máximos que permite el tablero, se sale del bucle.
		n=n+1 !Avanza un turno.
		Print*, ""
		call mostrar(tablero) !Está función muestra el tablero tal y como se encuentra, el ordenador juega con 4, y el humano con 1
		Print*, ""
		!Al final de cada jugada se comprueba si alguien ha ganado, se hace sumando, si se suma 12 o 3 es que se ha hecho linea.
		Call check(tablero,cw,hw)
		!Si gana alguno, se sale del DOWHILE antes de tiempo
		If (cw) then
			theta=9 !Se fuerza la salida
			Print*, "Gana la maquina"
		elseif (hw) then
			theta=9 !Se fuerza la salida
			Print*, "Gana el humano"
		Elseif (theta==9) then
			Print*, "No gana nadie"
		endif
	ENDDO
	Print*, "Deseas empezar otra partida?"
	Read*, sel
	If (sel == "s" .or. sel == "S") then
		sel="w"
		n=1
		theta=0
		tablero=0
		loc=(/0,0/)
	else
		exit
	endif
enddo
Pause "Pulse 'intro' para salir"
!###########!
contains
!***************!
!*** MOSTRAR ***!
!***************!
Subroutine Mostrar(fichas) !La funcion mostrar muestra el tablero. No tiene ningun misterio.
	Integer, dimension (3,3):: fichas
	Character, dimension(3,3):: tb
	Integer:: i,j
	Print*, "+---+---+---+"
	Do i=1,3
		Do j=1,3
			If (fichas(i,j)==4) then
				tb(i,j)="X"
			elseif (fichas(i,j)==1) then
				tb(i,j)="O"
			else
				tb(i,j)=" "
			endif
		enddo
		Print*, "| ",tb(i,1)," | ",tb(i,2)," | ",tb(i,3)," |"
		Print*, "+---+---+---+"
	enddo	
endsubroutine

!***************!
!*** COLOCAR ***!
!***************!
function colocar(tab,loc,n) !Esta funcion detecta de quien es el turno, y donde hay que colocar. No se preocupa de si el tablero está vacio.
	!Por ello son las funciones que usan esta quien deben evitar que se use una posición ocupada.
	!Esto se hace así para evitar bucles de comprobación sobre si se ha puesto o no.
	Integer,dimension(3,3)::tab, colocar
	Integer,dimension(2)::loc
	Integer::n
	If (mod(n,2)==0) then
		!Pone en el tablero 1, que es la marca del jugador
		colocar=tab
		colocar(loc(1),loc(2))=1
	else
		!Pone en el tablero 4, que es la marca del pc
		colocar=tab
		colocar(loc(1),loc(2))=4
	endif

endfunction
!******************************!
!***** HA GANADO ALGUIEN? *****!
!******************************!
!La función check comrpueba si ha ganado alguien. El juego ha sido pensado para que las sumas laterales den 3 o 12 si alguien ha ganado.
!Es matematicamente imposible que se de una victoria sin estar en linea
!Esta función permite que ambos jugadores ganen, pero el sistema por turnos evita esto, pues si ha ganado alguno
!reinicia el programa.
Subroutine check(T,a,z)
	Integer,dimension(3,3)::T
	Integer,dimension(8)::SS
	Integer::i
	Logical::a,z
	a=.false.
	z=.false.
	SS(1)= Sum(T(1,:))					!
	SS(2)= Sum(T(2,:))					!   4 5 6  8
	SS(3)= Sum(T(3,:))					! 1 # # #
	SS(4)= Sum(T(:,1))					! 2 # # #
	SS(5)= Sum(T(:,2))					! 3 # # #
	SS(6)= Sum(T(:,3))					!              7
	SS(7)= T(1,1)+T(2,2)+T(3,3)			!
	SS(8)= T(3,1)+T(2,2)+T(1,3)			!
	Do i=1,8
		If (ss(i)==12) then
			a=.true.
		elseif (ss(i)==3) then
			z=.true.
		endif
	enddo
Endsubroutine
!*******************************!
!** PENSAMIENTO CRÍTICO  **!
!*******************************!
!Aquí está la madre del cordero.
!Esta función comprueba primero si puede ganar colocando una ficha de manera inmediata.
!Si esto no ocurre, comprueba si pueder evitar perder, colocando una ficha en una hilera en la que el oponente controlo dos casillas
!Si ninguna anterior ocurre la función puede llamar a dos subfunciones, "mejor jugada" o "inteligencia artificial".
!La primera es la versión antigua y la mantengo por curiosidad. A la segunda versión todavía no la he ganado.
Subroutine pc(T,L)
	Integer, dimension(3,3):: T
	Integer, Dimension(2)::L
	Integer, dimension(8):: SS
	Integer:: i,nw,nl
	Logical:: win, lose
	Win=.false. ; nw=0
	lose=.false. ; nl=0
	!Calculamos el vector suma. 
	SS(1)= Sum(T(1,:))					!
	SS(2)= Sum(T(2,:))					!   4 5 6  8
	SS(3)= Sum(T(3,:))					! 1 # # #
	SS(4)= Sum(T(:,1))					! 2 # # #
	SS(5)= Sum(T(:,2))					! 3 # # #
	SS(6)= Sum(T(:,3))					!          7
	SS(7)= T(1,1)+T(2,2)+T(3,3)			        !
	SS(8)= T(3,1)+T(2,2)+T(1,3)		         	!
	Do i=1,8
		If (SS(i)==8) then
			win=.true.
			nw=i
		elseif (SS(i)==2) then
			lose=.true.
			nl=i
		endif
	enddo
	If (win) then
		!Buscar posición de ganador
		L=ganar(T,nw)
	elseif (lose) then
		!Buscar posicion donde pierdes
		L=ganar(t,nl)
	else
		!Nos toca pensar la mejor jugada
		!L=MJ(T,SS) !Método Antiguo
		L=ia(T,SS) !El sistema moderno
	endif
endsubroutine
	!*************!
	!**  GANAR  **!
	!*************!
	!La subfunción ganar, fue diseñada para buscar posibles victorias pero finalmente se utilizo para evitar posubles derrotas
	!Tecnicamente deberia llamarse NoPerder
	!Comprueba huecos vacios usando sumas parciales.
	Function ganar(T,nw)
		Integer, dimension(3,3)::T
		Integer, dimension(2):: ganar
		Integer::nw
		ganar=(/0,0/)
			If (nw<=3) then ! Analiza por filas
				ganar=(/nw,minloc(T(nw,:))/)
			elseif (nw<=6) then ! Analiza por columnas
				nw=nw-3
				ganar=(/minloc(T(:,nw)),nw/)
			Elseif (nw==7) then !Analiza la diagonal 7, Izda,sup :: Dcha,inf
				If (T(1,1)==0) then
					ganar=(/1,1/)
				elseif (T(2,2)==0) then
					ganar=(/2,2/)
				else
					ganar=(/3,3/)
				endif
			else !Analiza la diagonal 8
				If (T(1,3)==0) then
					ganar=(/1,3/)
				elseif (T(2,2)==0) then
					ganar=(/2,2/)
				else
					ganar=(/3,1/)
				endif
			endif
	endfunction
	!**********************!
	!**** MEJOR JUGADA ****!
	!**********************!
	!La función mejor jugada es el sistema antiguo de pensamiento. Esta decide cual es la mejor jugada.
	!La mejor jugada es aquella en la que las lineas están compuestas en su totalidad por numeros pares y
	!tienen sus sumas parciales más altas.
	Function MJ(T,Ss)
		Integer,dimension(3,3)::T, Naux
		Integer,Dimension(2)::mj
		Integer,dimension(8)::ss
		Integer::i,j
			Naux=0
			! Regeneramos el vector y le decimos que si la suma no es par, que no nos interesa poner ficha en esa linea(porque hay ocupación del enemigo).
			!**Regenerando el vector
			do i=1, 8
				If (mod(Ss(i),2)/=0) ss(i)=0
			enddo
			!** Ahora calculamos la nueva matrix naux, está tiene la suma de las parciales del vector regenerado.
			Do i=1,3
				do j=1,3
					Naux(i,j)=Naux(i,j)+ss(i)
				enddo
			enddo
			Do i=1,3
				do j=1,3
					Naux(j,i)=Naux(j,i)+ss(i+3)
				enddo
			enddo
			Do i=1,3
				Naux(i,i)=Naux(i,i)+ss(7)
				Naux(i,4-i)=Naux(i,4-i)+ss(8)
			enddo
			!** En este momento podemos tener seleccionado como maximo valor un lugar donde ya esté la posición ocupada,
			!** Por ello vamos a darle el valor cero a esas posiciones con el siguiente algoritmo.
			!** Comprueba si el tablero tiene un cero (sin ficha colocada) si no es así, se carga esa posición en naux
			Do i=1,3
				Do j=1,3
					!El numero es negativo para que no existan problemas en la primera tirada donde el resto son ceros.
					If (T(i,j)/=0) Naux(i,j)=-5
				enddo
			enddo
			MJ=maxloc(naux) !Se asigna la mejor jugada
			!Este sistema es vulnerable si empieza la maquina y pones en: 3,3 luego en 3,1 y por ultimo en 1,3
			!Por eso fue sustituido por la inteligencia avanzada.
	endfunction
	
	!*********************!
	!*** IA  AVANZADA  ***!
	!*********************!
	!Este sistema lo que hace es coger el tablero y rellenar todos los huecos vacios posibles con 4
	!Cuenta que casillas comparten más victorias y posibles lineas.
	!Si una casilla con 4 provoca una victoria se le suma 1, si provoca 2 victorias se le suma 2 y así sucesivamente.
	!Para entedernos, lo que hace es coger el tablero, y lo rellena con sus fichas.
	!Acto seguida marca todas las posibles formas de victoria como si pintaras lineas encima del tablero
	!Por ultimo cuenta las lineas que pasan por cada casilla. La que más tenga es la candidata.
	!Hasta la fecha no he conseguido ganarle.
	fUNCTION ia(T,SS)
		Integer, dimension(3,3)::T,copy
		Integer,Dimension(2)::IA
		Integer,dimension(8)::SS
		Integer,Dimension(6)::sv
		Integer::i,j
		copy=t
		Do i=1,3
			Do j=1,3
				If(copy(i,j)==0) copy(i,j)=4
			enddo
		enddo
		SS(1)= Sum(copy(1,:))					!
		SS(2)= Sum(copy(2,:))					! 7 4 5 6 8
		SS(3)= Sum(copy(3,:))					! 1 # # #
		SS(4)= Sum(copy(:,1))					! 2 # # #
		SS(5)= Sum(copy(:,2))					! 3 # # #
		SS(6)= Sum(copy(:,3))					! 8       7
		SS(7)= copy(1,1)+copy(2,2)+copy(3,3)			!
		SS(8)= copy(3,1)+copy(2,2)+copy(1,3)			!
		copy=0
		Do i=1,3
			If (ss(i)==12) then
				copy(i,:)=copy(i,:)+1
			endif
			If (ss(i+3)==12) then
				copy(:,i)=copy(:,i)+1
			endif
		enddo
		If (ss(7)==12) then
			copy(1,1)=copy(1,1)+1
			copy(2,2)=copy(2,2)+1
			copy(3,3)=copy(3,3)+1
		endif
		If (ss(8)==12) then
			copy(1,3)=copy(1,3)+1
			copy(2,2)=copy(2,2)+1
			copy(3,1)=copy(3,1)+1
		endif
		Do i=1,3
			Do j=1,3
				!El numero es negativo para que no existan problemas en la primera tirada donde el resto son ceros.
				If (T(i,j)/=0) copy(i,j)=-5
			enddo
		enddo
		ia=maxloc(copy)
	endfunction
!ENDCONTAINS
endprogram
