% SISTEMA DE REGISTRO DE ESTUDIANTES - PROLOG
% ST0244 - Lenguajes de Programacion - EAFIT

:- dynamic estudiantes/1.
estudiantes([]).

cargar_archivo :-
    (exists_file('University.txt') ->
        open('University.txt', read, Stream),
        leer_lineas(Stream, Lista),
        close(Stream),
        retractall(estudiantes(_)),
        assert(estudiantes(Lista)),
        format("~`=t~50|~n"),
        write('Archivo University.txt cargado correctamente.'), nl,
        format("~`=t~50|~n")
    ;
        retractall(estudiantes(_)),
        assert(estudiantes([])),
        write('No se encontro University.txt. Iniciando con lista vacia.'), nl
    ).

leer_lineas(Stream, []) :-
    at_end_of_stream(Stream), !.

leer_lineas(Stream, [Estudiante | Resto]) :-
    read(Stream, Termino),
    Termino = estudiante(ID, Nombre, Estado, Entrada, Salida),
    Estudiante = estudiante(ID, Nombre, Estado, Entrada, Salida),
    leer_lineas(Stream, Resto).

guardar_archivo :-
    estudiantes(Lista),
    open('University.txt', write, Stream),
    escribir_estudiantes(Stream, Lista),
    close(Stream).

escribir_estudiantes(_, []).
escribir_estudiantes(Stream, [estudiante(ID, Nombre, Estado, Entrada, Salida) | Resto]) :-
    format(Stream, 'estudiante(~w, ~q, ~w, ~w, ~w).~n',
           [ID, Nombre, Estado, Entrada, Salida]),
    escribir_estudiantes(Stream, Resto).

% CHECK IN
check_in :-
    nl, write('--- CHECK IN ---'), nl,
    write('Ingresa el ID del estudiante: '),
    read(ID),
    estudiantes(Lista),
    (member(estudiante(ID, _, inside, _, _), Lista) ->
        write('ERROR: Este estudiante ya tiene check-in activo.'), nl
    ;
        write('Ingresa el nombre completo del estudiante: '),
        read(Nombre),
        get_time(T),
        HoraEntrada is round(T * 1440),  % Minutos desde medianoche
        NuevoEstudiante = estudiante(ID, Nombre, inside, HoraEntrada, 0),
        append(Lista, [NuevoEstudiante], NuevaLista),
        retractall(estudiantes(_)),
        assert(estudiantes(NuevaLista)),
        guardar_archivo,
        write('Check-in registrado exitosamente.'), nl
    ).

% CHECK OUT
check_out :-
    nl, write('--- CHECK OUT ---'), nl,
    write('Ingresa el ID del estudiante: '),
    read(ID),
    estudiantes(Lista),
    (member(estudiante(ID, Nombre, inside, Entrada, _), Lista) ->
        get_time(T),
        HoraSalida is round(T * 1440),
        eliminar_estudiante(ID, Lista, ListaSin),
        NuevoEst = estudiante(ID, Nombre, outside, Entrada, HoraSalida),
        append(ListaSin, [NuevoEst], NuevaLista),
        retractall(estudiantes(_)),
        assert(estudiantes(NuevaLista)),
        guardar_archivo,
        Duracion is HoraSalida - Entrada,
        format('Check-out registrado exitosamente. Duracion: ~w minutos.~n', [Duracion])
    ;
        write('ERROR: ID no encontrado o ya tiene check-out.'), nl
    ).

eliminar_estudiante(ID, [estudiante(ID, _, _, _, _) | Resto], Resto) :- !.
eliminar_estudiante(ID, [Otro | Resto], [Otro | RestoNuevo]) :-
    eliminar_estudiante(ID, Resto, RestoNuevo).

% BUSCAR ESTUDIANTE POR ID
buscar_estudiante :-
    nl, write('--- BUSCAR ESTUDIANTE ---'), nl,
    write('Ingresa el ID del estudiante: '),
    read(ID),
    estudiantes(Lista),
    (member(estudiante(ID, Nombre, Estado, Entrada, Salida), Lista) ->
        format('ID      : ~w~n', [ID]),
        format('Nombre  : ~w~n', [Nombre]),
        format('Estado  : ~w~n', [Estado]),
        format('Entrada : ~w~n', [Entrada]),
        format('Salida  : ~w~n', [Salida])
    ;
        write('Estudiante no encontrado.'), nl
    ).

% CALCULAR TIEMPO DE PERMANENCIA
calcular_tiempo :-
    nl, write('--- CALCULAR TIEMPO ---'), nl,
    write('Ingresa el ID del estudiante: '),
    read(ID),
    estudiantes(Lista),
    (member(estudiante(ID, Nombre, outside, Entrada, Salida), Lista) ->
        Duracion is Salida - Entrada,
        Horas is Duracion // 60,
        Minutos is Duracion mod 60,
        format('Estudiante: ~w~n', [Nombre]),
        format('Tiempo total: ~w horas y ~w minutos (~w minutos totales)~n',
               [Horas, Minutos, Duracion])
    ;
        (member(estudiante(ID, _, inside, _, _), Lista) ->
            write('El estudiante aun no ha hecho check-out.'), nl
        ;
            write('Estudiante no encontrado.'), nl
        )
    ).

% LISTAR ESTUDIANTES
listar_estudiantes :-
    nl, write('--- LISTA DE ESTUDIANTES ---'), nl,
    estudiantes(Lista),
    (Lista = [] ->
        write('No hay estudiantes registrados.'), nl
    ;
        format('~w~t~20|~w~t~40|~w~t~52|~w~t~60|~w~n',
               ['ID', 'Nombre', 'Estado', 'Entrada', 'Salida']),
        write('----------------------------------------------------------------'), nl,
        maplist(mostrar_estudiante, Lista),
        nl, length(Lista, Total),
        format('Total de estudiantes: ~w~n', [Total])
    ).

mostrar_estudiante(estudiante(ID, Nombre, Estado, Entrada, Salida)) :-
    format('~w~t~20|~w~t~40|~w~t~52|~w~t~60|~w~n',
           [ID, Nombre, Estado, Entrada, Salida]).

% MENU PRINCIPAL 

menu :-
    nl,
    write('========================================'), nl,
    write('     SISTEMA DE REGISTRO - EAFIT          '), nl,
    write('========================================'), nl,
    write('  1. Check In                           '), nl,
    write('  2. Check Out                          '), nl,
    write('  3. Buscar estudiante por ID           '), nl,
    write('  4. Listar estudiantes                  '), nl,
    write('  5. Calcular tiempo en universidad     '), nl,
    write('  6. Salir                              '), nl,
    write('========================================'), nl,
    write('Elige una opcion: '),
    read(Opcion),
    nl,
    manejar_opcion(Opcion).

manejar_opcion(1) :- !, check_in, menu.
manejar_opcion(2) :- !, check_out, menu.
manejar_opcion(3) :- !, buscar_estudiante, menu.
manejar_opcion(4) :- !, listar_estudiantes, menu.
manejar_opcion(5) :- !, calcular_tiempo, menu.
manejar_opcion(6) :- !,
    nl, write('Guardando datos y saliendo...'), nl,
    guardar_archivo,
    write('Hasta luego!'), nl.
manejar_opcion(_) :-
    write('Opcion invalida. Por favor intenta de nuevo.'), nl,
    menu.

:- initialization(main, main).

main :-
    cargar_archivo,
    menu.