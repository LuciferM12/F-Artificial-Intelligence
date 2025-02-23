namespace Busqueda
//variables de tipo
// 's: estado, 'a: accion, 'b: bolsa

// Problema
type problema<'s, 'a> =
    { inicial: 's
      sucesores: 's -> list<'a * 's>
      meta: 's -> bool
      costo: 's -> 'a -> 's -> float }

// type 'a option = None | Some of 'a
//option<nodo<'s,'a>

//nodo
type nodo<'s, 'a> =
    { profundidad: int
      g: float
      estado: 's
      padre: nodo<'s, 'a> option
      accion: option<'a> }

// estrategia

type estrategia<'s, 'a, 'b> =
    { insertar: nodo<'s, 'a> -> 'b -> 'b
      sacar: 'b -> option<nodo<'s, 'a> * 'b>
      vacia: 'b }

module Capitulo3 =
    let hacer_nodo estado = // 'a -> nodo<'a, 'b>
        { padre = None
          accion = None
          profundidad = 0
          estado = estado
          g = 0.0 }

    let expand nodo problema =
        problema.sucesores nodo.estado
        |> List.map (fun (accion, estado) ->
            { padre = Some nodo
              accion = Some accion
              estado = estado
              profundidad = nodo.profundidad + 1
              g = nodo.g + problema.costo nodo.estado accion estado

            })
