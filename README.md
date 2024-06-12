# Programación Logica - Laboratorio 2


## Prerequisitos

- Instalar [SWI_Prolog](https://www.swi-prolog.org/Download.html)
- Instalar [Problog](https://problog.readthedocs.io/en/latest/install.html)
- Agregar problog al path

## Archivos

Los siguientes archivos deben estar en la misma carpeta para poder ejecutarse.

- yahtzeelog.pl: Archivo con la logica principal del programa
- modelo.pl: Contiene los predicados en problog
- conexion_problog.pl: Archivo para conecta yahtzeelog.pl con modelo.pl

## Ejecución de yahtzeelog

#### Abrir una consola en la carpeta con los archivos

`yahtzeelog.pl`, `modelo.pl`, `conexion_problog.pl`

#### Ejecutar SWI-Prolog

`swipl`

#### Consultar yahtzeelog

`consult("yahtzeelog.pl").`

#### Juego interactivo con el usuario

`yahtzee(humano, Seed)`, donde Seed es un numero que funciona como semilla de numeros aleatorios.

#### Juego con estrategia

`yahtzeelog(Estrategia, Seed)`, donde Estrategia puede ser ia_det o ia_prob, y Seed un numero que funciona como semilla de numeros aleatorios.

