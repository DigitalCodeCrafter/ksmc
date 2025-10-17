# KASM assembly language

## Progress

- [X] lexer 
- [X] parser 
- [X] preprocessor 
- - [X] macros 
- - [X] directives 
- - [X] symbol resolution 
- - [X] type checking 
- [X] encoder 

## Example

```
.const .f64v REACT_TIME 2.0

.macro print
    push @
    swap
    call #, "print()"
    pop
.endm

jmp main

hover_throttle:
    ; t = (g + (tgt_v - vertical_speed) / REACT_TIME) m / (F * dot(fore, up))
    push "$ship"
    gmb "verticalspeed"
    sub
    pushv REACT_TIME
    div
    swap
    argb
    swap
    

    ; g
    push "$ship"
    gmb "body"
    gmb "mu"
    push "$ship"
    gmb "body"
    gmb "position"
    gmb "sqrmagnitude"
    div

    add

    push "$ship"
    gmb "mass"
    mul

    push "$ship"
    gmb "availablethrust"

    push @

    push "$ship"
    gmb "facing"
    gmb "forevector"

    push "$ship"
    gmb "body"
    gmb "position"
    gmb "normalized"
    neg

    call #, "vdot()"

    mul

    div

    ret 0

main:
    argb
    push 3
    wait
    pushv 0
    sto "$tgt_v"
    ; lock rcs
    push "$ship"
    gmb "control"
    pushv 0.001
    smb "top"

loop:
    push 0
    wait
    
    push @
    push "$tgt_v"
    eval
    call #, hover_throttle
    sto "$throttle"

    push "$tgt_v"
    eval
    print

    push "$tgt_v"
    push "$ship"
    gmb "control"
    gmb "pilottop"
    pushv 0.05
    mul
    sub
    sto "$tgt_v"

    push "$ship"
    gmb "control"
    dup
    gmb "pilotroll"
    neg
    smb "roll"

    push "$ship"
    gmb "control"
    gmb "pilotmainthrottle"
    push 0
    cgt
    btr loop

exit:
    push "$ship"
    gmb "control"
    push "$throttle"
    eval
    smb "pilotmainthrottle"

    push "$ship"
    gmb "control"
    dup
    pushv 0
    smb "top"
    pushv 0
    smb "roll"
    
    push @
    push "throttle"
    push false
    call #, "toggleflybywire"

    eop
```

## Instructions

### Stack manipulation

| Instruction | Argument                                                        |
|:-----------:|:----------------------------------------------------------------|
| push        | Bool / Int16 / Int32 / Double / String / @                      |
| pushv       | BooleanValue / ScalarIntValue / ScalarDoubleValue / StringValue |
| pop         |                                                                 |
| dup         |                                                                 |
| swap        |                                                                 |
| eval        |                                                                 |
| argb        |                                                                 |
| targ        |                                                                 |

### Arithmetic

| Instruction |
|:-----------:|
| add         |
| sub         |
| mul         |
| div         |
| pow         |
| neg         |
| bool        |

### Truthness

| Instruction |
|:-----------:|
| cgt         | 
| clt         | 
| cge         | 
| cle         | 
| ceq         | 
| cne         | 
| not         | 
| and         | 
| or          | 

### Flow Control

| Instruction | Argument 1     | Argument 2         |
|:-----------:|:---------------|:-------------------|
| eop         |                |                    |
| nop         |                |                    |
| jmp         | String / Int32 |                    |
| call        | String / #     | String / Int32 / # |
| ret         | Int16          |                    |
| btr         | String / Int32 |                    |
| bfa         | String / Int32 |                    |
| bst         |                |                    |
| wait        |                |                    |

### Variables

| Instruction | Argument |
|:-----------:|:---------|
| sto         | String   |
| uns         |          |
| stol        | String   |
| stog        | String   |
| stoe        | String   |
| exst        |          |

### Structures

| Instruction | Argument |
|:-----------:|:---------|
| gmb         | String   |
| smb         | String   |
| gidx        |          |
| sidx        |          |
| gmet        | String   |

### Triggers

| Instruction | Argument 1 | Argument 2 |
|:-----------:|:-----------|:-----------|
| addt        | Bool       | Int32      |
| rmvt        |            |            |

### Scopes

| Instruction | Argument 1 | Argument 2 |
|:-----------:|:-----------|:-----------|
| bscp        | Int16      | Int16      |
| escp        | Int16      |            |

### Delegates & Relocation

| Instruction | Argument 1 | Argument 2 |
|:-----------:|:-----------|:-----------|
| pdl         | Int32      | Bool       |
| prl         | String     |            |
| pdrl        | String     | Bool       |

### Misc

| Instruction | Argument |
|:-----------:|:---------|
| lbrt        | String   |
| tcan        |          |

## Directives

| Directive | Argument 1 | Argument 2 | Argument 3 |
|:---------:|:-----------|:-----------|:-----------|
| .const    | Type       | Ident      | Value      |
| .label    | name       |            |            |

### Typing

example: `.push .i32 20`

| Type Directive | Type              |
|:--------------:|:------------------|
| .b             | Bool              |
| .i16           | Int16             |
| .i32           | Int32             |
| .f64           | Double            |
| .s             | String            |
| .bv            | BooleanValue      |
| .i32v          | ScalarIntValue    |
| .f64v          | ScalarDoubleValue |
| .sv            | StringValue       |

## Built-in kOS Functions

`call #, "<name>"`

| Function            | Arguments                                                                                                         | Returns           |
|:-------------------:|:------------------------------------------------------------------------------------------------------------------|:-----------------:|
| clearscreen         |                                                                                                                   |                   |
| hudtext             | String, Int32, Int32, Int32, RGBA, Bool                                                                           |                   |
| stage               |                                                                                                                   |                   |
| add                 | Node                                                                                                              |                   |
| remove              | Node                                                                                                              |                   |
| warpto              | Double                                                                                                            |                   |
| processor           | String / Volume                                                                                                   | KOSProcessor      |
| vcrs                | Vector, Vector                                                                                                    | Vector            |
| vdot                | Vector, Vector                                                                                                    | Double            |
| vxcl                | Vector, Vector                                                                                                    | Vector            |
| vang                | Vector, Vector                                                                                                    | Double            |
| edit                | Volume / VolumeItem / String                                                                                      |                   |
| range               | Double, (Double), (Double)                                                                                        | Range             |
| constant            |                                                                                                                   | Constant          |
| buildlist           | String                                                                                                            | List< T >         |
| sin                 | Double                                                                                                            | Double            |
| cos                 | Double                                                                                                            | Double            |
| tan                 | Double                                                                                                            | Double            |
| arcsin              | Double                                                                                                            | Double            |
| arccos              | Double                                                                                                            | Double            |
| arctan              | Double                                                                                                            | Double            |
| arctan2             | Double                                                                                                            | Double            |
| anglediff           | Double                                                                                                            | Double            |
| addAlarm            | String, Double, String, String                                                                                    | KACAlarm          |
| listAlarms          | String                                                                                                            | List< KACAlarm >  |
| deleteAlarm         | String                                                                                                            | Bool              |
| stack               | Any< Structure >, ...                                                                                             | Stack             |
| uniqueset           | Any< Structure >, ...                                                                                             | UniqueSet         |
| volume              | (String / Int32)                                                                                                  | Volume            |
| path                | (Volume / VolumeItem / String)                                                                                    | Path              |
| printlist           | String                                                                                                            |                   |
| list                | Any< Structure >, ...                                                                                             | List              |
| pidloop             | (Double), (Double), (Double), (Double), (Double), (Double)                                                        | PIDLoop           |
| lex                 | Any< Structure > / List< Structure >                                                                              | Lex               |
| node                | Double / TimeSpan / TimeStamp, Double, Double, Double                                                             | Node              |
| v                   | Double, Double, Double                                                                                            | Vector            |
| r                   | Double, Double, Double                                                                                            | Direction         |
| q                   | Double, Double, Double                                                                                            | Direction         |
| createOrbit         | Vector, Vector, Body / String, Double                                                                             | Orbit             |
| createOrbit         | Double, Double, Double, Double, Double, Double, Double, Body / String                                             | Orbit             |
| rotatefromto        | Vector, Vector                                                                                                    | Direction         |
| lookdirup           | Vector, Vector                                                                                                    | Direction         |
| angleaxis           | Double, Vector                                                                                                    | Direction         |
| latlng              | Double, Double                                                                                                    | GeoCoordinates    |
| vessel              | String                                                                                                            | Vessel            |
| body                | String                                                                                                            | Body              |
| bodyexists          | String                                                                                                            | Bool              |
| bodyatmosphere      | String                                                                                                            | Atmosphere        |
| bounds              | Vector, Direction, Vector, Vector                                                                                 | Bounds            |
| heading             | Double, Double, (Double)                                                                                          | Direction         |
| slidenote           | Scalar / StringValue, Scalar / Double, Double, (Double), (Double)                                                 | Note              |
| note                | Scalar / StringValue, Double, (Double), (Double)                                                                  | Note              |
| GetVoice            | Int32                                                                                                             | Voice             |
| StopAllVoices       |                                                                                                                   |                   |
| timestamp           | (Double), (Double), (Double), (Double), (Double)                                                                  | TimeStamp         |
| timespan            | (Double), (Double), (Double), (Double), (Double)                                                                  | TimeSpan          |
| hsv                 | Double, Double, Double                                                                                            | HSVA              |
| hsva                | Double, Double, Double, Double                                                                                    | HSVA              |
| rgb                 | Double, Double, Double                                                                                            | RGBA              |
| rgba                | Double, Double, Double, Double                                                                                    | RGBA              |
| vecdraw             | (Vector / Delegate), (Vector / Delegate), (RGBA / Delegate), (String), (Double), (bool), (Double), (Bool), (Bool) | Vecdraw           |
| clearvecdraws       |                                                                                                                   |                   |
| clearguis           |                                                                                                                   |                   |
| gui                 | Double, (Double)                                                                                                  | GUI               |
| positionat          | Orbitable, TimeStamp / Double                                                                                     | Vector            |
| velocityat          | Orbitable, TimeStamp / Double                                                                                     | Vector            |
| highlight           | Part / List< Part > / Element, RGBA                                                                               | HIGHLIGHT         |
| orbitat             | Orbitable, TimeStamp / Double                                                                                     | Orbit             |
| career              |                                                                                                                   | Career            |
| allwaypoints        |                                                                                                                   | List< Waypoint >  |
| waypoint            | String                                                                                                            | Waypoint          |
| transferall         | String, Part / List< Part > / Element, Part / List< Part > / Element                                              | Transfer          |
| transfer            | String, Part / List< Part > / Element, Part / List< Part > / Element, Double                                      | Transfer          |
| abs                 | Double                                                                                                            | Double            |
| mod                 | Double, Double                                                                                                    | Double            |
| floor               | Double, (Int32)                                                                                                   | Double            |
| ceiling             | Double, (Int32)                                                                                                   | Double            |
| round               | Double, (Int32)                                                                                                   | Double            |
| sqrt                | Double                                                                                                            | Double            |
| ln                  | Double                                                                                                            | Double            |
| log10               | Double                                                                                                            | Double            |
| min                 | Scalar / String, Scalar / String                                                                                  | Scalar / String   |
| max                 | Scalar / String, Scalar / String                                                                                  | Scalar / String   |
| random              | (String)                                                                                                          | Scalar            |
| randomseed          | String, Int32                                                                                                     |                   |
| char                | Double                                                                                                            | StringValue       |
| unchar              | String                                                                                                            | Scalar            |
| queue               | Any< Structure >, ...                                                                                             | Queue             |
| print               | String                                                                                                            |                   |
| printat             | String, Int32, Int32                                                                                              |                   |
| toggleflybywire     | String, Bool                                                                                                      |                   |
| selectautopilotmode | String                                                                                                            |                   |
| run                 | Any, ..., @, Volume / VolumeItem / String, #                                                                      |                   |
| load                | Volume / VolumeItem / String, Bool, String / #                                                                    | Bool, Int32       |
| logFile             | String, String                                                                                                    |                   |
| reboot              |                                                                                                                   |                   |
| shutdown            |                                                                                                                   |                   |
| debugdump           |                                                                                                                   | String            |
| debugfreezegame     | Int32                                                                                                             |                   |
| profilerresult      |                                                                                                                   | String            |
| makebuitindelegate  | String                                                                                                            | BuiltinDelegate   |
| droppriority        |                                                                                                                   |                   |
| scriptpath          |                                                                                                                   | Path              |
| switch              | Volume / String / Int32                                                                                           |                   |
| cd                  | (Volume / VolumeItem / String)                                                                                    |                   |
| copypath            | Volume / VolumeItem / String, Volume / VolumeItem / String                                                        | Bool              |
| movepath            | Volume / VolumeItem / String, Volume / VolumeItem / String                                                        | Bool              |
| deletepath          | Volume / VolumeItem / String                                                                                      | Bool              |
| writejson           | Structure, Volume / VolumeItem / String                                                                           | VolumeFile        |
| readjson            | Volume / VolumeItem / String                                                                                      | Structure         |
| exists              | Volume / VolumeItem / String                                                                                      | Bool              |
| open                | Volume / VolumeItem / String                                                                                      | Bool / VolumeItem |
| create              | Volume / VolumeItem / String                                                                                      | VolumeFile        |
| createdir           | Volume / VolumeItem / String                                                                                      | VolumeDirectory   |
