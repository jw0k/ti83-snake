.NOLIST
#define equ .equ
#define EQU .equ
#define end .end

;#include "../ti83asm.inc"
#include "../tokens.inc"
#include "../squish.inc"
.LIST

#define b_call(X) CALL X

;there is 531 bytes of free space starting at STATVARS
;to use it we have to call _DelRes first to invalidate the statistic results
snake equ STATVARS

.org 9327h

    di

    call _runIndicOff
    call _ClrLCDFull
    call _DelRes

    call setupScreen

    call initSnake
    call initFreeSpace
    call randomizeFood

gameLoop:

    call eraseTail
    call drawSnake

    ;delay and read keyboard
    ld DE, (delayValue)
delayLoop
    ld A, $FD ;enter, +, -, x, /, ^, clear
    out (1), A
    nop
    nop
    in A, (1)
    cp $BF ;clear
    jr z, quit

    ld A, $FE ;down, left, right, up
    out (1), A
    nop
    nop
    in A, (1)
    cp $FE ;down
    jr z, downPressed
    cp $F7 ;up
    jr z, upPressed
    cp $FD ;left
    jr z, leftPressed
    cp $FB ;right
    jr z, rightPressed
    jr delayEnd

    ;direction 0=right, 1=down, 2=left, 3=up
downPressed:
    ld A, (direction)
    cp 3
    jr z, delayEnd
    ld A, 1
    ld (direction), A
    jr delayEnd

upPressed:
    ld A, (direction)
    cp 1
    jr z, delayEnd
    ld A, 3
    ld (direction), A
    jr delayEnd

leftPressed:
    ld A, (direction)
    cp 0
    jr z, delayEnd
    ld A, 2
    ld (direction), A
    jr delayEnd

rightPressed:
    ld A, (direction)
    cp 2
    jr z, delayEnd
    ld A, 0
    ld (direction), A
    ;jr delayEnd

delayEnd:
    dec DE
    ld A, D
    or E
    jr nz, delayLoop


    call moveSnake
    call checkSelfCollission
    jr c, quit

    jr gameLoop
    ;ret

quit:
    call _ClrLCDFull
    ei
    ret

initFreeSpace:

    ret
    ;freespace
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
;    .db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01


initSnake:
    ;copy initial snake values
    ld HL, initialSnakeStart
    ld DE, snake
    ld BC, (initialSnakeEnd-initialSnakeStart)
    ldir

    ;fill rest with zeros
    ld HL, snake + (initialSnakeEnd-initialSnakeStart)
    ld DE, snake + (initialSnakeEnd-initialSnakeStart) + 1
    ld BC, 12*8*2-(initialSnakeEnd-initialSnakeStart) - 1
    ld (HL), 0
    ldir

    ret

initialSnakeStart
    .db $21, $88 ; column, row
    .db $22, $88
    .db $23, $88
    .db $24, $88
    .db $25, $88
    .db $26, $88
initialSnakeEnd


randomizeFood:
    ;call _RANDOM     ;181465 T-states (!!!)
    ;ld HL, ninetysix
    ;call _Mov9ToOP2  ;232 T-states
    ;call _FPMult     ;6702 T-states
    ;call _Trunc      ;3026 T-states
    ;call _ConvOP1    ;1175 T-states

    ;now A is a random number between 0 and 95 inclusive

    ret

checkSelfCollission:
    ld BC, (snakelen)
    ld B, 0
    dec C
    sla C
    ld HL, snake
    add HL, BC
    ld DE, snake-1
    ld A, (snakelen)
    ld C, A
    dec C
    ; DE = tail-1, HL = head, C = snakelen-1

collissionLoop:
    inc DE
    ld A, (DE)
    cp (HL)
    inc DE
    jr nz, collissionStep
    inc HL
    ld A, (DE)
    cp (HL)
    jr z, collissionYes
    dec HL

collissionStep:
    dec C
    jr nz, collissionLoop

collissionNo:
    or A ; clear the carry flag
    ret

collissionYes:
    scf
    ret


moveSnake:
    ;save tail
    ld hl, snake
    ld de, oldTail
    ldi
    ldi

    ld A, (direction)
    cp 3
    jr z, up
    cp 2
    jr z, left
    cp 1
    jr z, down

right:
    call copyLastCell
    ld A, (newSnakeCell)
    inc A
    cp $2C
    jr nz, rightO
    ld A, $20
rightO
    ld (newSnakeCell), A
    jr moveSnakeEnd

down:
    call copyLastCell
    ld A, (newSnakeCell+1)
    add A, 8
    cp $C0
    jr nz, downO
    ld A, $80
downO
    ld (newSnakeCell+1), A
    jr moveSnakeEnd

left:
    call copyLastCell
    ld A, (newSnakeCell)
    dec A
    cp $1F
    jr nz, leftO
    ld A, $2B
leftO
    ld (newSnakeCell), A
    jr moveSnakeEnd

up:
    call copyLastCell
    ld A, (newSnakeCell+1)
    sub A, 8
    cp $78
    jr nz, upO
    ld A, $B8
upO
    ld (newSnakeCell+1), A
    ;jr moveSnakeEnd

moveSnakeEnd:
    ld bc, (snakelen)
    ld b, 0
    dec c
    sla c
    ld hl, snake
    inc hl
    inc hl
    ld de, snake
    ldir

    ld hl, snake
    ld de, (snakelen)
    ld d, 0
    sla e
    add hl, de
    dec hl
    ld de, newSnakeCell+1
    ex de, hl
    ldd
    ldd

    ret

copyLastCell
    ld hl, snake
    ld de, (snakelen)
    ld d, 0
    sla e
    add hl, de
    dec hl
    ld de, newSnakeCell+1
    ldd
    ldd
    ret

drawSnake:
    ld A, (snakelen)
    ld B, A ; B - snake length
    ld HL, snake

drawSnakeLoop:
    ;kolumny $20 - $2B (12 kolumn)
    ;wiersze:
    ;0: $80
    ;1: $88
    ;2: $90
    ;3: $98
    ;4: $A0
    ;5: $A8
    ;6: $B0
    ;7: $B8

    ld A, (HL)
    out ($10), A
    call _lcd_busy
    inc HL
    ld A, (HL)
    out ($10), A
    call _lcd_busy
    inc HL
    ld A, $FF
    call drawSquare

    dec B
    jr nz, drawSnakeLoop

    ret

eraseTail:
    ld HL, oldtail

    ld A, (HL)
    out ($10), A
    call _lcd_busy
    inc HL
    ld A, (HL)
    out ($10), A
    call _lcd_busy

    ld A, 0
    call drawSquare
    ret

setupScreen:
    ld A, $05 ;set X auto-increment
    out ($10), A
    call _lcd_busy
    ret

drawSquare:
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    out ($11), A
    call _lcd_busy
    ret

delayValue
    .dw $3800
direction
    .db $00 ; 0=right, 1=down, 2=left, 3=up
food
    .db $00, $00
newSnakeCell
    .db $00, $00
oldtail
    .db $20, $88
snakelen
    .db $06 ; snake length

