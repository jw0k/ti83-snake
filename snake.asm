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
    ld BC, initialSnakeEnd-initialSnakeStart
    ldir

    ;fill rest with zeros
    ;ld HL, snake + (initialSnakeEnd-initialSnakeStart)
    ;ld DE, snake + (initialSnakeEnd-initialSnakeStart) + 1
    ;ld BC, 12*8*2-(initialSnakeEnd-initialSnakeStart) - 1
    ;ld (HL), 0
    ;ldir

    ret

initialSnakeStart
    .db $11, $21, $31, $41, $51, $61
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

setHLToHead:
    ld BC, (snakelen)
    ld B, 0
    dec BC
    ld HL, snake
    add HL, BC
    ret

checkSelfCollission:
    call setHLToHead
    ld DE, snake-1
    ld A, (snakelen)
    ld C, A
    dec C

    ; DE = tail-1, HL = head, C = snakelen-1

collissionLoop:
    inc DE
    ld A, (DE)
    cp (HL)
    jr z, collissionYes
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

    ld A, (direction)
    cp 3
    jr z, up
    cp 2
    jr z, left
    cp 1
    jr z, down

right:
    call copyLastCell
    ld A, (newsnakecell)
    add A, $10
    cp $C0
    jr c, rightO
    and $0F
rightO
    ld (newsnakecell), A
    jr moveSnakeEnd

down:
    call copyLastCell
    ld A, (newsnakecell)
    inc A
    bit 3, A
    jr z, downO
    and $F0
downO
    ld (newsnakecell), A
    jr moveSnakeEnd

left:
    call copyLastCell
    ld A, (newsnakecell)
    sub A, $10
    jp p, leftO
    and $BF
leftO
    ld (newsnakecell), A
    jr moveSnakeEnd

up:
    call copyLastCell
    ld A, (newsnakecell)
    and $0F
    ld A, (newsnakecell)
    jr nz, upO
    or $08
upO
    dec A
    ld (newsnakecell), A
    ;jr moveSnakeEnd

moveSnakeEnd:
    ld BC, (snakelen)
    ld B, 0
    dec C

    ld HL, snake
    inc HL
    ld DE, snake
    ldir

    ; DE is pointing at snake's head

    ld HL, newsnakecell
    ldd

    ret


copyLastCell
    call setHLToHead
    ld DE, newsnakecell
    ldd
    ret


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
drawCell: ; input: HL - cell data, C==0 -> blank cell (for erasing the tail); C==$FF -> black cell
    ld A, (HL)
    and $F0
    srl A
    srl A
    srl A
    srl A
    or $20
    out ($10), A
    call _lcd_busy   ; probably can be commented out
    ld A, (HL)
    and $0F
    sla A
    sla A
    sla A
    add A, $80
    out ($10), A
    call _lcd_busy

    ld A, C
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


drawSnake:
    ld A, (snakelen)
    ld B, A ; B - snake length
    ld HL, snake
    ld C, $FF
drawSnakeLoop:
    call drawCell
    inc HL
    dec B
    jr nz, drawSnakeLoop
    ret

eraseTail:
    ld HL, oldtail
    ld C, 0
    call drawCell
    ret


setupScreen:
    ld A, $05 ;set X auto-increment
    out ($10), A
    call _lcd_busy
    ret


delayValue
    .dw $3800
snakelen
    .db $06 ; snake length
direction
    .db $00 ; 0=right, 1=down, 2=left, 3=up
food
    .db $00
newsnakecell
    .db $00
oldtail
    .db $01

