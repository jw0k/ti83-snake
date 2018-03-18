.NOLIST
#define equ .equ
#define EQU .equ
#define end .end

;#include "ti83asm.inc"
#include "tokens.inc"
#include "squish.inc"
.LIST

#define b_call(X) CALL X

;there are 531 bytes of free space starting at STATVARS
;to use it we have to call _DelRes first to invalidate the statistic results
snake          equ STATVARS            ; array of snake cells
freecells      equ snake + (12*8)      ; array of free cells. ends with $FF
food           equ freecells + (12*8)  ; food position
newsnakecell   equ food + 1
oldtail        equ newsnakecell + 1

.org 9327h

    di

    call _runIndicOff
    call _ClrLCDFull
    call _DelRes

    call setupScreen

    call initSnake
    call initFreeCells
    call randomizeFood

gameLoop:

    call eraseTail
    call drawSnake
    call drawFood

    ld A, (snakelen)
    sub A, 3
    srl A
    srl A
    sla A
    ld C, A
    ld B, 0
    ld HL, delayValues
    add HL, BC
    ld E, (HL)
    inc HL
    ld D, (HL)

    ld A, (direction)
    ld B, A
delayLoop
    ld A, $FD ;enter, +, -, x, /, ^, clear
    out (1), A
    nop
    nop
    in A, (1)
    cp $BF ;clear
    jr z, quitWithoutMessage

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
    ld B, 1
    jr delayEnd

upPressed:
    ld A, (direction)
    cp 1
    jr z, delayEnd
    ld B, 3
    jr delayEnd

leftPressed:
    ld A, (direction)
    cp 0
    jr z, delayEnd
    ld B, 2
    jr delayEnd

rightPressed:
    ld A, (direction)
    cp 2
    jr z, delayEnd
    ld B, 0
    ;jr delayEnd

delayEnd:
    dec DE
    ld A, D
    or E
    jr nz, delayLoop

    ld A, B
    ld (direction), A

    call moveSnake
    call checkSelfCollission
    jr c, quit

    jp gameLoop
    ;ret

shake:
    ld HL, shakeValues

shakeLoop
    ld A, (HL)
    inc HL
    cp A, $FF
    jr z, shakeEnd
    out ($10), A

    ld B, 5
    ei
_   halt
    djnz -_
    di

    jr shakeLoop

shakeEnd
    ld A, $40
    out ($10), A

    ret

quitWithoutMessage:
    call _clrScrnFull
    ei
    ret

quit:
    call shake

    call _clrScrnFull

    ld A, 0
    ld (CURROW), A
    ld A, 0
    ld (CURCOL), A
    ld HL, byeMessage1
    call _puts
    ld A, 1
    ld (CURROW), A
    ld A, 0
    ld (CURCOL), A
    ld HL, byeMessage2
    call _puts

    ld A, (score)
    cp 10
    jr c, lessThan10
    ld H, 0
    ld L, A
    call _divHLby10
    ld B, A
    ld A, L
    add A, '0'
    call _putc
    ld A, B
lessThan10:
    add A, '0'
    call _putc

    ld A, 2
    ld (CURROW), A
    ld A, 0
    ld (CURCOL), A

    ei
    ret



replaceFreeCell: ;input: A - value of a free cell that needs to be replaced; B - value that this cell will be replaced with
    ld HL, freecells-1

    ld C, A

_   inc HL
    ld A, (HL)
    cp C
    jr nz, -_

    ;now HL points to the found cell

    ld (HL), B

    ret


removeFreeCell: ;input: B - value of a free cell to remove
    ld HL, freecells

_   ld A, (HL)
    inc HL
    cp B
    jr nz, -_

    ;now HL points to one byte after found cell

    ld D, H
    ld E, L
    dec DE
trimLoop:
    ldi
    ld A, (DE)
    cp $FF
    jr nz, trimLoop

    ret


initFreeCells:
    ld HL, freecells
    ld BC, 0

freeCellsLoop:

    ;A = merge(B,C)
    ld A, B
    sla A
    sla A
    sla A
    sla A
    or C

    ld (HL), A
    inc HL
    inc B
    ld A, B
    cp $0C
    jr c, freeCellsLoop
    ld B, 0
    inc C
    ld A, C
    cp $08
    jr nz, freeCellsLoop

    ld (HL), $FF


    ld HL, initialSnakeStart
    ld C, initialSnakeEnd-initialSnakeStart
removeLoop:
    ld B, (HL)
    inc HL
    push HL
    push BC
    call removeFreeCell
    pop BC
    pop HL
    dec C
    jr nz, removeLoop

    ;$00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0
    ;$01,                               $71, $81, $91, $A1, $B1
    ;$02, $12, $22, $32, $42, $52, $62, $72, $82, $92, $A2, $B2
    ;$03, $13, $23, $33, $43, $53, $63, $73, $83, $93, $A3, $B3
    ;...

    ret


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

randomizeFood:
    ;call _RANDOM     ;181465 T-states (!!!)
    ;ld HL, ninetysix
    ;call _Mov9ToOP2  ;232 T-states
    ;call _FPMult     ;6702 T-states
    ;call _Trunc      ;3026 T-states
    ;call _ConvOP1    ;1175 T-states
    ;now A is a random number between 0 and 95 inclusive

    ld A, (snakelen)
    cp 12*8
    jr nz, randomizeFoodContinue
    ld A, $FF
    ld (food), A
    ret

randomizeFoodContinue:
    ld HL, freecells
    ld A, R
    ld B, A
    jr z, randomizeEnd
randomFreeCellLoop:
    inc HL
    ld A, (HL)
    cp $FF
    jr nz, _
    ld HL, freecells
_   dec B
    jr nz, randomFreeCellLoop

randomizeEnd:
    ld DE, food
    ldi

    ret

setHLToHead:
    ld BC, (snakelen)
    ld B, 0
    dec BC
    ld HL, snake
    add HL, BC
    ret


checkFoodCollission:
    ld A, (food)
    ld B, A
    ld A, (newsnakecell)
    cp B
    jr z, foodCollissionYes

foodCollissionNo:
    or A
    ret

foodCollissionYes:
    scf
    ret


checkSelfCollission:
    call setHLToHead
    ld DE, snake-1
    ld A, (snakelen)
    ld C, A
    dec C

    ; DE = tail-1, HL = head, C = snakelen-1

selfCollissionLoop:
    inc DE
    ld A, (DE)
    cp (HL)
    jr z, selfCollissionYes
    dec C
    jr nz, selfCollissionLoop

selfCollissionNo:
    or A ; clear the carry flag
    ret

selfCollissionYes:
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
    jr c, _
    and $0F
_   ld (newsnakecell), A
    jr moveSnakeEnd

down:
    call copyLastCell
    ld A, (newsnakecell)
    inc A
    bit 3, A
    jr z, _
    and $F0
_   ld (newsnakecell), A
    jr moveSnakeEnd

left:
    call copyLastCell
    ld A, (newsnakecell)
    sub A, $10
    jp p, _
    and $BF
_   ld (newsnakecell), A
    jr moveSnakeEnd

up:
    call copyLastCell
    ld A, (newsnakecell)
    and $0F
    ld A, (newsnakecell)
    jr nz, _
    or $08
_   dec A
    ld (newsnakecell), A
    ;jr moveSnakeEnd

moveSnakeEnd:
    call checkFoodCollission
    jr nc, noFoodCollission

    ld IX, snakelen
    inc (IX)

    call setHLToHead
    ld A, (newsnakecell)
    ld (HL), A

    ld B, A
    call removeFreeCell

    call randomizeFood

    ld IX, score
    inc (IX)
    ret

noFoodCollission:
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

    ;update freecells array - oldtail becomes a new free cell, newsnakecell must be removed from freecells
    ;so let's just find newsnakecell in freecells and override it with oldtail
    ld A, (oldtail)
    ld B, A
    ld A, (newsnakecell)
    call replaceFreeCell

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
setDrawPosition: ; input: DE - cell data
    ld A, (DE)
    and $F0
    srl A
    srl A
    srl A
    srl A
    or $20
    out ($10), A
    call _lcd_busy   ; probably can be commented out
    ld A, (DE)
    and $0F
    sla A
    sla A
    sla A
    add A, $80
    out ($10), A
    call _lcd_busy
    ret

drawBlock: ;input: DE - points to cell position info, HL - points to cell data
    call setDrawPosition

    ld C, $11
    ld B, 8

_   outi
    call _lcd_busy
    jr nz, -_

    ret

drawFullCell: ; input: DE - points to cell position info
    ld HL, fullCellData
    call drawBlock
    ret
fullCellData:
    .db $7E, $FF, $FF, $FF, $FF, $FF, $FF, $7E

drawEmptyCell: ; input: DE - points to cell position info
    ld HL, emptyCellData
    call drawBlock
    ret
emptyCellData:
    .db $00, $00, $00, $00, $00, $00, $00, $00

drawFood:
    ld A, (food)
    cp $FF
    jr z, _
    ld DE, food
    ld HL, foodData
    call drawBlock
_   ret
foodData:
    .db $18, $7E, $FF, $FF, $FF, $FF, $7E, $18


drawHead: ; input: DE - points to cell position info
    ld A, (direction)
    cp 3
    jr z, drawUp
    cp 2
    jr z, drawLeft
    cp 1
    jr z, drawDown

drawRight:
    ld HL, rightHeadData
    call drawBlock
    ret
drawDown:
    ld HL, downHeadData
    call drawBlock
    ret
drawLeft:
    ld HL, leftHeadData
    call drawBlock
    ret
drawUp:
    ld HL, upHeadData
    call drawBlock
    ret

rightHeadData:
    .db $78, $FE, $F6, $FF, $FF, $F6, $FE, $78
downHeadData:
    .db $7E, $FF, $FF, $FF, $DB, $7E, $7E, $18
leftHeadData:
    .db $1E, $7F, $6F, $FF, $FF, $6F, $7F, $1E
upHeadData:
    .db $18, $7E, $7E, $DB, $FF, $FF, $FF, $7E



;B   A
;B5, 05 - 40 (left tail)
;05, 15 - 10 (left tail)

;05, B5 - B0 (right tail)
;15, 05 - F0 (right tail)

;57, 50 - F9 (up tail)               1111 1001
;50, 51 - 01 (up tail)               0000 0001

;50, 57 - 07 (down tail)             0000 0111
;51, 50 - FF (down tail)             1111 1111

drawTail: ; input: DE - points to cell position info
    ld A, (DE)
    ld B, A
    inc DE
    ld A, (DE)
    xor B
    and $0F
    jr z, leftOrRight
upOrDown:
    ld A, (DE)
    dec DE
    sub B
    and $02
    jr z, drawTailUp
    jr drawTailDown
leftOrRight:
    ld A, (DE)
    dec DE
    sub B
    cp $80
    jr c, drawTailLeft
    jr drawTailRight

drawTailRight:
    ld HL, rightTailData
    call drawBlock
    ret
drawTailDown:
    ld HL, downTailData
    call drawBlock
    ret
drawTailLeft:
    ld HL, leftTailData
    call drawBlock
    ret
drawTailUp:
    ld HL, upTailData
    call drawBlock
    ret

rightTailData:
    .db $00, $FC, $FF, $FE, $FE, $F8, $E0, $00
downTailData:
    .db $7E, $7E, $7E, $3E, $3E, $1E, $1C, $04
leftTailData:
    .db $00, $07, $1F, $7F, $7F, $FF, $3F, $00
upTailData:
    .db $20, $38, $78, $7C, $7C, $7E, $7E, $7E


drawSnake:
    ld DE, snake
    call drawTail

    ld A, (snakelen)
    dec A
    dec A
    ld DE, snake
    inc DE
_   push AF
    call drawFullCell
    pop AF
    inc DE
    dec A
    jr nz, -_

    call drawHead

    ret


eraseTail:
    ld DE, oldtail
    call drawEmptyCell
    ret


setupScreen:
    ld A, $05 ;set X auto-increment
    out ($10), A
    call _lcd_busy
    ret


INITALSNAKELEN equ 3
;INITALSNAKELEN equ 80

delayValues ; for snake lengths 3, 7, 11, ..., 95
    .dw $3803, $318A, $2BE8, $2700, $22BA, $1F03, $1BC6, $18F5, $1681, $145E, $1282, $10E4
    .dw $0F7B, $0E41, $0D30, $0C42, $0B73, $0ABF, $0A22, $0999, $0922, $08BB, $0860, $0812
    ;.dw $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803
    ;.dw $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803, $3803
snakelen
    .db INITALSNAKELEN ; snake length
direction
    .db $00 ; 0=right, 1=down, 2=left, 3=up
initialSnakeStart
    .db $11, $21, $31
    ;.db $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0
    ;.db $B1, $A1, $91, $81, $71, $61, $51, $41, $31, $21, $11, $01
    ;.db $02, $12, $22, $32, $42, $52, $62, $72, $82, $92, $A2, $B2
    ;.db $B3, $A3, $93, $83, $73, $63, $53, $43, $33, $23, $13, $03
    ;.db $04, $14, $24, $34, $44, $54, $64, $74, $84, $94, $A4, $B4
    ;.db $B5, $A5, $95, $85, $75, $65, $55, $45, $35, $25, $15, $05
    ;.db $06, $16, $26, $36, $46, $56, $66, $76
initialSnakeEnd
shakeValues
    .db $40, $46, $40, $46, $40, $46, $40, $45, $40, $45, $40, $42, $40, $42, $40, $41
    .db $40, $40, $41, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $FF
score
    .db $00
byeMessage1
    .db "Zjedzonych", 0
byeMessage2
    .db "jablek: ", 0
