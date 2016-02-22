;****************************************************************************************************************
; Paddle - Mike Daley
; 15/02/2016
;
; Experiment into drawing sprites, performing stack based copies and any other effects I can create to eventually
; be used in my Paddle game.
;
; Notes
; * Using IM1 so IY can't be used because of a ROM bug that can cause IY to be changed.
; * Need to switch to IM 2 to have more control over frame rate
;****************************************************************************************************************

                org     32768                       ; RAM from here up is not contended by the ULA

;****************************************************************************************************************
; Init
;****************************************************************************************************************
Init 
                ld      a, 0                        ; Set the background colour
                out     (254), a                    
                        
                ld      a, 7                        ; Set the ink colour
                ld      (23693), a  

                ; Create the y-axis screen memory lookup table
                ld      hl, SCRN_ADDR_LOOKUP
                ld      de, SCRN_BUFFER
                ld      b, 192
YLookupLoop     ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl
                push    hl
                ld      hl, 32
                add     hl, de
                ld      d, h
                ld      e, l
                pop     hl
                djnz    YLookupLoop

AttrLoop
                ; Move the attribute data into the attribute buffer
                ld      de, ATTR_BUFFER             
                ld      hl, AttributeData
                ld      bc, 32 * 27                 ; 32 bytes per row for 27 rows (24 + 3 extra rows)
                ldir

                call    0xDAF                       ; ROM Clear screen    

                ld      de, ScoreLabelText
                ld      bc, 10
                call    0x203C                      ; ROM Print Screen

                ld      de, ScoreText
                ld      bc, 12
                call    0x203C                      ; ROM Print Screen

                ld      de, LivesLabelText
                ld      bc, 10
                call    0x203C                      ; ROM Print Screen

                call    DrawBorders

;****************************************************************************************************************
; Main loop
;
; NOTE: Need to try using IM 2 rather than IM 1
;****************************************************************************************************************
MainLoop 
                ; Update the attribute buffer offset to achive scrolling attributs
                ld      hl, (AttrBfrOffset)
                ld      de, 32
                add     hl, de
                ld      (AttrBfrOffset), hl
                ld      a, (AttrCount)
                inc     a
                ld      (AttrCount), a
                cp      4
                jp      c, KeepLooping
                ld      a, 0
                ld      (AttrCount), a
                ld      hl, 0
                ld      (AttrBfrOffset), hl
KeepLooping
                
                ld      ix, ObjectBall
REPT 19         ; Repeat this code to move all the balls rather than looping
                call    MoveBalls
                ld      de, 4
                add     ix, de
ENDM




                call    DrawBalls                   ; Draw all the ball sprites

                halt                                ; Wait for the scanline to reach the end of the screen
                call    FastCopy                    ; Copy the contents of the screen buffer to the screen file

                call    DrawBalls                   ; Draw the ball sprites again to remove them from the screen (XOR)

                jp      MainLoop

;****************************************************************************************************************
; Draw game scene borders
;****************************************************************************************************************
DrawBalls

REPT 19, ball
                ld      hl, ObjectBall + (ball * 4) ; Use HL rather than IX which uses more T-states
                ld      b, (hl)                     ; X-Pos       
                inc     hl
                ld      c, (hl)                     ; Y-Pos
                ld      de, SpriteBallData
                call    Draw_8x8_Sprite
ENDM
                ret

;****************************************************************************************************************
; Draw game scene borders
;****************************************************************************************************************
DrawBorders
                ; Draw top wall
                ld      h, 0
                ld      b, 8
                ld      c, 2
HorizLoop1      
                push    hl
                push    bc
                ld      de, HorizBlockData
                call    Draw_8x8_Sprite
                pop     bc
                pop     hl
                ld      a, b
                add     a, 8
                ld      b, a
                inc     h
                ld      a, h
                cp      30
                jp      nz, HorizLoop1

                ; Draw right hand wall
                ld      h, 0
                ld      b, SCRN_RIGHT
                ld      c, 9
VertLoop1
                push    hl
                push    bc
                ld      de, VertLBlockData
                call    Draw_8x8_Sprite
                pop     bc
                pop     hl
                ld      a,c
                add     a, 8
                ld      c, a
                inc     h
                ld      a, h
                cp      23
                jp      nz, VertLoop1

                ; Draw Left hand wall
                ld      h, 0
                ld      b, 0
                ld      c, 9
VertLoop2
                push    hl
                push    bc
                ld      de, VertRBlockData
                call    Draw_8x8_Sprite
                pop     bc
                pop     hl
                ld      a,c
                add     a, 8
                ld      c, a
                inc     h
                ld      a, h
                cp      23
                jp      nz, VertLoop2

                ret

;****************************************************************************************************************
; Draw Ball
; Draws the ball sprite at the location held in the ObjectBall structure
;****************************************************************************************************************
DrawBall 
                ld      de, SpriteBallData          ; Point DE to the ball sprite data
                ld      a, (ObjectBall + BALL_X_POS); Load BC with the ball sprite objects location
                ld      b, a
                ld      a, (ObjectBall + BALL_Y_POS)
                ld      c, a
                call    DrawSprite
                ret

;****************************************************************************************************************
; Draw Ball Sprite 
; B = X, C = Y
; Uses: DE, HL, BC
;****************************************************************************************************************
Draw_8x8_Sprite
                ld      a, b                        
                and     7                           ; Get the Bit rotate count (lower 3 bits of X position)
        
                ; Load DE with the address of the sprite we need to use based on the x location offset in memory as
                ; we are using pre-shifted sprites
                ld      l, a                        ; Load A with the number of shifts needed
                ld      h, 0                        ; Reset the HL high byte
                add     hl, hl                      ; Double HL as the lookup table entries are words
                add     hl, de                      ; Add base address of sprite table which is held in DE
                ld      e, (hl)                     ; Load E with the contents of (HL)
                inc     hl                          ; Move HL to the next byte of address in the table
                ld      d, (hl)                     ; Load D with the high byte
        
                ; Work out the X offset of the screen memory address based on the X pixel position
                ld      a, b                        ; Work out the X Offset using the shift value
                rra
                rra
                rra
                and     31
                ld      b, a                        ; Store the X Byte Offset
                push    bc                          ; Need to use BC so save the value in B on the stack

                ; Load IX with the first address of the y-axis lookup table
                ld      b, 0                        ; Clear B
                ld      ix, SCRN_ADDR_LOOKUP        ; Load IY with the lookup table address
                add     ix, bc                      ; Increment IX by the Y pixel position
                add     ix, bc                      ; twice as the table contains word values
                pop     bc                          ; Restore B which holds the X Byte offset

REPT 8                                              ; Repeat this code 8 times for the 8 pixles rows of a ball sprite
                ld      a, (ix + 0)                 ; Get the current line
                or      b                           ; Merge in our X Offset
                ld      l, a                        ; Load the merged low byte in L
                ld      h, (ix + 1)                 ; Get the high byte from the lookup table
                inc     ix  
                inc     ix                          ; Move to the next line which is a word away
    
                ld      a, (de)                     ; Grab the first byte of sprite data into A             
                inc     de                          ; Move to the next byte of sprite data
                xor     (hl)                        ; Merge the screen contents with the sprite data
                ld      (hl), a                     ; Load the merged data back into the screen
                inc     l                           ; Move to the next byte of screen memory

                ld      a, (de)                     ; Grab the second byte of sprite data into A             
                inc     de                          ; Move to the next row of sprite data
                xor     (hl)                        ; Merge the screen contents with the sprite data
                ld      (hl), a                     ; Load the merged data back into the screen
                inc     l                           ; Move to the next byte of screen memory
ENDM                
                ret                                 ; All done!                

;****************************************************************************************************************
; Responsible for bouncing the ball sprite around the screen, detecting when it hits the edges of the screen
; and any tile objects
;****************************************************************************************************************
MoveBalls
                ld      a, (ix + BALL_X_POS) 
                add     a, (ix + BALL_XSPEED)
                cp      SCRN_RIGHT - BALL_PIXEL_WIDTH
                jp      nc, BounceX
                cp      SCRN_LEFT
                jp      c, BounceX
                ld      (ix + BALL_X_POS), a 
MoveY           
                ld      a, (ix + BALL_Y_POS) 
                add     a, (ix + BALL_YSPEED)
                cp      SCRN_BOTTOM - BALL_PIXEL_HEIGHT
                jp      nc, BounceY
                cp      SCRN_TOP
                jp      c, BounceY
                ld      (ix + BALL_Y_POS), a 
                ld      a, (ix + BALL_YSPEED)
                ret             

BounceX         
                ld      a, (ix + BALL_XSPEED)
                neg
                ld      (ix + BALL_XSPEED), a
                jp      MoveY

BounceY         
                ld      a, (ix + BALL_YSPEED)
                neg
                ld      (ix + BALL_YSPEED), a

                ret

;****************************************************************************************************************
; Copy the screen buffer to the screen file using a Stack based approach.
; Original code written by Andrew Owen in Bob.asm that was downloaded from the Z80 Assembly
; Programming for the ZX Spectrum Facebook group.
;
; Use the SCRN_EDGE_SIZE constant to define the number of bytes on the left and right NOT to move. 
; Handy when having sprites move in smoothly from the edge of the screen and to reduce the amount of 
; data being copied
;****************************************************************************************************************
FastCopy
                di                                  ; We don't want other interrupts while using the stack
                ld      (ScrnStackPtr), sp          ; Save the current SP 

                ; First off we copy over the attribute data
REPT 46, attr               
                ld      ix, ATTR_BUFFER + 768 - 16 - (attr * 16)
                                        
                ld      de, (AttrBfrOffset)         ; Grab the offset to use when reading the attributes
                add     ix, de                      ; and add it to the base buffer location for a scroll effect

                ld      iy, 16384 + 6912 - (attr * 16)  ; IY points to the last byte of screen memory

                ld      hl, $ + 9                   ; set return location (no stack available)
                ld      (EndCall), hl           
                jp      Blit                        ; call blit
ENDM

REPT 8, row
REPT 8, cell
REPT 2, line
                ; Bottom third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 32) - (row * 256)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 256) - (row * 32)
                ld      hl, $ + 9                   ; Self modifying code to update the JP location inside the Blit routine
                ld      (EndCall), hl           
                jp      Blit
ENDM
ENDM
ENDM

REPT 8, row
REPT 8, cell
REPT 2, line
                ; Middle third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 32) - (row * 256) - (1 * 2048)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 256) - (row * 32) - (1 * 2048)
                ld      hl, $ + 9                   ; Self modifying code to update the JP location inside the Blit routine
                ld      (EndCall), hl           
                jp      Blit
ENDM
ENDM
ENDM

REPT 7, row     ; Only copy 8 character rows as the top row is for the score
REPT 8, cell
REPT 2, line
                ; Top third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 32) - (row * 256) - (2 * 2048)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (line * (16 - SCRN_EDGE_SIZE)) - (cell * 256) - (row * 32) - (2 * 2048)
                ld      hl, $ + 9                   ; Self modifying code to update the JP location inside the Blit routine
                ld      (EndCall), hl           
                jp      Blit
ENDM
ENDM
ENDM
                ld      sp, (ScrnStackPtr)          ; Resotre stack pointer           
                ei

                ret

;****************************************************************************************************************
; Routine to copy 16 bytes from the location in IX to the location in IY - 16
;****************************************************************************************************************
Blit 
                ld      sp, ix                  ; Update the Stack Pointer to the read location
                pop     af                      ; Read two bytes into the main registers
                pop     bc
                pop     de
                pop     hl
                ex      af, af' 
                exx                             ; Switch to the alternate registers
                pop     af
                pop     bc
                pop     de
                pop     hl                      ; Now stored 16 bytes of data
                
                ld      sp, iy                  ; Swith the Stack Pointer to the screen file
                push    hl                      ; Write the bytes on the stack into the screen file
                push    de
                push    bc
                push    af
                ex      af, af'
                exx
                push    hl
                push    de
                push    bc
                push    af
                db      0xc3                    ; 0xC3 is JP and...
EndCall         dw      EndCall                 ; ... EndCall is modified to point to the return location dynamically 

;****************************************************************************************************************
; Variables
;****************************************************************************************************************
BallMT          dw      0                           ; Stores the x, y attr position of the balls collision points
BallMR          dw      0
BallMB          dw      0
BallML          dw      0

ScrnStackPtr    dw      0

AttrBfrOffset   dw      0
AttrCount       db      0

;****************************************************************************************************************
; Text
;****************************************************************************************************************
ScoreLabelText  db      16, 6, 22, 0, 4, 'SCORE'
ScoreText       db      16, 6, 22, 0, 10, '0000000'
LivesLabelText  db      16, 6, 22, 0, 21, 'LIVES'
LivesText       db      16, 6, 22, 0, 27, '5'
GameOverText    db      16, 2, 17, 2, 22, 15, 11, 'GAME  OVER'

;****************************************************************************************************************
; Object data
;****************************************************************************************************************
ObjectBall      db      10                          ; IX + 0 = X position
                db      130                         ; IX + 1 = Y position
                db      1                           ; IX + 4 = XSpeed
                db      -1                           ; IX + 5 = YSpeed

ObjectBall1     db      20                          ; IX + 0 = X position
                db      130                         ; IX + 1 = Y position
                db      5                           ; IX + 4 = XSpeed
                db      4                           ; IX + 5 = YSpeed

ObjectBall2     db      30                          ; IX + 0 = X position
                db      170                         ; IX + 1 = Y position
                db      3                           ; IX + 4 = XSpeed
                db      4                           ; IX + 5 = YSpeed

ObjectBall3     db      40                          ; IX + 0 = X position
                db      84                          ; IX + 1 = Y position
                db      2                           ; IX + 4 = XSpeed
                db      6                           ; IX + 5 = YSpeed

ObjectBall4     db      50                          ; IX + 0 = X position
                db      100                         ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall5     db      50                          ; IX + 0 = X position
                db      60                          ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall6     db      70                          ; IX + 0 = X position
                db      75                          ; IX + 1 = Y position
                db      2                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall7     db      80                          ; IX + 0 = X position
                db      175                         ; IX + 1 = Y position
                db      2                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall8     db      90                          ; IX + 0 = X position
                db      75                          ; IX + 1 = Y position
                db      3                           ; IX + 4 = XSpeed
                db      3                           ; IX + 5 = YSpeed

ObjectBall9     db      100                         ; IX + 0 = X position
                db      90                          ; IX + 1 = Y position
                db      1                           ; IX + 4 = XSpeed
                db      3                           ; IX + 5 = YSpeed

ObjectBall10    db      110                         ; IX + 0 = X position
                db      110                         ; IX + 1 = Y position
                db      3                           ; IX + 4 = XSpeed
                db      2                           ; IX + 5 = YSpeed

ObjectBall11    db      120                         ; IX + 0 = X position
                db      184                         ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      5                           ; IX + 5 = YSpeed

ObjectBall12    db      130                         ; IX + 0 = X position
                db      180                         ; IX + 1 = Y position
                db      1                           ; IX + 4 = XSpeed
                db      2                           ; IX + 5 = YSpeed

ObjectBall13    db      140                         ; IX + 0 = X position
                db      78                          ; IX + 1 = Y position
                db      3                           ; IX + 4 = XSpeed
                db      5                           ; IX + 5 = YSpeed

ObjectBall14    db      150                         ; IX + 0 = X position
                db      107                         ; IX + 1 = Y position
                db      3                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall15    db      160                         ; IX + 0 = X position
                db      169                         ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      4                           ; IX + 5 = YSpeed

ObjectBall16    db      199                         ; IX + 0 = X position
                db      50                          ; IX + 1 = Y position
                db      1                           ; IX + 4 = XSpeed
                db      1                           ; IX + 5 = YSpeed

ObjectBall17    db      220                         ; IX + 0 = X position
                db      169                         ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      4                           ; IX + 5 = YSpeed

ObjectBall18    db      120                         ; IX + 0 = X position
                db      169                         ; IX + 1 = Y position
                db      4                           ; IX + 4 = XSpeed
                db      2                           ; IX + 5 = YSpeed

ObjectBall19    db      230                         ; IX + 0 = X position
                db      169                         ; IX + 1 = Y position
                db      2                           ; IX + 4 = XSpeed
                db      4                           ; IX + 5 = YSpeed

ObjectBat       db      76                          ; IX + 0 = X Position                       
                db      175                         ; IX + 1 = Y Position
                db      1                           ; IX + 2 = Speed
                db      1                           ; IX + 3 = unused   

;****************************************************************************************************************
; Includes
;****************************************************************************************************************
include     Constants.asm

                END Init
