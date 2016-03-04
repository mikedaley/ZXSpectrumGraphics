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
_YLookupLoop    ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl
                push    hl
                ld      hl, 32
                add     hl, de
                ld      d, h
                ld      e, l
                pop     hl
                djnz    _YLookupLoop

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

.sprites        equ     10
.frames         equ     0
.debug          equ     0    
.moveAttr       equ     0

IF .moveAttr
                ; Update the attribute buffer offset to achive scrolling attributs
                ld      hl, (AttrBfrOffset)
                ld      de, 32                      ; We need to move the buffer offset one row down to make...
                add     hl, de                      ; ...the attributes look like they are moving
                ld      (AttrBfrOffset), hl         ; Save the new buffer offset
                ld      a, (AttrCount)              ; Get the counter we are using to track how many rows we have moved...
                inc     a                           ; ...and increment it 
                ld      (AttrCount), a              ; Now save it
                cp      4                           ; Has the counter reached 4?
                jp      c, _KeepLooping              ; If not then keep going...
                ld      a, 0                        ; ...otherwise reset the counter...
                ld      (AttrCount), a              ; ...and save it
                ld      hl, 0                       ; Reset the buffer offset...
                ld      (AttrBfrOffset), hl         ; ...and save it
_KeepLooping
ENDIF

IF .debug
                ld      a, 1
                out     (254), a
ENDIF
                
                ld      ix, SpriteTable             ; Point to the first sprite to be moved
REPT .sprites                                       ; Repeat this code to move all the sprites rather than looping
                call    MoveSprites                 ; Move the current sprite
                ld      de, 4                       ; It's 4 bytes to the next sprite table entry...
                add     ix, de                      ; ...so add that to IX
ENDM

IF .debug
                ld      a, 2
                out     (254), a
ENDIF
                call    DrawSprites                 ; Draw all the sprites

IF .debug
                ld      a, 0
                out     (254), a
ENDIF

IF .frames
                ld      a, (0x5c78)
                out     (0xfe), a
ENDIF
                halt                                ; Wait for the scanline to jump back to the top of the screen
IF .frames
                ld      hl, 0
                ld      (0x5c78), hl
ENDIF
                call    CopyScrnBuffer              ; Copy the contents of the screen buffer to the screen file
IF .debug
                ld      a, 4
                out     (254), a
ENDIF
                call    DrawSprites                 ; Draw the sprites again to remove them from the screen (XOR)

                jp      MainLoop

;****************************************************************************************************************
; Draw game scene borders
;****************************************************************************************************************
DrawSprites

REPT .sprites, .ball
                ld      hl, SpriteTable + (.ball * 4) ; Use HL rather than IX which uses more T-states
                ld      b, (hl)                     ; X-Pos       
                inc     hl
                ld      c, (hl)                     ; Y-Pos
                ld      de, KnightData
                call    Draw_16x18_Sprite
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
_HorizLoop1      
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
                jp      nz, _HorizLoop1

                ; Draw right hand wall
                ld      h, 0
                ld      b, SCRN_RIGHT
                ld      c, 9
_VertLoop1
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
                jp      nz, _VertLoop1

                ; Draw Left hand wall
                ld      h, 0
                ld      b, 0
                ld      c, 9
_VertLoop2
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
                jp      nz, _VertLoop2

                ret

;****************************************************************************************************************
; Draw 8x8 Sprite
; B = X, C = Y
; Uses: DE, HL, BC
;****************************************************************************************************************
Draw_8x8_Sprite
                ld      a, b                        ; Load A with the X pixel position
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
                and     %00011111                   ; 31
                ld      b, a                        ; Store the X pixel byte offset into the screen buffer
                push    bc                          ; Save B as we will be using it to merge the X offset into the 
                                                    ; buffer address

                ; Load IX with the first address of the y-axis lookup table
                ld      b, 0                        ; Clear B
                ld      ix, SCRN_ADDR_LOOKUP        ; Load IY with the lookup table address
                add     ix, bc                      ; Increment IX by the Y pixel position
                add     ix, bc                      ; twice as the table contains word values
                pop     bc                          ; Restore B which holds the X byte offset

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
; Draw 24x8 Sprite
; B = X, C = Y
; Uses: DE, HL, BC
;****************************************************************************************************************
Draw_24x8_Sprite
                ld      a, b                        ; Load A with the X pixel position
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
                and     %00011111                   ; 31
                ld      b, a                        ; Store the X pixel byte offset into the screen buffer
                push    bc                          ; Save B as we will be using it to merge the X offset into the 
                                                    ; buffer address

                ; Load IX with the first address of the y-axis lookup table
                ld      b, 0                        ; Clear B
                ld      ix, SCRN_ADDR_LOOKUP        ; Load IY with the lookup table address
                add     ix, bc                      ; Increment IX by the Y pixel position
                add     ix, bc                      ; twice as the table contains word values
                pop     bc                          ; Restore B which holds the X byte offset

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

                ld      a, (de)                     ; Grab the third byte of sprite data into A             
                inc     de                          ; Move to the next row of sprite data
                xor     (hl)                        ; Merge the screen contents with the sprite data
                ld      (hl), a                     ; Load the merged data back into the screen
                inc     l                           ; Move to the next byte of screen memory

                ld      a, (de)                     ; Grab the fourth byte of sprite data into A             
                inc     de                          ; Move to the next row of sprite data
                xor     (hl)                        ; Merge the screen contents with the sprite data
                ld      (hl), a                     ; Load the merged data back into the screen
                inc     l                           ; Move to the next byte of screen memory
ENDM                
                ret                                 ; All done!   

;****************************************************************************************************************
; Draw 24x8 Sprite
; B = X, C = Y
; Uses: DE, HL, BC
;****************************************************************************************************************
Draw_16x18_Sprite
                ld      a, b                        ; Load A with the X pixel position
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
                and     %00011111                   ; 31
                ld      b, a                        ; Store the X pixel byte offset into the screen buffer
                push    bc                          ; Save B as we will be using it to merge the X offset into the 
                                                    ; buffer address

                ; Load IX with the first address of the y-axis lookup table
                ld      b, 0                        ; Clear B
                ld      ix, SCRN_ADDR_LOOKUP        ; Load IY with the lookup table address
                add     ix, bc                      ; Increment IX by the Y pixel position
                add     ix, bc                      ; twice as the table contains word values
                pop     bc                          ; Restore B which holds the X byte offset

REPT 18                                              ; Repeat this code 8 times for the 8 pixles rows of a ball sprite
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

                ld      a, (de)                     ; Grab the third byte of sprite data into A             
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
MoveSprites
                ld      a, (ix + BALL_X_POS) 
                add     a, (ix + BALL_XSPEED)
                cp      SCRN_RIGHT - BAT_PIXEL_WIDTH
                jp      nc, _BounceX
                cp      SCRN_LEFT
                jp      c, _BounceX
                ld      (ix + BALL_X_POS), a 
_MoveY           
                ld      a, (ix + BALL_Y_POS) 
                add     a, (ix + BALL_YSPEED)
                cp      SCRN_BOTTOM - BAT_PIXEL_HEIGHT
                jp      nc, _BounceY
                cp      SCRN_TOP
                jp      c, _BounceY
                ld      (ix + BALL_Y_POS), a 
                ld      a, (ix + BALL_YSPEED)
                ret             

_BounceX         
                ld      a, (ix + BALL_XSPEED)
                neg
                ld      (ix + BALL_XSPEED), a
                jp      _MoveY

_BounceY         
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
CopyScrnBuffer
                di                                  ; We don't want other interrupts while using the stack
                ld      (ScrnStackPtr), sp          ; Save the current SP 

                ; First off we copy over the attribute data
REPT 46, .attr               
                ld      ix, ATTR_BUFFER + 768 - 16 - (.attr * 16)
                                        
                ld      de, (AttrBfrOffset)         ; Grab the offset to use when reading the attributes
                add     ix, de                      ; and add it to the base buffer location for a scroll effect

                ld      iy, 16384 + 6912 - (.attr * 16)  ; IY points to the last byte of screen memory

                ld      hl, $ + 9                   ; set return location (no stack available)
                ld      (EndCall), hl           
                jp      Blit                        ; call blit
ENDM

REPT 8, .row
REPT 8, .cell
REPT 2, .line
                ; Bottom third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 32) - (.row * 256)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 256) - (.row * 32)
                ld      hl, $ + 9                   ; Self modifying code to update the JP location inside the Blit routine
                ld      (EndCall), hl           
                jp      Blit
ENDM
ENDM
ENDM

REPT 8, .row
REPT 8, .cell
REPT 2, .line
                ; Middle third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 32) - (.row * 256) - (1 * 2048)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 256) - (.row * 32) - (1 * 2048)
                ld      hl, $ + 9                   ; Self modifying code to update the JP location inside the Blit routine
                ld      (EndCall), hl           
                jp      Blit
ENDM
ENDM
ENDM

REPT 7, .row     ; Only copy 7 character rows as the top row is for the score
REPT 8, .cell
REPT 2, .line
                ; Top third
                ld      ix, SCRN_BUFFER + BM_SCR_SIZE - 16 - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 32) - (.row * 256) - (2 * 2048)
                ld      iy, BM_SCR_ADDR + BM_SCR_SIZE - SCRN_EDGE_SIZE - (.line * (16 - SCRN_EDGE_SIZE)) - (.cell * 256) - (.row * 32) - (2 * 2048)
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
; Routine to copy 16 bytes from the location in IX to the location in (IY - 16)
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
SpriteTable
                db       10, 130,  2, -3  
                db       20, 130,  5,  4
                db       30, 170,  3,  4   
                db       40,  84,  2,  6   
                db       50, 100,  4,  1   
                db       90,  60,  4,  1
                db      123,  34,  2,  1   
                db      200, 124,  3,  1   
                db       13,  12,  4,  1   
                db       83, 120, -5,  2   
                db       23, 163,  4, -3   
                db      225, 143,  3,  1   
                db      212,  89, -2,  5   
                db       54,  74,  1, -3   
                db       36,  99, -2,  2   
                db      170, 150,  3, -3   
                db      154, 134,  4,  1   
                db      189,  46, -5,  3   
                db      100, 113,  6, -1  
                db       12, 100,  1,  1   
                db       12, 100, -2,  4

                db       12, 105, -2,  1
                db       12, 110, -1,  2
                db       12, 115,  3,  3
                db       12, 120,  1,  4
                db       12, 125, -4,  5
                db       12, 130,  3,  6
                db       12,  35, -1,  1
                db       12,  40,  6,  2
                db       12,  45, -4,  3
                db       12,  80,  1,  4


;****************************************************************************************************************
; Includes
;****************************************************************************************************************
include     Constants.asm

                END Init
