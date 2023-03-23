.include "dsp_settings.asm"

.code

; X has the address to write to
.macro _screen_write_hex value
	lda #(value >> 4)
	jsr _screen_write_nibble 
	lda #(value & $0F)
	jsr _screen_write_nibble 
.endmacro

.macro _screen_write_hex_from_address addr
	lda addr
	pha ; push a copy on the stack for later
	lsr
	lsr
	lsr ; get the high byte
	lsr ; aka value >> 4
	jsr _screen_write_nibble 

	pla ; Bring back the copy we put on the stack
	and #$0F ; get just the low nibble
	jsr _screen_write_nibble 
.endmacro

screen_main_vblank_update:
  jsr screen_main_cursor_sprite_update_position
  jsr screen_main_write_volume
  jsr screen_main_write_mute
rts

; TILEMAP_BASE_ADDRESS = $0400 ; ($0400 + $0 + $0 * $20) ; screen position 0 
TILEMAP_BASE_ADDRESS = $0800 >> 1 ; This is the tilebase address. VRAM is in words (/2)
TILEMAP_BASE_ADDRESS_BG_2 = $1800 >> 1 ; We'll set the second address a little below

; TILEMAP_BASE_ADDRESS = $0800 >> 1 ; This is the tilebase address. VRAM is in words (/2)
; TILEMAP_BASE_ADDRESS_BG_2 = $0800 >> 1 ; We'll set the second address a little below

; TILEMAP_BASE_ADDRESS_BG_2 = $1800 >> 1 ; This is the tilebase address. VRAM is in words (/2)
; TILEMAP_BASE_ADDRESS = $0800 >> 1 ; We'll set the second address a little below


; Background 2 is going to have all the background tiles
; The color palette for BG must be between 0x20-0x40
; So BG2 pallete 0 is at 20
screen_basic_tile_load_tilemap:
    lda #V_INC_1
    sta VMAIN        ; Single Inc
 
    ldx #TILEMAP_BASE_ADDRESS
    stx VMADDL

	; $10 for color palette 4
	COLOR_PALETTE = $10		; Use palette 4
	COLOR_PALETTE_BG_2 = $00 ; Use palette 0

    ldy #(COLOR_PALETTE_BG_2 << 8) + $71 ; number 51 for basic tile set
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    iny ; Unusable screen real estate here
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles
    jsr _screen_fill_row_with_tiles

    lda #V_INC_1
    sta VMAIN        ; Single Inc

	; Tile Viewer         BG1   BG2
	; Base Tile Address   0000  4000
	; Tilemap Viewer
	; Base Address		  0800  0000
	; NBA				    20

	; Test character. Gets overwritten
    ldx #TILEMAP_BASE_ADDRESS_BG_2
    stx VMADDL
    ldy #(COLOR_PALETTE_BG_2 << 8) + $41 ; number 41 for character tile set
    sty VMDATAL



        ; Left label
        ; ldx #$1800 >> 1
        ; stx VMADDL
        ; ldx #_screen_str_main1
        ; jsr _screen_write_strz

        ldx #$181C >> 1
        stx VMADDL
        ldx #_screen_str_main1
        jsr _screen_write_strz

        ; Right label
        ldx #$183C >> 1
        stx VMADDL
        ldx #_screen_str_m2
        jsr _screen_write_strz

    POS_VOL_L = $1900 >> 1
    POS_VOL_R = $1980 >> 1
    POS_ECHO_L = $1A00 >> 1
    POS_ECHO_R = $1A80 >> 1
    POS_KEY_ON = $1B00 >> 1
    POS_KEY_OFF = $1B80 >> 1
    POS_CHANNELS = $1BCA >> 1

    POS_ECHO = $1C00 >> 1
    POS_MUTE = $1C80 >> 1
    POS_NOISE = $1D00 >> 1

    ; VOL
    ldx #POS_VOL_L
    stx VMADDL
    ldx #_screen_str_vol_L
    jsr _screen_write_strz
    ldx #POS_VOL_R
    stx VMADDL
    ldx #_screen_str_vol_R
    jsr _screen_write_strz

    ; Echo
    ldx #POS_ECHO_L
    stx VMADDL
    ldx #_screen_str_echo_L
    jsr _screen_write_strz
    ldx #POS_ECHO_R
    stx VMADDL
    ldx #_screen_str_echo_R
    jsr _screen_write_strz

    ; KEY
    ldx #POS_KEY_ON
    stx VMADDL
    ldx #_screen_str_key_on
    jsr _screen_write_strz
    ldx #POS_KEY_OFF
    stx VMADDL
    ldx #_screen_str_key_off
    jsr _screen_write_strz
    ldx #POS_CHANNELS
    stx VMADDL
    ldx #_screen_str_channels
    jsr _screen_write_strz


    ; Draw the boxes
    ldx #POS_KEY_ON + $5
    stx VMADDL
    jsr _screen_write_box8

    ldx #POS_KEY_OFF + $5
    stx VMADDL
    jsr _screen_write_box8


    ; ECHO
    ldx #POS_ECHO
    stx VMADDL
    ldx #_screen_str_echo
    jsr _screen_write_strz
    ldx #POS_MUTE
    stx VMADDL
    ldx #_screen_str_mute
    jsr _screen_write_strz
    ldx #POS_NOISE
    stx VMADDL
    ldx #_screen_str_noise_clk
    jsr _screen_write_strz


    ; Write noise and mute box
    ldx #POS_ECHO + $5
    stx VMADDL
    jsr _screen_write_box1
    ldx #POS_MUTE + $5
    stx VMADDL
    jsr _screen_write_box1


    jsr screen_main_cursor_sprite_draw

    ; Add Sample data here
    jsr screen_main_write_echo
    jsr screen_main_write_noise_clock
rts



screen_main_write_volume:
    ; Sample hex data
    ; Volume
    ldx #POS_VOL_L + $7
    stx VMADDL
    _screen_write_hex_from_address dp_vol_l
    ldx #POS_VOL_R + $7
    stx VMADDL
    _screen_write_hex_from_address dp_vol_r
rts

screen_main_write_echo:
    ; Echo Volume
    ldx #POS_ECHO_L + $7
    stx VMADDL
    _screen_write_hex $EE
    ldx #POS_ECHO_R + $7
    stx VMADDL
    _screen_write_hex $EF
rts

screen_main_write_noise_clock:
    ; Noise Clk
    ldx #POS_NOISE + $A
    stx VMADDL
    _screen_write_hex $FF
rts

screen_main_write_mute:
    ldx #POS_MUTE + $5
    stx VMADDL

    lda dp_mute
    beq @draw_empty
    ; Otherwise draw filled
    jsr _screen_write_box1_filled
    bra @done	

    @draw_empty:
    jsr _screen_write_box1

    @done:
rts 

screen_main_fill_mute:
    ; Fill mute
    ldx #POS_MUTE + $5
    stx VMADDL
    jsr _screen_write_box1_filled
rts

    ; Assume VMADDL is currently set to proper position autoinc-1
    ; Need Y to be the tile data - attr+name
    ; Modifies A
    _screen_fill_row_with_tiles:
    ; Write Y 32 times.
    lda #$4
    @loop_row:
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    sty VMDATAL
    dea
    bne @loop_row
rts


; VMADDL must be set to tilemap first
; Address is loaded in X
; X points to last value at end
; Only writes on BG1
_screen_write_strz:
    lda $00, x ; load char value into a
    beq _screen_write_strz_done

    ; Space check
    cmp #$20
    beq write_space

    cmp #$3F
    bcs write_char ; Branch >= $3F
    ; This value between $00..$3F	is an ascii number
    ; map it to my font range
    clc
    adc #($5E - $30) ; $30 is ascii '0'. $5E is font '0'
    bra write_char

    write_space:
    lda #$40

    write_char:
    sta VMDATAL
    lda #$00
    sta VMDATAH

    inx
    bra _screen_write_strz
    _screen_write_strz_done:
rts

_screen_write_box1:
    ldy #$0077
    sty VMDATAL
rts

_screen_write_box1_filled:
    ldy #$0073
    sty VMDATAL
rts

_screen_write_box8:
    ldx #$08
    @_screen_write_box8_loop:
    beq @_screen_write_box8_done

    ldy #$0077
    sty VMDATAL
    ; lda #$77
    ; sta VMDATAL
    ; lda #$00
    ; sta VMDATAH
    dex
    bra @_screen_write_box8_loop
    @_screen_write_box8_done:
rts


; A is the nibble 0-F
; X is the address
_screen_write_nibble:
    cmp #$0A
    bcs @write_hex; >=A

    @write_num:
    clc
    adc #$5E ; offset from number values
    bra @write_val

    @write_hex:
    clc
    adc #($41 - $0A) ; Offset from 'A' value 41 
    bra @write_val

    @write_val:
    sta VMDATAL
    lda #$00
    sta VMDATAH
    rts


screen_main_cursor_sprite_update_position:
	ldx #$0000
	stx OAMADDL

	lda dsp_cursor_position_x
	sta OAMDATA 
	lda dsp_cursor_position_y
	sta OAMDATA 
rts 

screen_main_cursor_sprite_draw:
	; Lets render a quick and dirty sprite
	; Go to first position in OAM table 1
	ldx #$0000
	stx OAMADDL
	lda #$0B ; position x=11
	sta OAMDATA 
	lda #$0C; position y=12
	sta OAMDATA 
	; Sloppilty reuse a 2bpp text as a 4bpp tile.
	; This is not normally a good idea, but I'm too lazy
	; to make another sprite for my cursor
	lda #$05C0 >> 5
	sta OAMDATA     ; Name: is char at 1
	; priority should be 3 so sprites render in proper place
	lda #$03 << 4
	sta OAMDATA     ; HBFlip/Pri/ColorPalette/9name
    ; Clear the negative positions in Table 2
    stz OAMADDL
    lda #$01     
    sta OAMADDH ; Sprite Table 2 at OAM $0100 - will autoinc after L/H write
    lda #$54     ; except for the first
	sta OAMDATA
    lda #$55     ; set the (h-pos) bit for each spot in the OAM table
	sta OAMDATA
rts


_screen_str_m1: .asciiz "M1"
_screen_str_m2: .asciiz "M2"
_screen_str_main1: .asciiz "MAIN"
_screen_str_main2: .asciiz "MAIN TWO"
_screen_str_voicex:  .asciiz "VOICE X"

_screen_str_vol_L:  .asciiz "VOL L"
_screen_str_vol_R:  .asciiz "VOL R"
_screen_str_echo_L:  .asciiz "ECHO L"
_screen_str_echo_R:  .asciiz "ECHO R"

_screen_str_key_on:  .asciiz "KON"
_screen_str_key_off:  .asciiz "KOFF"

_screen_str_echo:  .asciiz "ECHO"
_screen_str_mute:  .asciiz "MUTE"
_screen_str_noise_clk:  .asciiz "NOISECLK"
_screen_str_flg:  .asciiz "FLG"

_screen_str_channels: .asciiz "76543210"
