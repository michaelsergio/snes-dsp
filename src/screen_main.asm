
.code

TILE_BASE_ADDRESS = $0400 ; ($0400 + $0 + $0 * $20) ; screen position 0 
; This maths out to $8000
; BG2 is loaded to base address $4000

; Assumes Tile Data is loaded at $500 or $50 in VRAM
; Loads 4 tiles per row
screen_basic_tile_load_tilemap:
    lda #V_INC_1
    sta VMAIN        ; Single Inc
 
    ldx #TILE_BASE_ADDRESS
    stx VMADDL

	; $10 for color palette 4
	COLOR_PALETTE = $10

    ldy #(COLOR_PALETTE << 8) + $51 ; number 50 for basic tile set
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
    iny
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
