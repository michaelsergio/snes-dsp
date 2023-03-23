.define ROM_NAME "DSP Demo"

.include "snes_registers.asm"
.include "lorom128.inc"
.include "startup.inc"
.include "graphics.asm"
.include "joycon.asm"
.include "level_basic_tile.asm"
.include "screen_scroll.asm"
.include "input.asm"
.include "audio.asm"
; .include "dsp_settings.asm"
.include "screen_main.asm"

; Include the SPC audio program load position and size
.import __SPCIMAGE_LOAD__, __SPCIMAGE_SIZE__


.zeropage
dpTmp0: .res 1, $00
dpTmp1: .res 1, $00
dpTmp2: .res 1, $00
dpTmp3: .res 1, $00
dpTmp4: .res 1, $00
dpTmp5: .res 1, $00
wJoyInput: .res 2, $0000

bSpritePosX: .res 1, $00
bSpritePosY: .res 1, $00

.code
; Follow set up in chapter 23 of manual
Reset:
    ; Not in manual but part of common cpu setup
    startup_init_cpu
    
    ; Move to force blank and clear all the registers
    startup_clear_registers

    ; Initialize zeropage
    startup_clear_directpage




    main_init: 

    stz bSpritePosX
    stz bSpritePosY
    lda #$5
    sta dpTmp5

	joycon_read_joy1_init wJoyInput

    jsr dsp_settings_init

	jsr main_audio_init

    main_setup_video:
    jsr setup_video
    jsr screen_main_cursor_sprite_draw


    ; Display Period begins now
    lda #(NMI_ON | AUTO_JOY_ON) ; enable NMI Enable and Joycon
    sta NMITIMEN

    ; Release VBlank
    lda #FULL_BRIGHT  ; Full brightness
    sta INIDISP
    
    game_loop:
        ; TODO: Gen data of register to be renewed & mem to change BG & OBJ data
        ; aka Update
        ; react to input
		; joycon_read_joy1_blocking wJoyInput
		; jsr check_inputs
		jsr dsp_main_update_sprite_pos

        wai ; Wait for NMI
jmp game_loop

main_audio_init:
    ; Transfer the audio program
    audio_init ^__SPCIMAGE_LOAD__, .loword(__SPCIMAGE_LOAD__), __SPCIMAGE_SIZE__
rts


VBlank:
    ; Detect Beginning of VBlank (Appendix B-3)        
    lda RDNMI; Read NMI flag
    bpl endvblank ; loop if the MSB is 0 N=0  (positive number)

    ; TODO: set data changed registers and memory
    ; TODO: transfer renewed data via OAM
    ; TODO: change data settings for BG&OAM that renew picture

    ; Constant Screen Scrolling
    ; jsr screen_scroll_left

    ; Update the screen scroll register
	; screen_scroll_vupdate

	jsr screen_main_cursor_sprite_update_position


    ; read controllers last after DMA happens

	; block until input is ready
	joycon_read_joy1_blocking wJoyInput
    ; Move between the inputs
    input_on_left wJoyInput, dsp_main_previous_option
    input_on_right wJoyInput, dsp_main_next_option

    endvblank: 
rti 


PAL_FONT_A_ADDR = $00
PAL_BASIC_SET_ADDR = $10

setup_video:
    ; Main register settings
    ; Mode 0 is OK for now

    ; Set OAM, CGRAM Settings
    ; We're going to DMA the graphics instead of using 2121/2122
    ; These are Mode 0 palettes
    ; BG1 Starts at $00
    ;graphics_load_palette test_font_a_palette, PAL_BASIC_SET_ADDR, 4
    graphics_vload_palette palette_basic_set, PAL_BASIC_SET_ADDR, 4

    ; Color palettes for BG2 start at $20. 
    graphics_vload_palette palette_basic_set, $20, 4

    ;Sample palette
    jsr graphics_vload_sample_palette

    ; Second sample palette`
    jsr graphics_vload_sample_palette_snes

    ; force Black BG by setting first color in first palette to black
    jsr graphics_vload_palette_transparent_magenta

    ; Make sure hscroll is 0
    stz mBG1HOFS

    ; Set VRAM Settings
    ; Transfer VRAM Data via DMA

    ; Load tile data to VRAM
    ;jsr reset_tiles
    ;graphics_vload_block test_font_a_obj, $0000, $0020 ; 2 tiles, 2bpp * 8x8 / 8bits = 32 bytes
    graphics_vload_block font_charset, $0400 >> 1, 640 ; 40 tiles, 2bpp * 8x8 / 8 bits= 
    graphics_vload_block tiles_basic_set, $0380, 128 ; 8 tiles, 2bpp * 8x8 / 8 bits = 128

    ; BG2 blocks
    BG2_VRAM_TILE_START = $2000
    graphics_vload_block tiles_basic_set, BG2_VRAM_TILE_START, (8*2*8) ; num * bpp * size

    ; jsr level_basic_tile_load_tilemap
    jsr screen_basic_tile_load_tilemap

    ; TODO: Transfer OAM, CGRAM Data via DMA (2 channels)
    jsr graphics_reset_sprite_table

    ; Register initial screen settings
    jsr register_screen_settings
rts

.macro load_size num_tile, bpp, tile_width 
    (num_tile * bpp * tile_width * tile_width / 8)
.endmacro

register_screen_settings:
    stz BGMODE  ; mode 0 8x8 4-color 4-bgs

    ; Where to write the Tile Map in video ram
    ; - set BG1 tile offset to $0400 (Word addr) (0800 in vram) with sc_size=00
    ; The actual value for 0800>>9 will be 04 for BG1 tile offset. which means 4000 word ram
    ; 04 is %0000 0100
    lda #$1800 >> 11 << 2
    sta BG1SC   ; BG1SC 

    ; Tile Map Location - set BG2 tile offset to $1000 (Word addr) (2000 in vram) with sc_size=00
    lda #$0800 >> 11 << 2
    sta BG2SC   ; BG2SC 

    ; Where to find the tile sets
    lda #$00
    sta BG12NBA ; BG1 name base address to $0000 and 0000 (word addr) (Tiles offset)
    ; so $63 would be BG1=3000 and BG2=6000 (VRAM)
    lda #$00
    sta BG34NBA ; BG3 name base address to $0000 (word addr) (Tiles offset)

    lda #(BG1_ON | BG2_ON | SPR_ON) ; Enable BG1 and Sprites as main screen.
    ;lda #BG1_ON ; Enable BG1 on The Main screen
    ;lda #SPR_ON ; Enable Sprites on The Main screen.
    sta TM

    lda #$FF    ; Scroll down 1 pixel (FF really 03FF 63) (add -1 in 2s complement)
    sta BG1VOFS
    sta BG1VOFS ; Set V offset Low, High, to FFFF for BG1
    lda #$FF    ; Scroll down 1 pixel (FF really 03FF 63) (add -1 in 2s complement)
    sta BG2VOFS
    sta BG2VOFS ; Set V offset Low, High, to FFFF for BG1
rts


.segment "RODATA"

; Turns out sprite MUST be 4bpp
; 2bpp will make a mess of everything as it does now
font_charset:
.incbin "assets/chars.pic"

tiles_basic_set:
.incbin "assets/basic_tileset.pic"
palette_basic_set:
.incbin "assets/basic_tileset.clr"

