.zeropage

dp_vol_l: .res 1, $00
dp_vol_r: .res 1, $00
dp_echo_l: .res 1, $00
dp_echo_r: .res 1, $00
dp_key_on: .res 1, $00
dp_key_off: .res 1, $00

dp_echo: .res 1, $00
dp_mute: .res 1, $00
dp_noise: .res 1, $00

dsp_main_option_selected: .res 1, $00

dsp_cursor_position_x: .res 1, $00 
dsp_cursor_position_y: .res 1, $00 

.enum dsp_main_options
	vol_l
	vol_r
	echo_l
	echo_r
	key_on_0
	key_on_1
	key_on_2
	key_on_3
	key_on_4
	key_on_5
	key_on_6
	key_on_7
	key_off_0
	key_off_1
	key_off_2
	key_off_3
	key_off_4
	key_off_5
	key_off_6
	key_off_7
	echo_on
	mute
	noise_clk
.endenum 


.code 

; This table is wrong.
; We actuall want to map to the XY OAM position
; Currently this is written in tile places. 
; Position is written as an offset from this table as $XXYY
; dsp_map_option_position:
; 	op_pos_vol_l: .word $0704
; 	op_pos_vol_r: .word $0706
; 	op_pos_echo_l: .word $0708
; 	op_pos_echo_r: .word $070A
; 	op_pos_key_on_0: .word $050C
; 	op_pos_key_on_1: .word $060C
; 	op_pos_key_on_2: .word $070C
; 	op_pos_key_on_3: .word $080C
; 	op_pos_key_on_4: .word $090C
; 	op_pos_key_on_5: .word $0A0C
; 	op_pos_key_on_6: .word $0B0C
; 	op_pos_key_on_7: .word $0C0C
; 	op_pos_key_off_0: .word $050E
; 	op_pos_key_off_1: .word $060E
; 	op_pos_key_off_2: .word $070E
; 	op_pos_key_off_3: .word $080E
; 	op_pos_key_off_4: .word $090E
; 	op_pos_key_off_5: .word $0A0E
; 	op_pos_key_off_6: .word $0B0E
; 	op_pos_key_off_7: .word $0C0E
; 	op_pos_echo_on: .word $0500
; 	op_pos_mute: .word $0512
; 	op_pos_noise_clk: .word $0914

dsp_settings_init:
	lda #$7F
	sta dp_vol_l
	sta dp_vol_r

	stz dp_echo_l 
	stz dp_echo_r 
	stz dp_key_on 
	stz dp_key_off 
	
	stz dp_echo
	stz dp_mute 
	stz dp_noise

	stz dsp_main_option_selected

	stz dsp_cursor_position_x
	stz dsp_cursor_position_y
rts


DSP_MAIN_END_OPTION = dsp_main_options::noise_clk

dsp_main_next_option:
	lda dsp_main_option_selected
	cmp #DSP_MAIN_END_OPTION
	bne	@inc_and_store_value ; if < noise_clk_selected+1:  store_value
	; else wrap around to start
	lda #$FF  ; Go one less than inc in next step
	@inc_and_store_value:
	inc
	sta dsp_main_option_selected
rts 

dsp_main_previous_option:
	lda dsp_main_option_selected
	bne @decrement_and_store; if not 0 options, decrement_and_store
	; Else we are at 0: wrap to end
	lda #DSP_MAIN_END_OPTION + 1 ; add one since we will always subtract 1

	@decrement_and_store:
	dec
	sta dsp_main_option_selected
rts


; TODO Next thing to do is to have a sprite get drawn on the screen in the position
; that is set from the mapping found for the option_selected.
; Sprites get moved a little differently.
; The sprite will serve as a visual indicator of where we are on the screen
; It can look like an arrow if we want ->. Or just a period .
dsp_main_update_sprite_pos:
  lda dsp_main_option_selected

  ; Get the position from the table based on index
  ; TODO replace this with the slot from the selection index
  ; The selection index should map 1-to-1 with the sprite_pos_table
  lda dsp_main_option_selected ; This returns an index
  asl ; Double the index since since the table is word sized
  tay

  ; Go from selected index to x y positions
  ; get the x y position and put it in the correct registers
  lda sprite_pos_table, y
  sta dsp_cursor_position_x

  iny  ; get the second byte from the table entry
  lda sprite_pos_table, y
  sta dsp_cursor_position_y
rts


; Once we can move sprite to a screen be able to do a custom increment for each option.
; Sometimes it incrementes a byte.
; For the blocks, it xors a 1 (to be either one or zero) for a specifc bit.

; We also should have a signed write function that writes +/- signed int as a byte



dsp_increment_selected:
  lda dsp_main_option_selected

  ; Case: Vol L
  @check_vol_l:
  cmp #dsp_main_options::vol_l
  bne @check_vol_r
  ; Do the increment
  lda dp_vol_l
  inc
  cmp #$80 ; Dont go higher than signed bit
  beq @done ; 
  sta  dp_vol_l
  bra @done
  
  @check_vol_r:
  cmp #dsp_main_options::vol_r
  bne @check_echo_l
  ; Do the increment
  lda dp_vol_r
  inc
  cmp #$80 ; Dont go higher than signed bit
  beq @done ; 
  sta  dp_vol_r
  bra @done

  @check_echo_l:
  ; ... more stuf here ...

  @check_mute:
  cmp #dsp_main_options::mute
  bne @check_noise_clock
  lda #$01     ; mute on = 1
  sta dp_mute 
  bra @done

  @check_noise_clock:

  @done:
rts

dsp_decrement_selected:
  lda dsp_main_option_selected

  ; Case: Vol L
  @check_vol_l:
  cmp #dsp_main_options::vol_l
  bne @check_vol_r
  ; Do the increment
  lda dp_vol_l
  beq @done ; dont go lower than 0
  dec
  sta  dp_vol_l
  bra @done
  
  @check_vol_r:
  cmp #dsp_main_options::vol_r
  bne @check_echo_l
  ; Do the increment
  lda dp_vol_r
  beq @done ; dont go lower than 0
  dec
  sta  dp_vol_r
  bra @done

  @check_echo_l:
  ; ... more stuf here ...


  @check_mute:
  cmp #dsp_main_options::mute
  bne @check_noise_clock
  stz dp_mute ; mute off = 0
  bra @done

  @check_noise_clock:

  @done:
rts


; This is written in $YYXX values
sprite_pos_table: 
	.word $2030 ; vol L
	.word $3030 ; vol R
	.word $4030 ; echo L
	.word $5030 ; echo R
	.word $5A28 + $8 * 0	; Kon Position Above
	.word $5A28 + $8 * 1
	.word $5A28 + $8 * 2
	.word $5A28 + $8 * 3
	.word $5A28 + $8 * 4
	.word $5A28 + $8 * 5
	.word $5A28 + $8 * 6
	.word $5A28 + $8 * 7
	.word $6A28 + $8 * 0	; Koff Position 7 Above
	.word $6A28 + $8 * 1
	.word $6A28 + $8 * 2
	.word $6A28 + $8 * 3
	.word $6A28 + $8 * 4
	.word $6A28 + $8 * 5
	.word $6A28 + $8 * 6
	.word $6A28 + $8 * 7
	.word $8020 ; echo
	.word $9020 ; mute
	.word $A048 ; Noise Clock

; Koff position (left) is 7030
; Kon position (left) is 6030


