use ines::Ines;
use mos6502_assembler::{Addr, Block, LabelRelativeOffset, LabelRelativeOffsetOwned};
use mos6502_model::{addressing_mode::*, assembler_instruction::*, interrupt_vector, Address};

mod text_chr;

const PRG_START: Address = 0x8000;
const INTERRUPT_VECTOR_START_PC_OFFSET: Address = interrupt_vector::START_LO - PRG_START;
const INTERRUPT_VECTOR_NMI_OFFSET: Address = interrupt_vector::NMI_LO - PRG_START;

trait BlockHelpers {
    fn write_literal_byte(&mut self, addr: u16, value: u8);
    fn write_literal_byte_zero_page(&mut self, index: u8, value: u8);
}

impl BlockHelpers for Block {
    fn write_literal_byte(&mut self, addr: u16, value: u8) {
        self.inst(Lda(Immediate), value);
        self.inst(Sta(Absolute), Addr(addr));
    }

    fn write_literal_byte_zero_page(&mut self, index: u8, value: u8) {
        self.inst(Lda(Immediate), value);
        self.inst(Sta(ZeroPage), index);
    }
}

trait BlockNes {
    fn init_ppu(&mut self);
    fn write_ppu_address(&mut self, addr: u16);
    fn write_ppu_value(&mut self, value: u8);
    fn set_ppu_nametable_coord(&mut self, col: u8, row: u8);
    fn set_ppu_palette_universal_background(&mut self, value: u8);
    fn set_ppu_background_palette(&mut self, palette_index: u8, value0: u8, value1: u8, value2: u8);
    fn set_ppu_sprite_palette(&mut self, palette_index: u8, value0: u8, value1: u8, value2: u8);
    fn set_ppu_scroll(&mut self, x: u8, y: u8);
    fn write_ppu_oam_dma(&mut self, page: u8);
}

impl BlockNes for Block {
    fn init_ppu(&mut self) {
        self.write_literal_byte(0x2000, 0b00001000);
        // Enable sprite and background rendering, and sprite and background rendering in the left
        // column.
        self.write_literal_byte(0x2001, 0b00011110);
    }

    fn write_ppu_address(&mut self, addr: u16) {
        // Read PPU status register to clear address latch.
        self.inst(Bit(Absolute), Addr(0x2002));
        self.write_literal_byte(0x2006, (addr >> 8) as u8);
        self.write_literal_byte(0x2006, addr as u8);
    }

    fn write_ppu_value(&mut self, value: u8) {
        self.write_literal_byte(0x2007, value);
    }

    fn set_ppu_nametable_coord(&mut self, col: u8, row: u8) {
        const NUM_ROWS: u8 = 30;
        assert!(row < NUM_ROWS);
        const NUM_COLS: u8 = 32;
        assert!(col < NUM_COLS);
        const NAMETABLE_BASE_ADDR: u16 = 0x2000; // assumes nametable 1 only
        let addr = NAMETABLE_BASE_ADDR + ((row as u16 * NUM_COLS as u16) + col as u16);
        self.write_ppu_address(addr);
    }

    fn set_ppu_palette_universal_background(&mut self, value: u8) {
        self.write_ppu_address(0x3F00);
        self.write_ppu_value(value);
    }

    fn set_ppu_background_palette(
        &mut self,
        palette_index: u8,
        value0: u8,
        value1: u8,
        value2: u8,
    ) {
        assert!(palette_index <= 3);
        let start_address = 0x3F01 + (4 * palette_index as u16);
        self.write_ppu_address(start_address);
        self.write_ppu_value(value0);
        self.write_ppu_value(value1);
        self.write_ppu_value(value2);
    }

    fn set_ppu_sprite_palette(&mut self, palette_index: u8, value0: u8, value1: u8, value2: u8) {
        assert!(palette_index <= 3);
        let start_address = 0x3F11 + (4 * palette_index as u16);
        self.write_ppu_address(start_address);
        self.write_ppu_value(value0);
        self.write_ppu_value(value1);
        self.write_ppu_value(value2);
    }

    fn set_ppu_scroll(&mut self, x: u8, y: u8) {
        // Read PPU status register to clear address latch.
        self.inst(Bit(Absolute), Addr(0x2002));
        self.write_literal_byte(0x2005, x);
        self.write_literal_byte(0x2005, y);
    }

    fn write_ppu_oam_dma(&mut self, page: u8) {
        self.write_literal_byte(0x4014, page);
    }
}

const OAM_PAGE: u8 = 2;
mod var {
    use super::OAM_PAGE;
    use mos6502_model::address;

    pub mod cursor {
        use super::*;
        pub const X: u16 = address::from_u8_hi_lo(OAM_PAGE, 3);
        pub const Y: u16 = address::from_u8_hi_lo(OAM_PAGE, 0);
        pub const TILE_INDEX: u16 = address::from_u8_hi_lo(OAM_PAGE, 1);
        pub const ATTRIBUTES: u16 = address::from_u8_hi_lo(OAM_PAGE, 2);
        pub const BLINK_TIMEOUT_ZP: u8 = 0;
        pub const BLINK_INDEX: u8 = 1;
        pub const TILE_X: u8 = 2;
        pub const TILE_Y: u8 = 3;
    }

    pub mod controller {
        pub const PRESS_DELTA: u8 = 8;
        pub const CURR: u8 = 9;
        pub const PREV: u8 = 10;
        pub const FLAGS: u8 = 11;
        pub const COUNT_SINCE_CHANGE: u8 = 12;
        pub const REPEAT_COUNTDOWN: u8 = 13;
        pub const RELEASE_DELTA: u8 = 14;
        pub mod flags {
            pub const HORIZONTAL: u8 = 1;
            pub const VERTICAL: u8 = 2;
        }
    }

    pub mod shadow_registers {
        pub const START: u8 = 0x80;
    }
    pub mod staged_registers {
        pub const START: u8 = 0xa0;
    }

    pub mod bit_table_entry {
        use super::super::bit_table::fields as f;
        pub const START: u8 = 64;
        pub const ENABLED: u8 = START + f::ENABLED;
        pub const TILE_X: u8 = START + f::TILE_X;
        pub const TILE_Y: u8 = START + f::TILE_Y;
        pub const PPU_ADDR_HI: u8 = START + f::PPU_ADDR_HI;
        pub const PPU_ADDR_LO: u8 = START + f::PPU_ADDR_LO;
        pub const CHAR_TILE_BASE_INDEX: u8 = START + f::CHAR_TILE_BASE_INDEX;
        pub const SHADOW_REGISTER_INDEX: u8 = START + f::SHADOW_REGISTER_INDEX;
        pub const BIT_MASK: u8 = START + f::BIT_MASK;
    }

    pub mod bit_table_address {
        pub const LO: u8 = 72;
        pub const HI: u8 = 73;
    }

    pub mod scratch {
        pub const START: u8 = 80;
    }
}

mod conf {
    pub mod cursor {
        pub const BLINK_PERIOD_FRAMES: u8 = 4;
    }
    pub mod controller {
        pub const HOLD_FAST_MOVE_DELAY_FRAMES: u8 = 8;
    }
}

fn chidx(ch: char, style: text_chr::Style) -> u8 {
    for (c, idx) in text_chr::LUT {
        if (ch, style) == c {
            return idx;
        }
    }
    panic!()
}

mod bit_table {
    #[derive(Default, Clone, Debug)]
    pub struct Entry {
        enabled: u8,
        tile_x: u8,
        tile_y: u8,
        ppu_addr: u16,
        char_tile_base_index: u8,
        shadow_register_index: u8,
        bit_mask: u8,
    }

    #[derive(Clone, Copy)]
    pub enum Col {
        Left,
        Right,
    }

    #[derive(Clone)]
    pub struct Register {
        col: Col,
        tile_y: u8,
        chars: String,
    }

    pub fn register(chars: &str, col: Col, tile_y: u8) -> Register {
        assert!(chars.chars().count() == 8);
        Register {
            col,
            tile_y,
            chars: chars.to_string(),
        }
    }

    impl Entry {
        fn disabled() -> Self {
            Self {
                enabled: 0,
                ..Default::default()
            }
        }
        fn multi_from_register(register: &Register, index: u8) -> Vec<Self> {
            let enabled = 1;
            let tile_y = register.tile_y;
            let tile_x_start = match register.col {
                Col::Left => 6,
                Col::Right => 22,
            };
            register
                .chars
                .chars()
                .enumerate()
                .map(|(i, ch)| {
                    let tile_x = tile_x_start + i as u8;
                    let char_tile_base_index = super::chidx(ch, crate::text_chr::Style::S1);
                    let nametable_base = 0x2000;
                    let ppu_addr_offset = (tile_y as u16 * 32) + tile_x as u16;
                    let ppu_addr = nametable_base + ppu_addr_offset;
                    let shadow_register_index = index;
                    let bit_mask = 0x80 >> i;
                    Self {
                        enabled,
                        tile_x,
                        tile_y,
                        ppu_addr,
                        char_tile_base_index,
                        shadow_register_index,
                        bit_mask,
                    }
                })
                .collect()
        }
        pub fn multi_from_registers(registers: &[Option<Register>]) -> Vec<Self> {
            registers
                .into_iter()
                .enumerate()
                .map(|(i, maybe_register)| match maybe_register {
                    None => (0..8).map(|_| Self::disabled()).collect::<Vec<_>>(),
                    Some(register) => Self::multi_from_register(register, i as u8),
                })
                .collect::<Vec<_>>()
                .concat()
        }
        pub fn emit_bytes(&self, b: &mut mos6502_assembler::Block) {
            b.literal_byte(self.enabled);
            b.literal_byte(self.tile_x);
            b.literal_byte(self.tile_y);
            b.literal_byte((self.ppu_addr >> 8) as u8);
            b.literal_byte(self.ppu_addr as u8);
            b.literal_byte(self.char_tile_base_index);
            b.literal_byte(self.shadow_register_index);
            b.literal_byte(self.bit_mask);
        }
    }

    pub mod fields {
        pub const ENABLED: u8 = 0;
        pub const TILE_X: u8 = 1;
        pub const TILE_Y: u8 = 2;
        pub const PPU_ADDR_HI: u8 = 3;
        pub const PPU_ADDR_LO: u8 = 4;
        pub const CHAR_TILE_BASE_INDEX: u8 = 5;
        pub const SHADOW_REGISTER_INDEX: u8 = 6;
        pub const BIT_MASK: u8 = 7;
    }
}

fn program(b: &mut Block, dmc_encoded_sample: &[Vec<u8>]) {
    use mos6502_model::addressing_mode::*;
    use mos6502_model::assembler_instruction::*;

    fn text_at_coord(b: &mut Block, text: &str, x: u8, y: u8) {
        b.set_ppu_nametable_coord(x, y);
        for ch in text.chars() {
            b.write_ppu_value(chidx(ch, text_chr::Style::S1));
        }
    }

    let bit_table_registers = {
        use bit_table::{Col, Col::*, Register};
        fn pulse(col: Col, y: u8) -> Vec<Register> {
            vec![
                bit_table::register("DDLCVVVV", col, y + 0),
                bit_table::register("EPPPNSSS", col, y + 1),
                bit_table::register("TTTTTTTT", col, y + 2),
                bit_table::register("LLLLLTTT", col, y + 3),
            ]
        }
        let pulse1 = pulse(Left, 4);
        let pulse2 = pulse(Right, 4);
        let triangle = {
            let y = 12;
            vec![
                bit_table::register("CRRRRRRR", Left, y + 0),
                bit_table::register("TTTTTTTT", Left, y + 1),
                bit_table::register("LLLLLTTT", Left, y + 2),
            ]
        };
        let noise = {
            let y = 12;
            vec![
                bit_table::register("--LCVVVV", Right, y + 0),
                bit_table::register("M---PPPP", Right, y + 1),
                bit_table::register("LLLLL---", Right, y + 2),
            ]
        };
        let dmc = {
            let y = 19;
            vec![
                bit_table::register("IL--RRRR", Left, y + 0),
                bit_table::register("-DDDDDDD", Left, y + 1),
                bit_table::register("AAAAAAAA", Left, y + 2),
                bit_table::register("LLLLLLLL", Left, y + 3),
            ]
        };
        let control = {
            let y = 19;
            vec![
                bit_table::register("---DNT21", Right, y + 0),
                bit_table::register("MI------", Right, y + 1),
            ]
        };
        #[rustfmt::skip]
        let layout = vec![
            Some(pulse1[0].clone()),
            Some(pulse2[0].clone()),
            Some(pulse1[1].clone()),
            Some(pulse2[1].clone()),
            Some(pulse1[2].clone()),
            Some(pulse2[2].clone()),
            Some(pulse1[3].clone()),
            Some(pulse2[3].clone()),
            Some(triangle[0].clone()),
            Some(noise[0].clone()),
            Some(triangle[1].clone()),
            Some(noise[1].clone()),
            Some(triangle[2].clone()),
            Some(noise[2].clone()),
            Some(dmc[0].clone()),
            Some(control[0].clone()),
            Some(dmc[1].clone()),
            Some(control[1].clone()),
            Some(dmc[2].clone()),
            None,
            Some(dmc[3].clone()),
            None,
        ];
        layout
    };
    let bit_table_entries = {
        use bit_table::Entry;
        Entry::multi_from_registers(&bit_table_registers)
    };

    // Add the interrupt vector at the end of the ROM
    b.set_offset(INTERRUPT_VECTOR_START_PC_OFFSET);
    b.label_offset_le("reset");
    b.set_offset(INTERRUPT_VECTOR_NMI_OFFSET);
    b.label_offset_le("nmi");

    // Start adding instructions to PRG ROM at the beginning of memory
    b.set_offset(0);

    // interrupt handler for nmi
    b.label("nmi");

    // update the nametable based on the state of the bit changed in the previous frame
    {
        b.inst(Ldy(Immediate), 1);
        b.inst(Ldx(ZeroPage), var::bit_table_entry::SHADOW_REGISTER_INDEX);
        b.inst(Lda(ZeroPageXIndexed), var::staged_registers::START);
        b.inst(And(ZeroPage), var::bit_table_entry::BIT_MASK);
        b.inst(Beq, LabelRelativeOffset("end_set_tile_offset"));
        b.inst(Iny, ());
        b.label("end_set_tile_offset");
        b.inst(Sty(ZeroPage), var::scratch::START);

        b.inst(Bit(Absolute), Addr(0x2002));
        b.inst(Lda(ZeroPage), var::bit_table_entry::PPU_ADDR_HI);
        b.inst(Sta(Absolute), Addr(0x2006));
        b.inst(Lda(ZeroPage), var::bit_table_entry::PPU_ADDR_LO);
        b.inst(Sta(Absolute), Addr(0x2006));
        b.inst(Lda(ZeroPage), var::bit_table_entry::CHAR_TILE_BASE_INDEX);
        b.inst(Clc, ());
        b.inst(Adc(ZeroPage), var::scratch::START);
        b.inst(Sta(Absolute), Addr(0x2007));
    }

    // cursor blinking
    b.inst(And(Immediate), 1);
    {
        b.inst(Dec(ZeroPage), var::cursor::BLINK_TIMEOUT_ZP);
        b.inst(Bne, LabelRelativeOffset("cursor_blink_end"));
        // reset the count
        b.write_literal_byte_zero_page(
            var::cursor::BLINK_TIMEOUT_ZP,
            conf::cursor::BLINK_PERIOD_FRAMES,
        );
        // read the blink index and increment it mod 8 (assumes there are 8 blink colours)
        b.inst(Lda(ZeroPage), var::cursor::BLINK_INDEX);
        b.inst(Clc, ());
        b.inst(Adc(Immediate), 1);
        b.inst(And(Immediate), 0b00000111);
        // transfer the blink index into the X register and use it to look up a colour in the table
        b.inst(Tax, ());
        b.inst(Ldy(AbsoluteXIndexed), "blink_colour_table");
        // store the updated blink index
        b.inst(Stx(ZeroPage), var::cursor::BLINK_INDEX);
        // write the blink colour to the palette
        b.write_ppu_address(0x3F11);
        b.inst(Sty(Absolute), Addr(0x2007));
        // Setting the ppu address to a palette address breaks scrolling unless it's explicitly set
        // outside the palette afterwards.
        b.write_ppu_address(0);
        b.label("cursor_blink_end");
    }

    b.write_ppu_oam_dma(OAM_PAGE);
    // reset scroll
    b.set_ppu_scroll(0, 0);

    // finished rendering

    b.inst(Jsr(Absolute), "copy_controller_state_to_zp");

    // update cursor based on controller state
    {
        b.inst(Ldx(Immediate), 0);
        b.inst(Stx(ZeroPage), var::controller::FLAGS);
        // button timing
        {
            b.inst(Inc(ZeroPage), var::controller::COUNT_SINCE_CHANGE);

            b.inst(Lda(ZeroPage), var::controller::PRESS_DELTA);
            b.inst(Bne, LabelRelativeOffset("button_timing_just_pressed")); // branch if a button was just pressed

            // no button was just pressed
            b.inst(Lda(ZeroPage), var::controller::REPEAT_COUNTDOWN);
            b.inst(Beq, LabelRelativeOffset("fast_cursor")); // branch if countdown is zero

            // countdown was not zero
            b.inst(Dec(ZeroPage), var::controller::REPEAT_COUNTDOWN);
            b.inst(Jmp(Absolute), "button_timing_end");
            b.label("button_timing_just_pressed");
            // button was just pressed
            b.inst(Lda(Immediate), 0);
            b.inst(Sta(ZeroPage), var::controller::COUNT_SINCE_CHANGE);
            b.inst(
                Lda(Immediate),
                conf::controller::HOLD_FAST_MOVE_DELAY_FRAMES,
            );
            b.inst(Sta(ZeroPage), var::controller::REPEAT_COUNTDOWN);
            b.label("button_timing_end");
        }
        // if start is pressed then every 4 frames, use CURR instead of PRESS_DELTA to move cursor
        b.inst(Lda(Immediate), 1 << 4);
        b.inst(Bit(ZeroPage), var::controller::CURR);
        b.inst(Beq, LabelRelativeOffset("after_fast_cursor_check"));
        b.label("fast_cursor");
        b.inst(Lda(Immediate), 0b11);
        b.inst(Bit(ZeroPage), var::controller::COUNT_SINCE_CHANGE);
        b.inst(Bne, LabelRelativeOffset("after_fast_cursor_check"));
        b.inst(Ldx(Immediate), 1);
        b.label("after_fast_cursor_check");

        b.inst(Lda(Immediate), 1 << 0);
        b.inst(And(ZeroPageXIndexed), var::controller::PRESS_DELTA);
        b.inst(Beq, LabelRelativeOffset("after_right"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_X);
        b.inst(Clc, ());
        b.inst(Adc(Immediate), 1);
        b.inst(Sta(ZeroPage), var::cursor::TILE_X);
        b.inst(Lda(ZeroPage), var::controller::FLAGS);
        b.inst(Ora(Immediate), var::controller::flags::HORIZONTAL);
        b.inst(Sta(ZeroPage), var::controller::FLAGS);
        b.label("after_right");

        b.inst(Lda(Immediate), 1 << 1);
        b.inst(And(ZeroPageXIndexed), var::controller::PRESS_DELTA);
        b.inst(Beq, LabelRelativeOffset("after_left"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_X);
        b.inst(Sec, ());
        b.inst(Sbc(Immediate), 1);
        b.inst(Sta(ZeroPage), var::cursor::TILE_X);
        b.inst(Lda(ZeroPage), var::controller::FLAGS);
        b.inst(Ora(Immediate), var::controller::flags::HORIZONTAL);
        b.inst(Sta(ZeroPage), var::controller::FLAGS);
        b.label("after_left");

        b.inst(Lda(Immediate), 1 << 2);
        b.inst(And(ZeroPageXIndexed), var::controller::PRESS_DELTA);
        b.inst(Beq, LabelRelativeOffset("after_down"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Clc, ());
        b.inst(Adc(Immediate), 1);
        b.inst(Sta(ZeroPage), var::cursor::TILE_Y);
        b.inst(Lda(ZeroPage), var::controller::FLAGS);
        b.inst(Ora(Immediate), var::controller::flags::VERTICAL);
        b.inst(Sta(ZeroPage), var::controller::FLAGS);
        b.label("after_down");

        b.inst(Lda(Immediate), 1 << 3);
        b.inst(And(ZeroPageXIndexed), var::controller::PRESS_DELTA);
        b.inst(Beq, LabelRelativeOffset("after_up"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Sec, ());
        b.inst(Sbc(Immediate), 1);
        b.inst(Sta(ZeroPage), var::cursor::TILE_Y);
        b.inst(Lda(ZeroPage), var::controller::FLAGS);
        b.inst(Ora(Immediate), var::controller::flags::VERTICAL);
        b.inst(Sta(ZeroPage), var::controller::FLAGS);
        b.label("after_up");
    }

    // bounds checks for cursor
    {
        // bounds check for tile x
        b.inst(Lda(ZeroPage), var::controller::FLAGS);
        b.inst(And(Immediate), var::controller::flags::HORIZONTAL);
        b.inst(Beq, LabelRelativeOffset("after_bound_tile_x_write"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Cmp(Immediate), 9);
        b.inst(Bpl, LabelRelativeOffset("bottom_two_rows"));
        b.inst(Lda(ZeroPage), var::cursor::TILE_X);
        b.inst(And(Immediate), 0x0F);
        b.inst(Jmp(Absolute), "after_bound_tile_x");
        b.label("bottom_two_rows");
        b.inst(Lda(ZeroPage), var::cursor::TILE_X);
        b.inst(And(Immediate), 0x07);
        b.label("after_bound_tile_x");
        b.inst(Sta(ZeroPage), var::cursor::TILE_X);
        b.label("after_bound_tile_x_write");

        // wrap y differently depending on which column we're in
        b.inst(Lda(ZeroPage), var::cursor::TILE_X);
        b.inst(And(Immediate), 1 << 3);
        b.inst(Bne, LabelRelativeOffset("bounds_check_y_right"));

        // bounds check for tile y
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Cmp(Immediate), 0xFF);
        b.inst(Bne, LabelRelativeOffset("before_upper_bound_tile_y_left"));
        b.inst(Lda(Immediate), 10);
        b.inst(Jmp(Absolute), "after_bound_tile_y_left");
        b.label("before_upper_bound_tile_y_left");
        b.inst(Cmp(Immediate), 11);
        b.inst(Bne, LabelRelativeOffset("after_bound_tile_y_left"));
        b.inst(Lda(Immediate), 0);
        b.label("after_bound_tile_y_left");
        b.inst(Jmp(Absolute), "after_bound_tile_y");

        b.label("bounds_check_y_right");
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Cmp(Immediate), 0xFF);
        b.inst(Bne, LabelRelativeOffset("before_upper_bound_tile_y_right"));
        b.inst(Lda(Immediate), 8);
        b.inst(Jmp(Absolute), "after_bound_tile_y_right");
        b.label("before_upper_bound_tile_y_right");
        b.inst(Cmp(Immediate), 9);
        b.inst(Bne, LabelRelativeOffset("after_bound_tile_y_right"));
        b.inst(Lda(Immediate), 0);
        b.label("after_bound_tile_y_right");

        b.label("after_bound_tile_y");
        b.inst(Sta(ZeroPage), var::cursor::TILE_Y);
    }

    // copy the current bit table entry into zero page
    {
        b.inst(Lda(ZeroPage), var::cursor::TILE_Y);
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Ora(ZeroPage), var::cursor::TILE_X);
        b.inst(Jsr(Absolute), "copy_bit_table_entry_to_zp");
    }

    b.inst(Ldx(ZeroPage), var::bit_table_entry::TILE_X);
    b.inst(Ldy(ZeroPage), var::bit_table_entry::TILE_Y);
    b.inst(Jsr(Absolute), "set_cursor_to_tile_coord");

    // toggle the bit under the cursor if the A button was just pressed
    {
        b.inst(Lda(Immediate), 1 << 7);
        b.inst(And(ZeroPage), var::controller::PRESS_DELTA);
        b.inst(Beq, LabelRelativeOffset("end_of_bit_toggle"));

        b.inst(Lda(Immediate), 1 << 6);
        b.inst(And(ZeroPage), var::controller::CURR);
        b.inst(Beq, LabelRelativeOffset("bit_toggle_b_is_not_down"));
        b.inst(Ldx(ZeroPage), var::bit_table_entry::SHADOW_REGISTER_INDEX);
        b.inst(Lda(ZeroPageXIndexed), var::staged_registers::START);
        b.inst(Eor(ZeroPage), var::bit_table_entry::BIT_MASK);
        b.inst(Sta(ZeroPageXIndexed), var::staged_registers::START);
        b.inst(Jmp(Absolute), "end_of_bit_toggle");
        b.label("bit_toggle_b_is_not_down");
        b.inst(Ldx(ZeroPage), var::bit_table_entry::SHADOW_REGISTER_INDEX);
        b.inst(Lda(ZeroPageXIndexed), var::staged_registers::START);
        b.inst(Eor(ZeroPage), var::bit_table_entry::BIT_MASK);
        b.inst(Sta(ZeroPageXIndexed), var::staged_registers::START);
        b.inst(Ldy(AbsoluteXIndexed), "apu_register_lo_table");
        b.inst(Sta(AbsoluteYIndexed), Addr(0x4000));
        b.inst(Sta(ZeroPageXIndexed), var::shadow_registers::START);
        b.label("end_of_bit_toggle");
    }

    // handle the case where B is released
    {
        b.inst(Lda(Immediate), 1 << 6);
        b.inst(And(ZeroPage), var::controller::RELEASE_DELTA);
        b.inst(Bne, LabelRelativeOffset("start_of_commit_loop"));
        b.inst(Jmp(Absolute), "end_of_commit_loop");
        b.label("start_of_commit_loop");
        for i in 0..bit_table_registers.len() as u8 {
            b.inst(Lda(Immediate), i);
            b.inst(Cmp(ZeroPage), var::bit_table_entry::SHADOW_REGISTER_INDEX);
            b.inst(
                Beq,
                LabelRelativeOffsetOwned(format!("commit_register_{}", i)),
            );
            b.inst(Ldx(Immediate), i);
            b.inst(Lda(ZeroPageXIndexed), var::shadow_registers::START);
            b.inst(Cmp(ZeroPageXIndexed), var::staged_registers::START);
            b.inst(
                Beq,
                LabelRelativeOffsetOwned(format!("commit_register_{}_end_of_iteration", i)),
            );
            b.label(format!("commit_register_{}", i));
            b.inst(Ldx(Immediate), i);
            b.inst(Lda(ZeroPageXIndexed), var::staged_registers::START);
            b.inst(Sta(ZeroPageXIndexed), var::shadow_registers::START);
            b.inst(Ldy(AbsoluteXIndexed), "apu_register_lo_table");
            b.inst(Sta(AbsoluteYIndexed), Addr(0x4000));
            b.label(format!("commit_register_{}_end_of_iteration", i));
        }
        b.label("end_of_commit_loop");
    }

    b.inst(Rti, ());

    b.label("copy_bit_table_entry_to_zp");
    {
        b.inst(Stx(ZeroPage), var::scratch::START);

        // clear the high byte of the address in zero page
        b.inst(Ldx(Immediate), 0xF0 >> 3);
        b.inst(Stx(ZeroPage), var::bit_table_address::HI);

        // multiply by 8 as each entry is 8 bytes
        b.inst(Asl(Accumulator), ());
        b.inst(Rol(ZeroPage), var::bit_table_address::HI);
        b.inst(Asl(Accumulator), ());
        b.inst(Rol(ZeroPage), var::bit_table_address::HI);
        b.inst(Asl(Accumulator), ());
        b.inst(Rol(ZeroPage), var::bit_table_address::HI);
        b.inst(Sta(ZeroPage), var::bit_table_address::LO);
        // copy the entry into the zero page
        b.inst(Ldx(Immediate), 0);
        for i in 0..8 {
            b.inst(Lda(XIndexedIndirect), var::bit_table_address::LO);
            b.inst(Sta(ZeroPage), var::bit_table_entry::START + i);
            b.inst(Inc(ZeroPage), var::bit_table_address::LO);
        }

        b.inst(Ldx(ZeroPage), var::scratch::START);
        b.inst(Rts, ());
    }

    b.label("copy_controller_state_to_zp");
    {
        const CONTROLLER_REG: Addr = Addr(0x4016);

        // copy the current controller state
        b.inst(Lda(ZeroPage), var::controller::CURR);
        b.inst(Sta(ZeroPage), var::controller::PREV);

        // toggle the controller strobe bit to copy its current value into shift register
        b.inst(Lda(Immediate), 1);
        b.inst(Sta(Absolute), CONTROLLER_REG); // set controller strobe
        b.inst(Sta(ZeroPage), var::controller::CURR); // store a 1 at destination
        b.inst(Lsr(Accumulator), ()); // clear accumulator
        b.inst(Sta(Absolute), CONTROLLER_REG); // clear controller strobe
                                               // shift each of the 8 bits of controller state from the shift register into address 0
        b.label("copy_controller_state_to_zp_loop");
        b.inst(Lda(Absolute), CONTROLLER_REG); // load single bit into LBS of acculumator
        b.inst(Lsr(Accumulator), ()); // shift bit into carry flag
        b.inst(Rol(ZeroPage), var::controller::CURR); // shift carry flag into 0, and MSB of 0 into carry flag

        // if that set the carry flag, this was the 8th iteration
        b.inst(Bcc, LabelRelativeOffset("copy_controller_state_to_zp_loop"));

        b.inst(Lda(ZeroPage), var::controller::PREV);
        b.inst(Eor(Immediate), 0xFF);
        b.inst(And(ZeroPage), var::controller::CURR);
        b.inst(Sta(ZeroPage), var::controller::PRESS_DELTA);

        b.inst(Lda(ZeroPage), var::controller::CURR);
        b.inst(Eor(Immediate), 0xFF);
        b.inst(And(ZeroPage), var::controller::PREV);
        b.inst(Sta(ZeroPage), var::controller::RELEASE_DELTA);

        b.inst(Rts, ());
    }

    b.label("set_cursor_to_tile_coord");
    {
        b.inst(Txa, ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Sta(Absolute), Addr(var::cursor::X));
        b.inst(Tya, ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Asl(Accumulator), ());
        b.inst(Sec, ());
        b.inst(Sbc(Immediate), 1); // sprite rendering implicitly adds 1 pixel
        b.inst(Sta(Absolute), Addr(var::cursor::Y));
        b.inst(Rts, ());
    }

    b.label("reset");

    b.inst(Sei, ());
    b.inst(Cld, ());

    // zero out the zero page
    b.inst(Ldx(Immediate), 0);
    b.inst(Lda(Immediate), 0);
    b.label("zero_out_zero_page_loop");
    b.inst(Sta(ZeroPageXIndexed), 0);
    b.inst(Inx, ());
    b.inst(Bne, LabelRelativeOffset("zero_out_zero_page_loop"));

    b.inst(Ldx(Immediate), 0xFF);
    b.inst(Tsx, ()); // initialize stack

    b.init_ppu();
    b.set_ppu_palette_universal_background(0x2f); // black
    b.set_ppu_background_palette(0, 0x14, 0x04, 0x24); // red
    b.set_ppu_background_palette(1, 0x1c, 0x0c, 0x2c); // blue
    b.set_ppu_background_palette(2, 0x1a, 0x0a, 0x2a); // green
    b.set_ppu_background_palette(3, 0x18, 0x08, 0x28); // yellow

    let x = 0;
    let y = 2;
    text_at_coord(b, "      Pulse1", x, y + 0);
    text_at_coord(b, " 4000", x, y + 2);
    text_at_coord(b, " 4001", x, y + 3);
    text_at_coord(b, " 4002", x, y + 4);
    text_at_coord(b, " 4003", x, y + 5);

    let x = 16;
    text_at_coord(b, "      Pulse2", x, y + 0);
    text_at_coord(b, " 4004", x, y + 2);
    text_at_coord(b, " 4005", x, y + 3);
    text_at_coord(b, " 4006", x, y + 4);
    text_at_coord(b, " 4007", x, y + 5);

    let x = 0;
    let y = 10;
    text_at_coord(b, "      Triangle", x, y + 0);
    text_at_coord(b, " 4008", x, y + 2);
    text_at_coord(b, " 400A", x, y + 3);
    text_at_coord(b, " 400B", x, y + 4);

    let x = 16;
    text_at_coord(b, "      Noise", x, y + 0);
    text_at_coord(b, " 400C", x, y + 2);
    text_at_coord(b, " 400E", x, y + 3);
    text_at_coord(b, " 400F", x, y + 4);

    let x = 0;
    let y = 17;
    text_at_coord(b, "      DMC", x, y + 0);
    text_at_coord(b, " 4010", x, y + 2);
    text_at_coord(b, " 4011", x, y + 3);
    text_at_coord(b, " 4012", x, y + 4);
    text_at_coord(b, " 4013", x, y + 5);

    let x = 16;
    text_at_coord(b, "      Control", x, y + 0);
    text_at_coord(b, " 4015", x, y + 2);
    text_at_coord(b, " 4017", x, y + 3);

    // set attributes
    #[rustfmt::skip]
    let attribute_table = [
        0x00, 0x00, 0x00, 0x00, 0x55, 0x55, 0x55, 0x55,
        0x00, 0x00, 0x00, 0x00, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0xFF, 0xFF, 0xFF, 0xFF,
        0xAA, 0xAA, 0xAA, 0xAA, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x00, 0x00, 0x00, 0x55, 0x55, 0x55, 0x55,
        0x00, 0x00, 0x00, 0x00, 0x55, 0x55, 0x55, 0x55,
    ];
    b.write_ppu_address(0x23C0);
    for byte in attribute_table {
        b.write_ppu_value(byte);
    }

    // set up blink sprite
    b.set_ppu_sprite_palette(0, 0x20, 0, 0);
    b.write_literal_byte(var::cursor::TILE_INDEX, 1);
    b.write_literal_byte(var::cursor::ATTRIBUTES, 0b00100000);
    b.write_literal_byte_zero_page(
        var::cursor::BLINK_TIMEOUT_ZP,
        conf::cursor::BLINK_PERIOD_FRAMES,
    );

    b.set_ppu_scroll(0, 0);

    // enable vblank nmi
    b.write_literal_byte(0x2000, 0b10001000);

    b.write_literal_byte_zero_page(var::cursor::TILE_X, 0);
    b.write_literal_byte_zero_page(var::cursor::TILE_Y, 0);

    // draw all the bit labels
    b.inst(Ldx(Immediate), 0);
    b.label("draw_bit_labels_loop");
    b.inst(Txa, ());
    b.inst(Cmp(Immediate), bit_table_entries.len() as u8);
    b.inst(Beq, LabelRelativeOffset("draw_bit_labels_loop_end"));
    b.inst(Inx, ());
    b.inst(Jsr(Absolute), "copy_bit_table_entry_to_zp");
    b.inst(Lda(ZeroPage), var::bit_table_entry::ENABLED);
    b.inst(Beq, LabelRelativeOffset("draw_bit_labels_loop"));
    b.inst(Bit(Absolute), Addr(0x2002));
    b.inst(Lda(ZeroPage), var::bit_table_entry::PPU_ADDR_HI);
    b.inst(Sta(Absolute), Addr(0x2006));
    b.inst(Lda(ZeroPage), var::bit_table_entry::PPU_ADDR_LO);
    b.inst(Sta(Absolute), Addr(0x2006));
    b.inst(Lda(ZeroPage), var::bit_table_entry::CHAR_TILE_BASE_INDEX);
    b.inst(Clc, ());
    b.inst(Adc(Immediate), 1); // select the dark colour
    b.inst(Sta(Absolute), Addr(0x2007));
    b.inst(Jmp(Absolute), "draw_bit_labels_loop");
    b.label("draw_bit_labels_loop_end");

    // spin until nmi
    b.label("spin");
    b.inst(Jmp(Absolute), "spin");

    // static data:

    // blink colour table
    b.label("blink_colour_table");

    #[rustfmt::skip]
    const BLINK_COLOURS: [u8; 8] = [
        0x20,
        0x20,
        0x10,
        0x10,
        0x00,
        0x00,
        0x10,
        0x10,
    ];
    for c in BLINK_COLOURS {
        b.literal_byte(c);
    }

    b.label("apu_register_lo_table");
    #[rustfmt::skip]
    const APU_REGISTER_LO_TABLE: [u8; 22] = [
        0x00, 0x04,
        0x01, 0x05,
        0x02, 0x06,
        0x03, 0x07,
        0x08, 0x0C,
        0x0A, 0x0E,
        0x0B, 0x0F,
        0x10, 0x15,
        0x11, 0x17,
        0x12, 0xFF,
        0x13, 0xFF,
    ];
    for byte in APU_REGISTER_LO_TABLE {
        b.literal_byte(byte);
    }

    b.set_offset(0x7000);
    b.label("bit_table");
    for entry in bit_table_entries {
        entry.emit_bytes(b);
    }

    for (i, sample) in dmc_encoded_sample.iter().enumerate() {
        b.set_offset(0x4000 + (i as u16 * 0x1000));
        println!("{}", sample.len());
        for &sample_byte in sample {
            b.literal_byte(sample_byte);
        }
    }
}

fn prg_rom(dmc_encoded_sample: &[Vec<u8>]) -> Vec<u8> {
    let mut block = Block::new();
    program(&mut block, dmc_encoded_sample);
    let mut prg_rom = Vec::new();
    block
        .assemble(PRG_START, ines::PRG_ROM_BLOCK_BYTES * 2, &mut prg_rom)
        .expect("Failed to assemble");
    prg_rom
}

fn chr_rom() -> Vec<u8> {
    let mut rom = vec![0; ines::CHR_ROM_BLOCK_BYTES * 2];
    rom[0..text_chr::CHR.len()].copy_from_slice(&text_chr::CHR);
    #[rustfmt::skip]
    rom[(ines::CHR_ROM_BLOCK_BYTES + 16)..(ines::CHR_ROM_BLOCK_BYTES + 32)].copy_from_slice(&[
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b11111111,

        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
        0b00000000,
    ]);
    rom
}

struct Args {
    wav_path: Vec<String>,
    output_path: String,
}

impl Args {
    fn parser() -> impl meap::Parser<Item = Self> {
        meap::let_map! {
            let {
                wav_path = pos_multi("PATH");
                output_path = opt_req("PATH", "output").name('o');
            } in {
                Self { wav_path, output_path }
            }
        }
    }
}

fn read_audio_samples(wav_path: String) -> Vec<i32> {
    use hound::WavReader;
    let mut reader = WavReader::open(wav_path).unwrap();
    let spec = reader.spec();
    let original_data = reader
        .samples::<i32>()
        .map(|r| r.unwrap())
        .collect::<Vec<_>>();
    let mut channel_means = Vec::new();
    for sample in original_data.chunks(spec.channels as usize) {
        let mean = sample.iter().sum::<i32>() / (spec.channels as i32);
        channel_means.push(mean);
    }
    channel_means
}

fn naive_dmc_encode(samples: &[i32]) -> Vec<u8> {
    let mut last = 0;
    let mut ret = Vec::new();
    for block in samples.chunks(8) {
        let mut byte = 0;
        for (i, &sample) in block.iter().enumerate() {
            let bit = if last < sample { 1 } else { 0 };
            byte = byte | (bit << i);
            last = sample;
        }
        ret.push(byte);
    }
    ret
}

fn main() {
    use meap::Parser;
    let Args {
        wav_path,
        output_path,
    } = Args::parser().with_help_default().parse_env_or_exit();
    let dmc_encoded_samples = wav_path
        .into_iter()
        .map(|wav_path| {
            let samples = read_audio_samples(wav_path);
            naive_dmc_encode(&samples)
        })
        .collect::<Vec<_>>();
    use std::io::Write;
    let ines = Ines {
        header: ines::Header {
            num_prg_rom_blocks: 2,
            num_chr_rom_blocks: 2,
            mapper: ines::Mapper::Nrom,
            mirroring: ines::Mirroring::Vertical,
            four_screen_vram: false,
        },
        prg_rom: prg_rom(&dmc_encoded_samples),
        chr_rom: chr_rom(),
    };
    let mut encoded = Vec::new();
    ines.encode(&mut encoded);
    let mut file = std::fs::File::create(output_path).unwrap();
    file.write_all(&encoded)
        .expect("Failed to write encoded rom to stdout");
}
