use std::{cell::RefCell, rc::Rc};

use crate::memory::Mem;

const LCD_CONTROL_ADDESS: u16 = 0xFF40;
const LCD_STATUS_ADDRESS: u16 = 0xFF41;
const LY_ADDRESS: u16 = 0xFF44;
const LYC_ADDRESS: u16 = 0xFF45;
const INTERRUPT_FLAG_ADDRESS: u16 = 0xFF0F;

const TILE_MAP_AREA_0_START: u16 = 0x9800;
const TILE_MAP_AREA_1_START: u16 = 0x9C00;

const TILE_DATA_AREA_0_START: u16 = 0x8800;
const TILE_DATA_AREA_1_START: u16 = 0x8000;

const WINDOW_Y_ADDRESS: u16 = 0xFF4A;
const WINDOW_X_ADDRESS: u16 = 0xFF4B;

const SC_Y_ADDRESS: u16 = 0xFF42;
const SC_X_ADDRESS: u16 = 0xFF43;

const OAM_START_ADDRESS: u16 = 0xFE00;
const OAM_END_ADDRESS: u16 = 0xFE9F;

const PALETTE_ADDRESS: u16 = 0xFF47;

const WHITE: u8 = 0xFF;
const LIGHT: u8 = 0xB6;
const DARK: u8 = 0x49;
const BLACK: u8 = 0x00;

struct Stat {
    lyc_interrupt: bool,
    mode_2_interrupt: bool,
    mode_1_interrupt: bool,
    mode_0_interrupt: bool,
    lyc_check: bool,
    ppu_mode: u8,
}

enum StatInterruptReason {
    LycEqualLy,
    OamScanModeEnabled,
    HblankModeEnabled,
    VblankModeEnabled,
}

pub struct LcdControl {
    ppu_enable: bool,
    window_tile_map_area: bool,
    window_enable: bool,
    tile_area: bool,
    background_tile_map_area: bool,
    obj_size: bool,
    obj_enable: bool,
    window_background_enable: bool,
}

pub enum PpuMode {
    Hblank,        // 0
    Vblank,        // 1
    OamScan,       // 2
    DrawingPixels, // 3
}

pub struct Ppu {
    pub memory: Rc<RefCell<Mem>>,
    pub current_dots: u16,
    pub mode: PpuMode,
    pub ly: u8,
    pub lcd_control: LcdControl,
    pub buffer: [u8; 160 * 144],
    pub bg_palette: [u8; 4],
    pub obj0_palette: [u8; 4],
    pub obj1_palette: [u8; 4],
}

impl Ppu {
    pub fn new(memory: Rc<RefCell<Mem>>) -> Self {
        return Ppu {
            memory,
            current_dots: 0,
            ly: 0,
            mode: PpuMode::OamScan,
            lcd_control: LcdControl {
                ppu_enable: true,
                window_tile_map_area: true,
                window_enable: true,
                tile_area: true,
                background_tile_map_area: true,
                obj_size: true,
                obj_enable: true,
                window_background_enable: true,
            },
            buffer: [0u8; 160 * 144],
            bg_palette: [0; 4],
            obj0_palette: [0; 4],
            obj1_palette: [0; 4],
        };
    }

    // LCD CONTROL REGISTER
    // 7        6                    5               4                  3             2         1          0
    // enable   window tile map      window enable   bg & window tiles  bg tile map   obj size  obj enable bg window prio
    fn update_lcd_control(&mut self) {
        let lcd_control = self.memory.borrow().read_byte(LCD_CONTROL_ADDESS);
        self.lcd_control = LcdControl {
            ppu_enable: (lcd_control & 0x80) == 0x80,
            window_tile_map_area: (lcd_control & 0x40) == 0x40,
            window_enable: (lcd_control & 0x20) == 0x20,
            tile_area: (lcd_control & 0x10) == 0x10,
            background_tile_map_area: (lcd_control & 0x08) == 0x08,
            obj_size: (lcd_control & 0x04) == 0x04,
            obj_enable: (lcd_control & 0x02) == 0x02,
            window_background_enable: (lcd_control & 0x01) == 0x01,
        };
    }

    // LCD STATUS REGISTER
    // 7        6               5               4               3               2         10
    // Blank    LYC reason      Mode 2 reason   Mode 1 reason   Mode 0 reason   ly==lyc   PPU mode
    fn stat_interrupt(&mut self, reason: StatInterruptReason) {
        let current_if = self.memory.borrow().read_byte(INTERRUPT_FLAG_ADDRESS);
        let current_status = self.memory.borrow().read_byte(LCD_STATUS_ADDRESS);
        let status_update = match reason {
            StatInterruptReason::LycEqualLy => 0x44,
            StatInterruptReason::OamScanModeEnabled => 0x20,
            StatInterruptReason::VblankModeEnabled => 0x10,
            StatInterruptReason::HblankModeEnabled => 0x08,
        };
        self.memory
            .borrow_mut()
            .write_byte(LCD_STATUS_ADDRESS, current_status | status_update);
        self.memory
            .borrow_mut()
            .write_byte(INTERRUPT_FLAG_ADDRESS, current_if | 0x02);
    }

    fn log(&self) {
        let m_str = match self.mode {
            PpuMode::OamScan => "oam",
            PpuMode::DrawingPixels => "draw",
            PpuMode::Hblank => "hblank",
            PpuMode::Vblank => "vblank",
        };
        let sy = self.memory.borrow().read_byte(SC_Y_ADDRESS);
        println!(
            "+++ PPU Mode {} Dots {} Ly {} , SY {}  ",
            m_str, self.current_dots, self.ly, sy
        );
    }
    pub fn next(&mut self, cycles: u16) -> Option<[u8; 160 * 144]> {
        self.ly = self.memory.borrow().read_byte(LY_ADDRESS);
        self.update_lcd_control();
        let mut updated_frame: Option<[u8; 160 * 144]> = None;
        if !self.lcd_control.ppu_enable {
            return updated_frame;
        }
        let new_dots = self.current_dots + cycles;

        match self.mode {
            PpuMode::OamScan => {
                if new_dots >= 80 {
                    self.mode = PpuMode::DrawingPixels;
                    self.current_dots = new_dots - 80;
                } else {
                    self.current_dots = new_dots;
                }
            }
            PpuMode::DrawingPixels => {
                if new_dots >= 289 {
                    self.mode = PpuMode::Hblank;
                    self.current_dots = new_dots - 289;
                    self.stat_interrupt(StatInterruptReason::HblankModeEnabled);
                    self.update_scan_line();
                } else {
                    self.current_dots = new_dots;
                }
            }
            PpuMode::Hblank => {
                if new_dots >= 87 {
                    let mut interrupt_reason = StatInterruptReason::OamScanModeEnabled;
                    self.mode = PpuMode::OamScan;
                    self.current_dots = new_dots - 87;
                    self.ly += 1;
                    if self.ly >= 144 {
                        updated_frame = Some(self.buffer);
                        self.mode = PpuMode::Vblank;
                        let current_if = self.memory.borrow().read_byte(INTERRUPT_FLAG_ADDRESS);
                        self.memory
                            .borrow_mut()
                            .write_byte(INTERRUPT_FLAG_ADDRESS, current_if | 0x01);
                        interrupt_reason = StatInterruptReason::VblankModeEnabled;
                    }
                    self.stat_interrupt(interrupt_reason);
                } else {
                    self.current_dots = new_dots;
                }
            }
            PpuMode::Vblank => {
                if new_dots >= 456 {
                    self.current_dots = new_dots - 456;
                    self.ly += 1;
                    if self.ly == 154 {
                        self.ly = 0;
                        self.current_dots = 0;
                        self.mode = PpuMode::OamScan;
                        self.stat_interrupt(StatInterruptReason::OamScanModeEnabled);
                    }
                } else {
                    self.current_dots = new_dots;
                }
            }
        }
        let current_status = self.memory.borrow().read_byte(LCD_STATUS_ADDRESS);
        let mode_update = match self.mode {
            PpuMode::OamScan => 0x2,
            PpuMode::DrawingPixels => 0x3,
            PpuMode::Hblank => 0x0,
            PpuMode::Vblank => 0x1,
        };
        self.memory
            .borrow_mut()
            .write_byte(LCD_STATUS_ADDRESS, (current_status & (!0x3)) | mode_update);

        if self.ly == self.memory.borrow().read_byte(LYC_ADDRESS) {
            self.stat_interrupt(StatInterruptReason::LycEqualLy);
        };
        self.memory.borrow_mut().write_byte(LY_ADDRESS, self.ly);
        return updated_frame;
    }

    fn update_scan_line(&mut self) {
        self.update_palettes();
        let buffer_start = self.ly as u16 * 160;
        if !self.lcd_control.window_background_enable {
            self.buffer[(buffer_start as usize)..((buffer_start + 160) as usize)]
                .copy_from_slice(&[0xFF; 160]);
        } else {
            let wx = self
                .memory
                .borrow()
                .read_byte(WINDOW_X_ADDRESS)
                .wrapping_sub(7);
            let wy = self.memory.borrow().read_byte(WINDOW_Y_ADDRESS);
            let sx = self.memory.borrow().read_byte(SC_X_ADDRESS);
            let sy = self.memory.borrow().read_byte(SC_Y_ADDRESS);
            for x in 0..160 {
                let offset_x: u16;
                let offset_y: u16;
                let tile_map_address: u16;
                if self.lcd_control.window_enable && x >= wx && self.ly >= wy {
                    offset_x = (x - wx) as u16;
                    offset_y = (self.ly - wy) as u16;
                    tile_map_address = if self.lcd_control.window_tile_map_area {
                        TILE_MAP_AREA_1_START
                    } else {
                        TILE_MAP_AREA_0_START
                    };
                } else {
                    offset_x = x.wrapping_add(sx) as u16;
                    offset_y = self.ly.wrapping_add(sy) as u16;
                    tile_map_address = if self.lcd_control.background_tile_map_area {
                        TILE_MAP_AREA_1_START
                    } else {
                        TILE_MAP_AREA_0_START
                    };
                }
                let tile_offset_x = (offset_x / 8) & 0x1F;
                let tile_offset_y = (offset_y / 8) & 0x1F;
                let tile_index = self
                    .memory
                    .borrow()
                    .read_byte(tile_map_address + tile_offset_x + (tile_offset_y * 32));
                let tile_address = if self.lcd_control.tile_area {
                    // use unsigned indexing
                    TILE_DATA_AREA_1_START + tile_index as u16 * 16 + 2 * (offset_y % 8)
                } else {
                    // use signed indexing
                    TILE_DATA_AREA_0_START
                        + (tile_index as i8 as i16 + 128) as u16 * 16
                        + 2 * (offset_y % 8)
                };
                let tile_data_l = self.memory.borrow().read_byte(tile_address);
                let tile_data_h = self.memory.borrow().read_byte(tile_address + 1);
                let pix_l = if tile_data_l & (0x80 >> (offset_x % 8)) != 0 {
                    1
                } else {
                    0
                };
                let pix_h = if tile_data_h & (0x80 >> (offset_x % 8)) != 0 {
                    2
                } else {
                    0
                };
                let color = self.bg_palette[pix_l | pix_h];
                self.buffer[(buffer_start + x as u16) as usize] = color;
            }
        }
        if self.lcd_control.obj_enable {
            self.draw_sprites();
        }
    }

    fn get_color(&self, byte: u8) -> u8 {
        match byte & 3 {
            0 => WHITE, // white
            1 => LIGHT, //light
            2 => DARK,  // dark
            3 => BLACK, // black
            _ => 0,
        }
    }

    fn update_palettes(&mut self) {
        let bg_pallete_byte = self.memory.borrow().read_byte(PALETTE_ADDRESS);
        let obj0_pallete_byte = self.memory.borrow().read_byte(PALETTE_ADDRESS + 1);
        let obj1_pallete_byte = self.memory.borrow().read_byte(PALETTE_ADDRESS + 2);
        self.bg_palette = [
            self.get_color(bg_pallete_byte),
            self.get_color(bg_pallete_byte >> 2),
            self.get_color(bg_pallete_byte >> 4),
            self.get_color(bg_pallete_byte >> 6),
        ];
        self.obj0_palette = [
            self.get_color(obj0_pallete_byte),
            self.get_color(obj0_pallete_byte >> 2),
            self.get_color(obj0_pallete_byte >> 4),
            self.get_color(obj0_pallete_byte >> 6),
        ];
        self.obj1_palette = [
            self.get_color(obj1_pallete_byte),
            self.get_color(obj1_pallete_byte >> 2),
            self.get_color(obj1_pallete_byte >> 4),
            self.get_color(obj1_pallete_byte >> 6),
        ];
    }
    fn read_palette(&self, pallete_index: u8) -> [u8; 4] {
        let pallete_byte = self
            .memory
            .borrow()
            .read_byte(PALETTE_ADDRESS + pallete_index as u16);
        let color_3 = self.get_color(pallete_byte >> 6);
        let color_2 = self.get_color(pallete_byte >> 4);
        let color_1 = self.get_color(pallete_byte >> 2);
        let color_0 = self.get_color(pallete_byte);
        return [color_0, color_1, color_2, color_3];
    }

    fn draw_sprites(&mut self) {
        let buffer_start = self.ly as u16 * 160;
        let sprite_size = if self.lcd_control.obj_size { 16 } else { 8 };
        for i in (OAM_START_ADDRESS..=OAM_END_ADDRESS).step_by(4) {
            let sprite_y_pos = self.memory.borrow().read_byte(i).wrapping_sub(16);
            let sprite_x_pos = self.memory.borrow().read_byte(i + 1).wrapping_sub(8);
            if self.ly > sprite_y_pos.wrapping_add(sprite_size)
                || self.ly < sprite_y_pos
                || sprite_x_pos > 160
            {
                continue;
            }
            let sprite_tile_index = self.memory.borrow().read_byte(i + 2);
            let sprite_attributes = self.memory.borrow().read_byte(i + 3);
            let offset_y = if sprite_attributes & 0x40 == 0 {
                self.ly - sprite_y_pos
            } else {
                sprite_size - (self.ly - sprite_y_pos)
            };
            let sprite_tile_address =
                TILE_DATA_AREA_1_START + sprite_tile_index as u16 * 16 + 2 * offset_y as u16;
            let sprite_tile_data_h = self.memory.borrow().read_byte(sprite_tile_address);
            let sprite_tile_data_l = self.memory.borrow().read_byte(sprite_tile_address + 1);
            let palette = if ((sprite_attributes >> 3) & 1) == 1 {
                self.obj1_palette
            } else {
                self.obj0_palette
            };

            for x in 0..8 {
                if sprite_x_pos + x > 160 {
                    continue;
                }
                let offset_x = if sprite_attributes & 0x20 == 0 {
                    x
                } else {
                    7 - x
                };
                let pix_l = if sprite_tile_data_l & (0x80 >> (offset_x % 8)) != 0 {
                    1
                } else {
                    0
                };
                let pix_h = if sprite_tile_data_h & (0x80 >> (offset_x % 8)) != 0 {
                    2
                } else {
                    0
                };
                let color = palette[pix_l | pix_h];
                if color == 0xFF
                    || (self.buffer[(buffer_start + sprite_x_pos as u16 + x as u16) as usize]
                        != self.bg_palette[0]
                        && sprite_attributes & 0x80 == 0x80)
                {
                    continue;
                }
                self.buffer[(buffer_start + sprite_x_pos as u16 + x as u16) as usize] = color;
            }
        }
    }
}
