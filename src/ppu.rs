use std::{cell::RefCell, rc::Rc};

use crate::memory::Mem;

const LCD_CONTROL_ADDESS: u16 = 0xFF40;
const LCD_STATUS_ADDRESS: u16 = 0xFF41;
const LY_ADDRESS: u16 = 0xFF44;
const LYC_ADDRESS: u16 = 0xFF45;
const INTERRUPT_FLAG_ADDRESS: u16 = 0xFF0F;

const TILE_MAP_AREA_0_START: u16 = 0x9800;
const TILE_MAP_AREA_0_END: u16 = 0x9BFF;
const TILE_MAP_AREA_1_START: u16 = 0x9C00;
const TILE_MAP_AREA_1_END: u16 = 0x9FFF;

const TILE_DATA_AREA_0_START: u16 = 0x8800;
const TILE_DATA_AREA_0_END: u16 = 0x97FF;
const TILE_DATA_AREA_1_START: u16 = 0x8000;
const TILE_DATA_AREA_1_END: u16 = 0x8FFF;

const WINDOW_Y_ADDRESS: u16 = 0xFF4A;
const WINDOW_W_ADDRESS: u16 = 0xFF4B;

const SC_Y_ADDRESS: u16 = 0xFF42;
const SC_W_ADDRESS: u16 = 0xFF43;

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
    window_background_priority: bool,
}

impl LcdControl {
    fn window_tile_map_area_address(&self) -> (u16, u16) {
        if self.window_tile_map_area {
            return (TILE_MAP_AREA_1_START, TILE_MAP_AREA_1_END);
        } else {
            return (TILE_MAP_AREA_0_START, TILE_MAP_AREA_0_END);
        }
    }
    fn background_tile_map_area_address(&self) -> (u16, u16) {
        if self.background_tile_map_area {
            return (TILE_MAP_AREA_1_START, TILE_MAP_AREA_1_END);
        } else {
            return (TILE_MAP_AREA_0_START, TILE_MAP_AREA_0_END);
        }
    }

    fn tile_data_area_address(&self) -> (u16, u16) {
        if self.tile_area {
            return (TILE_DATA_AREA_1_START, TILE_DATA_AREA_1_END);
        } else {
            return (TILE_DATA_AREA_0_START, TILE_DATA_AREA_0_END);
        }
    }

    fn obj_size_data(&self) -> (u16, u16) {
        if self.obj_size {
            return (16, 16);
        } else {
            return (8, 8);
        }
    }
}

pub enum PpuMode {
    OamScan,       // 2
    DrawingPixels, // 3
    Hblank,        // 0
    Vblank,        // 1
}

pub struct Ppu {
    pub memory: Rc<RefCell<Mem>>,
    pub current_dots: u16,
    pub mode: PpuMode,
    pub ly: u8,
    pub lcd_control: LcdControl,
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
                window_background_priority: true,
            },
        };
    }

    // LCD CONTROL REGISTER
    // 7        6                    5               4                  3             2         1          0
    // enable   window tile map      window enable   bg & window tiles  bg tile map   obj size  obj enable bg window prio
    fn update_lcd_control(&mut self) {
        let lcd_control = self.memory.borrow().read_byte(LCD_CONTROL_ADDESS);
        self.lcd_control = LcdControl {
            ppu_enable: (lcd_control >> 7) & 0x01 == 1,
            window_tile_map_area: (lcd_control >> 6) & 0x01 == 1,
            window_enable: (lcd_control >> 5) & 0x01 == 1,
            tile_area: (lcd_control >> 4) & 0x01 == 1,
            background_tile_map_area: (lcd_control >> 3) & 0x01 == 1,
            obj_size: (lcd_control >> 2) & 0x01 == 1,
            obj_enable: (lcd_control >> 1) & 0x01 == 1,
            window_background_priority: lcd_control & 0x01 == 1,
        }
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

    pub fn check_mode(&mut self, cycles: u16) -> Option<[u8; 160]> {
        self.ly = self.memory.borrow().read_byte(LYC_ADDRESS);
        self.update_lcd_control();
        let new_dots = self.current_dots + cycles;
        let mut updated_line: Option<[u8; 160]> = None;

        match self.mode {
            PpuMode::OamScan => {
                if new_dots >= 80 {
                    self.mode = PpuMode::DrawingPixels;
                    self.current_dots = new_dots - 80;
                }
            }
            PpuMode::DrawingPixels => {
                if new_dots >= 289 {
                    self.mode = PpuMode::Hblank;
                    self.current_dots = new_dots - 289;
                    self.stat_interrupt(StatInterruptReason::HblankModeEnabled);
                    updated_line = Some(self.update_scan_line());
                }
            }
            PpuMode::Hblank => {
                if new_dots >= 87 {
                    let mut interrupt_reason = StatInterruptReason::OamScanModeEnabled;
                    self.mode = PpuMode::OamScan;
                    self.current_dots = new_dots - 87;
                    self.ly += 1;
                    if self.ly == 144 {
                        self.mode = PpuMode::Vblank;
                        interrupt_reason = StatInterruptReason::VblankModeEnabled;
                    }
                    self.stat_interrupt(interrupt_reason);
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
                }
            }
        }
        if self.ly == self.memory.borrow().read_byte(LYC_ADDRESS) {
            self.stat_interrupt(StatInterruptReason::LycEqualLy);
        };
        self.memory.borrow_mut().write_byte(LY_ADDRESS, self.ly);
        return updated_line;
    }

    fn update_scan_line(&mut self) -> [u8; 160] {
        // check if window enabled
        // if window enabled check if current dot on current line is in window
        // check appropriate tilemap to see what tile needs to be fetched
        // fetch said tile and put it into a buffer
        let mut pixel_buffer: [u8; 160] = [0; 160];
        let wx = self.memory.borrow().read_byte(WINDOW_W_ADDRESS);
        let wy = self.memory.borrow().read_byte(WINDOW_Y_ADDRESS);
        let sx = self.memory.borrow().read_byte(SC_W_ADDRESS);
        let sy = self.memory.borrow().read_byte(SC_Y_ADDRESS);
        let mut tile_map_address: (u16, u16);
        let mut col: u16;
        let mut row: u16;
        for x in 0..160 {
            if self.lcd_control.window_enable && x >= wx - 7 && self.ly >= wy {
                tile_map_address = self.lcd_control.window_tile_map_area_address();
                col = x as u16 - wx as u16;
                row = self.ly as u16 - wy as u16;
            } else {
                tile_map_address = self.lcd_control.background_tile_map_area_address();
                col = x as u16 + sx as u16;
                row = self.ly as u16 + sy as u16;
            }
            let tile_index = self
                .memory
                .borrow()
                .read_byte(tile_map_address.0 + col / 8 + (row / 8) * 32);
            let tile_address = if self.lcd_control.tile_area {
                // use unsigned indexing
                self.lcd_control.tile_data_area_address().0 + tile_index as u16 * 16
            } else {
                // use signed indexing
                (self.lcd_control.tile_data_area_address().0 as i32 + (tile_index as i8) as i32)
                    as u16
            };
            let tile_data_l = self.memory.borrow().read_byte(tile_address + 2 * (row / 8));
            let tile_data_h = self
                .memory
                .borrow()
                .read_byte(tile_address + 2 * (row / 8) + 1);
            let pix_l = if tile_data_l & (0x80 >> (7 - (col % 8))) != 0 {
                1
            } else {
                0
            };
            let pix_h = if tile_data_h & (0x80 >> (7 - (col % 8))) != 0 {
                2
            } else {
                0
            };
            let color = match pix_l | pix_h {
                0 => 0xFF, // white
                1 => 0xC0, //light
                2 => 0x60, // dark
                3 => 0x00, // black
                _ => 0,
            };
        }
        return pixel_buffer;
    }
}
