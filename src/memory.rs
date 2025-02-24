const ROM_BANK_0_START: u16 = 0;
const ROM_BANK_0_END: u16 = 0x3FFF;
const ROM_BANK_N_START: u16 = 0x4000;
const ROM_BANK_N_END: u16 = 0x7FFF;
const VRAM_START: u16 = 0x8000;
const VRAM_END: u16 = 0x9FFF;
const RAM_BANK_START: u16 = 0xA000;
const RAM_BANK_END: u16 = 0xBFFF;
const WRAM_START: u16 = 0xC000;
const WRAM_END: u16 = 0xDFFF;
const OAM_START: u16 = 0xFE00;
const OAM_END: u16 = 0xFE9F;
const IO_START: u16 = 0xFF00;
const IO_END: u16 = 0xFF7F;
const HRAM_START: u16 = 0xFF80;
const HRAM_END: u16 = 0xFFFE;

const ROM_BANK_SIZE: u16 = 16384;
const RAM_BANK_SIZE: u16 = 8192;

#[derive(Clone)]
pub struct Mem {
    pub rom: Vec<u8>,
    pub rom_bank_n_ptr: u32,
    pub ram_bank_n_ptr: u16,
    pub vram: [u8; 8192],
    pub cart_ram: [u8; 8192 * 3],
    pub wram: [u8; 16384],
    pub oam: [u8; 1024],
    pub io: [u8; 512],
    pub hram: [u8; 1024],
    pub interrupt_enabled: u8,
    pub joyselect: u8,
}

impl Mem {
    pub fn setup(cartridge: Vec<u8>) -> Self {
        let mut m = Mem {
            rom: cartridge,
            rom_bank_n_ptr: 0,
            ram_bank_n_ptr: 0,
            vram: [0; 8192],
            cart_ram: [0; 8192 * 3],
            wram: [0; 16384],
            oam: [0; 1024],
            io: [0; 512],
            hram: [0; 1024],
            interrupt_enabled: 0,
            joyselect: 0x30,
        };
        m.boot_up();
        return m;
    }

    pub fn boot_up(&mut self) {
        self.io[0x00] = 0xCF;
        self.io[0x01] = 0x00;
        self.io[0x02] = 0x7E;
        self.io[0x04] = 0xAB;
        self.io[0x05] = 0x00;
        self.io[0x06] = 0x00;
        self.io[0x07] = 0xF8;
        self.io[0x0F] = 0xE1;
        self.io[0x10] = 0x80;
        self.io[0x11] = 0xBF;
        self.io[0x12] = 0xF3;
        self.io[0x13] = 0xFF;
        self.io[0x14] = 0xBF;
        self.io[0x16] = 0x3F;
        self.io[0x17] = 0x00;
        self.io[0x18] = 0xFF;
        self.io[0x19] = 0xBF;
        self.io[0x1A] = 0x7F;
        self.io[0x1B] = 0xFF;
        self.io[0x1C] = 0x9F;
        self.io[0x1D] = 0xFF;
        self.io[0x1E] = 0xBF;
        self.io[0x20] = 0xFF;
        self.io[0x21] = 0x00;
        self.io[0x22] = 0x00;
        self.io[0x23] = 0xBF;
        self.io[0x24] = 0x77;
        self.io[0x25] = 0xF3;
        self.io[0x26] = 0xF1;
        self.io[0x40] = 0x91;
        self.io[0x41] = 0x85;
        self.io[0x42] = 0x00;
        self.io[0x43] = 0x00;
        self.io[0x44] = 0x00;
        self.io[0x45] = 0x00;
        self.io[0x46] = 0xFF;
        self.io[0x47] = 0xFC;
        self.io[0x48] = 0xFF;
        self.io[0x49] = 0xFF;
        self.io[0x4A] = 0x00;
        self.io[0x4B] = 0x00;
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        let val = match address {
            ROM_BANK_0_START..=ROM_BANK_0_END => self.rom[address as usize],
            ROM_BANK_N_START..=ROM_BANK_N_END => {
                self.rom[(self.rom_bank_n_ptr as u32 + address as u32) as usize]
            }
            VRAM_START..=VRAM_END => self.vram[(address - VRAM_START) as usize],
            RAM_BANK_START..=RAM_BANK_END => {
                self.cart_ram[(self.ram_bank_n_ptr + address - RAM_BANK_START) as usize]
            }
            WRAM_START..=WRAM_END => self.wram[(address - WRAM_START) as usize],
            OAM_START..=OAM_END => self.oam[(address - OAM_START) as usize],
            0xFF00 => self.io[0],
            IO_START..=IO_END => self.io[(address - IO_START) as usize],
            HRAM_START..=HRAM_END => self.hram[(address - HRAM_START) as usize],
            0xFFFF => self.interrupt_enabled,
            _ => 0,
        };
        return val;
    }
    pub fn read_word(&self, address: u16) -> u16 {
        return (self.read_byte(address) as u16)
            | ((self.read_byte(address.wrapping_add(1)) as u16) << 8);
    }

    pub fn write_byte(&mut self, address: u16, val: u8) {
        match address {
            0x2000..=0x3FFF => self.set_current_rom_bank(val),
            0x4000..=0x5FFF => self.set_current_ram_bank(val),
            VRAM_START..=VRAM_END => self.vram[(address - VRAM_START) as usize] = val,
            RAM_BANK_START..=RAM_BANK_END => {
                self.cart_ram[(self.ram_bank_n_ptr + address - RAM_BANK_START) as usize] = val
            }
            WRAM_START..=WRAM_END => self.wram[(address - WRAM_START) as usize] = val,
            OAM_START..=OAM_END => self.oam[(address - OAM_START) as usize] = val,
            0xFF46 => self.dma_transfer(val),
            0xFF00 => self.joyselect = val,
            0xFF04 => self.io[0x04] = 0,
            IO_START..=IO_END => self.io[(address - IO_START) as usize] = val,
            HRAM_START..=HRAM_END => self.hram[(address - HRAM_START) as usize] = val,
            0xFFFF => self.interrupt_enabled = val,
            _ => (),
        };
    }

    pub fn set_current_rom_bank(&mut self, val: u8) {
        let n = val & 0x7F;
        match n {
            0 => self.rom_bank_n_ptr = 0,
            _ => self.rom_bank_n_ptr = (n - 1) as u32 * ROM_BANK_SIZE as u32,
        };
    }
    pub fn set_current_ram_bank(&mut self, val: u8) {
        let n = val & 0x1F;
        match n {
            0 => self.ram_bank_n_ptr = 0,
            _ => self.ram_bank_n_ptr = n as u16 * RAM_BANK_SIZE,
        }
    }

    pub fn dma_transfer(&mut self, val: u8) {
        let src = (val as u16) << 8;
        for i in 0..=0x9F {
            let tocpy = self.read_byte(src + i as u16);
            self.write_byte(0xFE00 + i, tocpy);
        }
    }
}
