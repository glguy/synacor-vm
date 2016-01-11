extern crate byteorder;

use std::env::args;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::io::stdin;
use std::io::stdout;
use std::path::Path;

/// The virtual machine memory is 0x8000 elements followed by 8 registers.
type Memory = [u16; 0x8008];

/// The default memory image name used when no arguments are provided.
const DEFAULT_BIN_NAME: &'static str = "challenge.bin";

/// Loads the memory image from the file at the given path.
fn open_program<P: AsRef<Path>>(path: P) -> Memory {
    let mut file = File::open(path).expect("open_program");
    read_program(&mut file)
}

/// Loads the memory image from the given file.
fn read_program<F: Read>(file: &mut F) -> Memory {

    use byteorder::{LittleEndian, ReadBytesExt};

    let mut mem = [0; 0x8008];

    for i in 0 .. 0x8000 {
        match file.read_u16::<LittleEndian>().ok() {
            Some(x) => mem[i] = x,
            None    => break
        }
    }

    mem
}

/// Read memory at the given index. If the value is less than 0x8000 then
/// it is a literal and is returned. Otherwise it is a register address and
/// the value of the register is returned.
fn get(mem: &Memory, i: u16) -> u16 {
    let v = mem[i as usize];
    if (v & 0x8000) == 0 {
        v
    } else {
        mem[v as usize]
    }
}

/// Assign the value `x` into the register specified by the value in memory
/// at index `i`.
fn set(mem: &mut Memory, i: u16, x: u16) {
    mem[mem[i as usize] as usize] = x
}

/// Attempt to read a single byte of input from stdin.
fn read_input() -> Option<u8> {
    stdin().bytes().next().and_then(Result::ok)
}

/// Start executing program loaded in memory at PC 0
fn vm(mem: &mut Memory) {
    let mut pc = 0;
    let mut stack = Vec::new();

    loop {
        match mem[pc as usize] {
            // HALT
            0 => return,
            // SET
            1 => {
                let b = get(mem, pc+2);
                set(mem, pc+1, b);
                pc += 3
            },
            // PUSH
            2 => {
                stack.push(get(mem, pc+1));
                pc += 2
            },
            // POP
            3 =>
                match stack.pop() {
                    Some(x) => {
                        set(mem, pc+1, x);
                        pc += 2
                    },
                    None => return
                },
            // EQ
            4 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                let r = if b == c { 1 } else { 0 };
                set(mem, pc+1, r);
                pc += 4
            },
            // GT
            5 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                let r = if b > c { 1 } else { 0 };
                set(mem, pc+1, r);
                pc += 4
            },
            // JMP
            6 => pc = get(mem, pc+1),
            // JT
            7 => pc = if get(mem, pc+1) > 0 { get(mem, pc+2) } else { pc + 3 },
            // JF
            8 => pc = if get(mem, pc+1) == 0 { get(mem, pc+2) } else { pc + 3 },
            // ADD
            9 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                set(mem, pc+1, (b+c) % 0x8000);
                pc += 4
            },
            // MULT
            10 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                set(mem, pc+1, b.wrapping_mul(c) % 0x8000);
                pc += 4
            },
            // MOD
            11 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                set(mem, pc+1, b%c);
                pc += 4
            },
            // AND
            12 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                set(mem, pc+1, b&c);
                pc += 4
            },
            // OR
            13 => {
                let b = get(mem, pc+2);
                let c = get(mem, pc+3);
                set(mem, pc+1, b|c);
                pc += 4
            },
            // NOT
            14 => {
                let b = get(mem, pc+2);
                set(mem, pc+1, b ^ 0x7fff);
                pc += 3
            },
            // RMEM
            15 => {
                let x = mem[get(mem, pc+2) as usize];
                set(mem, pc+1, x);
                pc += 3
            },
            // WMEM
            16 => {
                let a = get(mem, pc+1);
                let b = get(mem, pc+2);
                mem[a as usize] = b;
                pc += 3
            },
            // CALL
            17 => {
                stack.push(pc+2);
                pc = get(mem, pc+1)
            },
            // RET
            18 =>
                match stack.pop() {
                    Some(addr) => pc = addr,
                    None       => return
                },
            // OUT
            19 => {
                let c = get(mem,pc+1);
                if stdout().write_all(&[c as u8]).is_err() {
                    return
                }
                pc += 2
            },
            // IN
            20 => {
                match read_input() {
                    Some(x) => set(mem, pc+1, x as u16),
                    None => return,
                };
                pc += 2
            },
            // NO OP
            21 => pc += 1,
            // BAD
            _ => return
        }
    }
}

fn main() {
    let path = args().nth(1).unwrap_or(DEFAULT_BIN_NAME.to_string());
    vm(&mut open_program(path));
}
