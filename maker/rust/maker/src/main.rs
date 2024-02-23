use clap::Parser;
use std::path::PathBuf;
use std::fs::File;
use std::io::{BufReader, BufRead, Error, Write, Seek, SeekFrom};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Mutex;
mod ip_value;
pub use self::ip_value::ToUIntIP;
use std::time::{SystemTime, UNIX_EPOCH, Instant};

const HEADER_LEN:u32 = 256;
const VECTOR_INDEX_LEN:u32 = 256*256*8;
const SEGMENT_INDEX_BLOCK_SIZE:u32 = 14;
static mut START_INDEX_POS:u32 = 0;
static mut END_INDEX_POS:u32 = 0;
const PROTOCAL:u16 = 2;
const INDEX_POLICY:u16 = 1;

lazy_static! {
    static ref REG_MAP: Mutex<HashMap<String, u32>> =  Mutex::new(HashMap::new());
}

lazy_static! {
    static ref V_SEG: Mutex<Vec<Segment>> = Mutex::new(Vec::new());
}

#[derive(Parser)]
#[command(author="Kevin Wang <wanglong.kevin@gmail.com>", version="2.0")]
#[command(help_template = " Author: {author-with-newline} {about-section}Version: {version} \n {usage-heading} {usage} \n {all-args} {tab}")]
struct Args {
    #[arg(short, long)]
    in_file: PathBuf,
    #[arg(short, long)]
    out_file: PathBuf,
}

struct Segment {
    sip: u32,
    eip: u32,
    reg: String,
}

#[derive(Debug, Clone, Copy)]
struct IndexBlock {
    first_pos: u32,
    last_pos: u32,
}

impl Segment {
    fn new(sip: u32, eip: u32, reg: String) -> Segment {
        Segment {sip, eip, reg}
    }
}

fn load_segments(in_file: PathBuf) -> std::io::Result<String> {
    let in_f = File::open(in_file)?;
    let reader = BufReader::new(in_f);
    let mut count = 0;
    let last_eip = 0;
    for line in reader.lines() {
        let line = line?;
        let v: Vec<&str> = line.splitn(3, '|').collect();
        if v.len() != 3 {
            panic!("invalid ip segment line '{}'", line)
        }
        let sip = v[0].to_u32_ip().expect(&format!("invalid ip address '{}' in line {}", v[0], line));
        let eip = v[1].to_u32_ip().expect(&format!("invalid ip address '{}' in line {}", v[0], line));
        if sip > eip {
            panic!("start ip({}) should not be greater than end ip({})", sip, eip);
        }
        if v[2].len() < 1 {
            panic!("empty region info in segment line `{}`", line);
        }
        // Check the continuity of data segment
        if last_eip != 0 && last_eip + 1 != sip {
            panic!("discontinuous data segment: last.eip+1!=seg.sip in line {}", line);
        }
        let segment = Segment::new(sip, eip, v[2].to_string());
        V_SEG.lock().unwrap().push(segment);

        count += 1;
    }
    return Ok(count.to_string());
}

fn write_region(out_fd: &mut File) -> std::io::Result<()> {
    out_fd.seek(SeekFrom::Start((HEADER_LEN + VECTOR_INDEX_LEN).into()))?;
    let v_seg = V_SEG.lock().unwrap();
    for seg in v_seg.iter() {
        if REG_MAP.lock().unwrap().get(&seg.reg) == None {
            let pos = out_fd.stream_position()?;
            REG_MAP.lock().unwrap().insert(seg.reg.to_string(), pos as u32);
            out_fd.write(seg.reg.as_bytes())?;
        }
    }
    return Ok(());
}

fn split_ip(sip: u32, eip: u32, reg: String) -> Vec<Segment> {
    let s1 = sip >> 24 & 0xFF;
    let s2 = sip >> 16 & 0xFF;
    //let s3 = sip >> 8 & 0xFF;
    //let s4 = sip & 0xFF;

    let e1 = eip >> 24 & 0xFF;
    let e2 = eip >> 16 & 0xFF;
    //let e3 = eip >> 8 & 0xFF;
    //let e4 = eip & 0xFF;

    let mut node_list: Vec<Segment> = Vec::new();

   // println!("split:{}.{}.{}.{}~{}.{}.{}.{}", s1,s2,s3,s4,e1,e2,e3,e4);
    for i in s1..e1+1 {
        for j in (if i == s1 {s2} else {0})..(if i == e1 {e2+1} else {256}){
            let s_ip = if i == s1 && j == s2 {
                sip
            }else {
                ((i << 24) & 0xff000000) | (j << 16 & 0xff0000)
            };
            let e_ip = if i == e1 && j == e2 {
                eip
            }else {
                ((i << 24) & 0xff000000) | ((j << 16) &0xff0000) | 0xffff
            };
        node_list.push(Segment{sip:s_ip, eip:e_ip, reg:reg.to_string()});
        }
    }

    return node_list;
}

fn set_vector_index(arr: &mut [[IndexBlock; 256]; 256], ip:u32, block_pos:u32) {
    let row: usize = (ip >> 24 & 0xff) as usize;
    let col: usize = (ip >> 16 & 0xff ) as usize;
    let vi_block = &mut arr[row][col];
    if vi_block.first_pos == 0 {
            vi_block.first_pos = block_pos;
    }
    vi_block.last_pos = block_pos + SEGMENT_INDEX_BLOCK_SIZE;
}

fn write_index_block(out_fd: &mut File) -> std::io::Result<()> {
    let v_seg = V_SEG.lock().unwrap();
    let mut index_arr :[[IndexBlock; 256]; 256] =  [[IndexBlock{first_pos: 0, last_pos: 0}; 256]; 256];
    for seg in v_seg.iter() {
        let pos = REG_MAP.lock().unwrap().get(&seg.reg).copied().unwrap();
        let node_list = split_ip(seg.sip, seg.eip, seg.reg.to_string());
        for node in node_list {
            let block_pos = out_fd.stream_position()?;
            out_fd.write(&node.sip.to_le_bytes())?;
            out_fd.write(&node.eip.to_le_bytes())?;
            out_fd.write(&(node.reg.len() as u16).to_le_bytes())?;
            out_fd.write(&pos.to_le_bytes())?;
            set_vector_index(&mut index_arr, node.sip, block_pos as u32);
            unsafe {
                if START_INDEX_POS == 0 {
                    START_INDEX_POS = block_pos as u32;
                }
                END_INDEX_POS = block_pos as u32;
            }
        }
    }        
    println!("try to write the segment index ptr ...");

    out_fd.seek(SeekFrom::Start(HEADER_LEN.into()))?;
    for i in 0..256 {
        for j in 0..256 {
            let index = index_arr[i][j];
            out_fd.write(&index.first_pos.to_le_bytes())?;
            out_fd.write(&index.last_pos.to_le_bytes())?;
        }
    }
    return Ok(());
}

fn write_header(out_fd: &mut File) -> std::io::Result<()> { 
    out_fd.seek(SeekFrom::Start(0))?;
    out_fd.write(&PROTOCAL.to_le_bytes())?;
    out_fd.write(&INDEX_POLICY.to_le_bytes())?;
    let now = SystemTime::now();
    let timestamp = now.duration_since(UNIX_EPOCH).expect("Time went backwards").as_secs() as u32;
    out_fd.write(&timestamp.to_le_bytes())?;
    unsafe {
        out_fd.write(&START_INDEX_POS.to_le_bytes())?;
        out_fd.write(&END_INDEX_POS.to_le_bytes())?;
    }

    return Ok(())

}

fn main() -> Result<(), Error> {
    let args = Args::parse();
    let now = Instant::now();
    match load_segments(args.in_file) {
        Ok(result) => println!("load {} lines", result),
        Err(err) => println!("{}", err),
    }
    let mut out_fd = File::create(args.out_file).unwrap();
    write_region(&mut out_fd)?;
    write_index_block(&mut out_fd)?;
    write_header(&mut out_fd)?;
    unsafe {
        println!("write done, dataBlocks: {}, IndexBlock: {}, indexPtr: ({}, {})",
        REG_MAP.lock().unwrap().len(),
        V_SEG.lock().unwrap().len(),
        START_INDEX_POS, END_INDEX_POS
        );
    }
    let sec = now.elapsed().as_secs();

    println!("Done, elpsed: {}m{}s", sec/60, sec%60);
    return Ok(());
}
