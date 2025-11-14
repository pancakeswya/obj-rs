use std::{
    fs,
    io::{
        self,
        Read
    },
    iter,
    iter::Peekable,
    path::Path,
    slice::Iter
    ,
};

macro_rules! ascii_matches {
    ($expression:expr, $pattern:pat) => {
        if let Some(&&c) = $expression.peek() {
            matches!(c, $pattern)
        } else {
            false
        }
    }
}

macro_rules! ascii_matches_advance {
    ($expression:expr, $pattern:pat) => {
        if let Some(&c) = $expression.next() {
            matches!(c, $pattern)
        } else {
            false
        }
    }
}

macro_rules! ascii_eq_fn {
    ($expression:expr, $func:expr) => {
        if let Some(&&c) = $expression.peek() {
            $func(c)
        } else {
            false
        }
    }
}

macro_rules! ascii_eq_fn_advance {
    ($expression:expr, $func:expr) => {
        if let Some(&c) = $expression.next() {
            $func(c)
        } else {
            false
        }
    }
}

const MAX_POWER: usize = 20;
const BUFFER_SIZE: usize = 65536;

static POWER_10_POS: &'static [f64] = &[
    1.0e0,  1.0e1,  1.0e2,  1.0e3,  1.0e4,
    1.0e5,  1.0e6,  1.0e7,  1.0e8,  1.0e9,
    1.0e10, 1.0e11, 1.0e12, 1.0e13, 1.0e14,
    1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19,
];

static POWER_10_NEG: &'static [f64] = &[
    1.0e0,   1.0e-1,  1.0e-2,  1.0e-3,  1.0e-4,
    1.0e-5,  1.0e-6,  1.0e-7,  1.0e-8,  1.0e-9,
    1.0e-10, 1.0e-11, 1.0e-12, 1.0e-13, 1.0e-14,
    1.0e-15, 1.0e-16, 1.0e-17, 1.0e-18, 1.0e-19,
];

#[derive(Hash, Debug, Default, Copy, Clone)]
pub struct Facet {
    pub v: u32,
    pub n: u32,
    pub t: u32,
}

#[derive(Debug, Default)]
pub struct Mtl {
    pub name: String,
    pub map_ka: String,
    pub map_kd: String,
    pub map_ks: String,

    pub ns: f32,
    pub d: f32,
    pub ka: [f32; 3],
    pub kd: [f32; 3],
    pub ks: [f32; 3],
    pub ke: [f32; 3],
}

#[derive(Debug, Default)]
pub struct UseMtl {
    pub index: usize,
    pub offset: usize,
}

#[derive(Debug, Default)]
pub struct Vertices {
    pub n: Vec<f32>,
    pub t: Vec<f32>,
    pub v: Vec<f32>,
}

#[derive(Debug, Default)]
pub struct Data {
    pub dir_path: String,
    pub vertices: Vertices,
    pub facets: Vec<Facet>,
    pub use_mtls: Vec<UseMtl>,
    pub mtls: Vec<Mtl>,
}

impl PartialEq<Self> for Facet {
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v && self.n == other.n && self.t == other.t
    }
}

impl Eq for Facet {}

impl Mtl {
    fn new() -> Self {
        Mtl {
            ns: 32.0,
            d: 1.0,
            kd: [0.7; 3],
            ..Self::default()
        }
    }
}

#[inline(always)]
fn is_space(c: u8) -> bool {
    (c == b' ') || (c == b'\t') || (c == b'\r')
}

#[inline(always)]
fn is_exponent(c: u8) -> bool {
    c == b'e' || c == b'E'
}

#[inline(always)]
fn is_end_of_name(c: u8) -> bool {
    (c == b'\t') || (c == b'\r') || (c == b'\n')
}

fn skip_space(it: &mut Peekable<Iter<u8>>) {
    while ascii_eq_fn!(it, is_space) {
        it.next();
    }
}

fn skip_line(it: &mut Peekable<Iter<u8>>) {
    for &c in it {
        if c == b'\n' {
            break;
        }
    }
}

#[inline(always)]
fn get_dir_path<P: AsRef<Path>>(path_str: P) -> String {
    path_str.as_ref()
        .parent()
        .and_then(|p| p.to_str())
        .map(String::from)
        .unwrap_or_default()
}

fn get_name(it: &mut Peekable<Iter<u8>>) -> String {
    skip_space(it);
    let mut name = String::new();
    while let Some(&&c) = it.peek() {
        if is_end_of_name(c) {
            break;
        }
        name.push(c as char);
        it.next();
    }
    name
}

#[inline(always)]
fn get_index<const COUNT: usize>(index: i64, vertices_len: usize) -> u32 {
    if index < 0 {
        (vertices_len / COUNT) as u32 - (-index) as u32
    } else if index > 0 {
        index as u32 - 1
    } else {
        0
    }
}

fn file_read_all(path: &String) -> Option<Vec<u8>> {
    if let Ok(mut file) = fs::File::open(&path) {
        let mut buffer = Vec::new();
        if let Ok(_) =file.read_to_end(&mut buffer) {
            return Some(buffer);
        }
    }
    None
}

fn parse_int(it: &mut Peekable<Iter<u8>>) -> i64 {
    skip_space(it);
    let sign = if let Some(&&b'-') = it.peek() {
        it.next();
        -1
    } else {
        1
    };
    let mut num = 0;
    while let Some(&&c) = it.peek() {
        if !c.is_ascii_digit() {
            break;
        }
        num = 10 * num + (c - b'0') as i64;
        it.next();
    }
    sign * num
}

fn parse_float(it: &mut Peekable<Iter<u8>>) -> f64 {
    skip_space(it);
    let sign = match it.peek() {
        Some(&&b'-') => { it.next(); -1.0 }
        Some(&&b'+') => { it.next(); 1.0 }
        _            => 1.0
    };
    let mut num: f64 = 0.0;

    while let Some(&&c) = it.peek() {
        if !c.is_ascii_digit() {
            break;
        }
        num = 10.0 * num + (c - b'0') as f64;
        it.next();
    }
    if let Some(&&c) = it.peek() {
        if c == b'.' {
            it.next();
        }
    }
    let mut fra: f64 = 0.0;
    let mut div: f64 = 1.0;

    while let Some(&&c) = it.peek() {
        if !c.is_ascii_digit() {
            break;
        }
        fra  = 10.0 * fra + (c - b'0') as f64;
        div *= 10.0;
        it.next();
    }
    num += fra / div;

    let powers: &[f64];
    let mut eval: usize;
    if let Some(&&c) = it.peek() {
        if is_exponent(c) {
            it.next();
            match it.peek() {
                Some(&&b'+') => {
                    powers = POWER_10_POS;
                    it.next();
                }
                Some(&&b'-') => {
                    powers = POWER_10_NEG;
                    it.next();
                }
                _ => powers = POWER_10_POS
            }
            eval = 0;
            while let Some(&&c) = it.peek() {
                if !c.is_ascii_digit() {
                    break;
                }
                eval = 10 * eval + (c - b'0') as usize;
                it.next();
            }
            num *= if eval >= MAX_POWER {
                0.0
            } else {
                powers[eval]
            };
        }
    }
    sign * num
}

#[inline(always)]
fn parse_index<const COUNT: usize>(
    it : &mut Peekable<Iter<u8>>,
    vertices_len: usize
) -> u32 {
    let index = parse_int(it);
    get_index::<COUNT>(index, vertices_len)
}

#[inline(always)]
fn parse_index_sep<const COUNT: usize>(
    it : &mut Peekable<Iter<u8>>,
    vertices_len: usize
) -> u32 {
    if ascii_matches!(it, b'/') {
        it.next();
        parse_index::<COUNT>(it, vertices_len)
    } else {
        0
    }
}

#[inline(always)]
fn parse_facet(it: &mut Peekable<Iter<u8>>, vertices: &Vertices) -> Facet {
    Facet{
        v: parse_index::<3>(it, vertices.v.len()),
        t: parse_index_sep::<2>(it, vertices.t.len()),
        n: parse_index_sep::<3>(it, vertices.n.len()),
    }
}

fn parse_facets(
    it: &mut Peekable<Iter<u8>>,
    facets: &mut Vec<Facet>,
    vertices: &Vertices
) {
    while !ascii_matches!(it, b'\n') {
        let facet = parse_facet(it, vertices);
        facets.push(facet);
        skip_space(it);
    }
}

fn parse_vertex<const COUNT: usize>(
    it: &mut Peekable<Iter<u8>>,
    vertices: &mut Vec<f32>
) {
    for _ in 0..COUNT {
        let vertex = parse_float(it) as f32;
        vertices.push(vertex);
        skip_space(it)
    }
}

fn parse_vertices(it: &mut Peekable<Iter<u8>>, vertices: &mut Vertices) {
    if let Some(&c) = it.next() {
        match c {
            b' ' | b'\t' => parse_vertex::<3>(it, &mut vertices.v),
            b'n'         => parse_vertex::<3>(it, &mut vertices.n),
            b't'         => parse_vertex::<2>(it, &mut vertices.t),
            _            => ()
        }
    }
}

fn parse_mtl_spec<'a, It: Iterator<Item=&'a mut f32>>(
    it: &mut Peekable<Iter<u8>>,
    mtl_it: It
) {
    for num in mtl_it {
        *num = parse_float(it) as f32
    }
}

fn parse_mtl_map(it: &mut Peekable<Iter<u8>>, dir_path: &String, mtl: &mut Mtl) {
    if !ascii_matches_advance!(it, b'K') {
        return;
    }
    if let Some(c) = it.next() {
        if ascii_eq_fn_advance!(it, is_space) {
            let map_opt = match c {
                b'a' => {
                    mtl.map_ka = get_name(it);
                    Some(&mut mtl.map_ka)
                }
                b'd' => {
                    mtl.map_kd = get_name(it);
                    Some(&mut mtl.map_kd)
                }
                b's' => {
                    mtl.map_ks = get_name(it);
                    Some(&mut mtl.map_ks)
                }
                _ => None
            };
            if let Some(map) = map_opt {
                if Path::new(map).is_relative() {
                    map.insert_str(0, dir_path);
                }
            }
        }
    }
}

fn parse_mtl_buffer(buffer: &Vec<u8>, dir_path: &String, mtls: &mut Vec<Mtl>) {
    let mut mtl = Mtl::new();
    let mut found_d = false;

    let it = &mut buffer.iter().peekable();
    while it.peek().is_some() {
        skip_space(it);
        if let Some(&&c)  =it.peek() {
            match c {
                b'n' => {
                    it.next();
                    if ascii_matches_advance!(it, b'e') &&
                        ascii_matches_advance!(it, b'w') &&
                        ascii_matches_advance!(it, b'm') &&
                        ascii_matches_advance!(it, b't') &&
                        ascii_matches_advance!(it, b'l') {
                        if !mtl.name.is_empty() {
                            mtls.push(mtl);
                            mtl = Mtl::new();
                        }
                        mtl.name = get_name(it);
                    }
                }
                b'K' => {
                    it.next();
                    if let Some(c) =it.next() {
                        match c {
                            b'a' => parse_mtl_spec(it, mtl.ka.iter_mut()),
                            b'd' => parse_mtl_spec(it, mtl.kd.iter_mut()),
                            b's' => parse_mtl_spec(it, mtl.ks.iter_mut()),
                            b'e' => parse_mtl_spec(it, mtl.ke.iter_mut()),
                            _ => ()
                        }
                    }
                }
                b'N' => {
                    it.next();
                    if ascii_matches_advance!(it, b's') {
                        parse_mtl_spec(it, iter::once(&mut mtl.ns));
                    }
                }
                b'T' => {
                    it.next();
                    if ascii_matches_advance!(it, b'r') {
                        let mut tr = 0.0;
                        parse_mtl_spec(it, iter::once(&mut tr));
                        if !found_d {
                            mtl.d = 1.0 - tr;
                        }
                    }
                }
                b'd' => {
                    it.next();
                    if ascii_eq_fn_advance!(it, is_space) {
                        parse_mtl_spec(it, iter::once(&mut mtl.d));
                        found_d = true;
                    }
                }
                b'm' => {
                    it.next();
                    if ascii_matches_advance!(it, b'a') &&
                        ascii_matches_advance!(it, b'p') &&
                        ascii_matches_advance!(it, b'_') {
                        parse_mtl_map(it, dir_path, &mut mtl);
                    }
                }
                _ => {}
            }
        }
        skip_line(it);
    }
    if !mtl.name.is_empty() {
        mtls.push(mtl)
    }
}

fn parse_mtls(
    it: &mut Peekable<Iter<u8>>,
    dir_path: &String,
    mtls: &mut Vec<Mtl>
) {
    let path = dir_path.to_owned() + &get_name(it);
    if let Some(buffer) = file_read_all(&path) {
        parse_mtl_buffer(&buffer, dir_path, mtls);
    }
}

fn parse_usemtl(
    it: &mut Peekable<Iter<u8>>,
    mtls: &Vec<Mtl>,
    facets: &Vec<Facet>,
    usemtls: &mut Vec<UseMtl>
) {
    let use_mtl_name = get_name(it);
    for (i, mtl) in mtls.iter().enumerate() {
        if mtl.name == use_mtl_name {
            usemtls.push(UseMtl{
                index: i,
                offset: 0,
            });
            if !facets.is_empty() {
                let usemtls_len = usemtls.len();
                usemtls[usemtls_len - 2].offset = facets.len();
            }
            break;
        }
    }
}

fn parse_buffer(it: &mut Peekable<Iter<u8>>, data: &mut Data) {
    while it.peek().is_some() {
        skip_space(it);
        if let Some(&c) = it.next() {
            match c {
                b'v' => parse_vertices(it, &mut data.vertices),
                b'f' => if ascii_matches!(it, b' ' | b'\t') {
                    parse_facets(it, &mut data.facets, &data.vertices)
                },
                b'm' => if ascii_matches_advance!(it, b't') &&
                    ascii_matches_advance!(it, b'l') &&
                    ascii_matches_advance!(it, b'l') &&
                    ascii_matches_advance!(it, b'i') &&
                    ascii_matches_advance!(it, b'b') &&
                    ascii_eq_fn_advance!(it, is_space) {
                    parse_mtls(it, &data.dir_path, &mut data.mtls);
                }
                b'u' =>  if ascii_matches_advance!(it, b's') &&
                    ascii_matches_advance!(it, b'e') &&
                    ascii_matches_advance!(it, b'm') &&
                    ascii_matches_advance!(it, b't') &&
                    ascii_matches_advance!(it, b'l') &&
                    ascii_eq_fn_advance!(it, is_space) {
                    parse_usemtl(it, &data.mtls, &data.facets, &mut data.use_mtls);
                }
                _    => ()
            }
        }
        skip_line(it);
    }
}

pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Data> {
    let mut data = Data {
        dir_path: get_dir_path(path.as_ref()) + "/",
        ..Data::default()
    };
    let mut buffer = vec![0u8; 2 * BUFFER_SIZE];
    let mut file = fs::File::open(path.as_ref())?;
    let mut start = 0;
    loop {
        let read = file.read(
            &mut buffer[start..start + BUFFER_SIZE]
        )?;
        if read == 0 && start == 0 {
            break;
        }
        let mut end = start + read;
        if read == 0 ||
            (read < BUFFER_SIZE && buffer[end - 1] != b'\n') {
            buffer[end] = b'\n';
            end += 1;
        }
        let mut last = end;
        while last > 0 {
            last -= 1;
            if buffer[last] == b'\n' {
                break;
            }
        }
        if buffer[last] != b'\n' {
            break;
        }
        last += 1;
        parse_buffer(
            buffer[..last]
                .iter()
                .peekable()
                .by_ref(),
            &mut data,
        );
        start = end - last;
        buffer.copy_within(last..end, 0);
    }
    if !data.use_mtls.is_empty() {
        data.use_mtls.
            last_mut().
            unwrap().offset = data.facets.len();
    }
    Ok(data)
}
