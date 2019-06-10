use std::env::args;
use std::fs::File;
use std::io::prelude::*;

const ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

const ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

const DEBUG: bool = true;

#[macro_use]
#[allow(dead_code)]
pub mod logger {
    use std::fmt::{Debug, Display};

    pub trait AnsiCode {
        /// maps self to the corresponding ansi code
        fn to_ansi(&self) -> String;
    }

    /// Text colors
    pub enum Color {
        Black,
        DarkRed,
        DarkGreen,
        Grey,
        White,
        Green,
        Red,
        Yellow,
        Blue,
        Magenta,
        Cyan,
        Rgb(u8, u8, u8),
        Default,
    }

    impl AnsiCode for Color {
        fn to_ansi(&self) -> String {
            match self {
                Color::Black => "30".to_string(),
                Color::DarkRed => "31".to_string(),
                Color::DarkGreen => "32".to_string(),
                Color::Grey => "37".to_string(),
                Color::Rgb(r, g, b) => format!("38;2;{};{};{}", r, g, b),
                Color::Default => "39".to_string(),
                Color::Red => "91".to_string(),
                Color::Green => "92".to_string(),
                Color::Yellow => "93".to_string(),
                Color::Blue => "94".to_string(),
                Color::Magenta => "95".to_string(),
                Color::Cyan => "96".to_string(),
                Color::White => "97".to_string(),
            }
        }
    }

    /// Various ansi code text modifiers
    pub enum Modifier {
        Reset,
        Bold,
        Faint,
        Italic,
        Underline,
        SlowBlink,
        RapidBlink,
        Reverse,
        CrossedOut,
        DoubleUnderline,
    }

    impl AnsiCode for Modifier {
        fn to_ansi(&self) -> String {
            match self {
                Modifier::Reset => "0",
                Modifier::Bold => "1",
                Modifier::Faint => "2",
                Modifier::Italic => "3",
                Modifier::Underline => "4",
                Modifier::SlowBlink => "5",
                Modifier::RapidBlink => "6",
                Modifier::Reverse => "7",
                Modifier::CrossedOut => "9",
                Modifier::DoubleUnderline => "21",
            }
            .to_string()
        }
    }

    /// Background color
    pub enum BgColor {
        Black,
        DarkRed,
        DarkGreen,
        Grey,
        White,
        Green,
        Red,
        Yellow,
        Blue,
        Magenta,
        Cyan,
        Rgb(u8, u8, u8),
        Default,
    }

    impl AnsiCode for BgColor {
        fn to_ansi(&self) -> String {
            match self {
                BgColor::Black => "40".to_string(),
                BgColor::DarkRed => "41".to_string(),
                BgColor::DarkGreen => "42".to_string(),
                BgColor::Grey => "47".to_string(),
                BgColor::Rgb(r, g, b) => format!("48;2;{};{};{}", r, g, b),
                BgColor::Default => "49".to_string(),
                BgColor::Red => "101".to_string(),
                BgColor::Green => "102".to_string(),
                BgColor::Yellow => "103".to_string(),
                BgColor::Blue => "104".to_string(),
                BgColor::Magenta => "105".to_string(),
                BgColor::Cyan => "106".to_string(),
                BgColor::White => "107".to_string(),
            }
        }
    }

    /// formats the items provided via the format_str and applies the modifiers from modifier_vec
    /// or alternatively just applies modifiers to a provided string
    #[macro_export]
    macro_rules! colored {
        ( $string:expr, $modifier_vec:expr) => {
            format!(
                "\x1b[{}m{}\x1b[0m",
                $modifier_vec.into_iter().map(|elem| elem.to_ansi())
                    .collect::<Vec<String>>().join(";"),
                $string
            )
        };
        ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
            format!(
                "\x1b[{}m{}\x1b[0m",
                $modifier_vec.into_iter().map(|elem| elem.to_ansi())
                    .collect::<Vec<String>>().join(";"),
                format!($format_str, $( $item )*)
            )
        };
    }

    #[macro_export]
    macro_rules! println_colored {
        ( $format_str:expr, $modifier_vec:expr) => {
            println!("{}" colored!($format_str, $modifier_vec));
        };
        ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
            println!("{}", colored!($format_str, $modifier_vec, $( $item )*));
        };
    }

    /// Create a vector of AnsiCode Trait objects
    #[macro_export]
    macro_rules! params {
        ( $($item:expr),* ) => {
            {
                let mut v: Vec<&dyn crate::logger::AnsiCode> = Vec::new();
                $(
                    v.push(&$item);
                )*
                v
            }
        }
    }

    /// Log the success of some event
    pub fn success<T: Debug, S: Display>(message: S, element: Option<T>) {
        if let Some(element) = element {
            println!(
                "{} {} {}",
                colored!("{}", vec!(Color::Green), '✔'),
                message,
                colored!("{:?}", params!(Color::Green, Modifier::Italic), element)
            );
        } else {
            println!("{} {}", colored!("{}", vec!(Color::Green), '✔'), message);
        }
    }

    /// Log an error of some kind
    pub fn error<T: Debug, S: Display>(message: S, element: Option<T>) {
        if let Some(element) = element {
            println!(
                "{} {} {}",
                colored!("{}", vec!(&Color::Red), '✗'),
                message,
                colored!("{:?}", params!(Color::Red, Modifier::Italic), element)
            );
        } else {
            println!("{} {}", colored!("{}", vec!(&Color::Red), '✗'), message);
        }
    }

    /// Clear the terminal
    pub fn clear_screen() {
        println!("\x1b2J");
    }
}

pub mod tokenizer {
    use super::logger;
    use std::{
        convert::{From, TryFrom},
        error::Error,
        fmt,
        str::FromStr,
        string::ToString,
    };

    #[derive(Debug)]
    pub struct InstructionParseError {
        text: String,
    }
    impl InstructionParseError {
        pub fn new(text: String) -> Self {
            InstructionParseError { text }
        }
    }
    impl Error for InstructionParseError {}
    impl fmt::Display for InstructionParseError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "Unknown Instruction: {}", self.text)
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Instruction {
        Inbox,
        Outbox,
        Copyfrom,
        Copyto,
        Add,
        Sub,
        Bumpup,
        Bumpdn,
        Jump,
        Jumpz,
        Jumpn,
    }

    impl Instruction {
        /// Legacy - use `&<[u8; 1]>::from(instruction)` instead
        pub fn to_bytes(self) -> [u8; 1] {
            (self as u8).to_be_bytes()
        }
    }

    impl FromStr for Instruction {
        type Err = InstructionParseError;

        fn from_str(string: &str) -> Result<Self, Self::Err> {
            use Instruction::*;
            match string {
                "INBOX" => Ok(Inbox),
                "OUTBOX" => Ok(Outbox),
                "COPYFROM" => Ok(Copyfrom),
                "COPYTO" => Ok(Copyto),
                "ADD" => Ok(Add),
                "SUB" => Ok(Sub),
                "BUMPUP" => Ok(Bumpup),
                "BUMPDN" => Ok(Bumpdn),
                "JUMP" => Ok(Jump),
                "JUMPZ" => Ok(Jumpz),
                "JUMPN" => Ok(Jumpn),
                text => Err(Self::Err::new(text.to_string())),
            }
        }
    }

    impl ToString for Instruction {
        fn to_string(&self) -> String {
            use Instruction::*;
            match self {
                Inbox => "INBOX",
                Outbox => "OUTBOX",
                Copyfrom => "COPYFROM",
                Copyto => "COPYTO",
                Add => "ADD",
                Sub => "SUB",
                Bumpup => "BUMPUP",
                Bumpdn => "BUMPDN",
                Jump => "JUMP",
                Jumpz => "JUMPZ",
                Jumpn => "JUMPN",
            }
            .to_string()
        }
    }

    impl From<Instruction> for u8 {
        fn from(instruction: Instruction) -> Self {
            use Instruction::*;
            match instruction {
                Inbox => 0,
                Outbox => 1,
                Copyfrom => 2,
                Copyto => 3,
                Add => 4,
                Sub => 5,
                Bumpup => 6,
                Bumpdn => 7,
                Jump => 8,
                Jumpz => 9,
                Jumpn => 10,
            }
        }
    }

    impl From<Instruction> for [u8; 1] {
        fn from(inst: Instruction) -> Self {
            (inst as u8).to_be_bytes()
        }
    }

    impl TryFrom<&[u8]> for Instruction {
        type Error = &'static str;

        fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
            Instruction::try_from(bytes[0])
        }
    }

    impl TryFrom<u8> for Instruction {
        type Error = &'static str;

        fn try_from(byte: u8) -> Result<Self, Self::Error> {
            Instruction::try_from(&byte)
        }
    }

    impl TryFrom<&u8> for Instruction {
        type Error = &'static str;

        fn try_from(byte: &u8) -> Result<Self, Self::Error> {
            use Instruction::*;
            match byte {
                0 => Ok(Inbox), // Would be nice to be able to use Inbox.into() etc. here
                1 => Ok(Outbox),
                2 => Ok(Copyfrom),
                3 => Ok(Copyto),
                4 => Ok(Add),
                5 => Ok(Sub),
                6 => Ok(Bumpup),
                7 => Ok(Bumpdn),
                8 => Ok(Jump),
                9 => Ok(Jumpz),
                10 => Ok(Jumpn),
                _ => Err("Unknown instruction"),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Operand {
        pub text: String,
        pub pointer: bool,
    }

    impl Operand {
        pub fn new(text: String, pointer: bool) -> Self {
            Operand { text, pointer }
        }
    }

    impl From<Operand> for [u8; 2] {
        fn from(op: Operand) -> Self {
            let mut val = (&op.text).parse::<u16>().expect(&format!(
                "Tried converting a malformatted string to bytes {}",
                op.text
            ));
            if op.pointer {
                val |= 0x1000;
            }
            val.to_be_bytes()
        }
    }

    impl TryFrom<&[u8]> for Operand {
        type Error = &'static str;

        fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
            match bytes.len() {
                1 => Ok(Operand::new(bytes[0].to_string(), false)),
                2 => {
                    let pointer = bytes[0] & 0x80 == 0x80;
                    let data = ((bytes[0] & 0x7F) as u16) << 8 | (bytes[1] as u16);
                    Ok(Operand::new(data.to_string(), pointer))
                }
                _ => Err("Too many bytes in input"),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum TokenType {
        Comment,
        Instruction {
            instruction: Instruction,
            operand: Option<Operand>,
        },
        JumpMarker,
        /// size of grid
        GridDefinition(usize),
    }

    impl TokenType {
        pub fn new_instruction(instruction: Instruction, operand: Option<Operand>) -> Self {
            TokenType::Instruction {
                instruction,
                operand,
            }
        }
    }

    /// Get instruction corresponding to leftmost byte in input
    impl TryFrom<&[u8]> for TokenType {
        type Error = &'static str;

        fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
            match Instruction::try_from(bytes) {
                Ok(instruction) => {
                    let operand = match instruction {
                        Instruction::Inbox | Instruction::Outbox => None,
                        Instruction::Jump | Instruction::Jumpn | Instruction::Jumpz => {
                            Some(Operand::try_from(&bytes[1..2]).unwrap())
                        }
                        _ => Some(Operand::try_from(&bytes[1..3]).unwrap()),
                    };
                    Ok(TokenType::new_instruction(instruction, operand))
                }
                Err(text) => Err(text),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Token {
        pub line_no: usize,
        pub tok_type: TokenType,
        pub text: String,
    }

    impl Token {
        pub fn new(line_no: usize, tok_type: TokenType, text: String) -> Self {
            Token {
                line_no,
                tok_type,
                text,
            }
        }

        fn new_grid_definition(line_no: usize, text: String, size: usize) -> Self {
            Self::new(line_no, TokenType::GridDefinition(size), text)
        }

        fn new_comment(line_no: usize, text: String) -> Self {
            Self::new(line_no, TokenType::Comment, text)
        }

        fn new_jump_marker(line_no: usize, text: String) -> Self {
            Self::new(line_no, TokenType::JumpMarker, text)
        }

        fn new_instruction(
            line_no: usize,
            text: String,
            instruction: Instruction,
            operand: Option<Operand>,
        ) -> Self {
            Self::new(
                line_no,
                TokenType::new_instruction(instruction, operand),
                text,
            )
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Tokenizer {
        text: Vec<char>, // todo - rewrite to use a peekable iterator over chars
        pos: usize,
        current_char: Option<char>,
        line_no: usize,
    }

    impl Tokenizer {
        /// Constructor to create a new Tokenizer for the given text
        pub fn new(text: String) -> Tokenizer {
            let text = text.chars().collect::<Vec<char>>();
            let first_char = text[0];
            Tokenizer {
                text: text,
                pos: 0,
                current_char: Some(first_char),
                line_no: 1,
            }
        }

        /// Advance the pos-pointer into self.text
        fn advance(&mut self) {
            if self.current_char == Some('\n') {
                self.line_no += 1;
            }
            self.pos += 1;
            self.current_char = if self.pos > self.text.len() - 1 {
                None
            } else {
                Some(self.text[self.pos])
            };
        }

        /// advance n times
        fn advance_n(&mut self, n: usize) {
            for _ in 0..n {
                self.advance()
            }
        }

        fn peek(&self) -> char {
            self.text[self.pos + 1]
        }

        /// Skip all whitespace characters
        fn skip_whitespace(&mut self) {
            while let Some(char_) = self.current_char {
                if char_.is_whitespace() && char_ != '\n' {
                    self.advance()
                } else {
                    break;
                }
            }
        }

        /// Parse the a grid definition
        /// The grid definition defines how big the ingame grid is
        /// which equates to how many registers the final program will use
        /// panics on unwrap if current_char is None before a closing '~' is reached
        /// or if no valid size is provided between the two '~'s
        fn grid_definition(&mut self) -> Token {
            let line_no = self.line_no;
            let mut buf = String::new();
            while let Some(current_char) = {
                self.advance();
                self.current_char
            } {
                match current_char {
                    '~' => {
                        self.advance();
                        break;
                    }
                    c if c.is_digit(10) => buf.push(c),
                    c => {
                        logger::error("Invalid character in grid defition", Some(c));
                        panic!()
                    }
                }
            }
            let value = buf.parse().unwrap();
            Token::new_grid_definition(line_no, buf, value)
        }

        /// Parse an operand - current char should be the start of the identifier
        fn operand(&mut self) -> Operand {
            let mut operand_buf = String::new();
            let mut pointer = false;
            while let Some(current_char) = self.current_char {
                match current_char {
                    ']' if !pointer => {
                        logger::error(
                            "Closing brackets without opening ones",
                            None as Option<usize>,
                        );
                        panic!()
                    }
                    ']' if self.peek() == '\n' => {
                        self.advance();
                        break;
                    }
                    '[' => pointer = true,
                    '\n' if pointer => {
                        logger::error(
                            "Encountered newline while scanning pointer",
                            None as Option<usize>,
                        );
                        panic!()
                    }
                    '\n' => {
                        self.advance();
                        break;
                    }
                    c => operand_buf.push(c),
                }
                self.advance()
            }
            Operand::new(operand_buf, pointer)
        }

        fn instruction(&mut self) -> Token {
            let line_no = self.line_no;
            self.skip_whitespace();
            let mut buf = String::new();
            let mut operand_f = false;
            while let Some(current_char) = self.current_char {
                match current_char {
                    '\n' => {
                        self.advance();
                        break;
                    }
                    c if c.is_whitespace() => {
                        self.skip_whitespace();
                        if let Some(c) = self.current_char {
                            if c != '\n' {
                                operand_f = true;
                            }
                        }
                        break;
                    }
                    c => buf.push(c),
                }
                self.advance()
            }
            let operand = if operand_f {
                Some(self.operand())
            } else {
                None
            };
            let instruction = Instruction::from_str(&buf).unwrap();
            Token::new_instruction(line_no, buf, instruction, operand)
        }

        /// Parse a comment
        /// Comments are enclosed in a pair of "--"s like: -- This is a comment --
        fn comment(&mut self) -> Token {
            let line_no = self.line_no;
            let mut buf = String::new();
            self.advance(); // advance to second '-'
            while let Some(current_char) = {
                self.advance();
                self.current_char
            } {
                match current_char {
                    '-' if self.peek() == '-' => {
                        self.advance_n(2);
                        break;
                    }
                    '\n' => {
                        logger::error("Comment spans multiple lines.", Some(buf));
                        panic!()
                    }
                    c => buf.push(c),
                }
            }
            Token::new_comment(line_no, buf)
        }

        fn jump_marker(&mut self) -> Token {
            let line_no = self.line_no;
            let mut buf = String::new();
            while let Some(current_char) = self.current_char {
                match current_char {
                    ':' => {
                        self.advance();
                        break;
                    }
                    '\n' => {
                        logger::error("Improperly terminated jump marker", Some(buf));
                        panic!()
                    }
                    c => buf.push(c),
                }
                self.advance()
            }
            Token::new_jump_marker(line_no, buf)
        }
    }

    impl Iterator for Tokenizer {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(current_char) = self.current_char {
                match current_char {
                    '~' => Some(self.grid_definition()),
                    ' ' => Some(self.instruction()),
                    '-' => Some(self.comment()),
                    '\n' => {
                        self.advance();
                        self.next()
                    }
                    c if c.is_alphabetic() => Some(self.jump_marker()),
                    c => {
                        logger::error("Unknown character in input", Some(c));
                        panic!()
                    }
                }
            } else {
                None
            }
        }
    }
}

pub mod translator {
    use super::{
        tokenizer::{Instruction, Token, TokenType},
        ASCII_LOWER, ASCII_UPPER, DEBUG,
    };
    use std::{
        collections::HashMap,
        iter::{IntoIterator, Iterator},
    };

    pub fn get_identifier<I>(
        key: String,
        generator: &mut I,
        map: &mut HashMap<String, String>,
    ) -> String
    where
        I: Iterator<Item = String>,
    {
        let key2 = key.clone();
        if map.get(&key).is_none() {
            map.insert(key, generator.next().unwrap());
        }
        map.get(&key2).unwrap().to_string()
    }

    pub fn translate<T: IntoIterator<Item = Token>>(tokenizer: T) -> String {
        let mut globals = HashMap::new();
        let mut jmp_id_gen = ASCII_LOWER
            .iter()
            .chain(ASCII_UPPER.iter())
            .map(|c| c.to_string());
        let mut inst_id_gen = None;
        let mut program_counter = 0;
        let mut buf = String::from("-- HUMAN RESOURCE MACHINE PROGRAM --\n");
        for tok in tokenizer {
            match tok.tok_type {
                TokenType::GridDefinition(ramend) => {
                    inst_id_gen = Some((0..ramend).map(|n| n.to_string()).rev())
                }

                TokenType::Comment => {
                    buf.push_str(&format!("--{}--", tok.text));
                    buf.push('\n')
                }

                TokenType::Instruction {
                    instruction,
                    operand,
                } => {
                    if DEBUG {
                        program_counter += 1;
                        buf.push_str(&format!("-- Instruction {:03} --\n", program_counter));
                    }
                    let (text, pointer) = match operand {
                        Some(op) => {
                            let text = match instruction {
                                Instruction::Jump | Instruction::Jumpz | Instruction::Jumpn => {
                                    get_identifier(op.text, &mut jmp_id_gen, &mut globals)
                                }
                                _ => get_identifier(
                                    op.text,
                                    inst_id_gen.as_mut().unwrap(),
                                    &mut globals,
                                ),
                            };
                            (text, op.pointer)
                        }
                        None => ("".to_string(), false),
                    };
                    buf.push_str(&format!("    {: <9}", instruction.to_string()));
                    if pointer {
                        buf.push_str(&format!("[{}]", text))
                    } else {
                        buf.push_str(&text)
                    };
                    buf.push('\n');
                }
                TokenType::JumpMarker => {
                    if DEBUG {
                        buf.push_str(&format!("\n-- {} --\n", &tok.text));
                    }
                    let text = format!(
                        "{}:\n",
                        get_identifier(tok.text, &mut jmp_id_gen, &mut globals)
                    );
                    buf.push_str(&text)
                }
            }
        }
        buf.push_str("\n\n");
        buf
    }
}

pub mod bytecode {
    use super::{
        tokenizer::{Instruction, Operand, Token, TokenType},
        translator::get_identifier,
        ASCII_LOWER, ASCII_UPPER,
    };
    use std::{collections::HashMap, convert::TryFrom, iter::IntoIterator};

    /// Helper function for assemble that creates bytecode for instructions
    fn instruction(
        bytecodebuffer: &mut Vec<u8>,
        instruction: Instruction,
        operand: Option<Operand>,
        memory_map: &mut HashMap<String, u8>,
        to_patch: &mut HashMap<u8, String>,
        ip: &mut u8,
    ) {
        *ip += 1;
        let mut temp_buf: Vec<u8> = Vec::with_capacity(2);
        match operand {
            Some(op) => match instruction {
                Instruction::Jump | Instruction::Jumpz | Instruction::Jumpn => {
                    if memory_map.get(&op.text).is_none() {
                        to_patch.insert(*ip, op.text);
                        temp_buf.extend(&[0xFF]);
                    } else {
                        temp_buf.extend(&memory_map.get(&op.text).unwrap().to_be_bytes());
                    }
                    *ip += 1;
                }
                _ => {
                    temp_buf.extend(&<[u8; 2]>::from(op));
                    *ip += 2;
                }
            },
            None => (),
        };
        bytecodebuffer.extend(&<[u8; 1]>::from(instruction));
        bytecodebuffer.extend(temp_buf);
        ()
    }

    /// Helper function for assemble that creates bytecode for jump markers
    fn jump_marker(
        bytecodebuffer: &mut Vec<u8>,
        tok: &Token,
        memory_map: &mut HashMap<String, u8>,
        to_patch: &mut HashMap<u8, String>,
        ip: &mut u8,
    ) {
        if memory_map.contains_key(&tok.text) {
            panic!(
                "Multiple occurences of jump marker {:?} in input file",
                tok.text
            )
        }
        let text = tok.text.clone();
        memory_map.insert(text, *ip);
        for (key, value) in to_patch {
            if *value == tok.text {
                bytecodebuffer[*key as usize] = *ip;
            }
        }
    }

    /// Assemble a tokenstream into bytecode
    pub fn assemble<T: IntoIterator<Item = Token>>(tokenizer: T) -> Vec<u8> {
        let mut ip = 0; // Instruction pointer
        let mut memory_map = HashMap::new(); // Maps Jump markers like "a" to their memory location
        let mut to_patch = HashMap::new(); // Maps memory locations to labels that coulnd't be resolved yet
        let mut bytecodebuffer = Vec::new();

        for tok in tokenizer {
            match tok.tok_type {
                TokenType::Instruction {
                    instruction: inst,
                    operand,
                } => instruction(
                    &mut bytecodebuffer,
                    inst,
                    operand,
                    &mut memory_map,
                    &mut to_patch,
                    &mut ip,
                ),
                TokenType::JumpMarker => jump_marker(
                    &mut bytecodebuffer,
                    &tok,
                    &mut memory_map,
                    &mut to_patch,
                    &mut ip,
                ),
                _ => (),
            }
        }
        bytecodebuffer
    }

    pub fn disassemble(bytecode: &[u8]) -> String {
        /* Planned to do it generic but fails because TokenType::try_from isn't
        and I can't be bothered to change that right now
        where
        T: Index<Range<usize>> + Index<RangeFrom<usize>>,
        */
        use Instruction::*;
        let mut buf = Vec::new();
        let mut buf_to_bytes = HashMap::new(); // Mapping buffer indices to bytecode indices
        let mut bytes_to_names = HashMap::new(); // Mapping Bytecode indices as strings to names
        let mut jump_marker_gen = ASCII_LOWER
            .iter()
            .chain(ASCII_UPPER.iter())
            .map(|c| c.to_string());
        let mut ip = 0;
        while let Ok(TokenType::Instruction {
            instruction,
            operand,
        }) = TokenType::try_from(&bytecode[ip..])
        {
            buf_to_bytes.insert(buf.len(), ip);
            match instruction {
                instr @ Inbox | instr @ Outbox => {
                    ip += 1;
                    buf.push(format!("{: <9}", instr.to_string()))
                }
                instr @ Jump | instr @ Jumpz | instr @ Jumpn => {
                    ip += 2;
                    let marker = get_identifier(
                        operand.unwrap().text,
                        &mut jump_marker_gen,
                        &mut bytes_to_names,
                    );
                    buf.push(format!("{: <9}{}", instr.to_string(), marker));
                }
                instr => {
                    let op = operand.unwrap();
                    ip += 3;
                    let op_text = if op.pointer {
                        format!("[{}]", op.text)
                    } else {
                        op.text
                    };
                    buf.push(format!("{: <9}{}", instr.to_string(), op_text));
                }
            }
            if ip >= bytecode.len() - 1 {
                break;
            }
        }
        for (i_buf, i_bytes) in buf_to_bytes {
            if let Some(marker_name) = bytes_to_names.get(&i_bytes.to_string()) {
                buf[i_buf] = format!("{}:\n    {}", marker_name, buf[i_buf]);
            } else {
                buf[i_buf] = format!("    {}", buf[i_buf]);
            }
        }
        buf.join("\n")
    }

    pub struct CPU {
        code: &[u8],
        
    }
}

/// Get the root of the project's folder
pub fn get_root() -> std::path::PathBuf {
    let args = args().collect::<Vec<String>>();
    let mut file = std::path::Path::new(&args[0]);
    while let Some(new_parent) = file.parent() {
        // find project root
        if new_parent.ends_with("hrm") {
            file = new_parent;
            break;
        }
        file = new_parent;
    }
    file.to_path_buf()
}

/// Read a the contents of a file relative to the project's root directory
pub fn read_file(file_name: &str) -> String {
    let mut file = get_root();
    file.push(file_name);
    let path = &file;
    let mut file = File::open(path).expect("Failed to open file");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Failed to read file");
    source
}

/// Write a buffer to a file relative to the project's root directory
pub fn write_file(file_name: &str, data: &[u8]) {
    let mut file = get_root();
    file.push(file_name);
    let path = &file;
    let mut file = File::create(path).expect("Failed to create file");
    file.write_all(data).expect("Failed to write to file");
}

pub fn main() {
    let in_path = "test_source/simple_test.hrm";
    let out_path = "debug/out.hrm";
    let bytecode_path = "debug/out.chrm";

    let output1 = {
        let input = read_file(&in_path);
        let tkn = tokenizer::Tokenizer::new(input.clone());
        logger::success("Successfully tokenized input from", Some(in_path));
        let output = translator::translate(tkn);
        let data = output.bytes().collect::<Vec<u8>>();
        write_file(&out_path, &data[..]);
        /*
        println!(
            "{}",
            colored!("{}", params!(logger::Modifier::Faint), &output)
        );
        */
        logger::success("Successfully written output to", Some(out_path));
        output
    };
    println!("{}", colored!("{}", params!(logger::Modifier::Faint), output1));
    let bytecode = {
        let tkn2 = tokenizer::Tokenizer::new(output1);
        let bytes = bytecode::assemble(tkn2);
        write_file(&bytecode_path, &bytes[..]);
        logger::success("Successfully assembled and wrote bytecode to", Some(bytecode_path));
        bytes
    };
    println!("{}", colored!("{:?}", params!(logger::Modifier::Faint), bytecode));
    let disassembly = bytecode::disassemble(&bytecode);
    logger::success("Successfully disassembled Bytecode", None as Option<usize>);
    println!("{}", colored!("{}", params!(logger::Modifier::Faint), disassembly));
}
