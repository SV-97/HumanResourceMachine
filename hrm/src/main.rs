use std::fs::File;
use std::io::prelude::*;
use std::env::args;

const ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 
    'f', 'g', 'h', 'i', 'j', 
    'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 
    'u', 'v', 'w', 'x', 'y', 
    'z',
];

const ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 
    'F', 'G', 'H', 'I', 'J', 
    'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 
    'U', 'V', 'W', 'X', 'Y', 
    'Z',
];

#[macro_use]
mod logger {
    use std::fmt::Display;
    use std::fmt::Debug;

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
                Color::White => "97".to_string()
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
                Modifier::DoubleUnderline => "21"
            }.to_string()
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
                BgColor::White => "107".to_string()
            }
        }
    }

    /// formats the items provided via the format_str and applies the modifiers from modifier_vec
    #[macro_export]
    macro_rules! colored {
        ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
            format!(
                "\x1b[{}m{}\x1b[0m",
                $modifier_vec.into_iter().map(|elem| elem.to_ansi())
                    .collect::<Vec<String>>().join(";"),
                format!($format_str, $( $item )*)
            )
        }
    }

    /// Create a vector of AnsiCode Trait objects
    #[macro_export]
    macro_rules! params {
        ( $($item:expr),* ) => {
            {
                let mut v: Vec<&dyn AnsiCode> = Vec::new();
                $(
                    v.push(&$item);
                )*
                v
            }
        }
    }

    /// Log the success of some event
    pub fn success<T: Debug, S: Display> (message: S, element: T) {
        println!("{} {} {}", 
            colored!("{}", vec!(Color::Green), '✔'), 
            message, 
            colored!("{:?}", params!(Color::Green, Modifier::Italic), element)
        );
    }

    /// Log an error of some kind
    pub fn error<T: Debug, S: Display> (message: S, element: T) {
        println!("{} {} {}",
            colored!("{}", vec!(&Color::Red), '✗'),
            message, colored!("{:?}", 
            params!(Color::Red, Modifier::Italic), element)
        );
    }

    /// Clear the terminal
    pub fn clear_screen () {
        println!("\x1b2J");
    }
}


mod tokenizer {
    use super::logger;
    use std::{error::Error, fmt, str::FromStr, string::ToString};

    #[derive(Debug)]
    pub struct InstructionParseError {
        text: String
    }
    impl InstructionParseError {
        pub fn new(text: String) -> Self {
            InstructionParseError{text}
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
        Jumpn
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
                text => Err(Self::Err::new(text.to_string()))
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
                Jumpn => "JUMPN"
            }.to_string()
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Operand {
        pub text: String,
        pub pointer: bool
    }

    impl Operand {
        pub fn new(text: String, pointer: bool) -> Self {
            Operand{text, pointer}
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum TokenType {
        Comment,
        Instruction{instruction: Instruction, operand: Option<Operand>},
        JumpMarker,
        /// size of grid
        GridDefinition(usize)
    }

    impl TokenType {
        pub fn new_instruction (instruction: Instruction, operand: Option<Operand>) -> Self {
            TokenType::Instruction{instruction, operand}
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Token {
        pub line_no: usize,
        pub tok_type: TokenType,
        pub text: String
    }

    impl Token {
        pub fn new(line_no: usize, tok_type: TokenType, text: String) -> Self {
            Token {line_no, tok_type, text}
        }

        fn new_grid_definition(line_no: usize, text: String, size: usize) -> Self {
            Self::new(line_no, TokenType::GridDefinition(size), text)
        }

        fn new_comment(line_no: usize, text: String) -> Self {
            Self::new(line_no, TokenType::Comment, text)
        }

        fn new_jump_marker(line_no: usize, text:String) -> Self {
            Self::new(line_no, TokenType::JumpMarker, text)
        }

        fn new_instruction(line_no: usize, text:String, instruction: Instruction, operand: Option<Operand>) -> Self {
            Self::new(line_no, TokenType::new_instruction(instruction, operand), text)
        }
    }

    pub struct Tokenizer {
        text: Vec<char>, // todo - rewrite to use a peekable iterator over chars
        pos: usize,
        current_char: Option<char>,
        line_no: usize
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
                line_no: 1
            }
        }

        /// Advance the pos-pointer into self.text
        fn advance(&mut self) {
            if self.current_char == Some('\n') { self.line_no += 1; }
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
                if char_.is_whitespace() { self.advance() } else { break }
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
            while let Some(current_char) = {self.advance(); self.current_char} {
                match current_char {
                    '~' => {self.advance(); break},
                    c if c.is_digit(10) => buf.push(c),
                    c => {logger::error("Invalid character in grid defition", c); panic!()}
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
                    ']' if !pointer => {logger::error("Closing brackets without opening ones", ']'); panic!()},
                    ']' if self.peek() == '\n' => {self.advance(); break},
                    '[' => pointer=true,
                    '\n' if pointer => {logger::error("Encountered newline while scanning pointer", '\n'); panic!()},
                    '\n' => {self.advance(); break},
                    c => operand_buf.push(c)
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
                    '\n' => {self.advance(); break},
                    c if c.is_whitespace() => {self.skip_whitespace(); operand_f=true; break},
                    c => buf.push(c)
                }
                self.advance()
            }
            let operand = if operand_f { Some(self.operand()) } else { None };
            let instruction = Instruction::from_str(&buf).unwrap();
            Token::new_instruction(line_no, buf, instruction, operand)
        }

        /// Parse a comment
        /// Comments are enclosed in a pair of "--"s like: -- This is a comment --
        fn comment(&mut self) -> Token {
            let line_no = self.line_no;
            let mut buf = String::new();
            self.advance(); // advance to second '-'
            while let Some(current_char) = {self.advance(); self.current_char} {
                match current_char {
                    '-' if self.peek() == '-' => {self.advance_n(2); break},
                    '\n' => {logger::error("Comment spans multiple lines.", buf); panic!()},
                    c => buf.push(c)
                }
            }
            Token::new_comment(line_no, buf)
        }

        fn jump_marker(&mut self) -> Token {
            let line_no = self.line_no;
            let mut buf = String::new();
            while let Some(current_char) = self.current_char {
                match current_char {
                    ':' => {self.advance(); break},
                    '\n' => {logger::error("Improperly terminated jump marker", buf); panic!()},
                    c => buf.push(c)
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
                    '\n' => {self.advance(); self.next()},
                    c if c.is_alphabetic() => Some(self.jump_marker()),
                    c => {logger::error("Unknown character in input", c); panic!()}
                }
            } else {
                None
            }
        }
    }
}

mod translator {
    use super::{
        tokenizer::{
            Instruction, Tokenizer, 
            Token, TokenType},
        ASCII_LOWER, ASCII_UPPER
    };
    use std::{iter::Iterator, collections::HashMap};

    pub fn translate(tokenizer: Tokenizer) -> String {
        fn get_identifier<T> (key: String, generator: &mut I, map: &mut HashMap<String, String>) -> String
            where I: Iterator<Item=String>
        {
            let key2 = key.clone();
            if map.get(&key).is_none() { map.insert(key, generator.next().unwrap()); }
            map.get(&key2).unwrap().to_string()
        }
        
        let mut globals = HashMap::new();
        let mut jmp_id_gen = ASCII_LOWER.iter().chain(ASCII_UPPER.iter()).map(|c| c.to_string());
        let mut inst_id_gen = None;
        let mut program_counter = 0;
        let mut  buf = String::from("-- HUMAN RESOURCE MACHINE PROGRAM --\n");
        for tok in tokenizer {
            match tok.tok_type {
                TokenType::GridDefinition(ramend) => inst_id_gen = Some(
                    (0..ramend).map(|n| n.to_string()).rev()),

                TokenType::Comment => { 
                    buf.push_str(&format!("--{}--", tok.text));
                    buf.push('\n') 
                },

                TokenType::Instruction{instruction, operand} => {
                    program_counter += 1;
                    let (text, pointer) = match operand {
                        Some(op) => {
                            let text = match instruction {
                                Instruction::Jump |
                                Instruction::Jumpz |
                                Instruction::Jumpn => get_identifier(op.text, &mut jmp_id_gen, &mut globals),
                                _ => get_identifier(op.text, inst_id_gen.as_mut().unwrap(), &mut globals)
                            };
                            (text, op.pointer)
                        },
                        None => ("".to_string(), false)
                    };
                    buf.push_str(&format!("    {: <9}", instruction.to_string()));
                    if pointer { 
                        buf.push_str(&format!("[{}]", text)) 
                    }
                    else { buf.push_str(&text) };
                    buf.push('\n');
                },
                TokenType::JumpMarker => {
                    let text = format!("{}:\n", get_identifier(tok.text, &mut jmp_id_gen, &mut globals)); 
                    buf.push_str(&text)
                }
            }
        }
        buf
    }
}

/// Read a the contents of a file relative to the project's root directory
fn read_file(file_name: &str) -> String {
    let args = args().collect::<Vec<String>>();
    let mut file = std::path::Path::new(&args[0]);
    while let Some(new_parent) = file.parent() { // find project root
        if new_parent.ends_with("hrm") {
            file = new_parent;
            break;
        }
        file = new_parent;
    }

    let mut test_source = file.to_path_buf();
    test_source.push(file_name);
    let path = &test_source;
    let mut file = File::open(path).expect("Failed to open file");
    let mut source = String::new();
    file.read_to_string(&mut source).expect("Failed to read file");
    source
}

fn main() {
    let input = read_file(&"test_source/simple_test.hrm");
    
    let tkn = tokenizer::Tokenizer::new(input);
    let output = translator::translate(tkn);
    println!("{}", output);
}
