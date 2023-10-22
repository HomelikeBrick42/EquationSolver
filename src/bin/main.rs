use std::{
    iter::{FusedIterator, Peekable},
    str::CharIndices,
};

use anyhow::Result;

fn main() -> Result<()> {
    while let Some(line) = {
        use std::io::Write;

        print!("> ");
        std::io::stdout().flush()?;
        std::io::stdin().lines().next().transpose()?
    } {
        let line = line.trim();
        println!("{line}");
    }
    Ok(())
}

pub enum Token<'source> {
    Number(isize),
    Name(&'source str),
    Plus,
    Minus,
    Asterisk,
    OpenParenthesis,
    CloseParenthesis,
}

pub struct Lexer<'source> {
    source: &'source str,
    chars: Peekable<CharIndices<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<(usize, Token<'source>), String>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return Some(Ok(match self.chars.next() {
                Some((_, c)) if c.is_whitespace() => {
                    continue;
                }

                Some((i, c)) if c.is_ascii_digit() => {
                    // purposely allowing more characters to cause an error
                    while self
                        .chars
                        .next()
                        .map_or(false, |(_, c)| c.is_ascii_alphanumeric() || c == '_')
                    {
                        continue;
                    }

                    match self.source[i..self.chars.peek().map_or(self.source.len(), |&(i, _)| i)]
                        .parse()
                    {
                        Ok(value) => (i, Token::Number(value)),
                        Err(error) => {
                            return Some(Err(format!("Invalid integer at {i}: '{error}'")));
                        }
                    }
                }

                Some((i, c)) if c.is_alphabetic() || c == '_' => {
                    while self
                        .chars
                        .next()
                        .map_or(false, |(_, c)| c.is_alphanumeric() || c == '_')
                    {
                        continue;
                    }

                    let name =
                        &self.source[i..self.chars.peek().map_or(self.source.len(), |&(i, _)| i)];
                    (i, Token::Name(name))
                }

                Some((i, '+')) => (i, Token::Plus),
                Some((i, '-')) => (i, Token::Minus),
                Some((i, '*')) => (i, Token::Asterisk),
                Some((i, '(')) => (i, Token::OpenParenthesis),
                Some((i, ')')) => (i, Token::CloseParenthesis),

                Some((i, c)) => return Some(Err(format!("Unexpected character '{c}' at {i}"))),
                None => return None,
            }));
        }
    }
}

impl<'source> FusedIterator for Lexer<'source> where Peekable<CharIndices<'source>>: FusedIterator {}
