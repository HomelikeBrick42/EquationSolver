use crate::{
    lexer::{Lexer, Token},
    Atom, BasisElement, Expression, Term,
};
use std::iter::Peekable;

pub fn parse(source: &str) -> Result<Expression, String> {
    let mut lexer = Lexer::new(source).peekable();
    let expression = parse_expression(&mut lexer)?;
    if let Some(result) = lexer.next() {
        let (i, token) = result?;
        return Err(format!("Unexpected '{token}' at {i}"));
    }
    Ok(expression)
}

fn parse_expression(lexer: &mut Peekable<Lexer<'_>>) -> Result<Expression, String> {
    let mut terms = vec![];
    while {
        let term = parse_term(lexer)?;
        terms.push(term);

        if let Some(Ok((_, Token::Plus))) = lexer.peek() {
            lexer.next().transpose()?;
            true
        } else {
            false
        }
    } {}
    Ok(Expression(terms))
}

fn parse_term(lexer: &mut Peekable<Lexer<'_>>) -> Result<Term, String> {
    let mut atoms = vec![];
    while {
        while let Some(Ok((_, Token::Minus))) = lexer.peek() {
            lexer.next().transpose()?;
            atoms.push(Atom::Number(-1));
        }

        let atom = parse_atom(lexer)?;
        atoms.push(atom);

        if let Some(Ok((_, Token::Asterisk))) = lexer.peek() {
            lexer.next().transpose()?;
            true
        } else {
            false
        }
    } {}
    Ok(Term(atoms))
}

fn parse_atom(lexer: &mut Peekable<Lexer<'_>>) -> Result<Atom, String> {
    Ok(match lexer.next().transpose()? {
        Some((_, Token::Number(value))) => Atom::Number(value),
        Some((i, Token::Name(name))) => match name {
            "e0" => Atom::Basis(BasisElement::E0),
            "e1" => Atom::Basis(BasisElement::E1),
            "e2" => Atom::Basis(BasisElement::E2),
            "e3" => Atom::Basis(BasisElement::E3),
            "e4" => Atom::Basis(BasisElement::E4),
            _ if name
                .strip_prefix('e')
                .map_or(false, |number| number.parse::<isize>().is_ok()) =>
            {
                return Err(format!("Unknown basis element '{name}' at {i}"))
            }
            _ => Atom::Variable(name.to_string()),
        },
        Some((i, Token::OpenParenthesis)) => {
            let expression = parse_expression(lexer)?;
            match lexer.next().transpose()? {
                Some((_, Token::CloseParenthesis)) => Atom::Group(expression),
                Some((i, token)) => return Err(format!("Expected ')' but got '{token}' at {i}")),
                None => return Err(format!("Unclosed parenthesis opened at {i}")),
            }
        }
        Some((i, token)) => return Err(format!("Unexpected '{token}' at {i}")),
        None => return Err("Unexpected end of input when parsing atom".into()),
    })
}
