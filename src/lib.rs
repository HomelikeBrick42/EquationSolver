#![deny(rust_2018_idioms)]

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expression(pub Vec<Term>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Term(pub Vec<Atom>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Atom {
    Number(isize),
    Variable(String),
    Basis(BasisElement),
    Group(Expression),
    Fraction {
        numerator: Expression,
        denominator: Expression,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BasisElement {
    E0,
    E1,
    E2,
}

impl BasisElement {
    pub fn squares_to(&self) -> Atom {
        match self {
            BasisElement::E0 => Atom::Number(0),
            BasisElement::E1 => Atom::Number(1),
            BasisElement::E2 => Atom::Number(1),
        }
    }
}

enum Commutitivity {
    Commutitive,
    AntiCommutitive,
}

impl Atom {
    fn commutitivity(&self, other: &Atom) -> Option<Commutitivity> {
        match (self, other) {
            (Atom::Number(_) | Atom::Variable(_), _) | (_, Atom::Number(_) | Atom::Variable(_)) => {
                Some(Commutitivity::Commutitive)
            }
            (Atom::Basis(a), Atom::Basis(b)) => {
                if a == b {
                    Some(Commutitivity::Commutitive)
                } else {
                    Some(Commutitivity::AntiCommutitive)
                }
            }
            (Atom::Group(_) | Atom::Fraction { .. }, _)
            | (_, Atom::Group(_) | Atom::Fraction { .. }) => None,
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, term) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            write!(f, "{term}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, atom) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, "*")?;
            }
            write!(f, "{atom}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Number(number) => write!(f, "{number}"),
            Atom::Variable(name) => write!(f, "{name}"),
            Atom::Basis(basis) => match basis {
                BasisElement::E0 => write!(f, "e0"),
                BasisElement::E1 => write!(f, "e1"),
                BasisElement::E2 => write!(f, "e2"),
            },
            Atom::Group(group) => write!(f, "({group})"),
            Atom::Fraction {
                numerator,
                denominator,
            } => write!(f, "(({numerator}) / ({denominator}))"),
        }
    }
}

pub fn simplify_expression(Expression(terms): Expression) -> Expression {
    let mut terms = if terms.is_empty() {
        vec![Term(vec![Atom::Number(0)])]
    } else {
        terms.into_iter().map(simplify_term).collect()
    };

    // Expand groups that are not multiplied by anything
    {
        let mut new_terms = vec![];
        terms.retain_mut(|Term(atoms)| {
            if atoms.len() == 1 {
                if let Atom::Group(Expression(terms)) = &mut atoms[0] {
                    new_terms.append(terms);
                    false
                } else {
                    true
                }
            } else {
                true
            }
        });
        terms.extend(new_terms);
    }

    fn are_like_terms(Term(a_atoms): &Term, Term(b_atoms): &Term) -> bool {
        let mut i = 0;
        let mut j = 0;
        loop {
            if i >= a_atoms.len() && j >= b_atoms.len() {
                break true;
            }
            if matches!(a_atoms.get(i), Some(Atom::Number(_))) {
                i += 1;
                continue;
            }
            if matches!(b_atoms.get(j), Some(Atom::Number(_))) {
                j += 1;
                continue;
            }
            if i >= a_atoms.len() || j >= b_atoms.len() {
                break false;
            }
            if a_atoms[i] != b_atoms[j] {
                break false;
            }
            i += 1;
            j += 1;
        }
    }

    // TODO: combine like terms
    {
        let mut i = 0;
        while i < terms.len() {
            let mut like_terms = vec![];

            let mut j = i + 1;
            while j < terms.len() {
                if are_like_terms(&terms[i], &terms[j]) {
                    like_terms.push(terms.remove(j));
                } else {
                    j += 1;
                }
            }

            if !like_terms.is_empty() {
                let mut total_scalar = 1;
                terms[i].0.retain(|atom| {
                    if let &Atom::Number(value) = atom {
                        total_scalar *= value;
                        false
                    } else {
                        true
                    }
                });
                for like_term in like_terms {
                    let mut scalar = 1;
                    for atom in &like_term.0 {
                        if let &Atom::Number(value) = atom {
                            scalar *= value;
                        }
                    }
                    total_scalar += scalar;
                }
                terms[i].0.push(Atom::Number(total_scalar));
                if total_scalar != 0 {
                    terms[i] = simplify_term(Term(std::mem::take(&mut terms[i].0)));
                } else {
                    terms.remove(i);
                    continue;
                }
            }
            i += 1;
        }
    }

    // TODO: pull common factors out

    terms.sort();
    Expression(terms)
}

pub fn simplify_term(Term(atoms): Term) -> Term {
    let mut atoms = atoms.into_iter().map(simplify_atom).collect::<Vec<_>>();

    // Expand groups
    if let Some(group_index) = atoms
        .iter()
        .enumerate()
        .find_map(|(index, atom)| matches!(atom, Atom::Group(_)).then_some(index))
    {
        let Atom::Group(Expression(mut terms)) = atoms.remove(group_index) else {
            unreachable!("group_index is the index of an Atom::Group, this cant fail");
        };

        // Do this in 2 loops so the multiplication order is preserved
        for atom in atoms.drain(..group_index).rev() {
            for term in &mut terms {
                term.0.insert(0, atom.clone());
            }
        }
        for atom in atoms {
            for term in &mut terms {
                term.0.push(atom.clone());
            }
        }

        atoms = vec![simplify_atom(Atom::Group(Expression(terms)))];
    }

    // Combine scalar values
    let mut scalar = 1;
    atoms.retain(|atom| {
        if let &Atom::Number(number) = atom {
            scalar *= number;
            false
        } else {
            true
        }
    });
    if scalar != 1 || atoms.is_empty() {
        atoms.insert(0, Atom::Number(scalar));
    }

    let mut changed = false;

    let mut sorted = false;
    let mut sign_flipped = false;
    while !sorted {
        sorted = true;
        for i in 1..atoms.len() {
            let [ref mut a, ref mut b, ..] = atoms[i - 1..] else {
                unreachable!();
            };

            if a > b {
                if let Some(commutitivity) = a.commutitivity(b) {
                    sorted = false;
                    changed = true;

                    std::mem::swap(a, b);
                    match commutitivity {
                        Commutitivity::Commutitive => {}
                        Commutitivity::AntiCommutitive => sign_flipped = !sign_flipped,
                    }
                }
            }
        }
    }
    if sign_flipped {
        changed = true;
        atoms.insert(0, Atom::Number(-1));
    }

    let mut combined = false;
    while !combined {
        combined = true;
        let mut i = 0;
        while i < atoms.len() {
            if let [Atom::Basis(a), Atom::Basis(b), ..] = atoms[i..] {
                if a == b {
                    combined = false;
                    changed = true;

                    atoms.remove(i);
                    atoms[i] = a.squares_to();
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
    }

    if changed {
        simplify_term(Term(atoms))
    } else {
        Term(atoms)
    }
}

pub fn simplify_atom(atom: Atom) -> Atom {
    match atom {
        Atom::Number(number) => Atom::Number(number),
        Atom::Variable(name) => Atom::Variable(name),
        Atom::Basis(basis) => Atom::Basis(basis),
        Atom::Group(Expression(mut terms)) => {
            if terms.len() == 1 && terms[0].0.len() == 1 {
                terms.remove(0).0.remove(0)
            } else {
                Atom::Group(simplify_expression(Expression(terms)))
            }
        }
        Atom::Fraction {
            numerator,
            denominator,
        } => {
            let numerator = simplify_expression(numerator);
            let denominator = simplify_expression(denominator);
            if denominator.0.len() == 1
                && denominator.0[0].0.len() == 1
                && denominator.0[0].0[0] == Atom::Number(1)
            {
                simplify_atom(Atom::Group(numerator))
            } else {
                Atom::Fraction {
                    numerator,
                    denominator,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stuff() {
        assert_eq!(
            simplify_expression(Expression(vec![
                Term(vec![Atom::Number(5)]),
                Term(vec![Atom::Number(6), Atom::Variable("x".into())]),
                Term(vec![
                    Atom::Group(Expression(vec![
                        Term(vec![Atom::Number(-2)]),
                        Term(vec![Atom::Number(3)])
                    ])),
                    Atom::Variable("x".into())
                ]),
                Term(vec![Atom::Variable("x".into())])
            ])),
            Expression(vec![
                Term(vec![Atom::Number(5)]),
                Term(vec![Atom::Number(8), Atom::Variable("x".into())]),
            ])
        );
    }
}
