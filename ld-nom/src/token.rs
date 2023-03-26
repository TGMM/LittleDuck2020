use crate::ast::Token;
use nom::{Input, InputLength, Needed};
use std::iter::Enumerate;

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> Input for Tokens<'a> {
    // InputTake
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }

    // InputLength
    #[inline]
    fn input_len(&self) -> usize {
        1
    }

    // InputIter
    type Item = &'a Token;
    type Iter = ::std::slice::Iter<'a, Token>;
    type IterIndices = Enumerate<::std::slice::Iter<'a, Token>>;

    #[inline]
    fn iter_indices(&self) -> Self::IterIndices {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::Iter {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }

    // Slice?
    fn take_from(&self, index: usize) -> Self {
        let tok = &self.tok[index..];
        Tokens {
            tok,
            start: 0,
            end: tok.len(),
        }
    }
}
