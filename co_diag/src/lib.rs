#[macro_use]
extern crate lazy_static;

use std::{
    fmt,
    io::{stderr, Write},
    marker::PhantomData,
    mem,
    ops::Range,
    sync::Mutex,
};

lazy_static! {
    static ref OUTPUT: Mutex<Box<dyn Write + Send>> = Mutex::new(Box::new(stderr()));
}

/// This has a private field to force people to use associated functions to create an object.
#[derive(Debug)]
pub struct CompileError(());

pub struct CompileErrorBuilder<R = ()>(PhantomData<R>);

impl<R> Drop for CompileErrorBuilder<R> {
    fn drop(&mut self) {
        panic!("`CompileErrorBuilder` must be used by calling `build`");
    }
}

impl<R> CompileErrorBuilder<R> {
    pub fn with_location(self, span: Span) -> Self {
        let offset = span.line_offset();
        let pos = format!("{}:{}", span.line, offset);
        writeln!(OUTPUT.lock().unwrap(), "  --> {}:{}", span.file, pos).unwrap();
        writeln!(OUTPUT.lock().unwrap(), "({}): {}", pos, span.slice()).unwrap();
        for _ in 0..(offset as usize + pos.len() + 4) {
            write!(OUTPUT.lock().unwrap(), " ").unwrap();
        }
        for _ in 0..span.slice().chars().count().max(1) {
            write!(OUTPUT.lock().unwrap(), "^").unwrap();
        }
        writeln!(OUTPUT.lock().unwrap()).unwrap();
        self
    }

    pub fn with_help<D: fmt::Display>(self, help: D) -> Self {
        writeln!(OUTPUT.lock().unwrap(), "  help: {}", help).unwrap();
        self
    }

    pub fn build(self) -> Result<R, CompileError> {
        mem::forget(self);
        Err(CompileError(()))
    }
}

impl CompileError {
    pub fn new<R, D: fmt::Display>(span: Span, err: D) -> Result<R, Self> {
        Self::build(span, err).build()
    }

    pub fn without_location<R, D: fmt::Display>(err: D) -> Result<R, Self> {
        writeln!(OUTPUT.lock().unwrap(), "[ERROR]: {}", err).unwrap();
        Err(CompileError(()))
    }

    pub fn build<R, D: fmt::Display>(span: Span, err: D) -> CompileErrorBuilder<R> {
        writeln!(OUTPUT.lock().unwrap(), "[ERROR]: {}", err).unwrap();
        CompileErrorBuilder(PhantomData).with_location(span)
    }

    pub fn expected<R>(expected: &dyn fmt::Debug, span: Span) -> Result<R, Self> {
        Self::new(
            span,
            format_args!("Expected {:?} found `{}`", expected, span.slice()),
        )
    }

    pub fn set_output(output: Box<dyn Write + Send>) {
        *OUTPUT.lock().unwrap() = output;
    }
}

#[derive(Copy, Clone)]
struct CRange {
    start: usize,
    end: usize,
}

impl From<Range<usize>> for CRange {
    fn from(r: Range<usize>) -> CRange {
        CRange {
            start: r.start,
            end: r.end,
        }
    }
}

// A wrapper storing metadata
#[derive(Clone, Copy)]
pub struct Span<'a> {
    slice: CRange,
    pub file: &'a str,
    pub source: &'a str,
    pub line: u32,
}

impl<'a> Span<'a> {
    pub fn new(source: &'a str, file: &'a str, line: u32, slice: Range<usize>) -> Self {
        Span {
            slice: slice.into(),
            file,
            source,
            line,
        }
    }

    pub fn append(self, other: Self) -> Self {
        assert_eq!(self.file, other.file);
        Span {
            slice: (self.slice.start..other.slice.end).into(),
            file: self.file,
            source: self.source,
            line: self.line,
        }
    }

    pub fn fake() -> Span<'a> {
        Span {
            slice: (0..11).into(),
            file: "<fake file>",
            source: "<fake span>",
            line: 0,
        }
    }

    pub fn slice(self) -> &'a str {
        &self.source[self.slice.start..self.slice.end]
    }

    pub fn extend_left(self, to: char) -> Self {
        let target = self.source[..self.slice.start].rfind(to).unwrap();
        Self {
            slice: (target..self.slice.end).into(),
            ..self
        }
    }

    pub fn extend_right(self, to: char) -> Self {
        let target = self.source[self.slice.end..].find(to).unwrap();
        Self {
            slice: (self.slice.start..self.slice.end + target + to.len_utf8()).into(),
            ..self
        }
    }

    pub fn line_offset(&self) -> u32 {
        let mut offset = 0;
        for c in self.source[..self.slice.start].chars().rev() {
            if c == '\n' {
                break;
            } else {
                offset += 1;
            }
        }

        offset
    }

    pub fn line_str(&self) -> &str {
        let line_start = self.source[..self.slice.start]
            .rfind('\n')
            .map_or(0, |v| v + 1);
        self.source[line_start..].split('\n').next().unwrap()
    }
}

impl<'a> fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("span", &self.slice())
            .field("file", &self.file)
            .field("line", &self.line)
            .field("culumn", &self.line_offset())
            .finish()
    }
}
