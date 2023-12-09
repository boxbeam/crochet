#[derive(Clone)]
pub struct Cursor<'a> {
    cur: &'a str,
    original: &'a str,
}

impl<'a> From<&'a str> for Cursor<'a> {
    fn from(value: &'a str) -> Self {
        Cursor {
            cur: value,
            original: value,
        }
    }
}

impl<'a> Cursor<'a> {
    /// Returns the position of this cursor within the original input
    pub fn position(&self) -> usize {
        self.original.len() - self.cur.len()
    }

    /// Gets the current slice this cursor is operating on
    pub fn str(&self) -> &str {
        self.cur
    }

    /// Addvance this cursor by a set amount, returning the slice that was consumed. Will not go past the end of the input.
    pub fn advance(&mut self, len: usize) -> &str {
        let len = len.min(self.cur.len());
        let (advanced, rest) = self.cur.split_at(len);
        self.cur = rest;
        advanced
    }

    /// Look at a slice with a specific byte length without advancing. Will not look past the end of the input.
    pub fn peek(&mut self, len: usize) -> &str {
        let len = len.min(self.cur.len());
        &self.cur[len..]
    }

    /// Return the original input
    pub fn input(&self) -> &str {
        self.original
    }

    /// Look at the next character in the input
    pub fn peek_char(&mut self) -> Option<char> {
        self.cur.chars().next()
    }

    /// Advance past the next character in the input
    pub fn advance_char(&mut self) -> Option<char> {
        let c = self.peek_char();
        if let Some(c) = c {
            self.advance(c.len_utf8());
        }
        c
    }
}
