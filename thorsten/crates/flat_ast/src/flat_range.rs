use std::ops::Index;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    #[inline(always)]
    pub fn new(start: usize, end: usize) -> Range {
        Range { start, end }
    }

    #[inline(always)]
    pub fn merge(start: &Range, end: &Range) -> Range {
        Range { start: start.start, end: end.end }
    }

    #[inline(always)]
    pub fn slice(&self, left: usize, right: usize) -> Range {
        Range {
            start: self.start + left,
            end: self.end - right,
        }
    }
}

impl Index<Range> for str {
    type Output = str;

    #[inline(always)]
    fn index(&self, index: Range) -> &Self::Output {
        &self[index.start..index.end]
    }
}

impl Index<&Range> for str {
    type Output = str;

    #[inline(always)]
    fn index(&self, index: &Range) -> &Self::Output {
        &self[index.start..index.end]
    }
}
