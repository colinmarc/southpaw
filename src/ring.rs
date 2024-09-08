use std::collections::VecDeque;

pub(crate) struct Ring<T> {
    buf: VecDeque<T>,
    write_offset: usize,
    min_read_offset: usize,
}

pub(crate) struct RingRangeIterator<'a, T> {
    ring: &'a Ring<T>,
    off: usize,
    end: usize,
}

impl<'a, T> Iterator for RingRangeIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.off >= self.end {
            None
        } else {
            let index = self.off - self.ring.min_read_offset;
            self.off += 1;
            self.ring.buf.get(index)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
}

impl<T> ExactSizeIterator for RingRangeIterator<'_, T> {
    fn len(&self) -> usize {
        self.end - self.off
    }
}

impl<T> Ring<T> {
    pub(crate) fn new() -> Self {
        Self {
            buf: VecDeque::new(),
            write_offset: 0,
            min_read_offset: 0,
        }
    }

    /// Pushes an element onto the back of the ring.
    pub(crate) fn push_back(&mut self, value: T) {
        self.buf.push_back(value);
        self.write_offset += 1;
    }

    /// Extends the ring with the contents of an iterator.
    pub(crate) fn extend(&mut self, values: impl IntoIterator<Item = T>) {
        let len = self.buf.len();
        self.buf.extend(values);
        self.write_offset += self.buf.len() - len;
    }

    /// Returns an iterator over the available elements starting at offset
    /// p. If elements are pushed onto the back of the ring before iteration
    /// is complete, those elements will not be returned by the iterator.
    pub(crate) fn iter_from(&self, p: usize) -> RingRangeIterator<'_, T> {
        assert!(
            p >= self.min_read_offset && p <= self.write_offset,
            "Iterator start offset out of bounds"
        );

        RingRangeIterator {
            ring: self,
            off: p,
            end: self.write_offset,
        }
    }

    /// Discards elements of the ring such that position p is the earliest
    /// stored element, freeing up space for future writes. After a call to
    /// `discard_until(p)`, reads from an index less than `p` will panic.
    pub(crate) fn discard_until(&mut self, p: usize) {
        assert!(
            p >= self.min_read_offset && p <= self.write_offset,
            "Discard position out of bounds"
        );

        drop(self.buf.drain(..p - self.min_read_offset));
        self.min_read_offset = p;
    }

    pub(crate) fn current_offset(&self) -> usize {
        self.write_offset
    }
}

impl<T> Default for Ring<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iter_from() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        assert_eq!(ring.iter_from(0).len(), 3);
        assert_eq!(ring.iter_from(0).collect::<Vec<_>>(), vec![&1, &2, &3]);
        assert_eq!(ring.iter_from(2).len(), 1);
        assert_eq!(ring.iter_from(2).collect::<Vec<_>>(), vec![&3]);
        assert_eq!(ring.iter_from(3).len(), 0);
    }

    #[test]
    fn test_discard_until() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        ring.discard_until(2);
        assert_eq!(ring.buf.len(), 1);
        assert_eq!(ring.min_read_offset, 2);
        assert_eq!(ring.iter_from(2).collect::<Vec<_>>(), vec![&3]);

        ring.push_back(4);
        ring.push_back(5);

        assert_eq!(ring.iter_from(2).collect::<Vec<_>>(), vec![&3, &4, &5]);

        ring.discard_until(4);
        assert_eq!(ring.buf.len(), 1);
        assert_eq!(ring.min_read_offset, 4);

        let iter: Vec<_> = ring.iter_from(4).collect();
        assert_eq!(iter, vec![&5]);
    }

    #[test]
    fn test_discard_until_while_iterating() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);
        ring.push_back(4);
        ring.push_back(5);

        let mut iter = ring.iter_from(2);
        assert_eq!(iter.next(), Some(&3));

        ring.discard_until(3);
        assert_eq!(ring.iter_from(3).collect::<Vec<_>>(), vec![&4, &5]);
    }

    #[test]
    #[should_panic(expected = "Discard position out of bounds")]
    fn test_discard_out_of_bounds() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        ring.discard_until(4); // Should panic
    }

    #[test]
    #[should_panic(expected = "Discard position out of bounds")]
    fn test_discard_out_of_bounds2() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        ring.discard_until(2);
        ring.discard_until(1);
    }

    #[test]
    #[should_panic(expected = "Iterator start offset out of bounds")]
    fn test_iter_from_out_of_bounds() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        ring.iter_from(4); // Should panic
    }

    #[test]
    #[should_panic(expected = "Iterator start offset out of bounds")]
    fn test_iter_from_start_discarded() {
        let mut ring = Ring::new();
        ring.push_back(1);
        ring.push_back(2);
        ring.push_back(3);

        ring.discard_until(2);

        ring.iter_from(0); // Should panic
    }
}
