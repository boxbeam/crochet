pub struct DelimitedList<T, D> {
    pub head: T,
    pub rest: Vec<(D, T)>,
}

impl<T, D> DelimitedList<T, D> {
    pub fn to_vec(self) -> Vec<T> {
        let mut vec = Vec::with_capacity(self.len());
        vec.push(self.head);
        vec.extend(self.rest.into_iter().map(|(_, t)| t));
        vec
    }

    pub fn len(&self) -> usize {
        self.rest.len() + 1
    }

    pub fn fold(self, combine: impl Fn(T, D, T) -> T) -> T {
        let mut accum = self.head;
        for (delim, elem) in self.rest {
            accum = combine(accum, delim, elem);
        }
        accum
    }

    pub fn push(&mut self, delim: D, elem: T) {
        self.rest.push((delim, elem))
    }
}

impl<T, D> From<T> for DelimitedList<T, D> {
    fn from(value: T) -> Self {
        Self {
            head: value,
            rest: vec![],
        }
    }
}

impl<T, D> IntoIterator for DelimitedList<T, D> {
    type Item = T;

    type IntoIter = impl Iterator<Item = T>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self.head).chain(self.rest.into_iter().map(|(_, t)| t))
    }
}
