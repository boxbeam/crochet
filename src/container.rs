use std::collections::VecDeque;

pub trait Container<T>: Default {
    fn add(&mut self, data: T);
}

impl<T> Container<T> for Vec<T> {
    fn add(&mut self, data: T) {
        self.push(data);
    }
}

impl<T> Container<T> for VecDeque<T> {
    fn add(&mut self, data: T) {
        self.push_back(data);
    }
}

impl<T> Container<T> for () {
    fn add(&mut self, _data: T) {}
}
