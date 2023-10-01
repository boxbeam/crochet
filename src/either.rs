pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> From<A> for Either<A, B> {
    fn from(value: A) -> Self {
        Self::A(value)
    }
}

impl<A, B> From<B> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn from(value: B) -> Self {
        Self::B(value)
    }
}

pub trait MaybeInto<T> {
    fn maybe_into(self) -> Option<T>;
}

impl<T> MaybeInto<T> for T {
    fn maybe_into(self) -> Option<T> {
        Some(self)
    }
}

pub auto trait HeterogenousEither {}
#[allow(suspicious_auto_trait_impls)]
impl<T> !HeterogenousEither for Either<T, T> {}

impl<A: MaybeInto<C>, B, C> MaybeInto<C> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn maybe_into(self) -> Option<C> {
        match self {
            Either::A(a) => a.maybe_into(),
            _ => None,
        }
    }
}

impl<A, B: MaybeInto<C>, C> MaybeInto<C> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn maybe_into(self) -> Option<C> {
        match self {
            Either::B(b) => b.maybe_into(),
            _ => None,
        }
    }
}
