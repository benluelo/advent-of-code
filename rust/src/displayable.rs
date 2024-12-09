use core::marker::PhantomData;

use crate::utils::{array::ArrayVec, utf8};

pub struct Displayable<T: ConstDisplayable> {
    t: PhantomData<fn() -> T>,
    output: T::Output,
}

pub trait ConstDisplayable {
    type Output;
}

macro_rules! displayable_num {
    ($($T:ty)+) => {
        $(
            impl Displayable<$T> {
                #[must_use]
                pub const fn new(t: $T) -> Self {
                    Self {
                        t: PhantomData,
                        output: $crate::utils::Num(t).to_str(),
                    }
                }

                #[must_use]
                pub const fn as_str(&self) -> &str {
                    self.output.as_str()
                }
            }

            impl ConstDisplayable for $T {
                type Output = $crate::utils::array::ArrayVec<u8, { $crate::utils::Num::<$T>::STR_LEN }>;
            }
        )+
    };
}
displayable_num!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize);

#[cfg(not(feature = "const"))]
impl ConstDisplayable for alloc::string::String {
    type Output = alloc::string::String;
}

#[cfg(not(feature = "const"))]
impl Displayable<alloc::string::String> {
    #[must_use]
    pub const fn new(t: alloc::string::String) -> Self {
        Self {
            t: PhantomData,
            output: t,
        }
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.output
    }
}

impl<'a> ConstDisplayable for &'a str {
    type Output = &'a str;
}

impl<'a> Displayable<&'a str> {
    #[must_use]
    pub const fn new(t: &'a str) -> Self {
        Self {
            t: PhantomData,
            output: t,
        }
    }

    #[must_use]
    pub const fn as_str(&self) -> &str {
        self.output
    }
}

impl<const N: usize> ConstDisplayable for ArrayVec<u8, N> {
    type Output = ArrayVec<u8, N>;
}

impl<const N: usize> Displayable<ArrayVec<u8, N>> {
    #[must_use]
    pub const fn new(t: ArrayVec<u8, N>) -> Self {
        Self {
            t: PhantomData,
            output: t,
        }
    }

    #[must_use]
    pub const fn as_str(&self) -> &str {
        utf8(self.output.as_slice())
    }
}
